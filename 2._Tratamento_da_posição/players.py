# players_scraper.py
# -*- coding: utf-8 -*-

import time
import random
import requests
import pandas as pd
from typing import List, Dict, Any, Optional

# URL base com season_id explícito na query string
BASE_URL = "https://transfermarkt-api.fly.dev/clubs/{club_id}/players?season_id={season_id}"
SEASON_ID = 2023

# >>> COLOQUE AQUI os 20 IDs dos clubes <<<
CLUB_IDS: List[int] = [2462, 10870, 210, 6600, 10492, 1023, 8793, 585, 978, 2125]  # ex.: [679, 680, 681, ..., 698]

# Alguns User-Agents "de navegador" para rotacionar e reduzir bloqueio
USER_AGENTS = [
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0 Safari/537.36",
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:124.0) Gecko/20100101 Firefox/124.0",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.3 Safari/605.1.15",
]

def choose_excel_engine() -> str:
    """
    Retorna 'xlsxwriter' se instalado; caso contrário tenta 'openpyxl'.
    Lança erro explicativo se nenhum estiver disponível.
    """
    try:
        import xlsxwriter  # noqa: F401
        return "xlsxwriter"
    except Exception:
        try:
            import openpyxl  # noqa: F401
            return "openpyxl"
        except Exception:
            raise RuntimeError(
                "Nenhum engine Excel disponível. Instale um deles: "
                "`pip install xlsxwriter` (recomendado) ou `pip install openpyxl`."
            )

def safe_join_nationalities(nats: Any) -> str:
    """Converte lista de nacionalidades em 'A; B; C'. Se vier string, normaliza; senão, vazio."""
    if isinstance(nats, list):
        parts = [str(x).strip() for x in nats if isinstance(x, str) and str(x).strip()]
        return "; ".join(parts)
    if isinstance(nats, str):
        return nats.strip()
    return ""

def respectful_sleep(base_seconds: float = 0.6, jitter: float = 0.8):
    """Pausa com jitter para reduzir risco de rate-limit/bloqueio."""
    time.sleep(base_seconds + random.random() * jitter)

def fetch_with_retries(
    session: requests.Session,
    url: str,
    params: Optional[dict] = None,
    max_retries: int = 6,
    base_backoff: float = 1.6,
    timeout: tuple = (15, 40),
) -> Optional[dict]:
    """
    Faz GET com retries. Em 403/429/5xx aplica backoff exponencial + jitter.
    Respeita Retry-After se presente.
    """
    last_err = None
    for attempt in range(1, max_retries + 1):
        try:
            # Rotaciona User-Agent a cada tentativa
            session.headers["User-Agent"] = random.choice(USER_AGENTS)
            r = session.get(url, params=params, timeout=timeout)
            status = r.status_code

            if status == 200:
                return r.json()

            if status in (403, 429) or 500 <= status < 600:
                retry_after = r.headers.get("Retry-After")
                if retry_after:
                    try:
                        wait = float(retry_after)
                    except ValueError:
                        wait = (base_backoff ** attempt) + random.uniform(0, 1.5)
                else:
                    wait = (base_backoff ** attempt) + random.uniform(0, 1.5)
                print(f"[AVISO] HTTP {status} em {url} (tentativa {attempt}/{max_retries}). Aguardando {wait:.1f}s…")
                time.sleep(wait)
                continue

            # Outros status: falhar rápido
            r.raise_for_status()

        except (requests.RequestException, ValueError) as e:
            last_err = e
            wait = (base_backoff ** attempt) + random.uniform(0, 1.3)
            print(f"[AVISO] Erro '{e}' (tentativa {attempt}/{max_retries}). Aguardando {wait:.1f}s…")
            time.sleep(wait)

    print(f"[ERRO] Falha após {max_retries} tentativas: {last_err}")
    return None

def fetch_players_for_club(club_id: int, season_id: int, session: requests.Session) -> List[Dict[str, Any]]:
    """Busca o plantel do clube e retorna linhas com colunas de interesse."""
    url = BASE_URL.format(club_id=club_id, season_id=season_id)

    data = fetch_with_retries(session, url, params=None)
    if not data:
        print(f"[AVISO] Sem dados para club_id={club_id}.")
        return []

    players = data.get("players", []) or []
    rows: List[Dict[str, Any]] = []
    for p in players:
        rows.append({
            "club_id": str(data.get("id", str(club_id))),
            "season_id": season_id,
            "player_id": str(p.get("id", "")),
            "name": (p.get("name") or "").strip(),
            "position": (p.get("position") or "").strip(),
            "age": p.get("age", None),
            "nationality": safe_join_nationalities(p.get("nationality")),
        })
    return rows

def main():
    session = requests.Session()
    # Cabeçalhos “de navegador” para reduzir chance de bloqueio
    session.headers.update({
        "Accept": "application/json, text/plain, */*",
        "Accept-Language": "pt-BR,pt;q=0.9,en-US;q=0.8,en;q=0.7",
        "Referer": "https://transfermarkt-api.fly.dev/",
        "Connection": "keep-alive",
    })

    all_rows: List[Dict[str, Any]] = []
    total = len(CLUB_IDS)
    for i, club_id in enumerate(CLUB_IDS, start=1):
        print(f"[INFO] Buscando clube {club_id} ({i}/{total})…")
        rows = fetch_players_for_club(club_id, SEASON_ID, session)
        all_rows.extend(rows)
        respectful_sleep()  # pequeno throttle entre clubes

    if not all_rows:
        print("Nenhum dado retornado. A API pode estar limitando seu IP temporariamente.")
        return

    df = pd.DataFrame(
        all_rows,
        columns=["club_id", "season_id", "player_id", "name", "position", "age", "nationality"]
    )

    # Limpeza/normalização leve
    for col in ["name", "position", "nationality"]:
        df[col] = df[col].fillna("").astype(str).str.strip()
    df["age"] = pd.to_numeric(df["age"], errors="coerce").astype("Int64")

    # Remove duplicatas por (club_id, season_id, player_id)
    df = df.drop_duplicates(subset=["club_id", "season_id", "player_id"]).reset_index(drop=True)

    # Salva em XLSX (engine auto)
    engine = choose_excel_engine()
    out_path = "players_positions.xlsx"
    with pd.ExcelWriter(out_path, engine=engine) as writer:
        df.to_excel(writer, sheet_name="players", index=False)

    print(f"OK! Gerado {out_path} com {len(df)} linhas (engine: {engine}).")

if __name__ == "__main__":
    main()

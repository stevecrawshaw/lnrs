import duckdb
import os

def get_env_vars():
    env_vars = {}
    with open(".env") as f:
        for line in f:
            if line.strip() and not line.startswith("#"):
                key, value = line.strip().split("=", 1)
                env_vars[key.strip()] = value.strip().strip('"')
    return env_vars

env_vars = get_env_vars()

motherduck_token = env_vars.get("motherduck_token")
motherduck_share_link = env_vars.get("motherduck_share_link")

if not motherduck_token or not motherduck_share_link:
    print("Error: motherduck_token or motherduck_share_link not found in .env file")
else:
    try:
        # Connect to MotherDuck
        con = duckdb.connect(f"md:lnrs_weca?motherduck_token={motherduck_token}")
        print("Successfully connected to MotherDuck!")
        con.close()
    except Exception as e:
        print(f"Failed to connect to MotherDuck: {e}")

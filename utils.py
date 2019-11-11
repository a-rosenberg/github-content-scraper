import os

import database
import configure

CACHE_PATH = os.path.join(os.path.dirname(__file__), 'cache')

with database.SqliteConnection(configure.db_path) as conn:
    cursor = conn.cursor()
    rows = database.query(cursor, "SELECT * FROM content;")

for row in rows:
    url, username, repo, path, content, added = row

    cleaned_path = path.replace('/', '__')
    full_path = os.path.join(
        os.path.abspath(CACHE_PATH),
        f'{username}|{repo}|{cleaned_path}',
    )

    with open(full_path, 'w') as fid:
        fid.write(content)

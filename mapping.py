import os
import logging
import collections

import database

logging.basicConfig(level=logging.INFO)

cache_dir = os.path.join(os.path.dirname(__file__), 'cache')
database_path = os.path.join(os.path.dirname(__file__), 'content.sqlite')


with database.SqliteConnection(db_path=database_path) as conn:
    cursor = conn.cursor()
    contents = database.database_table_to_pandas_table(cursor, 'content')


mapping = dict()
for filename in os.listdir(cache_dir):

    try:
        username, repo, path = filename.split('|')
    except ValueError:
        logging.warning('can not unpack %s; skipping ...', filename)
        continue

    path = path.replace('__', '/')

    user_match = (contents['username'].str.lower().eq(username.lower()))
    repo_match = (contents['repo'].str.lower().eq(repo.lower()))
    path_match = (contents['path'].str.lower().eq(path.lower()))
    rmd_path_match = (contents['path'].str.lower().eq(path.lower() + 'md'))

    match_all = user_match & repo_match & path_match

    if sum(match_all) < 1:
        match_all = user_match & repo_match & rmd_path_match

    if sum(match_all) != 1:
        raise ValueError(f'something broke with {filename}')

    contents.loc[match_all, 'on_disk'] = filename


contents.to_csv('mapping.csv')

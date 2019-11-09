import logging
import sqlite3
import os
import pandas as pd
import shutil
import datetime

from api import content_scraper
import database as db
import utils


def load_links(urls_path: str) -> pd.DataFrame:
    """Load links DataFrame after some light parsing and cleaning

    Args:
        urls_path: System path where target links CSV lives.

    Returns:
        Cleaned Links DataFrame.
    """
    df = pd.read_csv(urls_path)
    df.dropna(subset=['processed_url'], inplace=True)
    df.drop_duplicates(subset=['processed_url'], inplace=True)

    url_col = 'processed_url'
    df['username'] = df[url_col].apply(lambda x: utils.extract_github_account_info(x).username)
    df['repo'] = df[url_col].apply(lambda x: utils.extract_github_account_info(x).repo)
    df['path'] = df[url_col].apply(lambda x: utils.extract_github_account_info(x).path)

    good_rows = ((df[url_col].str.contains('github.com'))
                 & (~df[url_col].str.contains('gist'))
                 & (~df['repo'].isna()))
    df = df.loc[good_rows]

    df.reset_index(inplace=True)

    return df


if __name__ == '__main__':
    """
    Iterates through Links DataFrame and scrapes GitHub files or repositories
    into a Sqlite3 Database located at `database.DB_PATH`.
    
    Notes:
        - Will create the Database if it does not exist.
    """
    logging.basicConfig(level=logging.INFO)

    ITER_LIMIT = 25
    SELECT_EXTENSIONS = ['.rmd', '.R']
    REPLACE_DB = True

    urls_path = os.path.join(os.path.dirname(__file__), 'data', 'processed_data.csv')
    links = load_links(urls_path)
    links.to_csv(os.path.join(os.path.dirname(__file__), 'data', 'links.csv'), index=False)

    if REPLACE_DB:
        logging.warning('Removing Database: %s', db.DB_PATH)
        os.remove(db.DB_PATH)

    with db.SqliteConnection(db.DB_PATH) as conn:
        cursor = conn.cursor()
        db.create_table(cursor, definition=db.GIT_CONTENT_DEFINITION)

        for ix, link in links.iterrows():

            scraper = content_scraper(
                username=link['username'],
                repo=link['repo'],
                path=link['path'],
                iter_limit=ITER_LIMIT,
                select_extensions=SELECT_EXTENSIONS,
            )

            for data in scraper:
                logging.info('(item: %s) inserting %s/%s/%s', ix, data['username'], data['repo'], data['path'])

                data.update({'url': link['processed_url']})

                try:
                    db.insert(cursor, table='content', data=data)
                    conn.commit()

                except sqlite3.IntegrityError:
                    logging.warning('skipping!... github object already in database')

    shutil.copy(
        src=db.DB_PATH,
        dst=os.path.join(
            os.path.dirname(__file__),
            os.path.basename(db.DB_PATH) + datetime.datetime.now().strftime('_%Y%m%d_%H%M%S')
        )
    )
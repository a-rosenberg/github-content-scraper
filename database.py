"""
Tools to create, manage and interact with Sqlite3 Database
"""
import sqlite3
import logging

import pandas as pd

from typing import List, Dict
from sqlite3 import Error as SqliteError

import configure


GIT_CONTENT_DEFINITION = ('CREATE TABLE IF NOT EXISTS content ('
                          'url TEXT,'
                          'username TEXT, '
                          'repo TEXT, '
                          'path TEXT, '
                          'content TEXT, '
                          'date_added DATETIME DEFAULT CURRENT_TIMESTAMP, ' 
                          'PRIMARY KEY (username, repo, path)'
                          ');')


class SqliteConnection(object):
    def __init__(self, db_path: str):
        """Create a database connection to a SQLite database
        Args:
            db_path: System path to current or desired DB file.
        """
        try:
            self.conn = sqlite3.connect(db_path)
        except SqliteError as e:
            logging.exception('Caught error during connection creation: %s', repr(e))

    def __enter__(self) -> sqlite3.Connection:
        return self.conn

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.conn.close()


def create_table(cursor: sqlite3.Cursor, definition: str) -> None:
    try:
        cursor.execute(definition)
    except SqliteError as e:
        logging.exception('Caught error during table creation: %s', repr(e))


def drop_table(cursor: sqlite3.Cursor, table_name: str) -> None:
    try:
        cursor.execute(f'DROP TABLE {table_name};')
    except SqliteError as e:
        logging.exception('Caught error during table creation: %s', repr(e))


def query(cursor: sqlite3.Cursor, command: str) -> List:
    try:
        cursor.execute(command)
        return cursor.fetchall()
    except SqliteError as e:
        logging.exception('Caught error during query: %s', repr(e))


def insert(cursor: sqlite3.Cursor, table: str, data: Dict) -> None:
    keys = ', '.join([str(x) for x in data.keys()])

    values = list(data.values())
    values_placeholders = ', '.join(['?'] * len(values))

    command = f'INSERT INTO {table} ({keys}) VALUES ({values_placeholders});'

    cursor.execute(command, values)

def database_table_to_pandas_table(cursor: sqlite3.Cursor, table_name: str) -> pd.DataFrame:
    cursor.execute(f'PRAGMA table_info({table_name});')
    column_names = [x[1] for x in cursor.fetchall()]

    cursor.execute(f'SELECT * FROM {table_name};')
    values = cursor.fetchall()

    data = {column: [k[ix] for k in values] for ix, column in enumerate(column_names)}
    return pd.DataFrame(data)


if __name__ == '__main__':
    with SqliteConnection(configure.db_path) as conn:
        cur = conn.cursor()
        create_table(cur, GIT_CONTENT_DEFINITION)
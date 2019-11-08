import logging

from api import content_scraper
import database as db


if __name__ == '__main__':
    logging.basicConfig(level=logging.INFO)

    with db.SqliteConnection(db.DB_PATH) as conn:
        cursor = conn.cursor()
        db.create_table(cursor, definition=db.GIT_CONTENT_DEFINITION)

        for data in content_scraper(username='a-rosenberg', repo='cookbook', select_extenstions=['.md']):

            logging.info('inserting %s/%s/%s',
                         data['username'],
                         data['repo'],
                         data['path'])

            db.insert(cursor, table='content', data=data)
            conn.commit()



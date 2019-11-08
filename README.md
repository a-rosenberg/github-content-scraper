# Github Repository Content Scraper

## Setup
- There are no library requirements outside of base Python 3.7.
- Create a file in the base of the repository named `ignore.py`.  Add a `client_id` and `secret_id` variable
containing Oauth credentials created via GitHub account: https://developer.github.com/apps/building-oauth-apps/creating-an-oauth-app/

## Run
- Call `python __main__.py`
- This will create a database named `content.db` or whatever variable is set to in the path at `database.DB_PATH`

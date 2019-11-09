# GitHub Repository Content Scraper

* Data collection process for Tidy Tuesday code on GitHub

## Setup
- Requires base Python 3.7 and a modern version of the pandas library (`pip install pandas`).
- Create a file in the base of the repository named `ignore.py`.  Add a `client_id` and `secret_id` variable
containing Oauth credentials created via GitHub account: https://developer.github.com/apps/building-oauth-apps/creating-an-oauth-app/

## Run
- Call `python run.py`
- This will create a database named `content.db` or whatever variable is set to in the path at `database.DB_PATH`

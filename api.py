"""
API crawling tools

Notes:
    Assumes there is a ignore.py module on the same level containing a client_id and client_secret
    variable to configure the API call.  Follow these instrucitons to get a key:
    https://developer.github.com/apps/building-oauth-apps/creating-an-oauth-app/.
"""
import logging
import requests
import base64
import time
import copy
import os

from typing import Dict, List, Optional, Sequence
from pprint import pprint

import ignore


def content_scraper(username: str,
                    repo: str,
                    path: Optional[str] = None,
                    sleep: float = 2,
                    iter_start: int = 0,
                    iter_limit: int = 100,
                    select_extenstions: Optional[Sequence] = None) -> List[Dict]:
    """Recursively scrape content from Github Repository

    Args:
        username: Target GitHub repository owner.
        repo: GitHub repository name.
        path: Optional path to a starting point within a repository.  Used
            to allow recursion for crawling directory structures. Defaults
            to None.
        sleep: Time to sleep between API requests, not accounting for code
            execution time.
        iter_start: Start counter for recursion iteration limitation.
        iter_limit: Maximum number of files to read per repository.
        select_extenstions: Sequence of file extensions to be returned.
            File types not in the sequence will not be yielded.

    Yields:
        Mapping of Github repository object path names and their contents.
    """
    counter = copy.copy(iter_start)

    for item in get_api_response(username, repo, path):

        _type = item['type']
        _path = item['path']
        _ext = os.path.splitext(_path)[-1]

        if _type == 'file':

            if _ext in select_extenstions if select_extenstions else True:

                response = get_api_response(username, repo, path=_path)
                file_content = parse_content(response)

                yield {
                    'path': _path,
                    'username': username,
                    'repo': repo,
                    'content': file_content,
                }
                counter += 1

            else:
                logging.info('file %s skipped because extension type: %s', _path, _ext)

        elif _type == 'dir':
            children = content_scraper(
                username=username,
                repo=repo,
                path=_path,
                iter_start=counter,
                iter_limit=iter_limit,
            )

            for child in children:
                yield child

        else:
            continue

        time.sleep(sleep)
        if counter >= iter_limit:
            logging.warning('limit of files per repo reached: %s', iter_limit)
            break


def get_api_response(username: str,
                     repo: str,
                     path: Optional[str] = None,
                     client_id: Optional[str] = ignore.client_id,
                     client_secret: Optional[str] = ignore.secret_key) -> Optional[Dict]:
    """Wrapper for Github Content API request

    Args:
        username: Target GitHub repository owner.
        repo: GitHub repository name.
        path: Optional path to a starting point within a repository.  Used
            to allow recursion for crawling directory structures. Defaults
            to None.
        client_id:
        client_secret:

    Returns:
        Python internal representation of API JSON response.
    """
    _path = path if path else ''

    if client_id and client_secret:
        api_url = (f'https://api.github.com/repos/'
                   f'{username}/{repo}/contents/{_path}'
                   f'?client_id={client_id}&client_secret={client_secret}')
    else:
        api_url = f'https://api.github.com/repos/{username}/{repo}/contents/{_path}'

    print(api_url)
    data = request_json(api_url)
    return data


def request_json(url: str) -> Dict:
    """Wrapper for HTML request to a JSON endpoint.

    Args:
        url: Valid and formed web address for API.

    Returns:
        API Response as JSON.
    """
    raw_ = requests.get(url)
    return raw_.json()


def parse_content(response: Dict) -> str:
    """Strips

    Args:
        response:

    Returns:

    """
    try:
        content = response['content']
    except KeyError:
        pprint(response)
        raise RuntimeError("'content' not a key in response JSON")

    return str(decode(content))


def decode(x: str) -> str:
    return base64.b64decode(x)


if __name__ == '__main__':
    data = content_scraper(username='a-rosenberg', repo='cookbook')

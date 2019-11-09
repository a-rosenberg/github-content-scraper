"""
API Scraping tools

Notes:
    Assumes there is a ignore.py module on the same level containing a client_id and client_secret
    variable to configure the API call.  Follow these instrucitons to get a key:
    https://developer.github.com/apps/building-oauth-apps/creating-an-oauth-app/.
"""
import time
import copy
import logging
import requests
import base64
import re
import os

from typing import Optional, Dict, NamedTuple, Any, List, Sequence
from pprint import pprint
from urllib.parse import quote

import ignore
import configure


def content_scraper(username: str,
                    repo: str,
                    path: Optional[str] = None,
                    sleep: float = 60*60/5000,
                    iter_limit: int = 100,
                    _iter_start: int = 0,
                    select_extensions: Optional[Sequence] = None) -> List[Dict]:
    """Recursively scrape content from Github Repository

    Args:
        username: Target GitHub repository owner.
        repo: GitHub repository name.
        path: Optional path to a starting point within a repository.  Used
            to allow recursion for crawling directory structures. Defaults
            to None.
        sleep: Time to sleep between API requests, not accounting for code
            execution time. Defaults to 60*60/5000 to reflect the maximum
            rate of 5,000 API call/hr limit in seconds.
        iter_limit: Maximum number of files to read per repository.
            This isn't working exactly how I want but will do for now to prevent
            heinous recursion depths.
        _iter_start: Start counter for recursion iteration limitation. Intended
            for use in recursion but not set in initial call.
        select_extensions: Sequence of file extensions to be returned.
            File types not in the sequence will not be yielded.

    Yields:
        Mapping of Github repository object path names and their contents.
    """
    counter = copy.copy(_iter_start)

    extensions_flag = bool(select_extensions)
    if extensions_flag:
        select_extensions = [x.lower() for x in select_extensions]

    initial_response = request_github_content_api(username, repo, path)
    target_is_file = isinstance(initial_response, dict)

    if target_is_file:
        # Ignore if item is ~404 "Not Found" message
        if 'message' in initial_response and initial_response['message'] == 'Not Found':
            logging.warning('not found, skipping %s/%s ... ', repo, path)
            return []

        _ext = get_file_extension(path)

        if _ext in select_extensions if extensions_flag else True:
            yield {
                'path': initial_response['path'],
                'username': username,
                'repo': repo,
                'content': extract_github_content(initial_response),
            }

    else:

        for item in initial_response:

            _type = item['type']
            _path = item['path']
            _ext = get_file_extension(_path)

            if _type == 'file':

                # Ignore item if not in selected file extensions (if select extensions given)
                if _ext not in select_extensions if extensions_flag else True:
                    logging.info('file %s skipped because extension type: "%s"', _path, _ext)
                    continue

                response = request_github_content_api(username, repo, path=_path)

                item_content = extract_github_content(response)

                yield {
                    'path': _path,
                    'username': username,
                    'repo': repo,
                    'content': item_content,
                }
                time.sleep(sleep)
                counter += 1

            elif _type == 'dir':

                directory_name = os.path.basename(_path)

                if directory_name in configure.ignore_dirs:
                    logging.warning('ignoring directory %s ... ', directory_name)
                    continue

                children = content_scraper(
                    username=username,
                    repo=repo,
                    path=_path,
                    iter_limit=iter_limit,
                    _iter_start=counter,
                    select_extensions=select_extensions,
                )

                for child in children:
                    yield child

            else:
                logging.warning('unknown data type: "%s"', _type)

            if counter >= iter_limit:
                logging.warning('limit of files per repo reached: %s', iter_limit)
                break


def request_github_content_api(username: str,
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

    Notes:
        Applies quoting to username, repo and path when building request URL.

    Returns:
        Python internal representation of API JSON response.
    """
    _path = path if path else ''

    api_url = f'https://api.github.com/repos/{quote(username)}/{quote(repo)}/contents/{quote(_path)}'

    if client_id and client_secret:
        api_url += f'?client_id={client_id}&client_secret={client_secret}'

    logging.info('requesting %s ... ', api_url)
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


def extract_github_content(response: Dict) -> str:
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


def decode(x: str, encoding: str = 'utf-8') -> str:
    """Decode base64 string

    Args:
        x: Target base64 encrypted sequence.
        encoding: Python standard encoding label. Defaults
            to 'utf-8'.

    Returns:
        Decoded string sequence.
    """
    byte_str = base64.b64decode(x)
    return byte_str.decode(encoding)


def get_file_extension(path: str) -> str:
    """

    Args:
        path:

    Returns:

    """
    return os.path.splitext(path)[-1].lower()


class GithubObject(NamedTuple):
    """

    """
    url: str
    username: Optional[str]
    repo: Optional[str]
    path: Optional[str]


def extract_github_account_info(url: str) -> Any:
    """
    Notes:
        Assumes people are giving us master branches.

    Args:
        url:

    Returns:

    """
    regex = re.compile(r'.+github.com/(.+?)/(.+?)(/.*)?$')
    match = regex.match(url)

    if match:

        username = match.group(1)
        repo = match.group(2)
        path = match.group(3)

        if path:
            test = re.match(r'.+master/(.+)', path)
            path = test.group(1) if test else path

        github_obj = GithubObject(
            url=url,
            username=username,
            repo=repo,
            path=path,
        )

    else:
        github_obj = GithubObject(
            url=url,
            username=None,
            repo=None,
            path=None,
        )

    return github_obj

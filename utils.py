import logging
import requests
import base64
import re
import os

from typing import Optional, Dict, NamedTuple, Any
from pprint import pprint

import ignore


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

    Returns:
        Python internal representation of API JSON response.
    """
    _path = path if path else ''

    api_url = f'https://api.github.com/repos/{username}/{repo}/contents/{_path}'

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


if __name__ == '__main__':
    import pandas as pd
    df = pd.read_csv('/Users/aaron.rosenberg/Projects/gitcrawler/all-urls.csv')
    for url in df['url']:
        print(extract_github_account_info(url))


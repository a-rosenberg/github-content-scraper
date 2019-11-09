"""
API Scraping tools

Notes:
    Assumes there is a ignore.py module on the same level containing a client_id and client_secret
    variable to configure the API call.  Follow these instrucitons to get a key:
    https://developer.github.com/apps/building-oauth-apps/creating-an-oauth-app/.
"""
import logging
import time
import copy
import os

from typing import Dict, List, Optional, Sequence

import utils
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

    initial_response = utils.request_github_content_api(username, repo, path)
    target_is_file = isinstance(initial_response, dict) and 'content' in initial_response

    if target_is_file:

        _ext = utils.get_file_extension(path)

        if _ext in select_extensions if extensions_flag else True:
            yield {
                'path': initial_response['path'],
                'username': username,
                'repo': repo,
                'content': utils.extract_github_content(initial_response),
            }

    else:

        for item in initial_response:
            _type = item['type']
            _path = item['path']
            _ext = utils.get_file_extension(_path)

            if _type == 'file':

                # Ignore item if not in selected file extensions (if select extensions given)
                if _ext not in select_extensions if extensions_flag else True:
                    logging.info('file %s skipped because extension type: "%s"', _path, _ext)
                    continue

                response = utils.request_github_content_api(username, repo, path=_path)

                # Ignore if item is ~404 "Not Found" message
                if 'message' in response and response['message'] == 'Not Found':
                    logging.warning('not found, skipping %s/%s ... ', repo, path)
                    continue

                item_content = utils.extract_github_content(response)

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


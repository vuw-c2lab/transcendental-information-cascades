#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import importlib
import inspect
import os

from typing import Any, Callable, Iterator, List


def get_tokeniser(path: str) -> Callable:
    """
    Get tokeniser function based on the module's relative path under
    tic/create/tokenisers. The main tokenising function in the submodule must
    match the signature:

    def tokenise(input_path: str, workers: Optional[int] = None) -> str:
        pass

    Examples:
        The path to tic/create/tokenisers/discrete/example.py should be given
        as:

        discrete/example

    Args:
        path: Tokeniser module path relative to tic/create/tokenisers

    Returns:
        Tokenising function

    """
    package = 'tic.create.tokenisers'
    components = [p.strip('/') for p in os.path.split(path) if p]
    basename = components[-1]
    basename = os.path.splitext(basename)[0]
    components[-1] = basename
    relative_name = f'.{".".join(components)}'
    module = importlib.import_module(relative_name, package)
    funcs = dict(inspect.getmembers(module, inspect.isfunction))
    return funcs['tokenise']


def expand_path(path: str) -> str:
    """
    Expand the given path's references to user and environment variables. Also
    convert to absolute path.

    Args:
        path: Path to expand

    Returns:
        Expanded path

    """
    expanded_path = os.path.abspath(
        os.path.expandvars(os.path.expanduser(path))
    )
    return expanded_path


def line_count(file_path: str) -> int:
    """
    Count the number of lines in the given file.

    Args:
        file_path: Path to file

    Returns:
        Line count

    """
    with open(file_path) as stream:
        for index, _ in enumerate(stream):
            pass
    return index + 1


def line_to_csv(line: List[Any]) -> str:
    """
    Turn a list of Python objects into a comma separated string.

    Args:
        line: List to turn into CSV

    Returns:
        Line represented as a comma separated string

    """
    items = []
    for item in line:
        if not isinstance(item, str):
            items.append(str(item))
        elif ',' in item:
            items.append(f'"{item}"')
        else:
            items.append(item)
    return ','.join(items) + '\n'


def batch_list(to_batch: List[Any], batch_size: int) -> Iterator[List[Any]]:
    """
    Split list into a list of batches.

    Args:
        to_batch: List to batch
        batch_size: Number of items per batch

    Returns:
        Batched list

    """
    batched = (
        to_batch[i:i + batch_size] for i in range(0, len(to_batch), batch_size)
    )
    return batched


def readlines(file_path: str) -> Iterator[str]:
    """
    Read a line at a time from the given file path.

    Args:
        file_path: Path to file

    Yields:
        Line contents

    """
    file_path = os.path.join(os.getcwd(), file_path)
    with open(file_path, 'r') as stream:
        while True:
            line = stream.readline()
            if not line:
                break
            yield line

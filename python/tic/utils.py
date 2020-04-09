#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import os

from typing import Iterator


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

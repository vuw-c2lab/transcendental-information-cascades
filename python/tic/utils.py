#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import os

from typing import Iterator


def readlines(file_path: str) -> Iterator[str]:
    file_path = os.path.join(os.getcwd(), file_path)
    with open(file_path, 'r') as stream:
        while True:
            line = stream.readline()
            if not line:
                break
            yield line

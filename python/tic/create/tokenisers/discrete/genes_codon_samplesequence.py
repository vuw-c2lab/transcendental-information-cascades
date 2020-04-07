#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import os

import math
from typing import Any, Optional, List

from tic.utils import readlines


def tokenise(data_source_path: str) -> Any:

    tokenised = []

    for line in readlines(data_source_path):
        line_tokens = []
        line = line.strip().lower()
        for idx in range(math.floor(len(line) / 3)):
            for char in range(3):
                start = (idx * 3) + char
                end = (idx * 3) + 3 + char
                codon = line[start:end]
                if len(codon) == 3 and 'n' not in codon and '-' not in codon:
                    token = f'pos{idx + 1}#+{char + 1}#{codon}'
                    line_tokens.append(token)
        tokenised.append(line_tokens)
    return tokenised


if __name__ == '__main__':
    from _datetime import datetime
    t0 = datetime.now()
    output_path = os.path.join(os.getcwd(), 'tokenised.csv')

    tokenised = tokenise('/Users/thimic/Developer/TIC/ordered.txt')
    lines = [', '.join(line_tokens) for line_tokens in tokenised]

    with open(output_path, 'w') as stream:
        stream.writelines(lines)

    print(f'Finished in {datetime.now() - t0}')

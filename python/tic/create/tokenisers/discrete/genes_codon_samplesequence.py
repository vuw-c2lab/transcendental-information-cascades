#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import os

import math
from typing import Iterator, List

from tic.utils import readlines


def tokenise(data_source_path: str) -> Iterator[List[str]]:
    for line in readlines(data_source_path):
        line = line.strip().lower()
        line_tokens = []
        for idx in range(math.floor(len(line) / 3)):
            for char in range(3):
                start = (idx * 3) + char
                end = (idx * 3) + 3 + char
                codon = line[start:end]
                if len(codon) == 3 and 'n' not in codon and '-' not in codon:
                    token = f'pos{idx + 1}#+{char + 1}#{codon}'
                    line_tokens.append(token)
        yield line_tokens


if __name__ == '__main__':

    input_path = os.path.join(os.getcwd(), '/Users/thimic/Developer/TIC/ordered.txt')
    output_path = os.path.join(os.getcwd(), 'tokenised.csv')

    # Remove existing output files before writing
    if os.path.isfile(output_path):
        os.remove(output_path)

    from _datetime import datetime
    t0 = datetime.now()

    with open(output_path, 'a') as stream:
        for line_tokens in tokenise(input_path):
            line = ', '.join(line_tokens)
            stream.write(line)

    print(f'Finished in {datetime.now() - t0}')

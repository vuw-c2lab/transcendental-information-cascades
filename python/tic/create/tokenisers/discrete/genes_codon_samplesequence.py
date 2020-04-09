#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import click
import math
import os

from concurrent.futures import ProcessPoolExecutor
from tqdm import tqdm
from typing import List

from tic.utils import expand_path, line_count, readlines


def tokenise_line(line: str) -> List[str]:
    """
    Create gene codon tokens for the given line.

    Args:
        line: Line to analyse

    Returns:
        List of tokens for the current line

    """
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
    return line_tokens


def process(input_path: str, output_path: str):
    """
    Process lines from the input file using the tokeniser.

    Args:
        input_path: Input file path
        output_path: Output file path

    """
    input_path = expand_path(input_path)
    output_path = expand_path(output_path)

    # Remove existing output files before writing
    if os.path.isfile(output_path):
        os.remove(output_path)

    with ProcessPoolExecutor() as executor:
        progress_iter = tqdm(
            iterable=executor.map(tokenise_line, readlines(input_path)),
            desc='Tokenise',
            unit='lines',
            ncols=100,
            total=line_count(input_path)
        )
        with open(output_path, 'a') as stream:
            for line_tokens in progress_iter:
                line = ', '.join(line_tokens)
                stream.write(line + '\n')


@click.command()
@click.option(
    '-i',
    '--input',
    'input_path',
    required=True,
    help='File to tokenise'
)
@click.option(
    '-o',
    '--output',
    'output_path',
    required=True,
    help='Tokenised output file'
)
def main(input_path: str = None, output_path: str = None):
    """
    Genes Codon SampleSequence Tokeniser
    """
    process(input_path, output_path)


if __name__ == '__main__':
    main()

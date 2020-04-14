#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import click
import math
import os
import traceback

from datetime import datetime
from tqdm import tqdm
from typing import Iterable, Optional, Tuple

from tic.utils import batch_list, expand_path, get_tokeniser, line_to_csv


def generate_discrete_tic(sequence: Iterable[str]) -> Tuple[list, list]:
    """
    Generate Transcendental Information Cascades node network from the given
    input sequence.

    Args:
        sequence: Input sequence

    Returns:
        Tuple of nodes and links

    """
    nodes = [['id', 'tokens', 'dpub']]
    links = [['source', 'target', 'token']]
    last_node = {}

    progress_iter = tqdm(
        iterable=sequence,
        desc='Generate TIC',
        unit='lines',
        ncols=100,
    )
    for index, line in enumerate(progress_iter, start=1):
        tokens = line.split(', ')
        nodes.append([str(index), line, str(index)])
        for token in tokens:
            if last_node.get(token):
                source_node = last_node[token]
                link = [str(source_node), str(index), token]
                links.append(link)
            last_node[token] = index
    return nodes, links


def create_tic(input_path: str, output_dir: str, tokeniser_path: str,
               out_prefix: Optional[str] = None,
               workers: Optional[int] = None) -> Tuple[list, list]:
    """
    Entry point function for creating discrete transcendental information
    cascades.

    Args:
        input_path: Path to source data
        output_dir: Path to output directory
        out_prefix: Optional string to prefix nodes and links files with
        tokeniser_path: Tokenisation function
        workers: Number of worker processes to use for tasks that can be
                 parallelised
    """

    try:
        tokenise = get_tokeniser(tokeniser_path)
    except Exception:
        error = traceback.format_exc()
        click.echo(
            f'Unable to find tokeniser for path {tokeniser_path!r}: \n'
            f'\n'
            f'{error}'
        )
        return [], []

    input_path = expand_path(input_path)
    output_dir = expand_path(output_dir)

    os.makedirs(output_dir, exist_ok=True)

    nodes_basename = 'nodes.csv'
    links_basename = 'links.csv'

    if out_prefix:
        nodes_basename = f'{out_prefix}_{nodes_basename}'
        links_basename = f'{out_prefix}_{links_basename}'

    nodes_path = os.path.join(output_dir, nodes_basename)
    links_path = os.path.join(output_dir, links_basename)

    # Remove existing output files before writing
    for output_file in [nodes_path, links_path]:
        if os.path.isfile(output_file):
            os.remove(output_file)

    t0 = datetime.now()
    lines = list(tokenise(input_path, workers=workers))
    nodes, links = generate_discrete_tic(lines)
    with open(nodes_path, 'a') as nodes_file:
        progress_iter = tqdm(
            iterable=nodes,
            desc='Write nodes',
            unit='nodes',
            ncols=100,
        )
        for node in progress_iter:
            nodes_file.write(line_to_csv(node))
    with open(links_path, 'a') as links_file:
        batch_size = 100000
        batched = batch_list(links, batch_size)
        total = math.ceil(len(links) / batch_size)
        progress_iter = tqdm(
            iterable=batched,
            desc='Write links',
            unit=f'x{batch_size:.0e}links',
            total=total,
            ncols=100,
        )
        for batch in progress_iter:
            csv_lines = ''.join([line_to_csv(b) for b in batch])
            links_file.write(csv_lines)

    click.echo(f'Finished in: {datetime.now() - t0}')

    return nodes, links


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
    '--output-dir',
    required=True,
    help='Output directory in which to write nodes.csv and links.csv'
)
@click.option(
    '-t',
    '--tokeniser',
    'tokeniser_path',
    default='discrete/genes_codon_samplesequence',
    show_default=True,
    help='Tokeniser module path relative to tic/create/tokenisers'
)
@click.option(
    '-p',
    '--out-prefix',
    required=False,
    help='Prefix for nodes.csv and links.csv files'
)
@click.option(
    '-w',
    '--workers',
    type=int,
    help=(
        'Number of workers used to by the tokeniser. Defaults to computer\'s '
        'CPU core count'
    )
)
def main(input_path: str = None, output_dir: str = None,
         tokeniser_path: Optional[str] = None, out_prefix: Optional[str] = None,
         workers: Optional[int] = None):
    """
    Generate discrete transcendental information cascades
    """
    create_tic(
        input_path=input_path,
        output_dir=output_dir,
        tokeniser_path=tokeniser_path,
        out_prefix=out_prefix,
        workers=workers
    )


if __name__ == '__main__':
    main()

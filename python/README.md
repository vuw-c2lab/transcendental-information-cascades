# Requirements

The Python package was written with Python 3.8 in mind, though 3.7 might also 
work.

To install required modules, run the following command with the Python 3 version 
if pip:

```
pip install -r requirements.txt
```

# Running

To run the script from the shell:

```
./bin/tic_create --help
```

# Options

```
Usage: main.py [OPTIONS]

  Generate discrete transcendental information cascades

Options:
  -i, --input TEXT       File to tokenise  [required]
  -o, --output-dir TEXT  Output directory in which to write nodes.csv and
                         links.csv  [required]

  -t, --tokeniser TEXT   Tokeniser module path relative to
                         tic/create/tokenisers  [default:
                         discrete/genes_codon_samplesequence]

  -p, --out-prefix TEXT  Prefix for nodes.csv and links.csv files
  -w, --workers INTEGER  Number of workers used to by the tokeniser. Defaults
                         to computer's CPU core count

  --help                 Show this message and exit.

```

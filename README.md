# giortazi

[![Build](https://img.shields.io/badge/build-passing-brightgreen.svg)](https://github.com/yourusername/agrep/actions)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![Haskell](https://img.shields.io/badge/language-haskell-5e5086.svg)](https://www.haskell.org/)

**giortazi** is a simple command-line tool to look up Greek Orthodox name days. It provides quick access to name day information directly from your terminal.

This project is implemented in **Haskell**, with a focus on correctness, simplicity, and performance.

## Features

* Lookup today's name day(s)
* Search name days by name
* Search name days by date
* Works offline with a built-in calendar
* Minimal and fast CLI interface

## Installation

You can build and install `giortazi` using [Cabal](https://www.haskell.org/cabal/).

### Using Cabal:

```bash
git clone https://github.com/Turtel216/giortazi.git
cd giortazi
cabal build
cabal run
```

## Usage

Here are some example commands:

```bash
# Show today's name days
giortazi today

# Lookup name day by name
giortazi name Giorgos

# Lookup name day by date
giortazi date 25-03
```

Options and flags:

```
Usage: giortazi [COMMAND] [OPTIONS]

Commands:
  today             Show today's name days
  name [NAME]       Find the name day for a given name
  date [DD-MM]      Find name days on a specific date
  help              Show help information
```

## Data

`giortazi` uses a the [Greek-namedays](https://github.com/alexstyl/Greek-namedays) dataset. For most accurate results, it focuses on fixed-name days and includes logic for calculating movable feasts like Easter.

## Contributing

Contributions are welcome! Please open an issue or submit a pull request if you'd like to improve `giortazi`.

## License

This project is licensed under the MIT License.

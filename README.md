# Baker-Bird / Aho-Corasick Algorithm in Haskell 

## How to Run

```sh
./baker bb_in.txt bb_out.txt

# checker (Python >= 3.7)
python checker.py bb_in.txt bb_out.txt cc_out.txt
```

## How to Build

### Install compiler and package manger

- Python >= 3.7
- GHC (Haskell compiler) >= 8.10

```sh
sudo apt-get update
sudo apt-get build-essential curl libffi-dev libffi7 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5

# IMPORTANT: SELECT YES on 'PATH variable setting' and 'install stack'
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

source ~/.bashrc # or source ~/.zshrc
```

### Build

```sh
stack build

# binary path
stack path --local-install-root

# run main code
stack run tp-assign-exe -- bb_in.txt bb_out.txt

# run checker
python checker.py bb_in.txt bb_out.txt cc_out.txt

# run checker on every test dataset
bash check.sh

# run benchmark on every test dataset
bash bench.sh
```

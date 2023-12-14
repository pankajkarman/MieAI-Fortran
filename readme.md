# MieAI-Fortran

Fortran engine to couple neural network based Mie emulator, called MieAI, with ICON-ART


It uses [Fortran-Keras Bridge](https://github.com/scientific-computing/FKB) for coupling MieAI to Fortran. 

# Usage

Compile the MieAI model in the terminal using:

```bash
make 
```

This will create a static library, called `libneural.a` inside `build/lib/` folder, which contains necessary objects for linking fortran with keras using FKB bridge and an executable, called MieAI, in `bin` folder which can be executed by running:

```bash
./bin/MieAI ./data/mie_input.txt 
```

Here, [`mie_input.txt`](./data/mie_input.txt) cantains the input data for Mie calculations. An example file is located in [`data`](./data) folder. Further, [`data`](./data) folder contains the [`csv`](./data) files containing the min-max data useful for normalization ([`mlp_min_max.csv`](./data/mlp_min_max.csv)) and quantile transform parameters ([`quantile_trasnform.csv`](./data/quantile_transform.csv)). The path for these files and MieAI model is specified in a namelist file, called [`MieAI.nml`](MieAI.nml), which is read by the MieAI fortran code.

# File Structure

- [FKB](./FKB): This directory contains source codes for Fortran-Keras bridge

- [model](./model): This directory contains various trained MieAI models

- [data](./data): This directory contains data to be fed to MieAI

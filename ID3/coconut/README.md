# Abstract

The subdirectories from here contain Coconut implementations of the ID3 algorithm.  This README covers instructions that are generally the same across all of the projects.


# Getting in the Development Environment

There is a Conda environment provided for each implementation of ID3 in Coconut.  Before you can get into a Conda environment, you first have to install Anaconda.  I highly recommend Miniconda3 64-bit, which you can get [here](https://docs.conda.io/en/latest/miniconda.html).

Once you have Miniconda3, you can create any of the environments provided in the subdirectories from here via this command:

```bash
conda env create -f python-environment.yaml
```

By default, newly created Conda environments get installed to `~/miniconda3/envs`.  This can be useful for telling editors like Emacs/Spacemacs, PyCharm, VSCode, etc. where to find the PyEnv/Conda environments (they are the same thing) that these projects are using.

Once an environment is created, you can update it when changes are made to the `python-environment.yaml` via the following command:

```bash
conda env update -f python-environment.yaml
```

To get into a Conda environment:

```bash
condaenvname=$(cat python-environment.yaml | head -n 1 | awk '{print $2}')
conda activate $condaenvname
```

You should now have coconut, jupyter, etc. available from your CLI. (use `which coconut`) to see that it's using the `coconut` command from your miniconda3 environment.


To get out of a Conda environment:

```bash
conda deactivate
```

Unless you have them installed globally, you should now no longer have coconut, jupyter, etc. directly available from your CLI.

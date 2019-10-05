# Creating a Python Environment with Hy and Jupyter Integration

1. Install Miniconda3
1.
    ```bash
    conda create -n hyid3-and-seq python=3.7
    conda activate hyid3-and-seq
    ```

1. [Follow the instructions on this GitHub issue comment](https://github.com/Calysto/calysto_hy/issues/15#issuecomment-526766024) **_EXCEPT_** You should install Hy version 0.15.  I've found that versions 0.16 and 0.17 have issues importing Pip installed Python modules such as Pandas and Numpy.

# San Francisco data

## Details on the dataset

We have the speed limits as covariates and the speed observations as the response variable. (add more details)

## Adding large files

First you need to install git-lfs:

```{bash}
brew install git-lfs
```

Then, you need to run on the terminal in the repository folder:

```{bash}
git lfs install
```

Finally, for each large file:

```{bash}
git lfs track filename
```

For example, for this repo we have to do:

```{bash}
git lfs track onlybuses.RData 
```

to be able to push `onlybuses.RData`.

To download large files one must run (after installing git-lfs):

```{bash}
git lfs pull
```
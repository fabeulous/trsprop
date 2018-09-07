# trsprop

A small program to filter term-rewrite systems (TRSs) by there properties.
It takes a list of file names, where each file contains a TRS in the [TPDB format](https://www.lri.fr/~marche/tpdb/format.html), and a number of properties.
`trsprop` then prints all filenames, whose TRSs have all the given properties, to STDOUT.

The main use of `trsprop` for me is to filter through problems in the [COPS](http://cops.uibk.ac.at/) database and for testing [CSI](http://cl-informatik.uibk.ac.at/software/csi/) on different subsets thereof.

## Usage

```
trsprop [-f trs-list] [-d trs-directory] [properties..]
```

The newline separated list of filenames is either supplied by STDIN or as a file using the `-f` flag. The `-d` flag sets the directory which contains the TRS files (default is the current directory).

### Properties

The follow properties can be filtered for:

  * shallow
  * flat
  * ground
  * left-ground
  * right-ground
  * linear
  * left-linear
  * right-linear
  * right-reducible
  * erasing
  * creating
  * expanding
  * duplicating
  * collapsing


## INSTALLATION

## MAC OSX

1. 1. Install Qt from http://qt.nokia.com/ 
   Use the LGPL version, and the SDK complete development environment

2. cmake 2.8.1 or later needs to be installed from cmake.org
Need to ensure that it makes the command line scripts

3. gcc installed from Xcode Apple Developer web site 

4. Might need to set PATH so that both are visible.

5. Change into the directory where you want to keep the libraries, and
   get a copy of qtbase and qtpaint (qtinterfaces) from
   http://github.com/ggobi/.

6. Install the libraries:

R CMD INSTALL qtbase --preclean
R CMD INSTALL qtpaint --preclean

7. Similarly, install plumbr from http://github.com/ggobi/plumbr

8. For qmosaic you'll also need to install the productplots package
   from http://github.com/hadley/productplots

## Ubuntu 10.04 LTS on a x86_64

Sources via synaptic package manager given when possible.

1. download qt

http://qt.nokia.com/downloads
Note: you will need to manually designate installation location by setting LD_LIBRARY_PATH, LIBS and LD_FLAGS

OR

from synaptic package manager, select libqt-dev (much easier!)

2. download source for R
http://www.r-project.org/
tools:  
fortran compiler [gfortran]
dvips [texlive-latex-recommended]
makeinfo[texinfo]
g++ [g++]
readline [libreadline-dev]
x11 [xorg-dev]

troubleshooting:
look at config.log

2a. Install R

in terminal, navigate to where R was downloaded:

./configure --enable-R-shlib 'CPICFLAGS=-fPIC'
make
make check
sudo make install

3. Download qtinterfaces
Note: requires git

in terminal, navigate to where you want source saved to:
git clone git://github.com/ggobi/qtbase.git
git clone git://github.com/ggobi/qtpaint.git

3.a.1  tool: Cmake

synaptic has version 2.8.0, which is insufficient
download from: http://www.cmake.org/cmake/resources/software.html

in terminal, navigate to directory containing cmake

./configure
make
sudo make install


3.b install qtbase and qtpaint
from R:
install.packages("qtbase", repos=NULL, type = "source")
install.packages("qtpaint", repos=NULL, type = "source")

4. Plumbr
Note: requires git

In terminal navigate to where you want to save plumbr
git clone git://github.com/ggobi/plumbr.git

4.a installation
from R:
install.packages("plumbr", repos=NULL, type="source")

## Layers Order - Di says, do we need this?

1. background
    * things that shouldn't change
    * grid, axes, title
2. persistent layer
    * mutaframe
      * data, colors, size, etc.
3. transient layer
    * temporary brushing, colors, etc.
4. NULL (empty)
5. others can be used for special plots.



#! /bin/tcsh -f
#Run bbftp client on aftpexp.bnl.gov
#> bbftp -V -u fisyak -i ctrlfile wacdr.cern.ch
#
#where I create 'ctrlfile' with the following entries:
#
#> cat ctrlfile
#setoption remoterfio
#setbuffersize 1024
#setrecvwinsize 2048
#setnbstream 10
#get \
#/castor/cern.ch/atlas/project/dc1/evgen/data/002000/dc1.002000.evgen.0010.hlt.pythia_jet_17.root \
#dc1.002000.evgen.0010.hlt.pythia_jet_17.root

#!/bin/tcsh
# $Id: MyPackage.csh,v 1.1 2009/11/24 19:02:49 fine Exp $
#*-- Author :    Valery Fine(fine@bnl.gov)   24/11/2009
echo -----------------------------------------------
echo 1.  Build the stand alone MyPackageMain application
echo -----------------------------------------------
g++ MyPackageMain.cxx -o MyPackageMain

echo -----------------------------------------------
echo 2.  Execute stand alone MyPackageMain application
echo -----------------------------------------------
MyPackageMain


echo
echo -----------------------------------------------
echo 3.  Build and execute the appcation with ROOT ACLiC
echo -----------------------------------------------
root.exe -b -q -l 'MyPackage.cxx("$USER/InFile","$SCRATCH/outfile.root")'

echo
echo -----------------------------------------------
echo 4.  Build and execute the appcation with ROOT ACLiC
echo -----------------------------------------------
root.exe -b -q -l 'MyPackageFuncy.cxx("$USER/InFile","$SCRATCH/outfile.root")'



#!/bin/tcsh
# $Id: MyPackage.csh,v 1.2 2009/11/25 03:55:42 fine Exp $
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
root.exe -b -q -l 'MyPackageFancy.cxx("$USER/InFile","$SCRATCH/outfile.root")'



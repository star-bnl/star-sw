// $Id: MyPackageFancy.cxx,v 1.2 2009/11/24 22:23:25 fine Exp $
//*-- Author :    Valery Fine(fine@bnl.gov)   24/11/2009
#include <stdlib.h>
#include <stdio.h>
#include "TString.h"
#include "TSystem.h"

namespace {
// the text inside of namespace is a copy of the standalone application
#include "MyPackage.cxx"
}

int MyPackageFancy(const char *infile="$USER/example.list"
             , const char *outfile = "$SCRATCH/example.root")
{
   TString sInFile =  infile;
   TString sOutFile =  outfile;
   // expand the environment variables if needed
   gSystem->ExpandPathName(sInFile);
   gSystem->ExpandPathName(sOutFile);

  char *argv[] = {(char*)"MyPackageFuncy",(char*) sInFile.Data(),(char*) sOutFile.Data()  };
 
  printf( "\n\nUsage:  root.exe -b -l -q \'%s.cxx+[(\"input_file\" [,\"output_file\"])\']\n",argv[0]);
  printf( "------    The square brackets [] are to show the optional components\n");
  
  return main(3, argv);
}


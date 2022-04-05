// $Id: MyPackage.cxx,v 1.1 2009/11/24 19:02:50 fine Exp $
//*-- Author :    Valery Fine(fine@bnl.gov)   24/11/2009
#include <stdlib.h>
#include <stdio.h>
#include "TString.h"
#include "TSystem.h"

namespace {
// the text inside of namespace is a copy of the standalone application
// #include "MyPackage.cxx"
void usage(const char *name) {
   printf( "\n\nUsage: %s [input_file [ output_file]]\n",name);
   printf( "-----\n");
}

int main(int argc,  char * argv[])
{
   const char *packageName="No_name";
   printf("Hello World %d: \n", argc);
   switch (argc) {
         case 3: printf("\tThe output file name <%s>\n", argv[2]);
         case 2: printf("\tThe input file name <%s>\n",  argv[1]);
         case 1: printf("\tThe package name <%s>",     argv[0]);
                 packageName =  argv[0];
                 break;
       default: 
         printf("\tNo paramater\n");
         break;
   }
   usage(packageName);
   return 0;
}

}

int MyPackage(const char *infile="$USER/example.list"
             , const char *outfile = "$SCRATCH/example.root")
{
   TString sInFile =  infile;
   TString sOutFile =  outfile;
   // expand the environment variables if needed
   gSystem->ExpandPathName(sInFile);
   gSystem->ExpandPathName(sOutFile);

  char *argv[] = {(char*)"MyPackage",(char*) sInFile.Data(),(char*) sOutFile.Data()  };
 
  printf( "\n\nUsage:  root.exe -b -l -q \'%s.cxx+[(\"input_file\" [,\"output_file\"])\']\n",argv[0]);
  printf( "------    The square brackets [] are to show the optional components\n");
  
  return main(3, argv);
}


// $Id: MyPackageMain.cxx,v 1.1 2009/11/24 19:02:50 fine Exp $
//*-- Author :    Valery Fine(fine@bnl.gov)   24/11/2009
#ifndef STAR_MYPACKAGEMAIN
#define STAR_MYPACKAGEMAIN
#include <stdlib.h>
#include <stdio.h>

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

#endif

//:Copyright 1995, Lawrence Berkeley Laboratory
//:>--------------------------------------------------------------------
//:FILE:        tst_asuString.C
//:DESCRIPTION: Tests of the string class.
//:AUTHOR:      cet - Craig E. Tull, cetull@lbl.gov
//:BUGS:        -- STILL IN DEVELOPMENT --
//:HISTORY:     27jul95-v000a-cet- creation
//:<--------------------------------------------------------------------

#include <stdlib.h>
#include <stream.h>
#include "sutClasses.hh"

void usage(char* prog);

//:>--------------------------------------------------------------------
//:ROUTINE:     main
//:DESCRIPTION: Program Mainline
//:ARGUMENTS:   -- NONE --
//:RETURN VALUE:-- NONE --
//:<--------------------------------------------------------------------

int main(int argc, char** argv)
{
   int i;
   string *s[8];
   if( argc < 3 )usage(argv[0]);
   if( argc > 9 )usage(argv[0]);

   cout << "tst_asuString## starting" << endl;

   for( i=0;i<argc;i++ ){
      cout << "########################################" << endl;
      cout << "create string from (" << argv[i] << ")\n";
      s[i] = new string(argv[i]);
      cout << "----------------------------------------" << endl;
      cout << "tst_asuString## show ("<< s[i]->show() <<")"<<endl;
      cout << "--------------------" << endl;
      cout << "tst_asuString## length ("<< s[i]->length() <<")"<<endl;
/*
06oct95 -- THIS TEST DOES NOT COMPILE
      cout << "--------------------" << endl;
      if( i>0
      &&  (s[i] == s[i-1]->show()) ){
      cout << "tst_asuString## SAME AS LAST STRING" << endl;
      } else {
      cout << "tst_asuString## DIFFERENT THAN LAST STRING" << endl;
      }
*/
   }

   for( i=0;i<argc;i++ ){
      delete s[i];
   }
   cout << "tst_asuString## ending" << endl;

   return 1;
}

//:>--------------------------------------------------------------------
//:ROUTINE:     void usage
//:DESCRIPTION: Shows usage of main.
//:ARGUMENTS:   char* prog      = Program name.
//:RETURN VALUE:*** KILLS PROGRAM ***
//:<--------------------------------------------------------------------
void usage(char* prog)
{
   printf("\nUsage: %s string1 string2\n\n",prog);
   printf("example: %s \"this is string1\" \"this is string2\"\n",prog);
   printf("\n");
   exit(0);
}


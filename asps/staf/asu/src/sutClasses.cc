#ifndef sun 

/*:Copyright 1995, Lawrence Berkeley Laboratory
*:>---------------------------------------------------------------------
*:FILE:         sutString.C
*:DESCRIPTION:  Character string class
*:AUTHOR:       cet - Craig E. Tull, cetull@lbl.gov
*:BUGS:         -- STILL IN DEVELOPMENT --
*:HISTORY:      10may96-v000b-cet- asuString.cc ==> sutClasses.cc
*:HISTORY:      20jul95-v000a-cet- creation
*:<---------------------------------------------------------------------
*/

#include "sutClasses.hh"

/*
*:>---------------------------------------------------------------------
*:ROUTINE:     class string
*:DESCRIPTION: character string class
*:<---------------------------------------------------------------------
*/
// CTORS & DTOR --------------------
// ATTRIBUTES ----------------------
// FUNCTIONS -----------------------
// OPERATORS -----------------------
#ifndef hpux
string string::operator + (string p)
{
   string temp;

   temp.myStr = new char[(temp.myLen = myLen + strlen(p.myStr) +1)];
   strcpy(temp.myStr,myStr);
   strcat(temp.myStr, p.myStr);
   return temp;
}
#endif /*hpux*/

string string::operator + (char* ptr)
{
   string temp;

   temp.myStr = new char[(temp.myLen = myLen + strlen(ptr) +1)];

   strcpy(temp.myStr,myStr);
   strcat(temp.myStr, ptr);
   return temp;
}

void string::operator += (string p)
{
   char* temp = new char[strlen(myStr) + strlen(p.myStr) +1];
   strcpy(temp, myStr);
   delete myStr;
   myStr = new char[myLen = (myLen + p.myLen +1)];
   strcpy(myStr, temp);
   strcat(myStr, p.myStr);
   delete temp;
}

void string::operator = (char* str)
{
   delete myStr;
   myStr = new char[ strlen(str) +1 ];
   strcpy(myStr, str);
}

char* string::operator [] (char* key)
{
   for(int i =0; myStr[i+strlen(key)]; i++)
      if( !strncmp(&myStr[i], key, strlen(key)) )
         return (char*)&myStr[i];
   return (char*)0;
}

int string::operator == (string p)
{
   if( p.length() == length()
   &&  strcmp(p.show(),show()) ){
      return 1; } else { return 0;
   }
}

int string::operator == (char* p)
{
   if( strcmp(p,show()) ){
      return 1; } else { return 0;
   }
}

int string::operator == (const char* p)
{
   if( strcmp(p,show()) ){
      return 1; } else { return 0;
   }
}

void string::operator << (ostream p)
{
   p << myStr;
}

#endif /*sun*/


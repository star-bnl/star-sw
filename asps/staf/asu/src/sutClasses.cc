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
*:ROUTINE:     class stafString
*:DESCRIPTION: character string class
*:<---------------------------------------------------------------------
*/
// CTORS & DTOR --------------------
// ATTRIBUTES ----------------------
// FUNCTIONS -----------------------
// OPERATORS -----------------------
#ifndef HPUX
stafString stafString::operator + (stafString p)
{
   stafString temp;

   temp.myStr = new char[(temp.myLen = myLen + strlen(p.myStr) +1)];
   strcpy(temp.myStr,myStr);
   strcat(temp.myStr, p.myStr);
   return temp;
}
#endif /*HPUX*/

stafString stafString::operator + (char* ptr)
{
   stafString temp;

   temp.myStr = new char[(temp.myLen = myLen + strlen(ptr) +1)];

   strcpy(temp.myStr,myStr);
   strcat(temp.myStr, ptr);
   return temp;
}

void stafString::operator += (stafString p)
{
   char* temp = new char[strlen(myStr) + strlen(p.myStr) +1];
   strcpy(temp, myStr);
   delete myStr;
   myStr = new char[myLen = (myLen + p.myLen +1)];
   strcpy(myStr, temp);
   strcat(myStr, p.myStr);
   delete temp;
}

void stafString::operator = (char* str)
{
   delete myStr;
   myStr = new char[ strlen(str) +1 ];
   strcpy(myStr, str);
}

char* stafString::operator [] (char* key)
{
   for(int i =0; myStr[i+strlen(key)]; i++)
      if( !strncmp(&myStr[i], key, strlen(key)) )
         return (char*)&myStr[i];
   return (char*)0;
}

int stafString::operator == (stafString p)
{
   if( p.length() == length()
   &&  strcmp(p.show(),show()) ){
      return 1; } else { return 0;
   }
}

int stafString::operator == (char* p)
{
   if( strcmp(p,show()) ){
      return 1; } else { return 0;
   }
}

int stafString::operator == (const char* p)
{
   if( strcmp(p,show()) ){
      return 1; } else { return 0;
   }
}

void stafString::operator << (ostream p)
{
   p << myStr;
}

#endif /*sun*/


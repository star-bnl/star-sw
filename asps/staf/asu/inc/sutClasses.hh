/*:Copyright 1995, Lawrence Berkeley Laboratory
*:>---------------------------------------------------------------------
*:FILE:         sutClasses.hh
*:DESCRIPTION:  header file for sutClasses.cc
*:AUTHOR:       cet - Craig E. Tull, cetull@lbl.gov
*:BUGS:         -- STILL IN DEVELOPMENT --
*:HISTORY:      10may96-v000b-cet- asuString.h ==> sutClasses.hh
*:HISTORY:      20jul95-v000a-cet- creation (from John Thomas Berry)
*:<---------------------------------------------------------------------
*/
#ifndef SUTCLASSES_HH
#define SUTCLASSES_HH

#include <iostream.h>
#include <string.h>

/*
*:>---------------------------------------------------------------------
*:ROUTINE:     class string
*:DESCRIPTION: character string class
*:<---------------------------------------------------------------------
*/
class string
{
public:
// CTORS & DTOR --------------------
   string() { myLen = 0; myStr = (char*)0; }
   string(char* s) { myStr = new char[(myLen=strlen(s))+1];
		strcpy(myStr,s); }
   string(const char* s) { myStr = new char[(myLen=strlen(s))+1];
		strcpy(myStr,s); }
   ~string() { delete myStr; }
// ATTRIBUTES ----------------------
   char* show(){ return myStr; }
   int length(){ return myLen; }
// FUNCTIONS -----------------------
// OPERATORS -----------------------
   string operator + (string);
   string operator + (char*);
   void operator += (string);
   void operator = (char*);
   char* operator [] (char*);
   int operator == (string);
   int operator == (char*);
   int operator == (const char*);
   void operator << (ostream);

private:
   char *myStr;
   int myLen;
};
#endif /*SUTCLASSES_HH*/

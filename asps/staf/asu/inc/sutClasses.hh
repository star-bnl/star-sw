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
#ifdef __cplusplus
#ifndef SUTCLASSES_HH
#define SUTCLASSES_HH

#include <iostream.h>
#include <string.h>
#include "asuAlloc.h"
/*
*:>---------------------------------------------------------------------
*:ROUTINE:     class stafString
*:DESCRIPTION: character string class
*:<---------------------------------------------------------------------
*/
class stafString
{
public:
// CTORS & DTOR --------------------
   stafString() { myLen = 0; myStr = (char*)0; }
   stafString(char* s) { myStr = new char[(myLen=strlen(s))+1];
		strcpy(myStr,s); }
   stafString(const char* s) { myStr = new char[(myLen=strlen(s))+1];
		strcpy(myStr,s); }
   ~stafString() { delete myStr; }
// ATTRIBUTES ----------------------
   const char* show(){ return myStr; }
   int length(){ return myLen; }
   char *copy() { char *c = (char*)MALLOC(myLen+1); strcpy(c,myStr); return c;}
// FUNCTIONS -----------------------
// OPERATORS -----------------------
   stafString operator + (stafString);
   stafString operator + (char*);
   void operator += (stafString);
   void operator = (char*);
   char* operator [] (char*);
   int operator == (stafString);
   int operator == (char*);
   int operator == (const char*);
   void operator << (ostream);

private:
   char *myStr;
   int myLen;
};
#endif /*SUTCLASSES_HH*/
#endif /*__cplusplus*/

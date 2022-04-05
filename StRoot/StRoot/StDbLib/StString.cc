#include <stdlib.h>
#include <stdio.h>
#include "StString.h"

//______________________________________________________________________________
StString& StString::operator<<(long long I)
{
  char cbuf[200];
  if (!fPrec) {
     sprintf(cbuf,"%ld",(long)I);
  } else {
     sprintf(cbuf,"%.*ld",fPrec,(long)I);
  }
  fPrec = 0;
  (*this) += cbuf;
  return *this;
}      
//______________________________________________________________________________
StString& StString::operator<<(long I)
{ long long l = I; *this << l;return *this;}
//______________________________________________________________________________
StString& StString::operator<<(int I)
{ long long l = I; *this << l;return *this;}
//______________________________________________________________________________
StString& StString::operator<<(short I)
{ long long l = I; *this << l;return *this;}
//______________________________________________________________________________
StString& StString::operator<<(unsigned long long I)
{
  char cbuf[100];
  if (!fPrec) {
     sprintf(cbuf,"%lu",(unsigned long)I);
  } else {
     sprintf(cbuf,"%.*lu",fPrec,(unsigned long)I);
  }
  fPrec = 0;
  (*this) += cbuf;
  return (*this);
}      
//______________________________________________________________________________
StString& StString::operator<<(unsigned long I)
{ unsigned long long l = I; *this << l;return *this;}
//______________________________________________________________________________
StString& StString::operator<<(unsigned int I)
{ unsigned long long l = I; *this << l;return *this;}
//______________________________________________________________________________
StString& StString::operator<<(unsigned short I)
{ unsigned long long l = I; *this << l;return *this;}
//______________________________________________________________________________
StString& StString::operator<<(double I)
{
  char cbuf[200];
  if (!fPrec) {
     sprintf(cbuf,"%g",I);
  } else {
     sprintf(cbuf,"%.*g",fPrec,I);
  }
  fPrec = 0;
  (*this) += cbuf;
  return *this;
}      

//______________________________________________________________________________
StString& StString::operator<<(float I)
{ double d = I; *this << d; return *this;}


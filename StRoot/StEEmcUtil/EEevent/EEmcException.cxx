// $Id: EEmcException.cxx,v 1.1 2003/01/28 23:16:07 balewski Exp $
// $Log: EEmcException.cxx,v $
// Revision 1.1  2003/01/28 23:16:07  balewski
// start
//
// Revision 1.2  2002/09/27 19:05:13  zolnie
// EEmcMCData updates
//
// Revision 1.1  2002/09/24 22:47:34  zolnie
// major rewrite: SMD incorporated, use constants rather hard coded numbers
// 	introducing exceptions (rather assert)
//

#include <stdio.h>
#include <iostream.h>

#include "EEmcException.h"


EEmcException::EEmcException() 
{
  mErrno = kEEmcUnknownError;
  cerr << "EEmcException:  unknown exception" << endl;
};



EEmcException::EEmcException(const EEmcErrno_t e, const char *msg, const int value)
{
  mErrno = e;
  cerr << "EEmcException: ";
  if(msg!=NULL) cerr << msg ;
  cerr << " (errno=" << mErrno << ")" ;
  cerr << " value=" << value << endl;
};


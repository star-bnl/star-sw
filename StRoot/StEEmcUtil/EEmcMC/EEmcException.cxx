// $Id: EEmcException.cxx,v 1.2 2003/02/20 20:13:20 balewski Exp $
// $Log: EEmcException.cxx,v $
// Revision 1.2  2003/02/20 20:13:20  balewski
// fixxy
// xy
//
// Revision 1.1  2003/02/20 05:14:07  balewski
// reorganization
//
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


EEmcException1::EEmcException1() 
{
  mErrno = kEEmcUnknownError;
  cerr << "EEmcException1:  unknown exception" << endl;
};



EEmcException1::EEmcException1(const EEmcErrno_t e, const char *msg, const int value)
{
  mErrno = e;
  cerr << "EEmcException1: ";
  if(msg!=NULL) cerr << msg ;
  cerr << " (errno=" << mErrno << ")" ;
  cerr << " value=" << value << endl;
};


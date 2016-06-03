// $Id: StiSsdIsActiveFunctor.cxx,v 1.2.10.2 2016/06/03 16:07:16 smirnovd Exp $
// 
// $Log: StiSsdIsActiveFunctor.cxx,v $
// Revision 1.2.10.2  2016/06/03 16:07:16  smirnovd
// Sync with MAIN branch as of 2016-05-31
//
// Revision 1.2  2005/06/21 15:31:48  lmartin
// CVS tags added
//
#include "StiSsd/StiSsdIsActiveFunctor.h"

StiSsdIsActiveFunctor::StiSsdIsActiveFunctor()
{} // StiSsdIsActiveFunctor

StiSsdIsActiveFunctor::~StiSsdIsActiveFunctor(){
} // ~StiSsdIsActiveFunctor

bool StiSsdIsActiveFunctor::operator()(double dYlocal, double dZlocal) const
{
  return true;
} // operator()

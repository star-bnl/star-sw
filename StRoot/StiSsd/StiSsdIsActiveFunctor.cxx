// $Id: StiSsdIsActiveFunctor.cxx,v 1.2 2005/06/21 15:31:48 lmartin Exp $
// 
// $Log: StiSsdIsActiveFunctor.cxx,v $
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

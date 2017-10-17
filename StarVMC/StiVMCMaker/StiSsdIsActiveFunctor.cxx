// $Id: StiSsdIsActiveFunctor.cxx,v 2.1 2009/04/29 14:36:34 fisyak Exp $
// 
// $Log: StiSsdIsActiveFunctor.cxx,v $
// Revision 2.1  2009/04/29 14:36:34  fisyak
// Freeze 0-th version of VMC base reconstruction
//
// Revision 1.2  2005/06/21 15:31:48  lmartin
// CVS tags added
//
#include "StiSsdIsActiveFunctor.h"

StiSsdIsActiveFunctor::StiSsdIsActiveFunctor()
{} // StiSsdIsActiveFunctor

StiSsdIsActiveFunctor::~StiSsdIsActiveFunctor(){
} // ~StiSsdIsActiveFunctor

bool StiSsdIsActiveFunctor::operator()(double dYlocal, double dZlocal) const
{
  return true;
} // operator()

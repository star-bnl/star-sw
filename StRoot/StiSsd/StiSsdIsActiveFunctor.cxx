// $Id: StiSsdIsActiveFunctor.cxx,v 1.2.10.1 2016/06/03 15:49:02 smirnovd Exp $
// 
// $Log: StiSsdIsActiveFunctor.cxx,v $
// Revision 1.2.10.1  2016/06/03 15:49:02  smirnovd
// Revert "Squashed commit of the following:"
//
// This reverts commit b0c5699a781ed8e5724e065390d3870af5de5b7c.
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

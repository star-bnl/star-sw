#include "StiSvt/StiSvtIsActiveFunctor.h"

StiSvtIsActiveFunctor::StiSvtIsActiveFunctor()
{} // StiSvtIsActiveFunctor

StiSvtIsActiveFunctor::~StiSvtIsActiveFunctor(){
} // ~StiSvtIsActiveFunctor

bool StiSvtIsActiveFunctor::operator()(double dYlocal, double dZlocal) const
{
  return true;
} // operator()

#include "Stiostream.h"
#include "StiPxlIsActiveFunctor.h"

StiPxlIsActiveFunctor::StiPxlIsActiveFunctor()
{
} // StiPxlIsActiveFunctor

StiPxlIsActiveFunctor::~StiPxlIsActiveFunctor()
{
} // ~StiPxlIsActiveFunctor

bool StiPxlIsActiveFunctor::operator()(double dYlocal, double dZlocal) const
{
   return true;
   //return false;
} // operator()


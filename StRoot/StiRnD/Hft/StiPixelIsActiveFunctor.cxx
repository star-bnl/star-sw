#include "Stiostream.h"
#include "StiPixelIsActiveFunctor.h"

StiPixelIsActiveFunctor::StiPixelIsActiveFunctor(){
} // StiPixelIsActiveFunctor

StiPixelIsActiveFunctor::~StiPixelIsActiveFunctor(){
} // ~StiPixelIsActiveFunctor

bool StiPixelIsActiveFunctor::operator()(double dYlocal, double dZlocal) const
{
    return true;
    //return false;
} // operator()


#include <iostream>
#include "StiPixelIsActiveFunctor.h"

StiPixelIsActiveFunctor::StiPixelIsActiveFunctor(){
} // StiPixelIsActiveFunctor

StiPixelIsActiveFunctor::~StiPixelIsActiveFunctor(){
} // ~StiPixelIsActiveFunctor

bool StiPixelIsActiveFunctor::operator()(double dYlocal, double dZlocal)
{
    return true;
    //return false;
} // operator()


#include "Stiostream.h"
#include "StiIstIsActiveFunctor.h"

StiIstIsActiveFunctor::StiIstIsActiveFunctor(){
} // StiIstIsActiveFunctor

StiIstIsActiveFunctor::~StiIstIsActiveFunctor(){
} // ~StiIstIsActiveFunctor

bool StiIstIsActiveFunctor::operator()(double dYlocal, double dZlocal) const
{
    return true;
    //return false;
} // operator()


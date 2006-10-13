#include "Stiostream.h"
#include "StiHpdIsActiveFunctor.h"

StiHpdIsActiveFunctor::StiHpdIsActiveFunctor(){
} // StiHpdIsActiveFunctor

StiHpdIsActiveFunctor::~StiHpdIsActiveFunctor(){
} // ~StiHpdIsActiveFunctor

bool StiHpdIsActiveFunctor::operator()(double dYlocal, double dZlocal) const
{
    return true;
    
} 


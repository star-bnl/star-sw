#include "StiSvtIsActiveFunctor.h"

StiSvtIsActiveFunctor::StiSvtIsActiveFunctor(){
} // StiSvtIsActiveFunctor

StiSvtIsActiveFunctor::~StiSvtIsActiveFunctor(){
} // ~StiSvtIsActiveFunctor

bool StiSvtIsActiveFunctor::operator()(double dYlocal, double dZlocal){
  return true;
} // operator()

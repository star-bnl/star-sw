#include "StiNeverActiveFunctor.h"

StiNeverActiveFunctor::StiNeverActiveFunctor(){
} // StiNeverActiveFunctor

StiNeverActiveFunctor::~StiNeverActiveFunctor(){
} // ~StiNeverActiveFunctor

bool StiNeverActiveFunctor::operator()(double dYlocal, double dZlocal){
  return false;
} // operator()

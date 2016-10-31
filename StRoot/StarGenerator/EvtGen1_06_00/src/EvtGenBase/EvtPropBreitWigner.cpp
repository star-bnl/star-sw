#include "EvtGenBase/EvtPatches.hh"
/*******************************************************************************
 * Project: BaBar detector at the SLAC PEP-II B-factory
 * Package: EvtGenBase
 *    File: $Id: EvtPropBreitWigner.cpp,v 1.1 2016/10/28 16:52:47 jwebb Exp $
 *  Author: Alexei Dvoretskii, dvoretsk@slac.stanford.edu, 2001-2002
 *
 * Copyright (C) 2002 Caltech
 *******************************************************************************/

#include <math.h>
#include "EvtGenBase/EvtConst.hh"
#include "EvtGenBase/EvtPropBreitWigner.hh"


EvtPropBreitWigner::EvtPropBreitWigner(double m0, double g0) 
  : EvtPropagator(m0,g0) 
{}


EvtPropBreitWigner::EvtPropBreitWigner(const EvtPropBreitWigner& other) 
  : EvtPropagator(other)
{}
  
 
EvtPropBreitWigner::~EvtPropBreitWigner() 
{}
  

EvtAmplitude<EvtPoint1D>* EvtPropBreitWigner::clone() const
{ 
  return new EvtPropBreitWigner(*this); 
}


EvtComplex EvtPropBreitWigner::amplitude(const EvtPoint1D& x) const
{
  double m = x.value();
  EvtComplex value = sqrt(_g0/EvtConst::twoPi)/(m-_m0-EvtComplex(0.0,_g0/2.));
  return value;
}

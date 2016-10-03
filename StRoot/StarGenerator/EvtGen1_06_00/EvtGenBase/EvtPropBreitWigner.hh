/*******************************************************************************
 * Project: BaBar detector at the SLAC PEP-II B-factory
 * Package: EvtGenBase
 *    File: $Id: EvtPropBreitWigner.hh,v 1.1 2016/09/23 18:37:32 jwebb Exp $
 *  Author: Alexei Dvoretskii, dvoretsk@slac.stanford.edu, 2001-2002
 *
 * Copyright (C) 2002 Caltech
 *******************************************************************************/

// Non-relativistic Breit-Wigner propagator

#ifndef EVT_PROP_BREIT_WIGNER_HH
#define EVT_PROP_BREIT_WIGNER_HH

#include "EvtGenBase/EvtComplex.hh"
#include "EvtGenBase/EvtPropagator.hh"

class EvtPropBreitWigner : public EvtPropagator {  
public:
  
  EvtPropBreitWigner(double m0, double g0);
  EvtPropBreitWigner(const EvtPropBreitWigner& other);
  ~EvtPropBreitWigner();
  
  EvtAmplitude<EvtPoint1D>* clone() const;

protected:

  EvtComplex amplitude(const EvtPoint1D& m) const;

};


#endif


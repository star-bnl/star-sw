//--------------------------------------------------------------------------
//--------------------------------------------------------------------------
//
// Environment:
//      This software is part of the EvtGen package developed jointly
//      for the BaBar and CLEO collaborations.  If you use all or part
//      of it, please give an appropriate acknowledgement.
//
// Copyright Information: See EvtGen/COPYRIGHT
//      Copyright (C) 1999      Caltech, UCSB
//
// Module: EvtSVPHelCPMix.hh
//
// Description: The decay of a scalar Bs meson to a vector particle and a photon is
//              performed with CP violation and different widths for
//              the heavy and light states (DeltaGamma_s =! 0). E.g. Bs->phi gamma.
//
// Modification history:
//
//    Clara Remon (Clara.Remon@ific.uv.es)       September 24, 2015      Module EvtSVPHelCPMix created
//
//--------------------------------------------------------------------------
//
#ifndef EVTSVPHELCPMIX_HH
#define EVTSVPHELCPMIX_HH

#include "EvtGenBase/EvtDecayAmp.hh"


class EvtParticle;


class EvtSVPHelCPMix : public EvtDecayAmp {

public:

  EvtSVPHelCPMix() {}
  virtual ~EvtSVPHelCPMix();

  std::string getName();
  EvtDecayBase* clone();

  void initProbMax();
  void init();

  void decay(EvtParticle *p);
  
};

#endif

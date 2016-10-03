//--------------------------------------------------------------------------
//
// Environment:
//      This software is part of the EvtGen package developed jointly
//      for the BaBar and CLEO collaborations.  If you use all or part
//      of it, please give an appropriate acknowledgement.
//
// Copyright Information: See EvtGen/COPYRIGHT
//      Copyright (C) 1998      Caltech, UCSB
//
// Module: EvtGen/EvtSVPHelAmp.hh
//
// Description: Routine to decay scalar -> vector + photon
//              by specifying the helicity amplitudes
//
// Modification history:
//
//    DJL/RYD                                    August 11, 1998         Module created
//    Clara Remon (Clara.Remon@ific.uv.es)       September 24, 2015      Function SVPHel created
//
//------------------------------------------------------------------------

#ifndef EVTSVPHELAMP_HH
#define EVTSVPHELAMP_HH

#include "EvtGenBase/EvtDecayAmp.hh"

//Class to handle decays of the form SCALAR -> VECTOR PHOTON
//where the helicity amplitudes must be specified. The
//first and third arguments are the magnitudes of the H+
//and H- helicity amplitudes respectively. The second and
//fourth arguements are the phases.
//Calls EvtSVPHel.

class EvtParticle;
class EvtAmp;
class EvtId;

class EvtSVPHelAmp:public  EvtDecayAmp  {

public:

  EvtSVPHelAmp() {}
  virtual ~EvtSVPHelAmp();

  std::string getName();
  EvtDecayBase* clone();

  void init();
  void initProbMax();

  void decay(EvtParticle *p); 
  
  static void SVPHel(EvtParticle *parent, EvtAmp& amp, EvtId n_v1, EvtId n_ph,
		     const EvtComplex& hp, const EvtComplex& hm);

  
};

#endif

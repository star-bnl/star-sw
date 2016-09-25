//--------------------------------------------------------------------------
//
// Environment:
//      This software is part of the EvtGen package developed jointly
//      for the BaBar and CLEO collaborations.  If you use all or part
//      of it, please give an appropriate acknowledgement.
//
// Copyright Information: See EvtGen/COPYRIGHT
//      Copyright (C) 2003      Caltech
//
// Module: EvtB2MuMuMuNu.hh
//
// Description: 
//
// Modification history:
//
//  Nikolai Nikitin (Lomonosov Moscow State Univ.)  August 04, 2015    Module created
//                   Email: Nikolai.Nikitine@cern.ch
//
//------------------------------------------------------------------------

#ifndef EVTBUTOLLLN_HH
#define EVTBUTOLLLN_HH

#include "EvtGenBase/EvtDecayAmp.hh"

class EvtParticle;
class EvtbTosllMSFF;     // my class with ff for the rare semileptonic B-decays
class EvtB2MuMuMuNuAmp;  // my class with amplitudes for four-leptonic B^{pm}-decays


class EvtB2MuMuMuNu:public  EvtDecayAmp{

public:

  EvtB2MuMuMuNu() {} ;
  virtual ~EvtB2MuMuMuNu();

  virtual std::string getName() ;
  virtual EvtDecayBase* clone();

  virtual void init();
  virtual void initProbMax();
  virtual void decay(EvtParticle *p);

private:
  EvtbTosllMSFF         *_msffmodel;
  EvtB2MuMuMuNuAmp      *_calcamp;
};

#endif

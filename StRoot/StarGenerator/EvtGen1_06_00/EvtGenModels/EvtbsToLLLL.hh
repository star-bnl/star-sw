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
// Module: EvtGen/EvtbsToLLLL.hh
//
// Description: 
//
// Modification history:
//
//  N.Nikitin (nnikit@mail.cern.ch)  July 24, 2011    Module created
//
//------------------------------------------------------------------------

#ifndef EVTBSTOLLLL_HH
#define EVTBSTOLLLL_HH

#include "EvtGenBase/EvtDecayAmp.hh"

class EvtParticle;
class Evtbs2llGammaFF;       // my class with ff for rare semileptonic B-decays
class EvtbsToLLLLAmp;        // my class with amplitudes for rare four-leptonic B-decays
class EvtbTosllWilsCoeffNLO; // my class with Wilson coefficients in NLO

class EvtbsToLLLL:public  EvtDecayAmp{

public:

  EvtbsToLLLL() {} ;
  virtual ~EvtbsToLLLL();

  virtual std::string getName() ;
  virtual EvtDecayBase* clone();

  virtual void init();
  virtual void initProbMax();
  virtual void decay(EvtParticle *p);

private:
  Evtbs2llGammaFF       *_mntffmodel;
  EvtbsToLLLLAmp        *_calcamp;
  EvtbTosllWilsCoeffNLO *_wilscoeff;
};

#endif

//--------------------------------------------------------------------------
//
// Environment:
//      This software is part of the EvtGen package developed jointly
//      for the BaBar and CLEO collaborations.  If you use all or part
//      of it, please give an appropriate acknowledgement.
//
// Copyright Information: See EvtGen/COPYRIGHT
//      Copyright (C) 2000      Caltech
//
// Module: Evtbs2llGammaFF.hh
// 
// Description: This is the NEW base class for form factors in b->sll transitions.
//
// Modification history:
//
//  N.Nikitin (nnikit@mail.cern.ch) October 13, 2008	Module created
//  A.Popov			    October 30, 2008	Add function "getPhotonFF"
//
//--------------------------------------------------------------------------------

#ifndef EVTBS2LLGAMMAFF_HH
#define EVTBS2LLGAMMAFF_HH

class EvtId;       // see H.Schildt, p.297 (in russian)
class EvtComplex;

class Evtbs2llGammaFF{
  public:
  virtual ~Evtbs2llGammaFF( ) { } ;
  
  virtual void getPhotonFF(int /*decay_id*/, double /*fb*/, EvtId /*parent*/, 
                           double /*q2*/, double /*M1*/, double /*mb*/, 
                           double /*mq*/, EvtComplex /*c7gam*/, EvtComplex /*a1*/,  
                           EvtComplex /*lambda_qu */, EvtComplex /*lambda_qc*/, 
                           EvtComplex& /*Fv*/,  EvtComplex& /*Fa*/, 
                           EvtComplex& /*Ftv*/, EvtComplex& /*Fta*/) {return;};   
  
  virtual double getQuarkMass(int /*i*/) {return 0.0;};
  
};

#endif

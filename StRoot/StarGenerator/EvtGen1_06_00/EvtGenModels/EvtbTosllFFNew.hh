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
// Module: EvtbTosllFFNew.hh
// 
// Description: This is the NEW base class for form factors in b->sll transitions.
//
// Modification history:
//
//  N.Nikitin (nnikit@mail.cern.ch) Avril 18, 2008  Module created
//
//------------------------------------------------------------------------

#ifndef EVTBTOSLLFFNEW_HH
#define EVTBTOSLLFFNEW_HH

class EvtId;  // see H.Schildt, p.297 (in russian)

class EvtbTosllFFNew{

 public:
  virtual ~EvtbTosllFFNew( ) { } ;
    
  virtual void getScalarFF(EvtId /*parent*/, EvtId /*daught*/,double /*t*/, 
                           double& /*fp*/,double& /*f0*/,
                           double& /*ft*/) {return;}

  virtual void getVectorFF(EvtId /*parent*/, EvtId /*daught*/,double /*t*/, 
                           double& /*a1*/, double& /*a2*/,
                           double& /*a0*/, double& /*v*/, double& /*t1*/, 
                           double& /*t2*/, double& /*t3*/ ) {return;}
  
  virtual double getQuarkMass(int /*i*/) {return 0.0;}

};

#endif

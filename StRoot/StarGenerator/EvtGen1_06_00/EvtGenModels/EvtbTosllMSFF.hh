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
// Module:      EvtbTosllMSFF.hh
// Description: Form factors for B^0_d -> K^* l^+ l^- transition according 
//              to the paper: D.Melikhov, B.Stech, PRD62, 014006 (2000).
//
// Modification history:
//
//    N.Nikitin   March 13, 2008         Module created
//
//------------------------------------------------------------------------

#ifndef EVTBTOSLLMSFF_HH
#define EVTBTOSLLMSFF_HH

#include "EvtGenModels/EvtbTosllFFNew.hh"

class EvtId;

class EvtbTosllMSFF : public EvtbTosllFFNew{

public:

  EvtbTosllMSFF();
 
  double equation9_10(double ff0, double M2, double q2, 
                      double sigma1, double sigma2, int eq_num);

  void getScalarFF(EvtId parent, EvtId daught,double t, 
		   double& fp,double& f0,double& ft);

  void getVectorFF(EvtId parent, EvtId daught,double t, 
		   double& a1,double& a2,double& a0, double& v,
		   double& t1, double& t2, double& t3 );

  double getQuarkMass(int i);

private:

};

#endif


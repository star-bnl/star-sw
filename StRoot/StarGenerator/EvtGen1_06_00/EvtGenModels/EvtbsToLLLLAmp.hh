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
// Module: EvtGen/EvtbsToLLLLAmp.hh
//
// Description:
//
// Modification history:
//
//  N.Nikitin	Jult 24, 2011	Module created
//
//------------------------------------------------------------------------

#ifndef EVTBSTOLLLL_AMP_HH
#define EVTBSTOLLLL_AMP_HH


class EvtId;
class EvtAmp;
class EvtParticle;
class Evtbs2llGammaFF;
class EvtbTosllWilsCoeffNLO;

class EvtbsToLLLLAmp{

  public:

    void CalcAmp( EvtParticle *parent, EvtAmp& amp,
		  Evtbs2llGammaFF *formFactors,
                  EvtbTosllWilsCoeffNLO *WilsCoeff, 
                  double mu, int Nf,   
                  int res_swch, int ias,
                  double CKM_A, double CKM_lambda, 
                  double CKM_barrho, double CKM_bareta);

    double CalcMaxProb(
//                       EvtId parnum, 
//                       EvtId l1num, EvtId l2num,
//                       EvtId l3num, EvtId l4num,
//		         Evtbs2llGammaFF *formFactors,
//                       EvtbTosllWilsCoeffNLO *WilsCoeff, 
//                       double mu, int Nf, int res_swch, int ias, 
//                       double CKM_A, double CKM_lambda, 
//                       double CKM_barrho, double CKM_bareta
                      );

    double lambda(double a, double b, double c); 

};

#endif


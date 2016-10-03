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
// Module: EvtGen/EvtbsToLLLLHyperCPAmp.hh
//
// Description:
//
// Modification history:
//
//  N.Nikitin	May 11, 2012	Module created
//
//------------------------------------------------------------------------

#ifndef EVTBSTOLLLL_HYPERCPAMP_HH
#define EVTBSTOLLLL_HYPERCPAMP_HH


class EvtId;
class EvtAmp;
class EvtParticle;

class EvtbsToLLLLHyperCPAmp{

  public:

    void CalcAmp( EvtParticle *parent, EvtAmp& amp,
                  double mS, double mP, 
                             double gammaS, double gammaP, 
                             double mLiiLR, 
                             double Fc, 
                             double mD23LL, double mD23RR, 
                             double mD32LL, double mD32RR, 
                             double mD13LL, double mD13RR, 
                             double mD31LL, double mD31RR );

    double CalcMaxProb(
                       EvtId parnum,
                       EvtId l1num, EvtId l2num, 
                       EvtId l3num, EvtId l4num,
                       double mS, double mP, 
                       double gammaS, double gammaP, 
                       double mLiiLR, 
                       double Fc, 
                       double mD23LL, double mD23RR, 
                       double mD32LL, double mD32RR, 
                       double mD13LL, double mD13RR, 
                       double mD31LL, double mD31RR
                      );

    double lambda(double a, double b, double c); 

};

#endif


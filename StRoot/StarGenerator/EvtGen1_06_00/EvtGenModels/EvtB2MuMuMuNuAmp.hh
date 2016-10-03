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
// Module: EvtB2MuMuMuNuAmp.hh
//
// Description:
//
// Modification history:
//
//  Nikolai Nikitin (Lomonosov Moscow State Univ.)  August 06, 2015    Module created
//                   Email: Nikolai.Nikitine@cern.ch
//
//------------------------------------------------------------------------

#ifndef EVTBUTOLLLN_AMP_HH
#define EVTBUTOLLLN_AMP_HH


class EvtId;
class EvtAmp;
class EvtParticle;
class EvtbTosllMSFF;     // my class with ff for the rare semileptonic B-decays

class EvtB2MuMuMuNuAmp{

  public:

    void CalcAmp( EvtParticle *parent, 
                  EvtAmp& amp,
                  EvtbTosllMSFF   *formFactorsms
                  );

    double CalcMaxProb();

    double lambda(double a, double b, double c); 
    
    double FF_B2BstarGamma_fromU(double q2);
    
    double FF_B2BstarGamma_fromB(double q2);

};

#endif


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
// Module:      Evtbs2llGammaFFMNT.hh
// Description: Form factors for rare radiative leptonic B-decays 
//              according to the papers:
//              1) F.Kruger, D.Melikhov,  Phys. Rev. D67, 034002, 2003.  
//              2) D.Melikhov, N.Nikitin, Phys. Rev. D70, 114028, 2004. 
//              3) I.Balakireva, D.Melikhov, N.Nikitin, D.Tlisov, 
//                                           e-Print: arXiv:0911.0605 [hep-ph].
//
// Modification history:
//
//   A.Popov	October  30, 2008	Module created
//   N.Nikitin  February 25, 2010       Module modifided
//
//------------------------------------------------------------------------

#ifndef EVTBS2LLGAMMAFFMNT_HH
#define EVTBS2LLGAMMAFFMNT_HH

#include "EvtGenModels/Evtbs2llGammaFF.hh"

class EvtId;

class EvtComplex;

class Evtbs2llGammaFFMNT : public Evtbs2llGammaFF{

  public:

    Evtbs2llGammaFFMNT();

    void getPhotonFF(int decay_id, double fb, EvtId parent,
                     double q2, double M1, double mb, double mq,
                     EvtComplex c7gam, EvtComplex a1,  
                     EvtComplex lambda_qu, EvtComplex lambda_qc, 
                     EvtComplex& Fv,  EvtComplex& Fa, EvtComplex& Ftv, EvtComplex& Fta);

    double getQuarkMass(int i);

};

#endif


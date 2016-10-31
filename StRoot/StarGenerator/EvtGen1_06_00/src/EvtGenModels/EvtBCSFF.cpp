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
// Module: EvtBCSFF.cc
//
// Description: form factors for Bc->Slnu 
//
// Modification history:
//
//    AVL Jul 6, Module created
//
//------------------------------------------------------------------------
// 
#include "EvtGenBase/EvtPatches.hh"
#include "EvtGenBase/EvtReport.hh"
#include "EvtGenModels/EvtBCSFF.hh"

#include "EvtGenBase/EvtId.hh"
#include <string>
#include <math.h>
#include "EvtGenBase/EvtPDL.hh"
#include <stdlib.h>
#include <iostream>

using namespace std;

EvtBCSFF::EvtBCSFF(int idS, int fit) {

  idScalar = idS;
  whichfit = fit;
  //cout<<"==== EvtBCSFF:: idScalar="<<idScalar<<" whichfit="<<whichfit<<endl;
  return;
}

void EvtBCSFF::getscalarff(EvtId /*p*/, EvtId /*d*/, double t, double /*mass*/,
                           double *fpf, double *f0f) {
  
  double q2 = t;

  if (whichfit == 0) {
    *fpf = 1;
    *f0f = 0;
    return;
  }

  if (idScalar == EvtPDL::getId("chi_c0").getId() ) { // Bc -> chi_c0
    if (whichfit == 3) { // FF from Wang et al 10.1103/PhysRevD.79.114018
      double Mbc = 6.277; // Experimental value
      //double Mchi = 3.41475; // Experimental value
      double ratio = q2 / (Mbc*Mbc);
      
      double fpf_0 = 0.47;
      double fpf_c1 = 2.03;
      double fpf_c2 = 0.43;

      double f0f_0 = 0.47;
      double f0f_c1 = -0.45;
      double f0f_c2 = -1.31;
      
      *fpf = fpf_0*exp(fpf_c1*ratio+fpf_c2*ratio*ratio);
      *f0f = f0f_0*exp(f0f_c1*ratio+f0f_c2*ratio*ratio);
      return;

    } else {
      EvtGenReport(EVTGEN_ERROR,"EvtGen") << "Must choose 0 (fpf = 1) or 3 (Wang).\n";  
      ::abort();
    }
  } else {
    EvtGenReport(EVTGEN_ERROR,"EvtGen") << "Only chi_c0 implemented in EvtBCSFF.\n";  
    ::abort();
  }
}

void EvtBCSFF::getvectorff(EvtId, EvtId, double, double,
                           double*, double*, double*, double*) {

  EvtGenReport(EVTGEN_ERROR,"EvtGen") << "Not implemented :getvectorff in EvtBCSFF.\n";  
  ::abort();

}

void EvtBCSFF::gettensorff(EvtId /*p*/, EvtId /*d*/, double t, double /*mass*/,
                              double*, double*, double*, double*) {
  

  EvtGenReport(EVTGEN_ERROR,"EvtGen") << "Not implemented :gettensorff in EvtBCSFF.\n";  
  ::abort();

}

void EvtBCSFF::getbaryonff(EvtId, EvtId, double, double,
                           double*, double*, double*, double*) {
  
  EvtGenReport(EVTGEN_ERROR,"EvtGen") << "Not implemented :getbaryonff in EvtBCSFF.\n";  
  ::abort();

}

void EvtBCSFF::getdiracff(EvtId, EvtId, double, double,
                          double*, double*, double*, double*, double*, double*) {
  
  EvtGenReport(EVTGEN_ERROR,"EvtGen") << "Not implemented :getdiracff in EvtBCSFF.\n";
  ::abort();

}

void EvtBCSFF::getraritaff(EvtId, EvtId, double, double,
                           double*, double*, double*, double*, double*, double*, double*, double*) {
  
  EvtGenReport(EVTGEN_ERROR,"EvtGen") << "Not implemented :getraritaff in EvtBCSFF.\n";
  ::abort();

}


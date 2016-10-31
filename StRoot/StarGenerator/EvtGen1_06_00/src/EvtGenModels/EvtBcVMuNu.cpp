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
// Module: EvtBcVMuNu.cc
//
// Description: Routine to implement semileptonic B->psi lnu decays 
//
// Modification history:
//
//    AVL     July 6, 2012        Module created
//
//------------------------------------------------------------------------
// 
#include "EvtGenBase/EvtPatches.hh"
#include <stdlib.h>
#include "EvtGenBase/EvtParticle.hh"
#include "EvtGenBase/EvtGenKine.hh"
#include "EvtGenBase/EvtPDL.hh"
#include "EvtGenBase/EvtReport.hh"
#include "EvtGenBase/EvtSemiLeptonicVectorAmp.hh"
#include <string>
#include <iostream>

#include "EvtGenModels/EvtBcVMuNu.hh"
#include "EvtGenModels/EvtBCVFF.hh"


using namespace std;



EvtBcVMuNu::~EvtBcVMuNu() {
//   cout<<"EvtBcVMuNu::destructor getProbMax(-1) = "<<getProbMax(-1)<<endl;
}

std::string EvtBcVMuNu::getName(){
  return "BC_VMN";     
}


EvtDecayBase* EvtBcVMuNu::clone(){
//   cout<<" === EvtBcVMuNu::clone() ============"<<endl;
  return new EvtBcVMuNu;

}

void EvtBcVMuNu::decay( EvtParticle *p ){
//  cout<<" === EvtBcVMuNu::decay() ============"<<endl;

  p->initializePhaseSpace(getNDaug(),getDaugs());
  calcamp->CalcAmp(p,_amp2,ffmodel);
//  cout<<"EvtBcVMuNu::decay() getProbMax(-1) = "<<getProbMax(-1)<<endl;
}


void EvtBcVMuNu::init(){
//   cout<<" === EvtBcVMuNu::init() ============"<<endl;
 
  
  checkNArg(1);
  checkNDaug(3);

  //We expect the parent to be a scalar 
  //and the daughters to be X lepton neutrino

  checkSpinParent(EvtSpinType::SCALAR);

  checkSpinDaughter(0,EvtSpinType::VECTOR);
  checkSpinDaughter(1,EvtSpinType::DIRAC);
  checkSpinDaughter(2,EvtSpinType::NEUTRINO);

  idVector = getDaug(0).getId();
  whichfit = int(getArg(0)+0.1);
  cout << "EvtBcVMuNu: whichfit = " << whichfit << "  idVector = " << idVector << endl;
  ffmodel = new EvtBCVFF(idVector,whichfit);

  calcamp = new EvtSemiLeptonicVectorAmp; 
 
}

void EvtBcVMuNu::initProbMax() {
//  cout<<" === EvtBcVMuNu::initProbMax() ============"<<endl;
  if (whichfit == 0) setProbMax(1700.);
  else if (idVector == EvtPDL::getId("J/psi").getId() && whichfit == 1) setProbMax(40000.0); // > 26.6k for mu, 20.0k for tau
  else if (idVector == EvtPDL::getId("J/psi").getId() && whichfit == 2) setProbMax(15000.); // > 12.8k for mu, 9.7k for tau
  else if (idVector == EvtPDL::getId("psi(2S)").getId() && whichfit == 1) setProbMax(700.); // > 600 for mu, 350 for tau
  else if (idVector == EvtPDL::getId("psi(2S)").getId() && whichfit == 2) setProbMax(300.); // > 216 for mu, 43 for tau
  else if (idVector == EvtPDL::getId("chi_c1").getId() && whichfit == 3) setProbMax(2000.0); // > 2000 for mu, 940 for tau
  else {
    cout<<"EvtBcVMuNu: Not realized yet"<<endl;
    ::abort();
  }
}

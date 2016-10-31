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
// Module: EvtBcSMuNu.cc
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
#include "EvtGenBase/EvtSemiLeptonicScalarAmp.hh"
#include <string>
#include <iostream>

#include "EvtGenModels/EvtBcSMuNu.hh"
#include "EvtGenModels/EvtBCSFF.hh"


using namespace std;



EvtBcSMuNu::~EvtBcSMuNu() {
//   cout<<"EvtBcSMuNu::destructor getProbMax(-1) = "<<getProbMax(-1)<<endl;
}

std::string EvtBcSMuNu::getName(){
  return "BC_SMN";     
}


EvtDecayBase* EvtBcSMuNu::clone(){
//   cout<<" === EvtBcSMuNu::clone() ============"<<endl;
  return new EvtBcSMuNu;

}

void EvtBcSMuNu::decay( EvtParticle *p ){
//  cout<<" === EvtBcSMuNu::decay() ============"<<endl;

  p->initializePhaseSpace(getNDaug(),getDaugs());
  calcamp->CalcAmp(p,_amp2,ffmodel);
//  cout<<"EvtBcSMuNu::decay() getProbMax(-1) = "<<getProbMax(-1)<<endl;
}


void EvtBcSMuNu::init(){
//   cout<<" === EvtBcSMuNu::init() ============"<<endl;
 
  
  checkNArg(1);
  checkNDaug(3);

  //We expect the parent to be a scalar 
  //and the daughters to be X lepton neutrino

  checkSpinParent(EvtSpinType::SCALAR);

  checkSpinDaughter(0,EvtSpinType::SCALAR);
  checkSpinDaughter(1,EvtSpinType::DIRAC);
  checkSpinDaughter(2,EvtSpinType::NEUTRINO);

  idScalar = getDaug(0).getId();
  whichfit = int(getArg(0)+0.1);
  cout << "EvtBcSMuNu: whichfit =" << whichfit << "  idScalar =" << idScalar << endl;
  ffmodel = new EvtBCSFF(idScalar,whichfit);
  
  calcamp = new EvtSemiLeptonicScalarAmp; 
 
}

void EvtBcSMuNu::initProbMax() {
  
//   PrintMaxProbs();

  //  cout<<" === EvtBcSMuNu::initProbMax() ============"<<endl;
  if (whichfit == 0) setProbMax(0.0); // NEED TO FIX
  else if (idScalar == EvtPDL::getId("chi_c0").getId() && whichfit == 3) setProbMax(3000.0); // NEED TO FIX
  else {
    cout<<"EvtBcSMuNu: Not realized yet"<<endl;
    ::abort();
  }
}

void EvtBcSMuNu::PrintMaxProbs()
{
  EvtId BcID = EvtPDL::getId("B_c+");
  EvtId Chic0ID = EvtPDL::getId("chi_c0");
  EvtId MuID = EvtPDL::getId("mu+");
  EvtId TauID = EvtPDL::getId("tau+");
  EvtId NuMuID = EvtPDL::getId("nu_mu");
  EvtId NuTauID = EvtPDL::getId("nu_tau");
  
  EvtBCSFF* testmodel;
  double mu_maxprob, tau_maxprob;

  // CHIC0
  testmodel = new EvtBCSFF(Chic0ID.getId(),3);
  mu_maxprob = calcamp->CalcMaxProb(BcID,Chic0ID,MuID,NuMuID,testmodel);
  tau_maxprob = calcamp->CalcMaxProb(BcID,Chic0ID,TauID,NuTauID,testmodel);
  cout << "B_c => chi_c0(1P) l nu transition w/ Wang:\n";
  cout << " --> Mu max prob should be: " << mu_maxprob << endl;
  cout << " --> Tau max prob should be: " << tau_maxprob << endl;
  delete testmodel;
}

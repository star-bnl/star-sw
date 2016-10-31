
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
// Module: EvtBcTMuNu.cc
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
#include "EvtGenBase/EvtSemiLeptonicTensorAmp.hh"
#include <string>
#include <iostream>

#include "EvtGenModels/EvtBcTMuNu.hh"
#include "EvtGenModels/EvtBCTFF.hh"


using namespace std;



EvtBcTMuNu::~EvtBcTMuNu() {
//   cout<<"EvtBcTMuNu::destructor getProbMax(-1) = "<<getProbMax(-1)<<endl;
}

std::string EvtBcTMuNu::getName(){
  return "BC_TMN";     
}


EvtDecayBase* EvtBcTMuNu::clone(){
//   cout<<" === EvtBcTMuNu::clone() ============"<<endl;
  return new EvtBcTMuNu;

}

void EvtBcTMuNu::decay( EvtParticle *p ){
//  cout<<" === EvtBcTMuNu::decay() ============"<<endl;

  p->initializePhaseSpace(getNDaug(),getDaugs());
  calcamp->CalcAmp(p,_amp2,ffmodel);
  cout<<"EvtBcTMuNu::decay() getProbMax(-1) = "<<getProbMax(-1)<<endl;
}


void EvtBcTMuNu::init(){
//   cout<<" === EvtBcTMuNu::init() ============"<<endl;
 
  
  checkNArg(1);
  checkNDaug(3);

  //We expect the parent to be a scalar 
  //and the daughters to be X lepton neutrino

  checkSpinParent(EvtSpinType::SCALAR);

  checkSpinDaughter(0,EvtSpinType::TENSOR);
  checkSpinDaughter(1,EvtSpinType::DIRAC);
  checkSpinDaughter(2,EvtSpinType::NEUTRINO);
  
  idTensor = getDaug(0).getId();
  whichfit = int(getArg(0)+0.1);
  cout << "EvtBcTMuNu: whichfit =" << whichfit << "  idTensor=" << idTensor << endl;
  ffmodel = new EvtBCTFF(idTensor,whichfit);
  
  calcamp = new EvtSemiLeptonicTensorAmp; 
 
}

void EvtBcTMuNu::initProbMax() {

  PrintMaxProbs();

  //  cout<<" === EvtBcTMuNu::initProbMax() ============"<<endl;
  if (whichfit == 0) setProbMax(0.0); // NEED TO FIX!
  else if (idTensor == EvtPDL::getId("chi_c2").getId() && whichfit == 3) setProbMax(30.0); // NEED TO FIX
  else {
    cout<<"EvtBcTMuNu: Not realized yet"<<endl;
    ::abort();
  }

}

void EvtBcTMuNu::PrintMaxProbs()
{
  EvtId BcID = EvtPDL::getId("B_c+");
  //EvtId JpsiID = EvtPDL::getId("J/psi(1S)");
  //EvtId PsiID = EvtPDL::getId("psi(2S)");
  EvtId Chic2ID = EvtPDL::getId("chi_c2");
  EvtId MuID = EvtPDL::getId("mu+");
  EvtId TauID = EvtPDL::getId("tau+");
  EvtId NuMuID = EvtPDL::getId("nu_mu");
  EvtId NuTauID = EvtPDL::getId("nu_tau");
  
  EvtBCTFF* testmodel;
  double mu_maxprob, tau_maxprob;
  
  // CHIC2
  testmodel = new EvtBCTFF(Chic2ID.getId(),3);
  mu_maxprob = calcamp->CalcMaxProb(BcID,Chic2ID,MuID,NuMuID,testmodel);
  tau_maxprob = calcamp->CalcMaxProb(BcID,Chic2ID,TauID,NuTauID,testmodel);
  cout << "B_c => chi_c2(1P) l nu transition w/ Wang:\n";
  cout << " --> Mu max prob should be: " << mu_maxprob << endl;
  cout << " --> Tau max prob should be: " << tau_maxprob << endl;
  delete testmodel;
  
}



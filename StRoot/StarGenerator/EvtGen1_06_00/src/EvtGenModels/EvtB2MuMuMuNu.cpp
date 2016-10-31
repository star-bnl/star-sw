//--------------------------------------------------------------------------
//
// Environment:
//      This software is part of the EvtGen package developed jointly
//      for the BaBar and CLEO collaborations.  If you use all or part
//      of it, please give an appropriate acknowledgement.
//
// Copyright Information: See EvtGen/COPYRIGHT
//      Copyright (C) 2003      Caltech, UCSB
//
// Module: EvtB2MuMuMuNu.cpp
//
// Description: The main file for the model "BUTOMMMN" which simulated the
//              the very rare four-leptonic decays 
//              B^-(p) -> Mu^+(k_1) Mu^-(k_2) \bar Nu_{Mu}(k_3) Mu^-(k_4)  
//
// Modification history:
//
// Nikolai Nikitin (Lomonosov Moscow State Univ.)  August 04, 2015    Module created
//                   Email: Nikolai.Nikitine@cern.ch
//
//------------------------------------------------------------------------
// 
#include <stdlib.h>
#include <string.h>
#include "EvtGenBase/EvtParticle.hh"
#include "EvtGenBase/EvtGenKine.hh"
#include "EvtGenBase/EvtPDL.hh"
#include "EvtGenBase/EvtReport.hh"
#include "EvtGenModels/EvtB2MuMuMuNu.hh"
#include "EvtGenModels/EvtB2MuMuMuNuAmp.hh"
#include "EvtGenModels/EvtbTosllMSFF.hh"     // FF for Bu -> Rho and Omega transitions



EvtB2MuMuMuNu::~EvtB2MuMuMuNu() {
  delete _msffmodel;
  if ( _calcamp ) delete _calcamp ;
}


// The module name specification
std::string EvtB2MuMuMuNu::getName( ) {
  return "BUTOMMMN" ;
}


// The implementation of the clone() method
EvtDecayBase* EvtB2MuMuMuNu::clone(){
  return new EvtB2MuMuMuNu;
}


// The inicialization of the decay model
void EvtB2MuMuMuNu::init(){

  // check that there are 0 arguments
  checkNArg(0);
  // check that there are 4 daughteres
  checkNDaug(4);

  // We expect that the parent to be a scalar (B^{-} meson)
  // and the daughters to be Mu^+(k_1), Mu^-(k_2), barNu_{Mu}(k_3) and Mu^-(k_4).
  checkSpinParent(EvtSpinType::SCALAR);

  checkSpinDaughter(0,EvtSpinType::DIRAC);    // Mu^+(k_1)
  checkSpinDaughter(1,EvtSpinType::DIRAC);    // Mu^-(k_2)
  checkSpinDaughter(2,EvtSpinType::NEUTRINO); // barNu_{Mu}(k_3)
  checkSpinDaughter(3,EvtSpinType::DIRAC);    // Mu^-(k_4)

  _msffmodel = new EvtbTosllMSFF();
  _calcamp   = new EvtB2MuMuMuNuAmp(); 

}


// Set the maximum probability of the decay 
void EvtB2MuMuMuNu::initProbMax(){

  double mymaxprob=-10.0; // maximum of the probability

  mymaxprob = _calcamp->CalcMaxProb();

  if(mymaxprob <= 0.0){
     EvtGenReport(EVTGEN_ERROR,"EvtGen") << "The function void EvtB2MuMuMuNu::initProbMax()" 
					 << "\n Unexpected value of the probability maximum!"
					 << "\n mymaxprob = " << mymaxprob
					 <<std::endl;
     ::abort();
  }

  setProbMax(mymaxprob);

}




void EvtB2MuMuMuNu::decay( EvtParticle *p ){

  p->initializePhaseSpace(getNDaug(),getDaugs());

  _calcamp->CalcAmp(p,_amp2, _msffmodel);

}


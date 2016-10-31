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
// Module: EvtbsToLLLLHyperCP.ccp
//
// Description:  
//
// Modification history:
//
//    N.Nikitin      May  30, 2012        Module created
//
//------------------------------------------------------------------------
// 
#include <stdlib.h>
#include <string.h>
#include "EvtGenBase/EvtParticle.hh"
#include "EvtGenBase/EvtGenKine.hh"
#include "EvtGenBase/EvtPDL.hh"
#include "EvtGenBase/EvtReport.hh"
#include "EvtGenModels/EvtbsToLLLLHyperCPAmp.hh"
#include "EvtGenModels/EvtbsToLLLLHyperCP.hh"



EvtbsToLLLLHyperCP::~EvtbsToLLLLHyperCP() {
  if ( _calcamp ) delete _calcamp ;
}


// The module name specification
std::string EvtbsToLLLLHyperCP::getName( ) {
  return "BQTOLLLLHYPERCP" ;
}


// The implementation of the clone() method
EvtDecayBase* EvtbsToLLLLHyperCP::clone(){
  return new EvtbsToLLLLHyperCP;
}


// The inicialization of the decay model
//
// Tn the our model we have are following 14 arguments:
//
//               mS      - the mass of the scalar sgoldstino "S" (GeV);
//               mP      - the mass of the pseudoscalar sgoldstino "P" (GeV);
//           gammaS      - the decay width of the scalar sgoldstino "S" (GeV);
//           gammaP      - the decay width of the pseudoscalar sgoldstino "P" (GeV);
//           mLiiLR      -
//               Fc      - coupling constant (GeV);
//        mDijLL(RR)     - parameters for \bar Bq-decays
//        mDjiLL(RR)     - parameters for Bq-decays (i <-> j!)
//                         d==1, s==2, b==3
//
void EvtbsToLLLLHyperCP::init(){

  // check that there are 14 arguments
  checkNArg(14);
  // check that there are 4 daughteres
  checkNDaug(4);

  // We expect that the parent to be a scalar (B-meson)
  // and the daughters to be l^+, l^-, l^+ and l^-
  checkSpinParent(EvtSpinType::SCALAR);

  // We expect that the all daughters are the ell+ or ell- == DIRAC
  checkSpinDaughter(0,EvtSpinType::DIRAC);
  checkSpinDaughter(1,EvtSpinType::DIRAC);
  checkSpinDaughter(2,EvtSpinType::DIRAC);
  checkSpinDaughter(3,EvtSpinType::DIRAC);

  _calcamp    = new EvtbsToLLLLHyperCPAmp(); 

}


// Set the maximum probability of the decay 
void EvtbsToLLLLHyperCP::initProbMax(){

  double mymaxprob=-10.0; // maximum of the probability

  EvtId parnum, l1num, l2num, l3num, l4num;
  
  parnum = getParentId();
  l1num  = getDaug(0);
  l2num  = getDaug(1);
  l3num  = getDaug(2);
  l4num  = getDaug(3);

  double mS     = getArg(0); 
  double mP     = getArg(1); 
  double gammaS = getArg(2); 
  double gammaP = getArg(3); 
  double mLiiLR = getArg(4); 
  double Fc     = getArg(5); 
  double mD23LL = getArg(6); 
  double mD23RR = getArg(7); 
  double mD32LL = getArg(8); 
  double mD32RR = getArg(9); 
  double mD13LL = getArg(10); 
  double mD13RR = getArg(11); 
  double mD31LL = getArg(12); 
  double mD31RR = getArg(13);

  mymaxprob = _calcamp->CalcMaxProb(
                                   parnum, l1num,l2num, l3num,l4num,
                                   mS, mP, gammaS,gammaP, 
                                   mLiiLR, 
                                   Fc, 
                                   mD23LL, mD23RR, mD32LL, mD32RR, 
                                   mD13LL, mD13RR, mD31LL, mD31RR
                                   );

  if(mymaxprob <= 0.0){
     EvtGenReport(EVTGEN_ERROR,"EvtGen") << "The function void EvtbsToLLLLHyperCP::initProbMax()" 
       << "\n Unexpected value of the probability maximum!"
       << "\n mymaxprob = " << mymaxprob
       <<std::endl;
     ::abort();
  }

  setProbMax(mymaxprob);

}



void EvtbsToLLLLHyperCP::decay( EvtParticle *p ){

  double mS     = getArg(0); 
  double mP     = getArg(1); 
  double gammaS = getArg(2); 
  double gammaP = getArg(3); 
  double mLiiLR = getArg(4); 
  double Fc     = getArg(5); 
  double mD23LL = getArg(6); 
  double mD23RR = getArg(7); 
  double mD32LL = getArg(8); 
  double mD32RR = getArg(9); 
  double mD13LL = getArg(10); 
  double mD13RR = getArg(11); 
  double mD31LL = getArg(12); 
  double mD31RR = getArg(13);
  
  p->initializePhaseSpace(getNDaug(),getDaugs());

  _calcamp->CalcAmp(p, _amp2, mS, mP, gammaS, gammaP, mLiiLR, Fc, 
                    mD23LL, mD23RR, mD32LL, mD32RR, mD13LL, mD13RR, mD31LL, mD31RR); 

//  EvtGenReport(NOTICE,"EvtGen") << "\n The function EvtbsToLLLLHyperCP::decay(...) passed with arguments:"
//                        << "\n mS = " << mS 
//                        << "\n mP = " << mP 
//                        << "\n gammaS = " << gammaS 
//                        << "\n gammaP = " << gammaP 
//                        << "\n mLiiLR = " << mLiiLR
//                        << "\n Fc = " << Fc
//                        << "\n mD23LL = " << mD23LL
//                        << "\n mD23RR = " << mD23RR 
//                        << "\n mD32LL = " << mD32LL
//                        << "\n mD32RR = " << mD32RR
//                        << "\n mD13LL = " << mD13LL
//                        << "\n mD13RR = " << mD13RR
//                        << "\n mD31LL = " << mD31LL
//                        << "\n mD31RR = " << mD31RR 
//                        << std::endl;

}


/**********************************************************************
 *
 * $Id: StEStructBinning.cxx,v 1.1 2003/10/15 18:20:46 porter Exp $
 *
 * Author: Jeff Porter 
 *
 **********************************************************************
 *
 * Description:  Binning defs as c-structs for 2pt-analysis
 *
 *
 ***********************************************************************/
#include "StEStructBinning.h"
#include <math.h>


StEStructBinning* StEStructBinning::mInstance = 0;

StEStructBinning::StEStructBinning(){

  //--> phi ranges <--

  minPhi=-M_PI;
  maxPhi=M_PI;
  nPhi  = EBYE_PHI_BINS-1;
  dPhi  = (maxPhi-minPhi)/(float)nPhi;

  maxDPhi=maxPhi*1.5;// 2.0*maxPhi;
  minDPhi=minPhi*0.5;// 0.0; //2*minPhi;
  nDPhi = EBYE_DPHI_BINS-1;
  dDPhi=(maxDPhi-minDPhi)/(float)nDPhi;

  maxSPhi=2*maxPhi;
  minSPhi=2*minPhi;
  nSPhi = EBYE_SPHI_BINS-1;
  dSPhi=(maxSPhi-minSPhi)/(float)nSPhi;

  //--> eta ranges <--

  minEta=-1.0;
  maxEta=1.0;
  nEta = EBYE_ETA_BINS-1;
  dEta= (maxEta-minEta)/(float)nEta;

  maxDEta=2*maxEta;//2*maxEta;
  minDEta=2*minEta;//0; //2*minEta;
  nDEta=EBYE_DETA_BINS-1;
  dDEta=(maxDEta-minDEta)/(float)nDEta;

  maxSEta=2*maxEta;
  minSEta=2*minEta;
  nSEta = EBYE_SETA_BINS-1;
  dSEta=(maxSEta-minSEta)/(float)nSEta;

  //--> mt (x) ranges <--
  
  minMt=0.9; //0.15;
  maxMt=4.5; // 0.925;
  nMt = EBYE_MT_BINS-1;
  dMt = (maxMt-minMt)/(float)nMt;

  maxDMt=4.;//maxMt;
  minDMt=-4.;//-maxMt;// 0; //-maxMt;
  nDMt= EBYE_DMT_BINS-1;
  dDMt=(maxDMt-minDMt)/(float)nDMt; 

  maxSMt=9.; //2 * maxMt + 1.0;
  minSMt=0.9; //2 * minMt;
  nSMt = EBYE_SMT_BINS-1;
  dSMt=(maxSMt-minSMt)/(float)nSMt;

  maxDeltaMt=5.;//3.0;
  minDeltaMt=0.;//0; //-3.0;
  nDeltaMt = EBYE_DELTAMT_BINS-1;
  dDeltaMt=(maxDeltaMt-minDeltaMt)/(float)nDeltaMt; 

};


/***********************************************************************
 *
 * $Log: StEStructBinning.cxx,v $
 * Revision 1.1  2003/10/15 18:20:46  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/










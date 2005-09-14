/**********************************************************************
 *
 * $Id: StEStructBinning.cxx,v 1.4 2005/09/14 17:14:21 msd Exp $
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

  //--> Q ranges <--
  minQ=0.;
  maxQ=1.1;
  nQ = ESTRUCT_Q_BINS-1;
  dQ  = (maxQ-minQ)/(float)nQ;

  //--> phi ranges <--

  minPhi=-M_PI;
  maxPhi=M_PI;
  nPhi  = ESTRUCT_PHI_BINS-1;
  dPhi  = (maxPhi-minPhi)/(float)nPhi;

  maxDPhi=maxPhi*1.5;// 2.0*maxPhi;
  minDPhi=minPhi*0.5;// 0.0; //2*minPhi;
  nDPhi = ESTRUCT_DPHI_BINS-1;
  dDPhi=(maxDPhi-minDPhi)/(float)nDPhi;

  maxSPhi=2*maxPhi;
  minSPhi=2*minPhi;
  nSPhi = ESTRUCT_SPHI_BINS-1;
  dSPhi=(maxSPhi-minSPhi)/(float)nSPhi;

  //--> eta ranges <--

  minEta=-1.0;
  maxEta=1.0;
  nEta = ESTRUCT_ETA_BINS-1;
  dEta= (maxEta-minEta)/(float)nEta;

  maxDEta=2*maxEta;//2*maxEta;
  minDEta=2*minEta;//0; //2*minEta;
  nDEta=ESTRUCT_DETA_BINS-1;
  dDEta=(maxDEta-minDEta)/(float)nDEta;

  maxSEta=2*maxEta;
  minSEta=2*minEta;
  nSEta = ESTRUCT_SETA_BINS-1;
  dSEta=(maxSEta-minSEta)/(float)nSEta;

  //--> yt ranges <--
  
  minYt=0.9; //0.15;
  maxYt=5.0;//4.5; // 0.925;
  nYt = ESTRUCT_YT_BINS-1;
  dYt = (maxYt-minYt)/(float)nYt;

  maxDYt=4.;//maxYt;
  minDYt=-4.;//-maxYt;// 0; //-maxYt;
  nDYt= ESTRUCT_DYT_BINS-1;
  dDYt=(maxDYt-minDYt)/(float)nDYt; 

  maxSYt=9.; //2 * maxYt + 1.0;
  minSYt=0.9; //2 * minYt;
  nSYt = ESTRUCT_SYT_BINS-1;
  dSYt=(maxSYt-minSYt)/(float)nSYt;
  
  /*maxDeltaYt=5.;//3.0;
    minDeltaYt=0.;//0; //-3.0;
    nDeltaYt = ESTRUCT_DELTAYT_BINS-1;
    dDeltaYt=(maxDeltaYt-minDeltaYt)/(float)nDeltaYt; */
  
  //--> xt ranges <--
  
  minXt= 1 - exp(-(sqrt(0.15*0.15+0.139*0.139)-0.139)/0.4);  // from Aya's code
  maxXt=0.99; // from Aya's code
  nXt = ESTRUCT_XT_BINS-1;
  dXt = (maxXt-minXt)/(float)nXt;

  //--> pt ranges <--
  
  minPt=0.15; //0.15;
  maxPt=6.0;//4.5; // 0.925;
  nPt = ESTRUCT_PT_BINS-1;
  dPt = (maxPt-minPt)/(float)nPt;

  maxDPt=5.85;//maxPt;
  minDPt=-5.85;//-maxPt;// 0; //-maxPt;
  nDPt= ESTRUCT_DPT_BINS-1;
  dDPt=(maxDPt-minDPt)/(float)nDPt; 

  maxSPt=12.; //2 * maxPt + 1.0;
  minSPt=0.3; //2 * minPt;
  nSPt = ESTRUCT_SPT_BINS-1;
  dSPt=(maxSPt-minSPt)/(float)nSPt;

   //--> TPC Separation ranges <--
  maxTPCSep = 50;  //cm
  minTPCSep = 0;
  nTPCSep = ESTRUCT_TPCSEP_BINS - 1;
  dTPCSep = (maxTPCSep-minTPCSep)/(float)nTPCSep;

};


/***********************************************************************
 *
 * $Log: StEStructBinning.cxx,v $
 * Revision 1.4  2005/09/14 17:14:21  msd
 * Large update, added new pair-cut system, added pair density plots for new analysis mode (4), added event mixing cuts (rewrote buffer for this)
 *
 * Revision 1.3  2005/03/03 01:30:43  porter
 * updated StEStruct2ptCorrelations to include pt-correlations and removed
 * old version of pt-correlations from chunhuih (StEStruct2ptPtNbar)
 *
 * Revision 1.2  2004/06/25 03:11:49  porter
 * New cut-binning implementation and modified pair-cuts for chunhui to review
 *
 * Revision 1.1  2003/10/15 18:20:46  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/










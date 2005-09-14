/**********************************************************************
 *
 * $Id: StEStructBinning.h,v 1.7 2005/09/14 17:14:21 msd Exp $
 *
 * Author: Jeff Porter 
 *
 **********************************************************************
 *
 * Description:  Binning defs as c-structs for 2pt-analysis
 *
 *
 ***********************************************************************/
#ifndef __STESTRUCTBINNING__H
#define __STESTRUCTBINNING__H

#include <math.h>

/*
 *  I made these c-structs as floats in order to able to have them contain
 *  more than just the counts. If wts are added I need another word
 *  and it seems to me to be safer to do it in the cstruct rather than
 *  in another separate array.
 *
 */


// 25 + 1 over&under
#define ESTRUCT_PHI_BINS 26
#define ESTRUCT_ETA_BINS 26
#define ESTRUCT_YT_BINS 26
//#define ESTRUCT_DELTAYT_BINS 26  //use dyt instead
#define ESTRUCT_PT_BINS 41
#define ESTRUCT_XT_BINS 26

#define ESTRUCT_DPHI_BINS 29
#define ESTRUCT_DETA_BINS 29
#define ESTRUCT_DYT_BINS 26
#define ESTRUCT_DPT_BINS 41

#define ESTRUCT_SPHI_BINS 26
#define ESTRUCT_SETA_BINS 26
#define ESTRUCT_SYT_BINS 51
#define ESTRUCT_SPT_BINS 41

#define ESTRUCT_Q_BINS 51
#define ESTRUCT_TPCSEP_BINS 50

struct qBins {
  float q[ESTRUCT_Q_BINS];
};

struct phiBins {
  double phi[ESTRUCT_PHI_BINS]; 
};
struct dphiBins {
  double dphi[ESTRUCT_DPHI_BINS]; 
};
struct sphiBins {
  double sphi[ESTRUCT_SPHI_BINS]; 
};

struct etaBins {
  double eta[ESTRUCT_ETA_BINS];
};
struct detaBins {
  double deta[ESTRUCT_DETA_BINS];
};
struct setaBins {
  double seta[ESTRUCT_SETA_BINS];
};


struct ytBins {
  double yt[ESTRUCT_YT_BINS]; 
};
struct dytBins {
  double dyt[ESTRUCT_DYT_BINS]; 
};
struct sytBins {
  double syt[ESTRUCT_SYT_BINS]; 
};

struct ptBins {
  float pt[ESTRUCT_PT_BINS]; 
};
struct dptBins {
  float dpt[ESTRUCT_DPT_BINS]; 
};
struct sptBins {
  float spt[ESTRUCT_SPT_BINS]; 
};

struct xtBins {
  double xt[ESTRUCT_XT_BINS];
};


/*struct deltaYtBins {  //use dyt
  double dyt[ESTRUCT_DELTAYT_BINS];
  };*/

struct TPCSepBins {
  float sep[ESTRUCT_TPCSEP_BINS];
};

class StEStructBinning {

protected:

  float maxPhi, minPhi, dPhi;     //! phi bins
  float maxEta, minEta, dEta;     //! eta bins
  float maxYt, minYt, dYt;        //! yt (x) bins
  float maxXt, minXt, dXt;        //! xt bins
  float maxPt, minPt, dPt;        //! mt (x) bins
  int   nPhi, nEta, nYt, nPt, nXt;//! n-bins

  float maxDPhi, minDPhi, dDPhi; //! delta phi bins
  float maxDEta, minDEta, dDEta; //! delta eta bins
  float maxDYt, minDYt, dDYt;    //! delta yt (x) bins
  float maxDPt, minDPt, dDPt;    //! delta mt (x) bins
  int   nDPhi, nDEta, nDYt, nDPt;         //! n-bins

  float maxSPhi, minSPhi, dSPhi; //! sigma phi bins
  float maxSEta, minSEta, dSEta; //! sigma eta bins
  float maxSYt, minSYt, dSYt;    //! sigma yt (x) bins
  float maxSPt, minSPt, dSPt;    //! sigma mt (x) bins
  int   nSPhi, nSEta, nSYt, nSPt;         //! n-bins
 
  //float maxDeltaYt, minDeltaYt, dDeltaYt; //! really yt  //use dyt
  //int   nDeltaYt;                   //! n-bins

  float maxQ,minQ,dQ;
  int   nQ;
  int i,j;

  float maxTPCSep, minTPCSep, dTPCSep; //! TPC separation dist
  int   nTPCSep;

  StEStructBinning();
  StEStructBinning(StEStructBinning& me){};
  static StEStructBinning* mInstance;

public:  

  ~StEStructBinning(){};
  static StEStructBinning* Instance();

  int iphi(float phi);
  int ieta(float eta);
  int iyt(float yt);
  int ixt(float xt);
  int ipt(float pt);
  int iq(float q);
  int isep(float sep);

  float phiVal(int iphi);
  float etaVal(int ieta);
  float ytVal(int iyt);
  float xtVal(int ixt);
  float ptVal(int ipt);
  float qVal(int iq);
  float sepVal(int is);

  int idphi(float phi);
  int ideta(float eta);
  int idyt(float yt);
  int idpt(float pt);
  
  float dphiVal(int idphi);
  float detaVal(int ideta);
  float dytVal(int idyt);
  float dptVal(int idpt);

  int isphi(float phi);
  int iseta(float eta);
  int isyt(float yt);
  int ispt(float pt);

  float sphiVal(int isphi);
  float setaVal(int iseta);
  float sytVal(int isyt);
  float sptVal(int ispt);
    
  /*int   iDeltaYt(float yt);  //use dyt
  float deltaYtMax()    { return maxDeltaYt; }
  float deltaYtMin()    { return minDeltaYt; }
  float getBinWidthDeltaYt()  { return dDeltaYt; }
  int   deltaYtBins() { return nDeltaYt; }
  float deltaYtVal(int ideltaYt);*/

  float qMax()   { return maxQ; }
  float qMin()   { return minQ; }
  float getBinWidthQ() { return dQ; }
  int   qBins() { return nQ; }

  float phiMax()   { return maxPhi; }
  float phiMin()   { return minPhi; }
  float getBinWidthPhi() { return dPhi; }
  int   phiBins() { return nPhi; }


  float etaMax()   { return maxEta; }
  float etaMin()   { return minEta; }
  float getBinWidthEta() { return dEta; }
  int   etaBins() { return nEta; };

  float ytMax()    { return maxYt; }
  float ytMin()    { return minYt; }
  float getBinWidthYt()  { return dYt; }
  int   ytBins() { return nYt; }

  float xtMax() { return maxXt; }
  float xtMin() { return minXt; }
  float getBinWidthXt() { return dXt; }
  int   xtBins() { return nXt; }

  float ptMax()    { return maxPt; }
  float ptMin()    { return minPt; }
  float getBinWidthPt()  { return dPt; }
  int   ptBins() { return nPt; }

  float dphiMax()   { return maxDPhi; }
  float dphiMin()   { return minDPhi; }
  float getBinWidthDPhi() { return dDPhi; }
  int   dphiBins() { return nDPhi; }

  float detaMax()   { return maxDEta; }
  float detaMin()   { return minDEta; }
  float getBinWidthDEta() { return dDEta; }
  int   detaBins() { return nDEta; };

  float dytMax()    { return maxDYt; }
  float dytMin()    { return minDYt; }
  float getBinWidthDYt()  { return dDYt; }
  int   dytBins() { return nDYt; }

  float dptMax()    { return maxDPt; }
  float dptMin()    { return minDPt; }
  float getBinWidthDPt()  { return dDPt; }
  int   dptBins() { return nDPt; }

  float sphiMax()   { return maxSPhi; }
  float sphiMin()   { return minSPhi; }
  float getBinWidthSPhi() { return dSPhi; }
  int   sphiBins() { return nSPhi; }

  float setaMax()   { return maxSEta; }
  float setaMin()   { return minSEta; }
  float getBinWidthSEta() { return dSEta; }
  int   setaBins() { return nSEta; };

  float sytMax()    { return maxSYt; }
  float sytMin()    { return minSYt; }
  float getBinWidthSYt()  { return dSYt; }
  int   sytBins() { return nSYt; }

  float sptMax()    { return maxSPt; }
  float sptMin()    { return minSPt; }
  float getBinWidthSPt()  { return dSPt; }
  int   sptBins() { return nSPt; }

  float TPCSepMax()  { return maxTPCSep; }
  float TPCSepMin()  { return minTPCSep; }
  int   TPCSepBins() { return nTPCSep;   }

};

inline StEStructBinning* StEStructBinning::Instance(){
  if(!mInstance) mInstance=new StEStructBinning;
  return mInstance;
}

inline float StEStructBinning::qVal(int iq){
  return minQ+iq*dQ+dQ/2;
}

inline int StEStructBinning::iq(float q){
  if( q < minQ ) return ESTRUCT_Q_BINS - 1;
  int j = (int)((q-minQ)/dQ);
  return (j > ESTRUCT_Q_BINS - 2) ? ESTRUCT_Q_BINS - 1 : j;
}

inline float StEStructBinning::sepVal(int is){
  return minTPCSep+is*dTPCSep+dTPCSep/2;
}

inline int StEStructBinning::isep(float sep){
  if( sep < minTPCSep ) return ESTRUCT_TPCSEP_BINS - 1;
  int j = (int)((sep-minTPCSep)/dTPCSep);
  return (j > ESTRUCT_TPCSEP_BINS - 2) ? ESTRUCT_TPCSEP_BINS - 1 : j;
}

inline int StEStructBinning::iphi(float phi){
  if( phi < minPhi ) return ESTRUCT_PHI_BINS - 1;
  int j = (int)((phi-minPhi)/dPhi);
  return (j > ESTRUCT_PHI_BINS - 2) ? ESTRUCT_PHI_BINS - 1 : j;
}

inline int StEStructBinning::idphi(float phi){
  if(phi<minDPhi)phi+=2*M_PI;
  if( phi < minDPhi ) return ESTRUCT_DPHI_BINS - 1;
  int j = (int)((phi-minDPhi)/dDPhi);
  return (j > ESTRUCT_DPHI_BINS - 2) ? ESTRUCT_DPHI_BINS - 1 : j;
}

inline int StEStructBinning::isphi(float phi){
  if( phi < minSPhi ) return ESTRUCT_SPHI_BINS - 1;
  int j = (int)((phi-minSPhi)/dSPhi);
  return (j > ESTRUCT_SPHI_BINS - 2) ? ESTRUCT_SPHI_BINS - 1 : j;
}

inline float StEStructBinning::phiVal(int iphi){
  return minPhi+iphi*dPhi+dPhi/2;
}

inline float StEStructBinning::sphiVal(int isphi){
  return minSPhi+isphi*dSPhi+dSPhi/2;
}

inline float StEStructBinning::dphiVal(int idphi){
  return minDPhi+idphi*dDPhi+dDPhi/2;
}

inline int StEStructBinning::ieta(float eta){
  if( eta < minEta ) return ESTRUCT_ETA_BINS - 1;
  int j = (int)( (eta - minEta) / dEta );
  return (j > ESTRUCT_ETA_BINS - 2) ? ESTRUCT_ETA_BINS - 1 : j;  
}

inline int StEStructBinning::ideta(float eta){
  if( eta < minDEta ) return ESTRUCT_ETA_BINS - 1;
  int j = (int)( (eta-minDEta)/dDEta );
  return (j > ESTRUCT_DETA_BINS - 2) ? ESTRUCT_DETA_BINS - 1 : j;
}

inline int StEStructBinning::iseta(float eta){
  if( eta < minSEta ) return ESTRUCT_ETA_BINS - 1;
  int j = (int)( (eta-minSEta)/dSEta );
  return (j > ESTRUCT_SETA_BINS - 2) ? ESTRUCT_SETA_BINS - 1 : j;
}

inline float StEStructBinning::etaVal(int ieta){
  return minEta+ieta*dEta+dEta/2;
}

inline float StEStructBinning::setaVal(int iseta){
  return minSEta+iseta*dSEta+dSEta/2;
}

inline float StEStructBinning::detaVal(int ideta){
  return minDEta+ideta*dDEta+dDEta/2;
}

inline int StEStructBinning::iyt(float yt){
  if( yt < minYt ) return ESTRUCT_YT_BINS - 1;
  int j = (int)((yt-minYt)/dYt);
  return (j > ESTRUCT_YT_BINS - 2) ? ESTRUCT_YT_BINS - 1 : j;  
}

inline int StEStructBinning::idyt(float yt){
  if( yt < minDYt ) return ESTRUCT_DYT_BINS - 1;
  int j = (int)((yt-minDYt)/dDYt);
  return (j > ESTRUCT_DYT_BINS - 2) ? ESTRUCT_DYT_BINS-1 : j;
}

inline int StEStructBinning::isyt(float yt){
  if( yt < minSYt ) return ESTRUCT_SYT_BINS - 1;
  int j = (int)((yt-minSYt)/dSYt);
  return (j > ESTRUCT_SYT_BINS - 2) ? ESTRUCT_SYT_BINS-1 : j;
}

inline float StEStructBinning::ytVal(int iyt){
  return minYt+iyt*dYt+dYt/2;
}

inline float StEStructBinning::sytVal(int isyt){
  return minSYt+isyt*dSYt+dSYt/2;
}

inline float StEStructBinning::dytVal(int idyt){
  return minDYt+idyt*dDYt+dDYt/2;
}

/*inline int StEStructBinning::iDeltaYt(float yt){
  if( yt < minDeltaYt ) return ESTRUCT_DELTAYT_BINS - 1;
  int j = (int)((yt-minDeltaYt)/dDeltaYt);
  return (j > ESTRUCT_DELTAYT_BINS - 2) ? ESTRUCT_DELTAYT_BINS-1 : j;
}

inline float StEStructBinning::deltaYtVal(int ideltaYt){
  return minDeltaYt+ideltaYt*dDeltaYt+dDeltaYt/2;
  }*/

inline int StEStructBinning::ixt(float xt){
  if( xt < minXt ) return ESTRUCT_XT_BINS - 1;
  int j = (int)((xt-minXt)/dXt);
  return (j > ESTRUCT_XT_BINS - 2) ? ESTRUCT_XT_BINS - 1 : j;
}

inline float StEStructBinning::xtVal(int ixt){
  return minXt+ixt*dXt+dXt/2;
}

inline int StEStructBinning::ipt(float pt){
  if( pt < minPt ) return ESTRUCT_PT_BINS - 1;
  int j = (int)((pt-minPt)/dPt);
  return (j > ESTRUCT_PT_BINS - 2) ? ESTRUCT_PT_BINS - 1 : j;  
}

inline int StEStructBinning::idpt(float pt){
  if( pt < minDPt ) return ESTRUCT_DPT_BINS - 1;
  int j = (int)((pt-minDPt)/dDPt);
  return (j > ESTRUCT_DPT_BINS - 2) ? ESTRUCT_DPT_BINS-1 : j;
}

inline int StEStructBinning::ispt(float pt){
  if( pt < minSPt ) return ESTRUCT_SPT_BINS - 1;
  int j = (int)((pt-minSPt)/dSPt);
  return (j > ESTRUCT_SPT_BINS - 2) ? ESTRUCT_SPT_BINS-1 : j;
}

inline float StEStructBinning::ptVal(int ipt){
  return minPt+ipt*dPt+dPt/2;
}

inline float StEStructBinning::sptVal(int ispt){
  return minSPt+ispt*dSPt+dSPt/2;
}

inline float StEStructBinning::dptVal(int idpt){
  return minDPt+idpt*dDPt+dDPt/2;
}

#endif

/***********************************************************************
 *
 * $Log: StEStructBinning.h,v $
 * Revision 1.7  2005/09/14 17:14:21  msd
 * Large update, added new pair-cut system, added pair density plots for new analysis mode (4), added event mixing cuts (rewrote buffer for this)
 *
 * Revision 1.6  2005/03/03 01:30:43  porter
 * updated StEStruct2ptCorrelations to include pt-correlations and removed
 * old version of pt-correlations from chunhuih (StEStruct2ptPtNbar)
 *
 * Revision 1.5  2004/07/23 21:50:33  chunhuih
 *
 * changed float to double in the structs. This is necessary when analyzing large
 * number of particle pairs. When a float reaches (int)2**24, adding 1 to it has no effect
 * due to IEEE floating point encoding scheme. A double has a much larger value
 * to meet this problem.
 *
 * Revision 1.4  2004/06/25 03:11:49  porter
 * New cut-binning implementation and modified pair-cuts for chunhui to review
 *
 * Revision 1.3  2004/06/10 17:09:22  msd
 * Quick-fixed EBYE_YT_BINS.  Better implementation of cut-binning on the way...
 *
 * Revision 1.2  2004/04/13 16:58:37  chunhuih
 *
 * changed a set of binning functions, so that when the variable is below the
 * minimum of the binning range, the function returns the correct overflow bin
 * index.
 *
 * Revision 1.1  2003/10/15 18:20:46  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/






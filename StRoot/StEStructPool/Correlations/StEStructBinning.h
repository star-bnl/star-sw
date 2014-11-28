/**********************************************************************
 *
 * $Id: StEStructBinning.h,v 1.17 2012/11/16 21:22:27 prindle Exp $
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
#include "TROOT.h"
/*
 *  I made these c-structs as floats in order to able to have them contain
 *  more than just the counts. If wts are added I need another word
 *  and it seems to me to be safer to do it in the cstruct rather than
 *  in another separate array.
 *
 */
/*
 * The pairdensity plots (which can be used to check pair cuts) use
 * idpt and dptval. I think these used to be used to make pt plots
 * covering much of the accpetance, but that role seems to have shifted
 * to idyt and dytval. Co-opt the dpt stuff so the pairdensity plots
 * have sensitivity to few MeV differences.
 *
 * djp 2-1-2008
 */

// 25 + 1 over&under
#define ESTRUCT_PHI_BINS 26
#define ESTRUCT_ETA_BINS 26
#define ESTRUCT_YT_BINS 26
//#define ESTRUCT_DELTAYT_BINS 26  //use dyt instead
#define ESTRUCT_MEANPT_BINS 501
#define ESTRUCT_PT_BINS 31
#define ESTRUCT_XT_BINS 26

#define ESTRUCT_DPHI_BINS 14
#define ESTRUCT_DETA_BINS 14
#define ESTRUCT_DYT_BINS 15
#define ESTRUCT_DPT_BINS 31

#define ESTRUCT_SPHI_BINS 26
#define ESTRUCT_SETA_BINS 26
#define ESTRUCT_SYT_BINS 51
#define ESTRUCT_SPT_BINS 41

#define ESTRUCT_Q_BINS 51
#define ESTRUCT_TPCSEP_BINS 51
#define ESTRUCT_TPCSEPPHI_BINS 51
#define ESTRUCT_TPCQUALITY_BINS 51

#define ESTRUCT_DEDX_BINS 151
#define ESTRUCT_PTOT_BINS 151

// 100 + 1 over&under for QA
#define ESTRUCT_QAPHI_BINS 101
#define ESTRUCT_QAETA_BINS 101
#define ESTRUCT_QAPT_BINS  101

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


struct TPCSepBins {
  float sep[ESTRUCT_TPCSEP_BINS];
};
struct TPCSepPhiBins {
  float sep[ESTRUCT_TPCSEPPHI_BINS];
};
struct TPCQualityBins {
  float sep[ESTRUCT_TPCQUALITY_BINS];
};

struct dEdxBins {
  float dEdx[ESTRUCT_DEDX_BINS];
};

struct PtotBins {
  float Ptot[ESTRUCT_PTOT_BINS];
};

struct QAEtaBins {
  float Eta[ESTRUCT_QAETA_BINS];
};

struct QAPhiBins {
  float Phi[ESTRUCT_QAPHI_BINS];
};

struct QAPtBins {
  float Pt[ESTRUCT_QAPT_BINS];
};


// oh-boy... I made c-struct and methods with same names so cannot
// use the c-structs directly in the StEStructBinning class.
// rather I have to wrap it!

class EtaDeltaWeights {
 public:
  detaBins x;
};

class StEStructBinning : public TObject {

protected:

  float maxPhi, minPhi, dPhi;     //! phi bins
  float maxEta, minEta, dEta;     //! eta bins
  float maxYt, minYt, dYt;        //! yt (x) bins
  float maxXt, minXt, dXt;        //! xt bins
  float maxPt, minPt, dPt;        //! mt (x) bins
  float maxMeanPt, minMeanPt, dmeanPt;        //! mean pt bins
  int   nPhi, nEta, nYt, nPt, nXt, nmeanPt;//! n-bins

  float dDPhi; //! delta phi bins
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
  float maxTPCSepPhi, minTPCSepPhi, dTPCSepPhi; //! TPC separation dist
  float maxTPCQuality, minTPCQuality, dTPCQuality;
  float maxdEdx, mindEdx, ddEdx;
  float maxPtot, minPtot, dPtot;
  int   nTPCSep, nTPCSepPhi, nTPCQuality, ndEdx, nPtot;

  float maxQAEta, minQAEta, dQAEta;
  float maxQAPhi, minQAPhi, dQAPhi;
  float maxQAPt,  minQAPt,  dQAPt;
  int   nQAEta, nQAPhi, nQAPt;

  EtaDeltaWeights mdetaWeights;

  StEStructBinning();
  StEStructBinning(StEStructBinning& me){};
  static StEStructBinning* mInstance;

public:  

  ~StEStructBinning(){};
  static StEStructBinning* Instance();

  void  calculateDEtaWeights();
  double getDEtaWeight(float deta);

  void setEtaRange(float etamin, float etamax);
    
  int iphi(float phi);
  int ieta(float eta);
  int iyt(float yt);
  int ixt(float xt);
  int imeanpt(float pt);
  int ipt(float pt);
  int iq(float q);
  int isep(float sep);
  int isepphi(float sep);
  int iqual(float qual);
  int idedx(float dedx);
  int iptot(float ptot);
  int iqaphi(float phi);
  int iqaeta(float eta);
  int iqapt(float pt);

  float phiVal(int iphi);
  float etaVal(int ieta);
  float ytVal(int iyt);
  float xtVal(int ixt);
  float meanptVal(int ipt);
  float ptVal(int ipt);
  float qVal(int iq);
  float sepVal(int is);
  float sepphiVal(int is);
  float qualityVal(int is);
  float dedxVal(int idedx);
  float ptotVal(int iptot);
  float qaetaVal(int ieta);
  float qaphiVal(int iphi);
  float qaptVal(int ipt);

  int idphi(float phi);
  int ideta(float eta);
  int idyt(float yt);
  int idpt(float pt);
  
  float dphiVal(int idphi, int which);
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
  int   setNEtaBins( int nbins) {
      nEta = nbins;
      dEta = (maxEta-minEta)/(float)nEta;
      calculateDEtaWeights();
      return nEta;
  };

  float ytMax()    { return maxYt; }
  float ytMin()    { return minYt; }
  float getBinWidthYt()  { return dYt; }
  int   ytBins() { return nYt; }

  float xtMax() { return maxXt; }
  float xtMin() { return minXt; }
  float getBinWidthXt() { return dXt; }
  int   xtBins() { return nXt; }

  float meanptMax()    { return maxMeanPt; }
  float meanptMin()    { return minMeanPt; }
  float getBinWidthMeanPt()  { return dmeanPt; }
  int   meanptBins() { return nmeanPt; }

  float ptMax()    { return maxPt; }
  float ptMin()    { return minPt; }
  float getBinWidthPt()  { return dPt; }
  int   ptBins() { return nPt; }

  float dEdxMax()   { return maxdEdx; }
  float dEdxMin()   { return mindEdx; }
  float getBinWidthdEdx() { return ddEdx; }
  int   dEdxBins() { return ndEdx; }

  float PtotMax()   { return maxPtot; }
  float PtotMin()   { return minPtot; }
  float getBinWidthPtot() { return dPtot; }
  int   PtotBins() { return nPtot; }

  // Thse dphi* and deta* are used to size histograms.
  // For now symmetrize the histograms here.
  float dphiMax()   {
      if (nDPhi%2 > 0) {
          return 3*M_PI/2 + M_PI/(hdphiBins()-1);
      } else {
          return 3*M_PI/2;
      }
  }
  float dphiMin()   {
      if (nDPhi%2 > 0) {
          return -M_PI/2 - M_PI/(hdphiBins()-1);
      } else {
          return -M_PI/2;
      }
  }
  float getBinWidthDPhi() { return dDPhi; }
  int   dphiBins() { return nDPhi; }
  int   setNDPhiBins( int nbins ) {
      nDPhi = nbins;
      dDPhi = M_PI/((float)nDPhi-1.0);
      return nDPhi;
  }
  int hdphiBins() {
      if (nDPhi%2 > 0) {
          return 2*nDPhi-1;
      } else {
          return 2*(nDPhi-1);
      }
  }
  int hdphiBin(float phi) {
      return 1 + int( (phi-dphiMin()) / dDPhi);
  }

  float detaMax()   { return maxDEta; }
  float detaMin()   { return -maxDEta; }
  float getBinWidthDEta() { return dDEta; }
  int   detaBins() { return nDEta; };
  int   setNDEtaBins( int nbins) {
      nDEta = nbins;
      dDEta = (maxDEta-minDEta)/((float)nDEta-0.5);
      calculateDEtaWeights();
      return nDEta;
  };
  int   hdetaBins() { return 2*nDEta-1; };
  int   hdetaBin(float eta) {
      return 1 + int( (eta-detaMin()) / dDEta);
  };

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
  int   setNSEtaBins( int nbins ) {
      nSEta = nbins;
      dSEta = (maxSEta-minSEta)/(float)nSEta;
      calculateDEtaWeights();
      return nSEta;
  };

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
  float TPCSepPhiMax()  { return maxTPCSepPhi; }
  float TPCSepPhiMin()  { return minTPCSepPhi; }
  int   TPCSepPhiBins() { return nTPCSepPhi;   }
  float TPCQualityMax()  { return maxTPCQuality; }
  float TPCQualityMin()  { return minTPCQuality; }
  int   TPCQualityBins() { return nTPCQuality;   }

  int   QAEtaBins() { return nQAEta; }
  int   QAPhiBins() { return nQAPhi; }
  int   QAPtBins()  { return nQAPt; }
  float QAEtaMax()   { return maxQAEta; }
  float QAEtaMin()   { return minQAEta; }
  float QAPhiMax()   { return maxQAPhi; }
  float QAPhiMin()   { return minQAPhi; }
  float QAPtMax()   { return maxQAPt; }
  float QAPtMin()   { return minQAPt; }

ClassDef(StEStructBinning,1)

};

inline StEStructBinning* StEStructBinning::Instance(){
  if(!mInstance) mInstance=new StEStructBinning;
  return mInstance;
}

inline double StEStructBinning::getDEtaWeight(float deta){
  return mdetaWeights.x.deta[ideta(deta)];
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
inline float StEStructBinning::sepphiVal(int is){
  return minTPCSepPhi+is*dTPCSepPhi+dTPCSepPhi/2;
}
inline float StEStructBinning::qualityVal(int is){
  return minTPCQuality+is*dTPCQuality+dTPCQuality/2;
}

inline int StEStructBinning::isep(float sep){
  if( sep < minTPCSep ) return ESTRUCT_TPCSEP_BINS - 1;
  int j = (int)((sep-minTPCSep)/dTPCSep);
  return (j > ESTRUCT_TPCSEP_BINS - 2) ? ESTRUCT_TPCSEP_BINS - 1 : j;
}
inline int StEStructBinning::isepphi(float dphi){
    if (dphi>M_PI) {
        dphi = 2*M_PI - dphi;
    } else if (dphi<-M_PI) {
        dphi = 2*M_PI + dphi;
    }
    int j = (int)((dphi + dTPCSepPhi/2 - minTPCSepPhi)/dTPCSepPhi);
    return (j > ESTRUCT_TPCSEPPHI_BINS - 2) ? ESTRUCT_TPCSEPPHI_BINS - 1 : j;
}
inline int StEStructBinning::iqual(float qual){
  if( qual < minTPCQuality ) return ESTRUCT_TPCQUALITY_BINS - 1;
  int j = (int)((qual-minTPCQuality)/dTPCQuality);
  return (j > ESTRUCT_TPCQUALITY_BINS - 2) ? ESTRUCT_TPCQUALITY_BINS - 1 : j;
}

inline int StEStructBinning::iphi(float phi){
  if( phi < minPhi ) return ESTRUCT_PHI_BINS - 1;
  int j = (int)((phi-minPhi)/dPhi);
  return (j > ESTRUCT_PHI_BINS - 2) ? ESTRUCT_PHI_BINS - 1 : j;
}

inline int StEStructBinning::idphi(float dphi){
    dphi = fabs(dphi);
    if (dphi>M_PI) {
        dphi = 2*M_PI - dphi;
    }
    if( dphi < 0 ) return ESTRUCT_DPHI_BINS - 1;
    int j = (int)((dphi + dDPhi/2)/dDPhi);
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

inline float StEStructBinning::dphiVal(int idphi, int which) {
    float dphi = idphi*dDPhi;
    if (1 == which) {
        return dphi;
    } else if (2 == which) {
      //if ((dphiBins()%2 > 0) && (idphi == (dphiBins()-1)/2)) {
      //      return 99;
      //  }
        if (dphi < M_PI/2) {
            return -dphi;
        } else {
            return 2*M_PI - dphi;
        }
    }
    return 99;
}

inline int StEStructBinning::ieta(float eta){
  if( eta < minEta ) return ESTRUCT_ETA_BINS - 1;
  int j = (int)( (eta - minEta) / dEta );
  return (j > ESTRUCT_ETA_BINS - 2) ? ESTRUCT_ETA_BINS - 1 : j;  
}

inline int StEStructBinning::ideta(float eta) {
    eta = fabs(eta);
    if( eta < minDEta ) return ESTRUCT_DETA_BINS - 1;
    int j = (int)( (eta + dDEta/2 - minDEta)/dDEta );
    return (j > ESTRUCT_DETA_BINS - 2) ? ESTRUCT_DETA_BINS - 1 : j;
}

inline int StEStructBinning::iseta(float eta){
  if( eta < minSEta ) return ESTRUCT_SETA_BINS - 1;
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
  return minDEta+ideta*dDEta;
}

inline int StEStructBinning::iyt(float yt){
  if( yt < minYt ) return ESTRUCT_YT_BINS - 1;
  int j = (int)((yt-minYt)/dYt);
  return (j > ESTRUCT_YT_BINS - 2) ? ESTRUCT_YT_BINS - 1 : j;  
}

inline int StEStructBinning::idyt(float yt){
  yt = fabs(yt);
  if( yt < minDYt ) return ESTRUCT_DYT_BINS - 1;
  int j = (int)((yt+dDYt/2-minDYt)/dDYt);
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

inline float StEStructBinning::dytVal(int idyt) {
    if (0 ==idyt) {
        return 0;
    } else {
        return minDYt+idyt*dDYt;
    }
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

inline int StEStructBinning::imeanpt(float pt){
  if( pt < 0 ) return ESTRUCT_MEANPT_BINS - 1;
  int j = (int)((pt-minMeanPt)/dmeanPt);
  return (j > ESTRUCT_MEANPT_BINS - 2) ? ESTRUCT_MEANPT_BINS - 1 : j;  
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

inline float StEStructBinning::meanptVal(int ipt){
  return minMeanPt+ipt*dmeanPt+dmeanPt/2;
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


inline int StEStructBinning::idedx(float dedx){
  if( dedx < mindEdx ) return ESTRUCT_DEDX_BINS - 1;
  int j = (int)((dedx-mindEdx)/ddEdx);
  return (j > ESTRUCT_DEDX_BINS - 2) ? ESTRUCT_DEDX_BINS - 1 : j;
}

inline float StEStructBinning::dedxVal(int idedx){
  return mindEdx+idedx*ddEdx+ddEdx/2;
}

inline int StEStructBinning::iptot(float ptot){
  if( ptot < minPtot ) return ESTRUCT_PTOT_BINS - 1;
  int j = (int)((ptot-minPtot)/dPtot);
  return (j > ESTRUCT_PTOT_BINS - 2) ? ESTRUCT_PTOT_BINS - 1 : j;
}

inline float StEStructBinning::ptotVal(int iptot){
  return minPtot+iptot*dPtot+dPtot/2;
}

inline int StEStructBinning::iqaeta(float eta){
  if( eta < minQAEta ) return ESTRUCT_QAETA_BINS - 1;
  int j = (int)((eta-minQAEta)/dQAEta);
  return (j > ESTRUCT_QAETA_BINS - 2) ? ESTRUCT_QAETA_BINS - 1 : j;
}
inline int StEStructBinning::iqaphi(float phi){
  if( phi < minQAPhi ) return ESTRUCT_QAPHI_BINS - 1;
  int j = (int)((phi-minQAPhi)/dQAPhi);
  return (j > ESTRUCT_QAPHI_BINS - 2) ? ESTRUCT_QAPHI_BINS - 1 : j;
}
inline int StEStructBinning::iqapt(float pt){
  if( pt < minQAPt ) return ESTRUCT_QAPT_BINS - 1;
  int j = (int)((pt-minQAPt)/dQAPt);
  return (j > ESTRUCT_QAPT_BINS - 2) ? ESTRUCT_QAPT_BINS - 1 : j;
}

inline float StEStructBinning::qaetaVal(int ieta){
  return minQAEta+ieta*dQAEta+dQAEta/2;
}
inline float StEStructBinning::qaphiVal(int iphi){
  return minQAPhi+iphi*dQAPhi+dQAPhi/2;
}
inline float StEStructBinning::qaptVal(int ipt){
  return minQAPt+ipt*dQAPt+dQAPt/2;
}


#endif

/***********************************************************************
 *
 * $Log: StEStructBinning.h,v $
 * Revision 1.17  2012/11/16 21:22:27  prindle
 * 2ptCorrelations: SS, AS histograms.  Get eta limits from cuts. Fit PtAll histogram. Add histograms to keep track of eta, phi limits. A few more histograms
 * Binning: Add quality cut.
 * CutBin: modify mode9
 * PairCuts: modify goodDeltaZ for case of one track leaving via endcap.
 *
 * Revision 1.16  2009/05/08 00:09:54  prindle
 * In 2ptCorrelations we added switches to select blocks of histograms to fill.
 * (See constructor in StEStruct2ptCorrelations.cxx)
 * Use a brute force method for checking crossing cuts. I had too many corner
 * cases with my clever check.
 * In Binning, change Yt limit and add methods for accessing number of histogram bins
 * to use (used in Support)
 *
 * Revision 1.15  2008/03/19 22:06:00  prindle
 * Added doInvariantMass flag.
 * Added some plots in pairDensityHistograms.
 * SetZOffset used to only be done when doPairDensity was true.
 * Moved creating/copying pairDensity histograms to same place as other histograms.
 * Added cutBinHistMode
 * mode3 neck was defined as yt1<2.2 && yt2<2.2 (and not soft)
 *            now is        1.8<yt1<2.2  && 1.8<yt2<2.2
 * Added gooddzdxy, Merging2 and Crossing2 to pair cuts.
 *
 * Revision 1.14  2007/11/26 19:55:24  prindle
 * In 2ptCorrelations: Support for keeping all z-bins of selected centralities
 *                     Change way \hat{p_t} is calculated for parent distributions in pid case.
 *    Binning          Added parent binning (for \hat{p_t}
 *    CutBin:          Mode 5 extensively modified.
 *                     Added invariant mass cuts (probably a bad idea in general.)
 *
 * Revision 1.13  2007/01/26 17:17:08  msd
 * Implemented new binning scheme: dEta stored in array with bin centered at zero, dPhi array has bins centered at zero and pi.  Final DEtaDPhi has 25x25 bins with dPhi bin width of pi/12 so all major angles are centered in bins.
 *
 * Revision 1.12  2006/10/02 22:20:58  prindle
 * Store only quadrant of eta_Delta - phi_Delta array/histogram.
 * Store half of eta_Sigma - phi_Delta array/histogram.
 * This required modifications in Binning.
 * I had a bug in the pair loop (which left +- not fully symmetrized)
 * and had to make changes in cut bins for mode 5 (and 3 I think)
 * when I fixed this.
 * Also change crossing cut to use only two parameters, the sign of
 * the magnetic field being taken from the MuDst.
 *
 * Revision 1.11  2006/04/25 21:03:57  msd
 * Fixed bugs in ideta and iseta
 *
 * Revision 1.10  2006/04/10 23:42:32  porter
 * Added sameSide() & awaySide() methods to PairCut (so only defined in 1 place)
 * and added the eta_delta weighting as a binned correctin defined by the eta-limits in
 * the StEStructBinning object
 *
 * Revision 1.9  2006/04/04 22:10:11  porter
 * a handful of changes (specific to correlations)
 *  - added StEStructQAHists so that if NOT input frm Maker, each analysis has its own
 *  - used ability to get any max,min val from the cut class - or z-vertex binning
 *  - put z-vertex binning into 1 place
 *  - switched back 1st line of pair cut method to keep pair if good, not to reject if bad.
 *  - Pair cut object is now pointer in correlations
 *  - some diagnostic printouts available from macro
 *  - Duncan's delta-phi binning change
 *
 * Revision 1.8  2006/02/22 22:05:15  prindle
 * Removed all references to multRef (?)
 * Added cut mode 5 for particle identified correlations.
 * Other cut modes should be same as before
 *
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






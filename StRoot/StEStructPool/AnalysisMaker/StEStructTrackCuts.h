/**********************************************************************
 *
 * $Id: StEStructTrackCuts.h,v 1.1 2003/10/15 18:20:32 porter Exp $
 *
 * Author: Jeff Porter 
 *
 **********************************************************************
 *
 * Description:  Cut class for track level quantities
 *
 *
 ***********************************************************************/
#ifndef __STEBYETRACKCUTS__H
#define __STEBYETRACLCUTS__H


#include "StEStructCuts.h"
#include "Stiostream.h"


class StEStructTrackCuts : public StEStructCuts {

protected:

   CutName mflagName;
   CutName mchargeName;
   CutName mnfitpointsName;
   CutName mnfitnmaxName;
   CutName mglobalDCAName;
   CutName mchi2Name;
   CutName mptName;
   CutName mytName;
   CutName mphiName;
   CutName metaName;
   CutName mnsigmaEName;
   CutName mnsigmaPiName;
   CutName mnsigmaKName;
   CutName mnsigmaPName;
 
  int   mflag[2];
  int   mcharge[2];
  int   mnfitpoints[2];
  float mnfitnmax[2];
  float mglobalDCA[2];
  float mchi2[2];
  float mpt[2];
  float myt[2];
  float mphi[2];
  float meta[2];  
  float mnsigmaE[2];
  float mnsigmaPi[2];
  float mnsigmaK[2];
  float mnsigmaP[2];

  void init();
  void initCuts();
  void initNames();

public:

  StEStructTrackCuts();
  StEStructTrackCuts(const char* cutFileName);
  virtual ~StEStructTrackCuts();

  virtual bool loadBaseCuts(const char* name, const char** vals, int nvals);
  virtual void loadUserCuts(const char* name, const char** vals, int nvals);
  virtual void printCuts(ostream& ofs);
  virtual void printCuts(const char* fname) { StEStructCuts::printCuts(fname); };

  bool goodFlag(int f);
  bool goodCharge(int c);
  bool goodNFitPoints(int n);
  bool goodNFitNMax(float r);
  bool goodGlobalDCA(float g);
  bool goodChi2(float x);
  bool goodPt(float p);
  bool goodYt(float p);
  bool goodPhi(float p);
  bool goodEta(float e);
  bool goodElectron(float e);
  bool goodPion(float p);
  bool goodKaon(float k);
  bool goodProton(float p);
 

  ClassDef(StEStructTrackCuts,1)

};

inline void StEStructTrackCuts::loadUserCuts(const char* name, const char** vals, int nvals){ }

inline bool StEStructTrackCuts::goodFlag(int f){
  mvalues[mflagName.idx] =(float)f;
  return ( (mflag[0]==mflag[1] && mflag[0]==0) ||
      (f>=mflag[0] && f<=mflag[1])  ) ;
  
}

inline bool StEStructTrackCuts::goodCharge(int c){
  mvalues[mchargeName.idx] =(float)c;
  return ( (mcharge[0]==mcharge[1] && mcharge[0]==0) ||
      (c>=mcharge[0] && c<=mcharge[1])  ) ;
  
}

inline bool StEStructTrackCuts::goodNFitPoints(int c){
  mvalues[mnfitpointsName.idx] =(float)c;
  return ( (mnfitpoints[0]==mnfitpoints[1] && mnfitpoints[0]==0) ||
      (c>=mnfitpoints[0] && c<=mnfitpoints[1])  ) ;
  
}

inline bool StEStructTrackCuts::goodNFitNMax(float c){
  mvalues[mnfitnmaxName.idx] =c;
  return ( (mnfitnmax[0]==mnfitnmax[1] && mnfitnmax[0]==0) ||
      (c>=mnfitnmax[0] && c<=mnfitnmax[1])  ) ;
  
}

inline bool StEStructTrackCuts::goodGlobalDCA(float c){
  mvalues[mglobalDCAName.idx] =c;
  return ( (mglobalDCA[0]==mglobalDCA[1] && mglobalDCA[0]==0) ||
      (c>=mglobalDCA[0] && c<=mglobalDCA[1])  ) ;
  
}

inline bool StEStructTrackCuts::goodChi2(float c){
  mvalues[mchi2Name.idx] =c;
  return ( (mchi2[0]==mchi2[1] && mchi2[0]==0) ||
      (c>=mchi2[0] && c<=mchi2[1])  ) ;
  
}

inline bool StEStructTrackCuts::goodPt(float c){
  mvalues[mptName.idx] =c;
  return ( (mpt[0]==mpt[1] && mpt[0]==0) ||
      (c>=mpt[0] && c<=mpt[1])  ) ;
  
}

inline bool StEStructTrackCuts::goodYt(float c){
  mvalues[mytName.idx] =c;
  return ( (myt[0]==myt[1] && myt[0]==0) ||
      (c>=myt[0] && c<=myt[1])  ) ;
  
}

inline bool StEStructTrackCuts::goodPhi(float c){
  mvalues[mphiName.idx] =c;
  return ( (mphi[0]==mphi[1] && mphi[0]==0) ||
      (c>=mphi[0] && c<=mphi[1])  ) ;
  
}

inline bool StEStructTrackCuts::goodEta(float c){
  mvalues[metaName.idx] =c;
  return ( (meta[0]==meta[1] && meta[0]==0) ||
      (c>=meta[0] && c<=meta[1])  ) ;
  
}

inline bool StEStructTrackCuts::goodElectron(float c){
  mvalues[mnsigmaEName.idx] =c;
  return ( (mnsigmaE[0]==mnsigmaE[1] && mnsigmaE[0]==0) ||
      (c>=mnsigmaE[0] && c<=mnsigmaE[1])  ) ;
  
}

inline bool StEStructTrackCuts::goodPion(float c){
  mvalues[mnsigmaPiName.idx] =c;
  return ( (mnsigmaPi[0]==mnsigmaPi[1] && mnsigmaPi[0]==0) ||
      (c>=mnsigmaPi[0] && c<=mnsigmaPi[1])  ) ;
  
}

inline bool StEStructTrackCuts::goodKaon(float c){
  mvalues[mnsigmaKName.idx] =c;
  return ( (mnsigmaK[0]==mnsigmaK[1] && mnsigmaK[0]==0) ||
      (c>=mnsigmaK[0] && c<=mnsigmaK[1])  ) ;
  
}

inline bool StEStructTrackCuts::goodProton(float c){
  mvalues[mnsigmaPName.idx] =c;
  return ( (mnsigmaP[0]==mnsigmaP[1] && mnsigmaP[0]==0) ||
      (c>=mnsigmaP[0] && c<=mnsigmaP[1])  ) ;
  
};


#endif

/***********************************************************************
 *
 * $Log: StEStructTrackCuts.h,v $
 * Revision 1.1  2003/10/15 18:20:32  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/


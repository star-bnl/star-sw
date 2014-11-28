/**********************************************************************
 *
 * $Id: StEStructTrackCuts.h,v 1.7 2012/11/16 21:19:08 prindle Exp $
 *
 * Author: Jeff Porter 
 *
 **********************************************************************
 *
 * Description:  Cut class for track level quantities
 *
 *
 ***********************************************************************/
#ifndef __STESTRUCTTRACKCUTS__H
#define __STESTRUCTTRACKCUTS__H


#include "StEStructCuts.h"
#include "Stiostream.h"


class StEStructTrackCuts : public StEStructCuts {

public:

   CutName mflagName;
   CutName mchargeName;
   CutName mnfitpointsName;
   CutName mnfitnmaxName;
   CutName mglobalDCAName;
   CutName mchi2Name;
   CutName mdPtByPtName;
   CutName mptName;
   CutName mxtName;
   CutName mytName;
   CutName mphiName;
   CutName metaName;
   CutName mTOFEMassName;
   CutName mnsigmaEName;
   CutName mnsigmaPiName;
   CutName mnsigmaKName;
   CutName mnsigmaPName;
   CutName mhijingFragmentName;
 
  int   mflag[2];
  int   mcharge[2];
  int   mnfitpoints[2];
  float mnfitnmax[2];
  float mglobalDCA[2];
  float mchi2[2];
  float mdPtByPt[2];
  float mpt[2];
  float myt[2];
  float mxt[2];
  float mphi[2];
  float meta[2];  
  float mTOFEMass[2];
  float mnsigmaE[2];
  float mnsigmaPi[2];
  float mnsigmaK[2];
  float mnsigmaP[2];
  float mhijingFragment[2];
  char  mFragmentType[1024];
  int   mFragTypes[10];
  int   mNFragTypes;
  int   mnJets;

  void init();
  void initCuts();
  void initNames();

  StEStructTrackCuts();
  StEStructTrackCuts(const char* cutFileName);
  virtual ~StEStructTrackCuts();

  virtual bool loadBaseCuts(const char* name, const char** vals, int nvals);
  virtual void loadUserCuts(const char* name, const char** vals, int nvals);
  virtual void printCutStats(ostream& ofs);

  bool goodFlag(int f);
  bool goodCharge(int c);
  bool goodNFitPoints(int n);
  bool goodNFitNMax(float r);
  bool goodGlobalDCA(float g);
  bool goodChi2(float x);
  bool gooddPtByPt(float x);
  bool goodPt(float p);
  bool goodXt(float p);
  bool goodYt(float p);
  bool goodPhi(float p);
  bool goodEta(float e);
  bool goodTOFEMass(float e);
  bool goodElectron(float e);
  bool hasElectronCut();
  bool goodPion(float p);
  bool goodKaon(float k);
  bool goodProton(float p);
  bool goodFragment(int ifragtype);
 

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

inline bool StEStructTrackCuts::goodXt(float c){
  mvalues[mxtName.idx] =c;
  return ( (mxt[0]==mxt[1] && mxt[0]==0) ||
	   (c>=mxt[0] && c<=mxt[1])  ) ;

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

inline bool StEStructTrackCuts::goodTOFEMass(float c){
  mvalues[mTOFEMassName.idx] =c;
  return ( (mTOFEMass[0]==mTOFEMass[1] && mTOFEMass[0]==0) ||
	   (c>=mTOFEMass[0] && c<=mTOFEMass[1])  ) ;

}
/*
inline bool StEStructTrackCuts::goodElectron(float c) {
  mvalues[mnsigmaEName.idx] =c;
  if (mnsigmaE[0]==mnsigmaE[1] && mnsigmaE[0]==0) {
    return true;
  }
  if (mnsigmaE[0]<=c && c<=mnsigmaE[1]) {
    return true;
  }
  return false;
}
*/
inline bool StEStructTrackCuts::goodElectron(float c){
  mvalues[mnsigmaEName.idx] =c;
  return ( (mnsigmaE[0]==mnsigmaE[1] && mnsigmaE[0]==0) ||
	   (c>=mnsigmaE[0] && c<=mnsigmaE[1])  ) ;

}
inline bool StEStructTrackCuts::hasElectronCut() {
    return ( mnsigmaE[0] != mnsigmaE[1] ) ;
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
 * Revision 1.7  2012/11/16 21:19:08  prindle
 * Moved EventCuts, TrackCuts to EventReader. Affects most readers.
 * Added support to write and read EStructEvents.
 * Cuts: 3D histo support, switch to control filling of histogram for reading EStructEvents
 * EventCuts: A few new cuts
 * MuDstReader: Add 2D to some histograms, treat ToFCut, PrimaryCuts, VertexRadius histograms like other cut histograms.
 * QAHists: Add refMult
 * TrackCuts: Add some hijing cuts.
 *
 * Revision 1.6  2011/08/02 20:31:26  prindle
 *   Change string handling
 *   Added event cuts for VPD, good fraction of global tracks are primary, vertex
 *   found only from tracks on single side of TPC, good fraction of primary tracks have TOF hits..
 *   Added methods to check if cuts imposed
 *   Added 2010 200GeV and 62 GeV, 2011 19 GeV AuAu datasets, 200 GeV pp2pp 2009 dataset.
 *   Added TOF vs. dEdx vs. p_t histograms
 *   Fix participant histograms in QAHists.
 *   Added TOFEMass cut in TrackCuts although I think we want to supersede this.
 *
 * Revision 1.5  2008/12/02 23:35:36  prindle
 * Added code for pileup rejection in EventCuts and MuDstReader.
 * Modified trigger selections for some data sets in EventCuts.
 *
 * Revision 1.4  2006/04/04 22:05:07  porter
 * a handful of changes:
 *  - changed the StEStructAnalysisMaker to contain 1 reader not a list of readers
 *  - added StEStructQAHists object to contain histograms that did exist in macros or elsewhere
 *  - made centrality event cut taken from StEStructCentrality singleton
 *  - put in  ability to get any max,min val from the cut class - one must call setRange in class
 *
 * Revision 1.3  2005/09/14 17:08:37  msd
 * Fixed compiler warnings, a few tweaks and upgrades
 *
 * Revision 1.2  2005/09/07 20:18:44  prindle
 *   AnalysisMaker: Keep track of currentAnalysis (for use in doEStruct macro)
 *   EventCuts.h:   Added trigger cuts including cucu and year 4.
 *   MuDstReader:   Added dE/dx histograms. Re-arranged code to count tracks
 *                    before making centrality cut.
 *   TrackCuts:     Random changes. Moved some variables from private to public.o
 *
 * Revision 1.1  2003/10/15 18:20:32  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/


/**********************************************************************
 *
 * $Id: StEStructCutBin.h,v 1.9 2007/11/26 19:55:25 prindle Exp $
 *
 * Author: Jeff Porter 
 *
 **********************************************************************
 *
 * Description:  Cut-bins for building histograms based on kinematic selections
 *               Singleton class with several implementations based on
 *               a mode ID
 *
 ***********************************************************************/
#ifndef __STESTRUCTCUTBIN__H
#define __STESTRUCTCUTBIN__H

#include "Stiostream.h"
#include "TObject.h"
#include "TH1D.h"
#include "StEStructPairCuts.h"

class StEStructTrack;

class StEStructCutBin : public TObject {

 protected:

  int mcutMode;
  int mnumBins;
  int mnumParentBins;
  char* mcutModeName;

  int mPtBins[100]; // returned list of indexes terminated by -1
  float mPtBinMax[100];
  float mPtBinMin[100];
  TH1D** mHCutBinHists[8];

  static StEStructCutBin* mInstance;
  StEStructCutBin(): mcutMode(0), mnumBins(1), mcutModeName(0) { setMode(0); };
  //  StEStructCutBin(int mode){ setMode(mode); };

  int getCutBinMode1(StEStructPairCuts *pc);
  int getCutBinMode2(StEStructPairCuts *pc);
  int getCutBinMode3(StEStructPairCuts *pc);
  int getCutBinMode4(StEStructPairCuts *pc);
  int getCutBinMode5(StEStructPairCuts *pc, int pairCase);
  int getCutBinMode6(StEStructPairCuts *pc, int zbin);
  int getCutBinMode7(StEStructPairCuts *pc, int zbin);
  int ignorePair5(StEStructPairCuts *pc);
  int symmetrizeYt5(StEStructPairCuts *pc);
  int switchYt5(StEStructPairCuts *pc);

  void initPtBinMode0();
  void initPtBinMode1();
  void initPtBinMode2();
  void initPtBinMode3();
  void initPtBinMode4();
  void initPtBinMode5();
  void initPtBinMode6();
  void initPtBinMode7();

  void writeCutBinHists5();

 public:

  static StEStructCutBin* Instance();
  //  static StEStructCutBin* Instance(int mode);

  virtual ~StEStructCutBin();

  void setMode(int mode);
  int  getMode();
  // Save histograms (mHCutBinHists) to currently opened file.
  void writeCutBinHists();
  int  getNumBins();
  int  getNumParentBins();
  int  getParentBin(StEStructPairCuts *p, StEStructTrack* trkPtr);
  int  getNumQABins();
  int  getCutBin(StEStructPairCuts *p, int pairCase=0);
  int  ignorePair(StEStructPairCuts *pc);
  int  symmetrizeYt(StEStructPairCuts *pc);
  int  switchYt(StEStructPairCuts *pc);
  int*  getPtBins(float pt);
  char* printCutBinName();

  ClassDef(StEStructCutBin,1)

};

inline char* StEStructCutBin::printCutBinName(){ return mcutModeName; }

inline int StEStructCutBin::getNumBins(){ return mnumBins; }
inline int StEStructCutBin::getNumParentBins(){ return mnumParentBins; }
inline int StEStructCutBin::getNumQABins(){
    if (5 == mcutMode) {
        return 4;
    } else if (mcutMode == 7) {
      return 10;
    } else {
        return mnumBins;
    }
}

inline int StEStructCutBin::getCutBin(StEStructPairCuts *pc, int pairCase){
  int retVal=0;

 switch (mcutMode){
   case 0:
      {  
	retVal=0;
	break;
      }
  case 1:
      {
	retVal=getCutBinMode1(pc);
	break;
      }
  case 2:
      {
	retVal=getCutBinMode2(pc);
	break;
      }
  case 3:
      {
	retVal=getCutBinMode3(pc);
	break;
      }
  case 4:
      {
	retVal=getCutBinMode4(pc);
	break;
      }
  case 5:
      {
	retVal=getCutBinMode5(pc,pairCase);
	break;
      }
 case 6:
   {
     retVal=getCutBinMode6(pc,pairCase);
     break;
   }
 case 7:
   {
     retVal=getCutBinMode7(pc,pairCase);
     break;
   }
 }
 return retVal;
}
inline int StEStructCutBin::getParentBin(StEStructPairCuts *pc, StEStructTrack* trkPtr) {
    if (5 != mcutMode) {
        return 0;
    } else {
        return pc->getdEdxPID(trkPtr);
    }
}
inline void StEStructCutBin::writeCutBinHists() {
    switch (mcutMode) {
        case 5: {
            writeCutBinHists5();
            break;
        }
        default: {
            break;
        }
    }
    return;
}
inline int StEStructCutBin::ignorePair(StEStructPairCuts *pc) {
    if (mcutMode != 5) {
        if ( (-1 == pc->Track1()->Charge()) &&
             (+1 == pc->Track2()->Charge()) ) {
            return 1;
        } else {
            return 0;
        }
    }
    return ignorePair5(pc);
}
inline int StEStructCutBin::symmetrizeYt(StEStructPairCuts *pc) {
    if (mcutMode != 5) {
        return 1;
    } else {
        return symmetrizeYt5(pc);
    }
}
inline int StEStructCutBin::switchYt(StEStructPairCuts *pc) {
    if (mcutMode != 5) {
        return 0;
    } else {
        return switchYt5(pc);
    }
}

//-----------------------------------------------------------
inline int* StEStructCutBin::getPtBins(float pt){

  if(mcutMode>0){  
   int j=0;
   for(int i=0;i<mnumBins;i++){
    if(pt>=mPtBinMin[i] && pt<mPtBinMax[i]){
      mPtBins[j]=i;
      j++;
    }
   } 
   mPtBins[j]=-1;
  }
 
  return mPtBins;
}


#endif


/***********************************************************************
 *
 * $Log: StEStructCutBin.h,v $
 * Revision 1.9  2007/11/26 19:55:25  prindle
 * In 2ptCorrelations: Support for keeping all z-bins of selected centralities
 *                     Change way \hat{p_t} is calculated for parent distributions in pid case.
 *    Binning          Added parent binning (for \hat{p_t}
 *    CutBin:          Mode 5 extensively modified.
 *                     Added invariant mass cuts (probably a bad idea in general.)
 *
 * Revision 1.8  2007/05/27 22:45:02  msd
 * Added new cut bin modes 2 (soft/hard SS/AS), 6 (z-vertex binning), and 7 (modes 2*6).
 * Fixed bug in merging cut.
 * Added a few histograms to 2pt corr.
 *
 * Revision 1.7  2007/01/26 17:17:10  msd
 * Implemented new binning scheme: dEta stored in array with bin centered at zero, dPhi array has bins centered at zero and pi.  Final DEtaDPhi has 25x25 bins with dPhi bin width of pi/12 so all major angles are centered in bins.
 *
 * Revision 1.6  2006/10/02 22:21:01  prindle
 * Store only quadrant of eta_Delta - phi_Delta array/histogram.
 * Store half of eta_Sigma - phi_Delta array/histogram.
 * This required modifications in Binning.
 * I had a bug in the pair loop (which left +- not fully symmetrized)
 * and had to make changes in cut bins for mode 5 (and 3 I think)
 * when I fixed this.
 * Also change crossing cut to use only two parameters, the sign of
 * the magnetic field being taken from the MuDst.
 *
 * Revision 1.5  2006/04/06 01:01:20  prindle
 *
 *   New mode in CutBin, 5, to do pid correlations. There is still an issue
 * of how to set the pt ranges allowed for the different particle types.
 * For data we probably want to restrict p to below 1GeV for pi and K, but
 * for Hijing and Pythia we can have perfect pid. Currently cuts are type
 * into the code (so you have to re-compile to change them.)
 *
 *   In the Correlations code I split -+ from +- and am keeping track of
 * pt for each cut bin. These required changes in the Support code.
 *
 * Revision 1.4  2006/04/04 22:10:12  porter
 * a handful of changes (specific to correlations)
 *  - added StEStructQAHists so that if NOT input frm Maker, each analysis has its own
 *  - used ability to get any max,min val from the cut class - or z-vertex binning
 *  - put z-vertex binning into 1 place
 *  - switched back 1st line of pair cut method to keep pair if good, not to reject if bad.
 *  - Pair cut object is now pointer in correlations
 *  - some diagnostic printouts available from macro
 *  - Duncan's delta-phi binning change
 *
 * Revision 1.3  2006/02/22 22:05:18  prindle
 * Removed all references to multRef (?)
 * Added cut mode 5 for particle identified correlations.
 * Other cut modes should be same as before
 *
 * Revision 1.2  2005/03/03 01:30:44  porter
 * updated StEStruct2ptCorrelations to include pt-correlations and removed
 * old version of pt-correlations from chunhuih (StEStruct2ptPtNbar)
 *
 * Revision 1.1  2004/06/25 03:11:49  porter
 * New cut-binning implementation and modified pair-cuts for chunhui to review
 *
 *
 *********************************************************************/

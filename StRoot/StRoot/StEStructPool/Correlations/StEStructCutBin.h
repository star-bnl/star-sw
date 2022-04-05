/**********************************************************************
 *
 * $Id: StEStructCutBin.h,v 1.16 2013/02/08 19:32:43 prindle Exp $
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
  int mcutBinHistMode;
  double mMaxDEta;

  TH1D** mHCutBinHists[8];

  static StEStructCutBin* mInstance;
  StEStructCutBin(): mcutMode(0), mnumBins(1), mcutModeName(0), mcutBinHistMode(0), mMaxDEta(2) { setMode(0); };
  //  StEStructCutBin(int mode){ setMode(mode); };

  int getCutBinMode1(StEStructPairCuts *pc);
  int getCutBinMode2(StEStructPairCuts *pc);
  int getCutBinMode3(StEStructPairCuts *pc);
  int getCutBinMode4(StEStructPairCuts *pc);
  int getCutBinMode5(StEStructPairCuts *pc, int pairCase);
  int getCutBinMode6(StEStructPairCuts *pc, int zbin);
  int getCutBinMode7(StEStructPairCuts *pc, int zbin);
  int getCutBinMode8(StEStructPairCuts *pc);
  int getCutBinMode9(StEStructPairCuts *pc);
  int getCutBinMode10(StEStructPairCuts *pc);
  int ignorePair5(StEStructPairCuts *pc);
  int symmetrizeXX3(StEStructPairCuts *pc);
  int symmetrizeXX5(StEStructPairCuts *pc);
  int symmetrizeXX8(StEStructPairCuts *pc);
  int symmetrizeXX9(StEStructPairCuts *pc);
  int switchXX3(StEStructPairCuts *pc);
  int switchXX5(StEStructPairCuts *pc);
  int switchXX8(StEStructPairCuts *pc);
  int switchXX9(StEStructPairCuts *pc);
  int notSymmetrizedXX3(int cutBin, int pairCharge);
  int notSymmetrizedXX5(int cutBin, int pairCharge);
  int notSymmetrizedXX8(int cutBin, int pairCharge);
  int notSymmetrizedXX9(int cutBin, int pairCharge);

  void initCutBinHists5();
  void writeCutBinHists5();

 public:

  static StEStructCutBin* Instance();
  //  static StEStructCutBin* Instance(int mode);

  virtual ~StEStructCutBin();

  void setMode(int mode);
  int  getMode();
  // Save histograms (mHCutBinHists) to currently opened file.
  void setCutBinHistMode(int mode); // Non-zero means fill histograms.
  void setMaxDEta(double deta);
  int  getCutBinHistMode();
  void initCutBinHists();
  void writeCutBinHists();
  int  getNumBins();
  int  getNumPairDensityBins();
  int  getNumParentBins();
  int  getParentBin(StEStructPairCuts *p, StEStructTrack* trkPtr);
  int  getNumQABins();
  int  getCutBin(StEStructPairCuts *p, int pairCase=0);
  int  getPairDensityBin(int ibin);
  int  ignorePair(StEStructPairCuts *pc);
  int  symmetrizeXX(StEStructPairCuts *pc);
  int  switchXX(StEStructPairCuts *pc);
  int  notSymmetrizedXX(int cutBin, int pairCharge);
  char* printCutBinName();

  ClassDef(StEStructCutBin,1)

};

inline char* StEStructCutBin::printCutBinName(){ return mcutModeName; }

inline void StEStructCutBin::setMaxDEta(double deta){ mMaxDEta = deta; }

inline int StEStructCutBin::getNumBins(){ return mnumBins; }
inline int StEStructCutBin::getNumPairDensityBins() {
    if (3 == mcutMode) {
        return 4;
    } else {
        return mnumBins;
    }
}
inline int StEStructCutBin::getNumParentBins(){ return mnumParentBins; }
inline int StEStructCutBin::getNumQABins() { return mnumParentBins; }

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
 case 8:
   {
     retVal=getCutBinMode8(pc);
     break;
   }
 case 9:
   {
     retVal=getCutBinMode9(pc);
     break;
   }
 case 10:
   {
     retVal=getCutBinMode10(pc);
     break;
   }
 }
 return retVal;
}
inline int StEStructCutBin::getPairDensityBin(int ibin){
    if (3 == mcutMode) {
        if (2 != ibin && 6 != ibin && 10 != ibin && 14 != ibin) {
            return -1;
        } else {
            return (ibin-2)/4;
        }
    } else {
        return ibin;
    }
}
inline int StEStructCutBin::getParentBin(StEStructPairCuts *pc, StEStructTrack* trkPtr) {
    // This is for calculating mean pt of parent population.
    if (3 == mcutMode) {
        float yt = trkPtr->Yt();
        // These numbers are also used in StEStructCutBin::getCutBinMode3 (change both)
        if (yt<1.99) {                        // soft
            return 0;
        } else if ((1.99<yt) && (yt<2.383)) {   // neck
            return 1;
        } else {                             // hard
            return 2;
        }
    } else if (5 == mcutMode) {
        return trkPtr->PID();
    } else if (8 == mcutMode) {
        float yt = trkPtr->Yt();
        // These numbers are also used in StEStructCutBin::getCutBinMode3 (change both)
        if (yt<1.8) {                        // soft
            return 0;
        } else if ((1.8<yt) && (yt<2.2)) {   // neck
            return 1;
        } else {                             // hard
            return 2;
        }
    } else if (9 == mcutMode) {
        float pt = trkPtr->Pt();
        // These numbers are also used in StEStructCutBin::getCutBinMode9 (change both)
        if (pt<0.5) {
            return 0;
        } else if (pt<1.0) {
            return 1;
        } else if (pt<2.0) {
            return 2;
        } else if (pt<3.0) {
            return 3;
        } else if (pt<4.0) {
            return 4;
        } else {
            return 5;
        }
    } else {
        return 0;
    }
}
inline void StEStructCutBin::initCutBinHists() {
    switch (mcutMode) {
        case 5: {
            initCutBinHists5();
            break;
        }
        default: {
            break;
        }
    }
    return;
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
// Normally we want to enter pair at (x1,x2) _and_ (x2,x1) for phi-phi, eta-eta etc. histograms.
// When Particles are distinguishable in some way we can postpone the symmetrization and
// perhaps get some useful information. Maybe this will work for ytyt, but eta and phi
// seem to be dominated by detector artifacts.
inline int StEStructCutBin::symmetrizeXX(StEStructPairCuts *pc) {
    switch (mcutMode) {
        case 3: {
            return symmetrizeXX3(pc);
        }
        case 5: {
            return symmetrizeXX5(pc);
        }
        case 8: {
            return symmetrizeXX8(pc);
        }
        case 9: {
            return symmetrizeXX9(pc);
        }
        default: {
            return 1;
        }
    }
}
// When we are not symmetrizing we want to order the pair. For example we may want the lower pt
// track to be the first of the pair.
inline int StEStructCutBin::switchXX(StEStructPairCuts *pc) {
    switch (mcutMode) {
        case 3: {
            return switchXX3(pc);
        }
        case 5: {
            return switchXX5(pc);
        }
        case 8: {
            return switchXX8(pc);
        }
        case 9: {
            return switchXX9(pc);
        }
        default: {
            return 0;
        }
    }
    return 0;
}
// When adding cut bins together some of them may have been symmetrized during analysis.
// If one hasn't it needs to be symmetrized before the addition.
// This method is called from StEStructHAdd in Support.
inline int StEStructCutBin::notSymmetrizedXX(int cutBin, int pairCharge) {
    switch (mcutMode) {
        case 3: {
            return notSymmetrizedXX3(cutBin,pairCharge);
        }
        case 5: {
            return notSymmetrizedXX5(cutBin,pairCharge);
        }
        case 8: {
            return notSymmetrizedXX8(cutBin,pairCharge);
        }
        case 9: {
            return notSymmetrizedXX9(cutBin,pairCharge);
        }
        default: {
            return 0;
        }
    }
    return 0;
}



#endif


/***********************************************************************
 *
 * $Log: StEStructCutBin.h,v $
 * Revision 1.16  2013/02/08 19:32:43  prindle
 * Added "Triggered" histograms in StEStruct2ptCorrelations.
 * Protected against using tracks cuts in StEStruct2ptCorrelations when reading EStruct format events.
 * Added comment in EventMaker/StEStructTrack.cxx pointing out need to set BField correctly
 * when reading EStruct format events. (This should be read from file somehow, but...)
 *
 * Revision 1.15  2012/11/16 21:22:27  prindle
 * 2ptCorrelations: SS, AS histograms.  Get eta limits from cuts. Fit PtAll histogram. Add histograms to keep track of eta, phi limits. A few more histograms
 * Binning: Add quality cut.
 * CutBin: modify mode9
 * PairCuts: modify goodDeltaZ for case of one track leaving via endcap.
 *
 * Revision 1.14  2012/10/30 00:16:50  dkettler
 * Cut bins for marginal pt bins added
 *
 * Revision 1.13  2011/08/02 20:34:02  prindle
 *   More detailed histograms for event mixing.
 *   Buffer: increased mixed events to 4 (from 2)
 *   CutBin: added mode 9 for exploration of p_t space, fixed place in mode 5 where
 *           histogram was written before checking it existed.
 *   OneBuffer: added ZDC coincidence rate to event sorting space.
 *
 * Revision 1.12  2010/09/02 21:24:08  prindle
 *   2ptCorrelations: Fill histograms for event mixing information
 *                    Option for common mixing buffer
 *                    Switch to selectively fill QInv histograms (which take a long time)
 *   CutBin: Moved PID code to Track class from Pair class. Needed to update this code.
 *   PairCuts: Moved PID code from here to Track class.
 *             Removed unnecessary creation of StThreeVector which seem to take a long time
 *             Add ToF momentum cuts, modify dEdx momentum cuts. (Now allow dEdx to be
 *             be identified up to 15GeV/c, ToF up to 10GeV/c.)
 *
 * Revision 1.11  2008/12/02 23:45:06  prindle
 * Changed switchYt to switchXX (etc.) to better reflect function.
 * Change minYt to 1.0 in Binning so YtYt histogram doesn't have empty lower bin (pt = 0.164 for yt = 1.0)
 * In CutBin: remove initPtBin
 *            add mode 8
 *            add notSymmetrized (used in Support)
 * Added LUT (Look Up Table) for pair cuts. Experimental for now.
 * Modified cutMerging2 (to look at track separation at a few radii)
 * and cutCrossing2 so it doesn't accidentally reject almost back to back tracks.
 *
 * Revision 1.10  2008/03/19 22:06:01  prindle
 * Added doInvariantMass flag.
 * Added some plots in pairDensityHistograms.
 * SetZOffset used to only be done when doPairDensity was true.
 * Moved creating/copying pairDensity histograms to same place as other histograms.
 * Added cutBinHistMode
 * mode3 neck was defined as yt1<2.2 && yt2<2.2 (and not soft)
 *            now is        1.8<yt1<2.2  && 1.8<yt2<2.2
 * Added gooddzdxy, Merging2 and Crossing2 to pair cuts.
 *
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

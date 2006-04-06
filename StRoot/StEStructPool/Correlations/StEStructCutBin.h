/**********************************************************************
 *
 * $Id: StEStructCutBin.h,v 1.5 2006/04/06 01:01:20 prindle Exp $
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
#include "StEStructPairCuts.h"

class StEStructTrack;

class StEStructCutBin : public TObject {

 protected:

  int mcutMode;
  int mnumBins;
  char* mcutModeName;

  int mPtBins[100]; // returned list of indexes terminated by -1
  float mPtBinMax[100];
  float mPtBinMin[100];

  static StEStructCutBin* mInstance;
  StEStructCutBin(): mcutMode(0), mnumBins(1), mcutModeName(0) { setMode(0); };
  //  StEStructCutBin(int mode){ setMode(mode); };

  int getCutBinMode1(StEStructPairCuts *pc);
  int getCutBinMode2(StEStructPairCuts *pc);
  int getCutBinMode3(StEStructPairCuts *pc);
  int getCutBinMode4(StEStructPairCuts *pc);
  int getCutBinMode5(StEStructPairCuts *pc);
  int switchBins5(StEStructPairCuts *pc);
  int symmetrizeBins5(StEStructPairCuts *pc);

  void initPtBinMode0();
  void initPtBinMode1();
  void initPtBinMode2();
  void initPtBinMode3();
  void initPtBinMode4();
  void initPtBinMode5();

 public:

  static StEStructCutBin* Instance();
  //  static StEStructCutBin* Instance(int mode);

  virtual ~StEStructCutBin();

  void setMode(int mode);
  int  getMode();
  int  getNumBins();
  int  getNumQABins();
  int  getCutBin(StEStructPairCuts *pc);
  int  switchBins(StEStructPairCuts *pc);
  int  symmetrizeBins(StEStructPairCuts *pc);
  int*  getPtBins(float pt);
  int   getdEdxPID(const StEStructTrack *t);
  char* printCutBinName();

  ClassDef(StEStructCutBin,1)

};

inline char* StEStructCutBin::printCutBinName(){ return mcutModeName; }

inline int StEStructCutBin::getNumBins(){ return mnumBins; }
inline int StEStructCutBin::getNumQABins(){
    if (5 == mcutMode) {
        return 4;
    } else {
        return mnumBins;
    }
}

inline int StEStructCutBin::getCutBin(StEStructPairCuts *pc){
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
	retVal=getCutBinMode5(pc);
	break;
      }
  default:
      {
    
	break;
      }
 }
 return retVal;
}
inline int StEStructCutBin::switchBins(StEStructPairCuts *pc){
  if (mcutMode != 5) {
      return 1;
  }
  return switchBins5(pc);
}
inline int StEStructCutBin::symmetrizeBins(StEStructPairCuts *pc){
  if (mcutMode != 5) {
      if (pc->Track1()->Charge() == pc->Track2()->Charge()) {
          return 1;
      } else {
          return 0;
      }
  } else {
      return symmetrizeBins5(pc);
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
 * Revision 1.5  2006/04/06 01:01:20  prindle
 * New mode in CutBin, 5, to do pid correlations. There is still an issue
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

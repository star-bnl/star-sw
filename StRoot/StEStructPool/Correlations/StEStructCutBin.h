/**********************************************************************
 *
 * $Id: StEStructCutBin.h,v 1.3 2006/02/22 22:05:18 prindle Exp $
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

class StEStructPairCuts;
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
  int switchYtBin5(StEStructPairCuts *pc);

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
  int  getCutBin(StEStructPairCuts *pc);
  int  switchYtBins(StEStructPairCuts *pc);
  int  symmetrizeYtBins(StEStructPairCuts *pc);
  int*  getPtBins(float pt);
  int   getdEdxPID(const StEStructTrack *t);
  char* printCutBinName();

  ClassDef(StEStructCutBin,1)

};

inline char* StEStructCutBin::printCutBinName(){ return mcutModeName; }

inline int StEStructCutBin::getNumBins(){ return mnumBins; }

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
inline int StEStructCutBin::switchYtBins(StEStructPairCuts *pc){
  if (mcutMode != 5) {
      return 1;
  }
  return switchYtBin5(pc);
}
inline int StEStructCutBin::symmetrizeYtBins(StEStructPairCuts *pc){
  if (mcutMode != 5) {
      return 1;
  } else {
      return 0;
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

/**********************************************************************
 *
 * $Id: StEStructCutBin.h,v 1.1 2004/06/25 03:11:49 porter Exp $
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


class StEStructCutBin : public TObject {

 protected:

  int mcutMode;
  int mnumBins;
  char* mcutModeName;

  static StEStructCutBin* mInstance;
  StEStructCutBin(): mcutMode(0), mnumBins(1), mcutModeName(0) { setMode(0); };
  //  StEStructCutBin(int mode){ setMode(mode); };

  int getCutBinMode1(StEStructPairCuts *pc);
  int getCutBinMode2(StEStructPairCuts *pc);
  int getCutBinMode3(StEStructPairCuts *pc);


 public:

  static StEStructCutBin* Instance();
  //  static StEStructCutBin* Instance(int mode);

  virtual ~StEStructCutBin();

  void setMode(int mode);
  int  getNumBins();
  int  getCutBin(StEStructPairCuts *pc);
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
  default:
      {
    
	break;
      }
 }
 return retVal;
}

#endif


/***********************************************************************
 *
 * $Log: StEStructCutBin.h,v $
 * Revision 1.1  2004/06/25 03:11:49  porter
 * New cut-binning implementation and modified pair-cuts for chunhui to review
 *
 *
 *********************************************************************/

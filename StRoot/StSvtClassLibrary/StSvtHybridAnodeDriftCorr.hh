/***************************************************************************
 *
 * $Id: StSvtHybridAnodeDriftCorr.hh,v 1.1 2004/07/31 00:46:59 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 *
 ***************************************************************************
 *
 * Description: Correction factors for drift velocity as a function of anodes 
 *
 ***************************************************************************/
 
#ifndef STSVTHYBRIDANODEDRIFTCORR_HH
#define STSVTHYBRIDANODEDRIFTCORR_HH

#include "StSvtClassLibrary/StSvtHybridObject.hh" 

#define MAX_NUMBER_OF_ANODES 240

class StSvtHybridAnodeDriftCorr:public StSvtHybridObject 
{
public:
  StSvtHybridAnodeDriftCorr();
  StSvtHybridAnodeDriftCorr(int barrel, int ladder, int wafer, int hybrid);
   ~StSvtHybridAnodeDriftCorr();

  void setValue(int anode, float value){mDriftCorr[anode-1]=value;}
  void getValue(int anode){return mDriftCorr[anode-1];}
  
protected:
 
  float mDriftCorr[MAX_NUMBER_OF_ANODES];
};

#endif

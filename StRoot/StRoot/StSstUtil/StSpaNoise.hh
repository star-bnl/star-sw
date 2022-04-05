// $Id: StSpaNoise.hh,v 1.2 2015/06/24 17:37:21 smirnovd Exp $
//
// $Log: StSpaNoise.hh,v $
// Revision 1.2  2015/06/24 17:37:21  smirnovd
// StSstUtil: Prepend included headers with path to submodule
//
// Revision 1.1  2015/06/23 16:26:19  jeromel
// First version created from the SSD code and reshaped
//
// Revision 1.1  2015/04/19 17:30:31  bouchet
// initial commit ; SST codes
//
// Revision 1.1  2015/01/29 20:16:37  bouchet
// SSD utils for hit reconstruction
//
// Revision 1.1  2006/10/16 16:43:29  bouchet
// StSstUtil regroups now methods for the classes StSstStrip, StSstCluster and StSstPoint
//
// Revision 1.2  2005/05/13 08:39:33  lmartin
// CVS tags added
//

#ifndef STSPANOISE_HH
#define STSPANOISE_HH
# include "St_base/Stiostream.h"
# include <stdlib.h>
# include <math.h>
#include "Rtypes.h"

class StSpaNoise {
 public:
  StSpaNoise(Int_t rNStrip, Int_t rPedestal, Int_t rSigma) :  
    mNStrip(rNStrip), mPedestal(rPedestal), mSigma(rSigma), mNoiseValue(0), mPrevNoise(0), mNextNoise(0), 
    mIsActive(1) {}

  ~StSpaNoise() {}

  void        setNStrip(Int_t rNStrip){ mNStrip = rNStrip; }		       
  void 	      setPedestal(Int_t rPedestal){ mPedestal = rPedestal; }	       
  void 	      setSigma(Int_t rSigma){  mSigma = rSigma; }		       
  void 	      setNoiseValue(Int_t rNoiseValue){ mNoiseValue = rNoiseValue; }  
  void 	      setIsActive(Int_t rIsActive){ mIsActive = rIsActive; }	       
  void 	      setPrevNoise(StSpaNoise *rPrevNoise){ mPrevNoise = rPrevNoise; }
  void 	      setNextNoise(StSpaNoise *rNextNoise){ mNextNoise = rNextNoise; }
  Int_t       getNStrip(){ return mNStrip; }		
  Int_t       getPedestal(){ return mPedestal; }	
  Int_t       getSigma(){ return mSigma; }		
  Int_t       getNoiseValue(){ return mNoiseValue; }	
  Int_t       getIsActive(){ return mIsActive; }      
  StSpaNoise* getPrevNoise(){ return mPrevNoise; }
  StSpaNoise* getNextNoise(){ return mNextNoise; }
  StSpaNoise* giveCopy();

private:
  Int_t       mNStrip;
  Int_t       mPedestal;
  Int_t       mSigma;
  Int_t       mNoiseValue;
  StSpaNoise *mPrevNoise;
  StSpaNoise *mNextNoise;
  Int_t       mIsActive;

};
#endif

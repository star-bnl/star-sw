// $Id: StTpcBadChanMaker.h,v 1.2 1999/10/11 08:04:09 fretiere Exp $
// $Log: StTpcBadChanMaker.h,v $
// Revision 1.2  1999/10/11 08:04:09  fretiere
// Fix bugg + add README, LOG and ID
//
///////////////////////////////////////////////////////////////////////////////
#ifndef STAR_StTpcBadChanMaker
#define STAR_StTpcBadChanMaker

#ifndef StMaker_H
#include "StMaker.h"
#endif
// Local
class StTpcCalibSetup;
class StTpcCalibSector;

class StTpcBadChanMaker : public StMaker {
private:

  const StTpcCalibSetup* mSetup; //!
  StTpcCalibSector** mTpcCalibSector; //!

  void CalcElectronicConvertor(int** aPadToFeeConvertor, 
			       int** aPadToRDOConvertor);
protected:
public: 
  StTpcBadChanMaker(const char *aMakerName,
		 const StTpcCalibSetup* aSetup);
		 //		 const StTpcDb* aTpcDb);
  virtual       ~StTpcBadChanMaker();
  virtual Int_t  Init();
  virtual Int_t  Make();
  virtual Int_t  Finish();
  virtual Int_t  Clear();
  virtual void   PrintInfo();
 ClassDef(StTpcBadChanMaker, 1)   //StAF chain virtual base class for Makers
};

#endif

// $Id: StTpcDeadChanMaker.h,v 1.2 1999/10/11 08:04:17 fretiere Exp $
// $Log: StTpcDeadChanMaker.h,v $
// Revision 1.2  1999/10/11 08:04:17  fretiere
// Fix bugg + add README, LOG and ID
//
///////////////////////////////////////////////////////////////////////////
#ifndef STAR_StTpcDeadChanMaker
#define STAR_StTpcDeadChanMaker

#ifndef StMaker_H
#include "StMaker.h"
#endif
// Local
class StTpcCalibSetup;
class StTpcCalibSector;

class StTpcDeadChanMaker : public StMaker {
private:

  const StTpcCalibSetup* mSetup; //!
  StTpcCalibSector** mTpcCalibSector; //!

  void CalcElectronicConvertor(int** aPadToFeeConvertor, 
			       int** aPadToRDOConvertor);
protected:
public: 
  StTpcDeadChanMaker(const char *aMakerName,
		 const StTpcCalibSetup* aSetup);
		 //		 const StTpcDb* aTpcDb);
  virtual       ~StTpcDeadChanMaker();
  virtual Int_t  Init();
  virtual Int_t  Make();
  virtual Int_t  Finish();
  virtual Int_t  Clear();
  virtual void   PrintInfo();
 ClassDef(StTpcDeadChanMaker, 1)   //StAF chain virtual base class for Makers
};

#endif

// $Id: StTpcFEEGainCalibMaker.h,v 1.3 2000/08/04 21:03:53 perev Exp $
// $Log: StTpcFEEGainCalibMaker.h,v $
// Revision 1.3  2000/08/04 21:03:53  perev
// Leaks + Clear() cleanup
//
// Revision 1.2  1999/10/11 08:04:18  fretiere
// Fix bugg + add README, LOG and ID
//
///////////////////////////////////////////////////////////////////////////
#ifndef STAR_StTpcFEEGainCalibMaker
#define STAR_StTpcFEEGainCalibMaker

#ifndef StMaker_H
#include "StMaker.h"
#endif
// Local
class StTpcCalibSetup;
class StTpcCalibSector;

class StTpcFEEGainCalibMaker : public StMaker {
private:

  const StTpcCalibSetup* mSetup; //!
  StTpcCalibSector** mTpcCalibSector; //!

  void CalcElectronicConvertor(int** aPadToFeeConvertor, 
			       int** aPadToRDOConvertor);
protected:
public: 
  StTpcFEEGainCalibMaker(const char *aMakerName,
		 const StTpcCalibSetup* aSetup);
		 //		 const StTpcDb* aTpcDb);
  virtual       ~StTpcFEEGainCalibMaker();
  virtual Int_t  Init();
  virtual Int_t  Make();
  virtual Int_t  Finish();
  virtual void   PrintInfo();
 ClassDef(StTpcFEEGainCalibMaker, 1)   //StAF chain virtual base class for Makers
};

#endif

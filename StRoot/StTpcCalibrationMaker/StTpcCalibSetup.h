// $Id: StTpcCalibSetup.h,v 1.3 1999/10/11 08:04:14 fretiere Exp $
// $Log: StTpcCalibSetup.h,v $
// Revision 1.3  1999/10/11 08:04:14  fretiere
// Fix bugg + add README, LOG and ID
//
/////////////////////////////////////////////////////////////////////////////
// This class is only a container of all the parameters I need.
// C/C++
#ifndef STAR_StTpcCalibSetup
#define STAR_StTpcCalibSetup

#include <strstream.h>
#include "TH1.h"

class StTpcCalibSetup {

 private:
  int  mMaxNumberOfCorruptedTB;
  ostrstream* mBadFileName; //!
  ostrstream* mDeadFileName; //!
  ostrstream* mGainCalibFileName; //!
  ostrstream* mRootOutFileName; //!

  int mFirstTB;
  int mLastTB;

  int mMinDistToPulse;
  int mExpectedPulsePos; 

  int mNSidePadsExcluded;

 public:
  StTpcCalibSetup();
  void setMaxNumberOfCorruptedTB(int aNumber)
    {mMaxNumberOfCorruptedTB=aNumber;}
  const int& getMaxNumberOfCorruptedTB() const
    {return mMaxNumberOfCorruptedTB;}

  void setBadFileName(char* aCalibOutFileName);
  const char* getBadFileName() const
    {return mBadFileName->str();}

  void setDeadFileName(char* aCalibOutFileName);
  const char* getDeadFileName() const
    {return mDeadFileName->str();}

  void setGainCalibFileName(char* aCalibOutFileName);
  const char* getGainCalibFileName() const
    {return mGainCalibFileName->str();}

  void setRootOutFileName(char* aRootOutFileName);
  const char* getRootOutFileName() const
    {return mRootOutFileName->str();}  
  void setMinDistToPulse(int aMinDistToPulse)
    {mMinDistToPulse=aMinDistToPulse;}
  const int& getMinDistToPulse() const
    {return mMinDistToPulse;}
  void setExpectedPulsePos(int aExpectedPulsePos)
    {mExpectedPulsePos=aExpectedPulsePos;}
  const int& getExpectedPulsePos() const
    {return mExpectedPulsePos;}
  void setNSidePadsExcluded(int aNSidePadsExcluded)
    {mNSidePadsExcluded=aNSidePadsExcluded;}
  const int& getNSidePadsExcluded() const
    {return mNSidePadsExcluded;}
  void setFirstTB(int aFirstTB)
    {mFirstTB=aFirstTB;}
  const int getFirstTB() const
    {return mFirstTB;}
  void setLastTB(int aLastTB)
    {mLastTB=aLastTB;}
  const int getLastTB() const
    {return mLastTB;}

  ClassDef(StTpcCalibSetup, 1) //
};

#endif

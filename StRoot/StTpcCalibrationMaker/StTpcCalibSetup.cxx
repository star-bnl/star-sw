// $Id: StTpcCalibSetup.cxx,v 1.5 2012/06/09 02:22:27 fisyak Exp $
// $Log: StTpcCalibSetup.cxx,v $
// Revision 1.5  2012/06/09 02:22:27  fisyak
// namespace std
//
// Revision 1.4  2003/09/02 17:59:11  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.3  1999/10/11 08:04:12  fretiere
// Fix bugg + add README, LOG and ID
//
/////////////////////////////////////////////////////////////////////////////
#include <string.h>
#include <Stsstream.h>
#include <Stiostream.h>
#include "StTpcCalibSetup.h"
using namespace std;
ClassImp(StTpcCalibSetup)

StTpcCalibSetup::StTpcCalibSetup(){
  mMaxNumberOfCorruptedTB=300;
  mBadFileName = new ostrstream;
  (*mBadFileName) << "Bad.txt" << ends;
  mDeadFileName = new ostrstream;
  (*mDeadFileName) << "Dead.txt" << ends;
  mGainCalibFileName = new ostrstream;
  (*mGainCalibFileName) << "GainCalib.txt" << ends;
  mRootOutFileName  = new ostrstream;
  (*mRootOutFileName) << "CalibCtrl.root" << ends;

  mFirstTB=11;
  mLastTB=361;

  mMinDistToPulse=3;
  mExpectedPulsePos=160;

  mNSidePadsExcluded=5; 
}

void StTpcCalibSetup::setBadFileName(char* aBadFileName){
  delete mBadFileName;
  mBadFileName = new ostrstream;
  (*mBadFileName) << aBadFileName;
}

void StTpcCalibSetup::setDeadFileName(char* aDeadFileName){
  delete mDeadFileName;
  mDeadFileName = new ostrstream;
  (*mDeadFileName) << aDeadFileName;
}

void StTpcCalibSetup::setGainCalibFileName(char* aGainCalibFileName){
  delete mGainCalibFileName;
  mGainCalibFileName = new ostrstream;
  (*mGainCalibFileName) << aGainCalibFileName;
}


void StTpcCalibSetup::setRootOutFileName(char* aRootOutFileName){
  delete mRootOutFileName;
  mRootOutFileName=new ostrstream;
  (*mRootOutFileName) << aRootOutFileName;
}

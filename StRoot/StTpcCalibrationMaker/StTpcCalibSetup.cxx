// $Id: StTpcCalibSetup.cxx,v 1.3 1999/10/11 08:04:12 fretiere Exp $
// $Log: StTpcCalibSetup.cxx,v $
// Revision 1.3  1999/10/11 08:04:12  fretiere
// Fix bugg + add README, LOG and ID
//
/////////////////////////////////////////////////////////////////////////////
#include <string.h>
#include "StTpcCalibSetup.h"

ClassImp(StTpcCalibSetup)

StTpcCalibSetup::StTpcCalibSetup(){
  mMaxNumberOfCorruptedTB=300;
  mBadFileName = new ostrstream((new char[30]),30);
  (*mBadFileName) << "Bad.txt" << ends;
  mDeadFileName = new ostrstream((new char[30]),30);
  (*mDeadFileName) << "Dead.txt" << ends;
  mGainCalibFileName = new ostrstream((new char[30]),30);
  (*mGainCalibFileName) << "GainCalib.txt" << ends;
  mRootOutFileName  = new ostrstream((new char[30]),30);
  (*mRootOutFileName) << "CalibCtrl.root" << ends;

  mFirstTB=11;
  mLastTB=361;

  mMinDistToPulse=3;
  mExpectedPulsePos=160;

  mNSidePadsExcluded=5; 
}

void StTpcCalibSetup::setBadFileName(char* aBadFileName){
  delete mBadFileName;
  mBadFileName = new ostrstream((new char[strlen(aBadFileName)]),
				strlen(aBadFileName));
  (*mBadFileName) << aBadFileName;
}

void StTpcCalibSetup::setDeadFileName(char* aDeadFileName){
  delete mDeadFileName;
  mDeadFileName = new ostrstream((new char[strlen(aDeadFileName)]),
				strlen(aDeadFileName));
  (*mDeadFileName) << aDeadFileName;
}

void StTpcCalibSetup::setGainCalibFileName(char* aGainCalibFileName){
  delete mGainCalibFileName;
  mGainCalibFileName = new ostrstream((new char[strlen(aGainCalibFileName)]),
				strlen(aGainCalibFileName));
  (*mGainCalibFileName) << aGainCalibFileName;
}


void StTpcCalibSetup::setRootOutFileName(char* aRootOutFileName){
  delete mRootOutFileName;
  mRootOutFileName=new ostrstream((new char[strlen(aRootOutFileName)]),
				  strlen(aRootOutFileName));
  (*mRootOutFileName) << aRootOutFileName;
}

#include <string.h>
#include "StTpcCalibSetup.h"

ClassImp(StTpcCalibSetup)

StTpcCalibSetup::StTpcCalibSetup(){
  mMaxNumberOfCorruptedTB=300;
  mBadFileName = new ostrstream((new char[30]),30);
  (*mBadFileName) << "Bad.txt" << ends;
  mRootOutFileName  = new ostrstream((new char[30]),30);
  (*mRootOutFileName) << "Bad.root" << ends;

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

void StTpcCalibSetup::setRootOutFileName(char* aRootOutFileName){
  delete mRootOutFileName;
  mRootOutFileName=new ostrstream((new char[strlen(aRootOutFileName)]),
				  strlen(aRootOutFileName));
  (*mRootOutFileName) << aRootOutFileName;
}

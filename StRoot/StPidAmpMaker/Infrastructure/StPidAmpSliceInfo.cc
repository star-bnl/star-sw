/***************************************************************************
 *
 * $Id: StPidAmpSliceInfo.cc,v 1.1.1.1 2000/03/09 17:48:35 aihong Exp $
 *
 * Author: Aihong Tang & Richard Witt (FORTRAN Version),Kent State U.
 *         Send questions to aihong@cnr.physics.kent.edu
 ***************************************************************************
 *
 * Description:part of StPidAmpMaker package
 *             StPidAmpSliceInfo contains fitted information of a slice.
 ***************************************************************************
 *
 * $Log: StPidAmpSliceInfo.cc,v $
 * Revision 1.1.1.1  2000/03/09 17:48:35  aihong
 * Installation of package
 *
 **************************************************************************/


#include "StPidAmpMaker/Infrastructure/StPidAmpSliceInfo.hh"

//----------------------------------
StPidAmpSliceInfo::StPidAmpSliceInfo(){

  /* no op */
}

//----------------------------------
StPidAmpSliceInfo::~StPidAmpSliceInfo(){

  /* no op */
}

//----------------------------------
StPidAmpSliceInfo::StPidAmpSliceInfo(double meanDedx, double sigma, double amp){
   mMeanDedx=meanDedx;
   mSigma=sigma;
   mAmp=amp;
}

//----------------------------------
ostream& operator<<(ostream& s, const StPidAmpSliceInfo& info){
  return s<<info.meanDedx()<<" "<<info.sigma()<<" "<<info.amp()<<endl;
}


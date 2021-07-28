/***************************************************************************
 *
 * $Id: fabricesPairCut.cxx,v 1.3 2003/09/02 17:58:21 perev Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   a do-nothing pair cut that simply says "true" to every pair           
 *
 ***************************************************************************
 *
 * $Log: fabricesPairCut.cxx,v $
 * Revision 1.3  2003/09/02 17:58:21  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.2  2002/12/12 17:03:51  kisiel
 * Add NDedxHits cut, slight modification for Y cuts and Fabrices probability
 *
 * Revision 1.1  2001/12/14 23:11:27  fretiere
 * Add class HitMergingCut. Add class fabricesPairCut = HitMerginCut + pair purity cuts. Add TpcLocalTransform function which convert to local tpc coord (not pretty). Modify StHbtTrack, StHbtParticle, StHbtHiddenInfo, StHbtPair to handle the hit information and cope with my code
 *
 * Revision 1.3  2000/01/25 17:35:02  laue
 * I. In order to run the stand alone version of the StHbtMaker the following
 * changes have been done:
 * a) all ClassDefs and ClassImps have been put into #ifdef __ROOT__ statements
 * b) unnecessary includes of StMaker.h have been removed
 * c) the subdirectory StHbtMaker/doc/Make has been created including everything
 * needed for the stand alone version
 *
 * II. To reduce the amount of compiler warning
 * a) some variables have been type casted
 * b) some destructors have been declared as virtual
 *
 * Revision 1.2  1999/07/06 22:33:21  lisa
 * Adjusted all to work in pro and new - dev itself is broken
 *
 * Revision 1.1.1.1  1999/06/29 16:02:56  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#include "StHbtMaker/Cut/fabricesPairCut.h"
#include <string>
#include <cstdio>
#include <Stsstream.h>

#ifdef __ROOT__
ClassImp(fabricesPairCut)
#endif

//__________________
fabricesPairCut::fabricesPairCut():HitMergingPairCut(){
  mNPairsPassed = mNPairsFailed = 0;
  mPiKPairMinProb=0.;
  mPiPPairMinProb=0.;
  mElPairMaxProb=1.;
  mPiPiPairMaxProb=1.;
  mKKPairMaxProb=1.;
}
//__________________
//fabricesPairCut::~fabricesPairCut(){
//  /* no-op */
//}
//__________________
bool fabricesPairCut::Pass(const StHbtPair* pair){
  bool temp = ( pair->track1()->TrackId()!=pair->track2()->TrackId() &&
  		pair->ElectronPairProbability() < mElPairMaxProb && 
  		((pair->track1()->Track()->PidProbPion()) * 
  		 (pair->track2()->Track()->PidProbPion()))<=mPiPiPairMaxProb &&
  		((pair->track1()->Track()->PidProbKaon()) * 
  		 (pair->track2()->Track()->PidProbKaon()))<=mKKPairMaxProb &&
  		((pair->track1()->Track()->PidProbPion()) * 
  		 (pair->track2()->Track()->PidProbKaon()))>=mPiKPairMinProb &&
  		((pair->track1()->Track()->PidProbPion()) * 
  		 (pair->track2()->Track()->PidProbProton()))>=mPiPPairMinProb &&
  		pair->getFracOfMergedRow()<mMaxFracPair
  		);
  temp ? mNPairsPassed++ : mNPairsFailed++;
  return temp;
}
//__________________
StHbtString fabricesPairCut::Report(){
  string Stemp = "Fabrices Pair Cut - total dummy-- always returns true\n";
  char Ctemp[100];
  sprintf(Ctemp,"Number of pairs which passed:\t%ld  Number which failed:\t%ld\n",mNPairsPassed,mNPairsFailed);
  Stemp += Ctemp;
  StHbtString returnThis = Stemp;
  return returnThis;
}
//__________________
ostrstream* fabricesPairCut::finalReport() const{
  ostrstream* tFinalReport = new ostrstream;
  (*tFinalReport) <<  "_____ Fabrices pair Cut _____ " << endl
		  << " N pairs passed : " << mNPairsPassed << endl 
		  << " N pairs failed : " << mNPairsFailed << endl 
		  << ends;
  return tFinalReport;
}

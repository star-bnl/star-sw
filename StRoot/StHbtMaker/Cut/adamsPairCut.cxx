/***************************************************************************
 *
 * $Id: adamsPairCut.cxx,v 1.2 2003/09/02 17:58:21 perev Exp $
 *
 * Author: Adam Kisiel, Warsaw University of Technology, kisiel@if.pw.edu.pl
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   Cuts on probability of a pair being e+e-, pi+pi- or K+K-
 *   using dEdx NSigma PID information
 *
 ***************************************************************************
 *
 *
 **************************************************************************/

#include "StHbtMaker/Cut/adamsPairCut.h"
#include <string>
#include <cstdio>
#include <Stsstream.h>

#ifdef __ROOT__
ClassImp(adamsPairCut)
#endif

//__________________
adamsPairCut::adamsPairCut():HitMergingPairCut(){
  mNPairsPassed = mNPairsFailed = 0;
  mElSigma=0.0;
  mPiSigma=0.0;
  mKSigma =0.0;
  mElPIDMax=1.0;
  mPiPIDMax=1.0;
  mKPIDMax =1.0;
  mPIDPThreshold= 0.0;
}
//__________________
//adamsPairCut::~adamsPairCut(){
//  /* no-op */
//}
//________`__________
bool adamsPairCut::Pass(const StHbtPair* pair){
  bool isElPair, isPiPair, isKPair;

  if (pair->track1()->Track()->P().mag() < mPIDPThreshold)
    {
      bool is1El = (TMath::Abs(pair->track1()->Track()->NSigmaElectron()) < mElSigma);
      bool is1Pi = (TMath::Abs(pair->track1()->Track()->NSigmaPion())     < mPiSigma);
      bool is1K  = (TMath::Abs(pair->track1()->Track()->NSigmaKaon())     < mKSigma);
      if (pair->track2()->Track()->P().mag() < mPIDPThreshold)
	{
	  isElPair = is1El && (TMath::Abs(pair->track2()->Track()->NSigmaElectron()) < mElSigma);
	  isPiPair = is1Pi && (TMath::Abs(pair->track2()->Track()->NSigmaPion())     < mPiSigma);
	  isKPair  = is1K  && (TMath::Abs(pair->track2()->Track()->NSigmaKaon())    < mKSigma);
	}
      else
	{
	  isElPair = is1El && (pair->track2()->Track()->PidProbElectron() <= mElPIDMax);
	  isPiPair = is1Pi && (pair->track2()->Track()->PidProbPion()     <= mPiPIDMax);
	  isKPair  = is1K  && (pair->track2()->Track()->PidProbKaon()     <= mKPIDMax);
	}
    }
  else
    {
      if (pair->track2()->Track()->P().mag() < mPIDPThreshold)
	{
	  bool is2El = (TMath::Abs(pair->track2()->Track()->NSigmaElectron()) < mElSigma);
	  bool is2Pi = (TMath::Abs(pair->track2()->Track()->NSigmaPion())     < mPiSigma);
	  bool is2K  = (TMath::Abs(pair->track2()->Track()->NSigmaKaon())     < mKSigma);
	  isElPair = is2El && (pair->track1()->Track()->PidProbElectron() <= mElPIDMax);
	  isPiPair = is2Pi && (pair->track1()->Track()->PidProbPion()     <= mPiPIDMax);
	  isKPair  = is2K  && (pair->track1()->Track()->PidProbKaon()     <= mKPIDMax);
	}
      else
	{
	  isElPair = (pair->track1()->Track()->PidProbElectron() * pair->track2()->Track()->PidProbElectron()) < mElPIDMax;
	  isPiPair = (pair->track1()->Track()->PidProbPion() * pair->track2()->Track()->PidProbPion()) < mPiPIDMax;
	  isKPair = (pair->track1()->Track()->PidProbKaon() * pair->track2()->Track()->PidProbKaon()) < mKPIDMax;
	}
    }
  bool temp = ( pair->track1()->TrackId()!=pair->track2()->TrackId() &&
		(!isElPair) &&
		(!isPiPair) &&
		(!isKPair)  &&
  		pair->getFracOfMergedRow()<mMaxFracPair
  		);
  temp ? mNPairsPassed++ : mNPairsFailed++;
  return temp;
}
//__________________
StHbtString adamsPairCut::Report(){
  string Stemp = "Adams Pair Cut - cut on e+e- pi+pi+ K+K- using NSigma\n";
  char Ctemp[100];
  sprintf(Ctemp,"Number of pairs which passed:\t%ld  Number which failed:\t%ld\n",mNPairsPassed,mNPairsFailed);
  Stemp += Ctemp;
  StHbtString returnThis = Stemp;
  return returnThis;
}
//__________________
ostrstream* adamsPairCut::finalReport() const{
  ostrstream* tFinalReport = new ostrstream;
  (*tFinalReport) <<  "_____ Adams pair Cut _____ " << endl
		  << " N pairs passed : " << mNPairsPassed << endl 
		  << " N pairs failed : " << mNPairsFailed << endl 
		  << ends;
  return tFinalReport;
}

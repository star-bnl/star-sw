/***************************************************************************
 *
 * $Id: HitMergingPairCut.cxx,v 1.2 2003/09/02 17:58:21 perev Exp $
 *
 * Author: Fabrice Retiere
 ***************************************************************************
 *
 * Description: Allow to cut on hit merging         
 * Usage :
 *  HitMergingPairCut* pairCut = new HitMergingPairCut();
 *  pairCut->setDefaultHalfFieldMergingPar();
 *  pairCut->setMaxFracOfMergedRow(MaxMergedHit);
 *
 ***************************************************************************
 *
 * $Log: HitMergingPairCut.cxx,v $
 * Revision 1.2  2003/09/02 17:58:21  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.1  2001/12/14 23:11:27  fretiere
 * Add class HitMergingCut. Add class fabricesPairCut = HitMerginCut + pair purity cuts. Add TpcLocalTransform function which convert to local tpc coord (not pretty). Modify StHbtTrack, StHbtParticle, StHbtHiddenInfo, StHbtPair to handle the hit information and cope with my code
 * 
 *
 **************************************************************************/

#include "StHbtMaker/Cut/HitMergingPairCut.h"
#include "Infrastructure/StHbtPair.hh"
#include <string>
#include <cstdio>
#include <Stsstream.h>

#ifdef __ROOT__
ClassImp(HitMergingPairCut)
#endif

//__________________
HitMergingPairCut::HitMergingPairCut(){
  mNPairsPassed = mNPairsFailed = 0;
  mMaxFracPair= 0.2;
  setDefaultHalfFieldMergingPar();
}
//__________________
//HitMergingPairCut::~HitMergingPairCut(){
//  /* no-op */
//}

void HitMergingPairCut::setMergingPar(double aMaxDuInner, double aMaxDzInner,
				      double aMaxDuOuter, double aMaxDzOuter){
  StHbtPair tPair;
  tPair.setMergingPar(aMaxDuInner, aMaxDzInner,
		      aMaxDuOuter, aMaxDzOuter);
}
void HitMergingPairCut::setDefaultFullFieldMergingPar(){
  StHbtPair tPair;
  tPair.setDefaultFullFieldMergingPar();
}
void HitMergingPairCut::setDefaultHalfFieldMergingPar(){
  StHbtPair tPair;
  tPair.setDefaultHalfFieldMergingPar();
}

//__________________
bool HitMergingPairCut::Pass(const StHbtPair* pair){
  bool temp = pair->getFracOfMergedRow()<mMaxFracPair;
  temp ? mNPairsPassed++ : mNPairsFailed++;
  return temp;
}
//__________________
StHbtString HitMergingPairCut::Report(){
  string Stemp = "HitMerging Pair Cut - total dummy-- always returns true\n";
  char Ctemp[100];
  sprintf(Ctemp,"Number of pairs which passed:\t%ld  Number which failed:\t%ld\n",mNPairsPassed,mNPairsFailed);
  Stemp += Ctemp;
  StHbtString returnThis = Stemp;
  return returnThis;
}
//__________________
ostrstream* HitMergingPairCut::finalReport() const{
  ostrstream* tFinalReport = new ostrstream;
  (*tFinalReport) <<  "_____ HitMerging pair Cut _____ " << endl
		  << " N pairs passed : " << mNPairsPassed << endl 
		  << " N pairs failed : " << mNPairsFailed << endl 
		  << ends;
  return tFinalReport;
}

/***************************************************************************
 *
 * $Id: EntranceSepPairCut.cxx,v 1.1 2000/07/31 01:19:24 lisa Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   cut on nominal Entrance Separation of the pair - for dealing with track merging
 *
 ***************************************************************************
 *
 * $Log: EntranceSepPairCut.cxx,v $
 * Revision 1.1  2000/07/31 01:19:24  lisa
 * add PairCut which contains collection of PairCuts - also 3D bertsch-pratt CorrFctn
 *
 **************************************************************************/

#include "StHbtMaker/Cut/EntranceSepPairCut.h"
#include <string>
#include <cstdio>

#ifdef __ROOT__
ClassImp(EntranceSepPairCut)
#endif

//__________________
EntranceSepPairCut::EntranceSepPairCut(){
  mNPairsPassed = mNPairsFailed = 0;
}
//__________________
//EntranceSepPairCut::~EntranceSepPairCut(){
//  /* no-op */
//}
//__________________
bool EntranceSepPairCut::Pass(const StHbtPair* pair){
  double sep = pair->NominalTpcEntranceSeparation();
  bool temp = ( (sep>mEntSepLo) &&
		(sep<mEntSepHi) );

  temp ? mNPairsPassed++ : mNPairsFailed++;
  return temp;
}
//__________________
StHbtString EntranceSepPairCut::Report(){
  string Stemp = "Entrance Separation Pair Cut\n";
  char Ctemp[100];
  sprintf(Ctemp,"Range of cut:\t%E ... \t%E\n",mEntSepLo,mEntSepHi);
  Stemp += Ctemp;
  sprintf(Ctemp,"Number of pairs which passed:\t%ld  Number which failed:\t%ld\n",mNPairsPassed,mNPairsFailed);
  Stemp += Ctemp;
  StHbtString returnThis = Stemp;
  return returnThis;
}
//__________________
void EntranceSepPairCut::SetEntranceSepRange(const double& Lo, const double& Hi) {
  mEntSepLo = Lo;
  mEntSepHi = Hi;
}
//__________________

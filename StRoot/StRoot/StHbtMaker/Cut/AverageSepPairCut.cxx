/***************************************************************************
 *
 * $Id: AverageSepPairCut.cxx,v 1.1 2000/10/05 23:09:02 lisa Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   cut on nominal average Separation of the pair - for dealing with track merging
 *
 ***************************************************************************
 *
 * $Log: AverageSepPairCut.cxx,v $
 * Revision 1.1  2000/10/05 23:09:02  lisa
 * Added kT-dependent radii to mixed-event simulator AND implemented AverageSeparation Cut and CorrFctn
 *
 *
 **************************************************************************/

#include "StHbtMaker/Cut/AverageSepPairCut.h"
#include <string>
#include <cstdio>

#ifdef __ROOT__
ClassImp(AverageSepPairCut)
#endif

//__________________
AverageSepPairCut::AverageSepPairCut(){
  mNPairsPassed = mNPairsFailed = 0;
}
//__________________
//AverageSepPairCut::~AverageSepPairCut(){
//  /* no-op */
//}
//__________________
bool AverageSepPairCut::Pass(const StHbtPair* pair){
  double sep = pair->NominalTpcAverageSeparation();
  bool temp = ( (sep>mAveSepLo) &&
		(sep<mAveSepHi) );

  temp ? mNPairsPassed++ : mNPairsFailed++;
  return temp;
}
//__________________
StHbtString AverageSepPairCut::Report(){
  string Stemp = "Average Separation Pair Cut\n";
  char Ctemp[100];
  sprintf(Ctemp,"Range of cut:\t%E ... \t%E\n",mAveSepLo,mAveSepHi);
  Stemp += Ctemp;
  sprintf(Ctemp,"Number of pairs which passed:\t%ld  Number which failed:\t%ld\n",mNPairsPassed,mNPairsFailed);
  Stemp += Ctemp;
  StHbtString returnThis = Stemp;
  return returnThis;
}
//__________________
void AverageSepPairCut::SetAveSepRange(const double& Lo, const double& Hi) {
  mAveSepLo = Lo;
  mAveSepHi = Hi;
}
//__________________

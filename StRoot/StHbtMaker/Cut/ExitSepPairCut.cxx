/***************************************************************************
 *
 * $Id: ExitSepPairCut.cxx,v 1.1 2000/09/14 18:36:58 lisa Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   cut on nominal Exit Separation of the pair - for dealing with track merging
 *
 ***************************************************************************
 *
 * $Log: ExitSepPairCut.cxx,v $
 * Revision 1.1  2000/09/14 18:36:58  lisa
 * Added Qinv and ExitSep pair cuts and BPLCMSFrame3DCorrFctn_SIM CorrFctn
 *
 *
 **************************************************************************/

#include "StHbtMaker/Cut/ExitSepPairCut.h"
#include <string>
#include <cstdio>

#ifdef __ROOT__
ClassImp(ExitSepPairCut)
#endif

//__________________
ExitSepPairCut::ExitSepPairCut(){
  mNPairsPassed = mNPairsFailed = 0;
}
//__________________
//ExitSepPairCut::~ExitSepPairCut(){
//  /* no-op */
//}
//__________________
bool ExitSepPairCut::Pass(const StHbtPair* pair){
  double sep = pair->NominalTpcExitSeparation();
  bool temp = ( (sep>mEntSepLo) &&
		(sep<mEntSepHi) );

  temp ? mNPairsPassed++ : mNPairsFailed++;
  return temp;
}
//__________________
StHbtString ExitSepPairCut::Report(){
  string Stemp = "Exit Separation Pair Cut\n";
  char Ctemp[100];
  sprintf(Ctemp,"Range of cut:\t%E ... \t%E\n",mEntSepLo,mEntSepHi);
  Stemp += Ctemp;
  sprintf(Ctemp,"Number of pairs which passed:\t%ld  Number which failed:\t%ld\n",mNPairsPassed,mNPairsFailed);
  Stemp += Ctemp;
  StHbtString returnThis = Stemp;
  return returnThis;
}
//__________________
void ExitSepPairCut::SetExitSepRange(const double& Lo, const double& Hi) {
  mEntSepLo = Lo;
  mEntSepHi = Hi;
}
//__________________

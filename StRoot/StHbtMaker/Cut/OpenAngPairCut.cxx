/***************************************************************************
 *
 * $Id: OpenAngPairCut.cxx,v 1.1 2000/10/26 16:09:11 lisa Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   cut on nominal Opening angle of the pair - for dealing with track merging
 *
 ***************************************************************************
 *
 * $Log: OpenAngPairCut.cxx,v $
 * Revision 1.1  2000/10/26 16:09:11  lisa
 * Added OpeningAngle PairCut class and method to StHbtPair
 *
 *
 **************************************************************************/

#include "StHbtMaker/Cut/OpenAngPairCut.h"
#include <string>
#include <cstdio>

#ifdef __ROOT__
ClassImp(OpenAngPairCut)
#endif

//__________________
OpenAngPairCut::OpenAngPairCut(){
  mNPairsPassed = mNPairsFailed = 0;
}
//__________________
//OpenAngPairCut::~OpenAngPairCut(){
//  /* no-op */
//}
//__________________
bool OpenAngPairCut::Pass(const StHbtPair* pair){
  double openAng = pair->OpeningAngle();
  bool temp = ( (openAng>mLo) &&
		(openAng<mHi) );

  temp ? mNPairsPassed++ : mNPairsFailed++;
  return temp;
}
//__________________
StHbtString OpenAngPairCut::Report(){
  string Stemp = "Opening Angle Pair Cut\n";
  char Ctemp[100];
  sprintf(Ctemp,"Range of cut:\t%E ... \t%E\n",mLo,mHi);
  Stemp += Ctemp;
  sprintf(Ctemp,"Number of pairs which passed:\t%ld  Number which failed:\t%ld\n",mNPairsPassed,mNPairsFailed);
  Stemp += Ctemp;
  StHbtString returnThis = Stemp;
  return returnThis;
}
//__________________
void OpenAngPairCut::SetOpenAngRange(const double& Lo, const double& Hi) {
  mLo = Lo;
  mHi = Hi;
}
//__________________

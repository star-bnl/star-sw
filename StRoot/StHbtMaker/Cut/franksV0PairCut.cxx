/***************************************************************************
 *
 * $Id: franksV0PairCut.cxx,v 1.1 2000/05/03 17:47:24 laue Exp $
 *
 * Author: Frank Laue, Ohio State, laue@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *
 ***************************************************************************
 *
 * $Log: franksV0PairCut.cxx,v $
 * Revision 1.1  2000/05/03 17:47:24  laue
 * new pair cut
 *
 *
 **************************************************************************/

#include "StHbtMaker/Cut/franksV0PairCut.h"
#include <string>
#include <cstdio>

#ifdef __ROOT__
ClassImp(franksV0PairCut)
#endif

//__________________
franksV0PairCut::franksV0PairCut() : mNPairsPassed(0), mNPairsFailed(0), mTrackIdCut(0) {
}
//__________________
//franksV0PairCut::~franksV0PairCut(){
//  /* no-op */
//}
//__________________
inline bool franksV0PairCut::Pass(const StHbtPair* pair){
  bool temp = true; 

  if ( mTrackIdCut == 1 ) {
    if ( (pair->track1()->NegTrackId() == pair->track2()->NegTrackId()) ||
	 (pair->track1()->PosTrackId() == pair->track2()->PosTrackId())
	 )
      temp = false;
  }
  

  temp ? mNPairsPassed++ : mNPairsFailed++;
  mNPairsPassed++;
  return temp;
}
//__________________
StHbtString franksV0PairCut::Report(){
  string Stemp = " Franks V0 Pair Cut \n";
  char Ctemp[100];
  sprintf(Ctemp,"mTrackIdCut = %d \n",mTrackIdCut);
  Stemp += Ctemp;
  sprintf(Ctemp,"Number of pairs which passed:\t%ld  Number which failed:\t%ld\n",mNPairsPassed,mNPairsFailed);
  Stemp += Ctemp;
  StHbtString returnThis = Stemp;
  return returnThis;
}
//__________________

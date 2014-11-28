/***************************************************************************
 *
 * $Id: ManyPairCuts.cxx,v 1.2 2003/02/02 21:43:58 magestro Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   ManyPairCuts is a StHbtPairCut that simply owns a collection
 *   of StHbtPairCut objects, and returns the AND of their return values
 *
 ***************************************************************************
 *
 * $Log: ManyPairCuts.cxx,v $
 * Revision 1.2  2003/02/02 21:43:58  magestro
 * Small change to remove compiler warning
 *
 * Revision 1.1  2000/07/31 01:19:24  lisa
 * add PairCut which contains collection of PairCuts - also 3D bertsch-pratt CorrFctn
 *
 *
 **************************************************************************/

#include "StHbtMaker/Cut/ManyPairCuts.h"
#include <string>
#include <cstdio>

#ifdef __ROOT__
ClassImp(ManyPairCuts)
#endif

//__________________
ManyPairCuts::ManyPairCuts(){
  mNPairsPassed = mNPairsFailed = 0;
}
//__________________
//ManyPairCuts::~ManyPairCuts(){
//  /* no-op */
//}
//__________________
bool ManyPairCuts::Pass(const StHbtPair* pair){
  // loop over all PairCuts in the collection... at the first failure, return "false"

  for (StHbtPairCutIterator iter=mPairCutCollection.begin();iter!=mPairCutCollection.end();iter++){
    if (!((*iter)->Pass(pair))){
      mNPairsFailed++;
      return false;
    }
  }
  // if you make it out of that loop, then you passed all cuts...
  mNPairsPassed++;
  return true;
}
//__________________
StHbtString ManyPairCuts::Report(){
  string Stemp = "ManyPairCuts Report\n";
  char Ctemp[100];
  sprintf(Ctemp,"Number of pairs which passed:\t%li  Number which failed:\t%li\n",mNPairsPassed,mNPairsFailed);
  Stemp += Ctemp;
  sprintf(Ctemp,"Here are the reports from the\t%i PairCuts in the collection\n",mPairCutCollection.size());
  Stemp += Ctemp;

  for (StHbtPairCutIterator iter=mPairCutCollection.begin();iter!=mPairCutCollection.end();iter++){
    Stemp += (*iter)->Report();
  }
  StHbtString returnThis = Stemp;
  return returnThis;
}
//__________________

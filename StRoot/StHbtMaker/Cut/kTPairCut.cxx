/***************************************************************************
 *
 * $Id: kTPairCut.cxx,v 1.2 2002/05/17 14:45:47 mercedes Exp $
 *
 * Author: Mercedes Lopez Noriega, OSU, mercedes@pacific.mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   cut on kT of the pair
 *
 ***************************************************************************
 *
 * $Log: kTPairCut.cxx,v $
 * Revision 1.2  2002/05/17 14:45:47  mercedes
 * Pair cut on kT, k is four-momentum of the pair
 *
 *
 **************************************************************************/

#include "StHbtMaker/Cut/kTPairCut.h"
#include <string>
#include <cstdio>

#ifdef __ROOT__
ClassImp(kTPairCut)
#endif

//__________________
kTPairCut::kTPairCut(){
  mNPairsPassed = mNPairsFailed = 0;
}
//__________________
//kTPairCut::~kTPairCut(){
//  /* no-op */
//}
//__________________
bool kTPairCut::Pass(const StHbtPair* pair){
  double kT = fabs(pair->kT());
  bool temp = ( (kT>mkTLo) &&
		(kT<mkTHi) );

  temp ? mNPairsPassed++ : mNPairsFailed++;
  return temp;
}
//__________________
StHbtString kTPairCut::Report(){
  string Stemp = "kT Pair Cut\n";
  char Ctemp[100];
  sprintf(Ctemp,"Range of cut:\t%E ... \t%E\n",mkTLo,mkTHi);
  Stemp += Ctemp;
  sprintf(Ctemp,"Number of pairs which passed:\t%ld  Number which failed:\t%ld\n",mNPairsPassed,mNPairsFailed);
  Stemp += Ctemp;
  StHbtString returnThis = Stemp;
  return returnThis;
}
//__________________
void kTPairCut::SetkTRange(const double& Lo, const double& Hi) {
  mkTLo = Lo;
  mkTHi = Hi;
}
//__________________

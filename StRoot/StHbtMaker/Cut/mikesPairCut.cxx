/***************************************************************************
 *
 * $Id: mikesPairCut.cxx,v 1.1.1.1 1999/06/29 16:02:56 lisa Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   a do-nothing pair cut that simply says "true" to every pair           
 *
 ***************************************************************************
 *
 * $Log: mikesPairCut.cxx,v $
 * Revision 1.1.1.1  1999/06/29 16:02:56  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#include "StHbtMaker/Cut/mikesPairCut.h"
#include <string>
#include <cstdio>

ClassImp(mikesPairCut)

//__________________
mikesPairCut::mikesPairCut(){
  mNPairsPassed = mNPairsFailed = 0;
}
//__________________
//mikesPairCut::~mikesPairCut(){
//  /* no-op */
//}
//__________________
bool mikesPairCut::Pass(const StHbtPair* pair){
  bool temp = true;
  temp ? mNPairsPassed++ : mNPairsFailed++;
  return true;
}
//__________________
string mikesPairCut::Report(){
  string Stemp = "Mikes Pair Cut - total dummy-- always returns true\n";
  char Ctemp[100];
  sprintf(Ctemp,"Number of pairs which passed:\t%ld  Number which failed:\t%ld\n",mNPairsPassed,mNPairsFailed);
  Stemp += Ctemp;
  return Stemp;
}
//__________________

/***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   a do-nothing triplet cut that simply says "true" to every triplet
 *
 ***************************************************************************/

#include "StHbtMaker/Cut/GenericTripletCut.h"
#include <string>
#include <cstdio>

ClassImp(GenericTripletCut)

//__________________
GenericTripletCut::GenericTripletCut(){
  mNTripletsPassed = mNTripletsFailed = 0;
}
//__________________
//GenericTripletCut::~GenericTripletCut(){
//  /* no-op */
//}
//__________________
bool GenericTripletCut::Pass(const StHbtTriplet* Triplet){
  bool temp = true;
  temp ? mNTripletsPassed++ : mNTripletsFailed++;
  return true;
}
//__________________
StHbtString GenericTripletCut::Report(){
  string Stemp = "Generic Triplet Cut - total dummy-- always returns true\n";
  char Ctemp[100];
  sprintf(Ctemp,"Number of Triplets which passed:\t%ld  Number which failed:\t%ld\n",mNTripletsPassed,mNTripletsFailed);
  Stemp += Ctemp;
  StHbtString returnThis = Stemp;
  return returnThis;
}
//__________________

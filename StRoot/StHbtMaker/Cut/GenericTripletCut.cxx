/***************************************************************************
 *
 * $Id: GenericTripletCut.cxx,v 1.3 2000/04/12 01:53:35 willson Exp $
 *
 * Author: Robert Willson, Ohio State, willson@bnl.gov
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package.
 *   A do-nothing triplet cut that simply says "true" to every triplet.
 *
 ***************************************************************************
 *
 * $Log: GenericTripletCut.cxx,v $
 * Revision 1.3  2000/04/12 01:53:35  willson
 * Initial Installation - Comments Added
 *
 * 
 ***************************************************************************/

#include "StHbtMaker/Cut/GenericTripletCut.h"
//#include <cstdio>

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
  StHbtString Stemp = "Generic Triplet Cut - total dummy-- always returns true\n";
  char Ctemp[100];
  sprintf(Ctemp,"Number of Triplets which passed:\t%ld  Number which failed:\t%ld\n",mNTripletsPassed,mNTripletsFailed);
  Stemp += Ctemp;
  StHbtString returnThis = Stemp;
  return returnThis;
}
//__________________

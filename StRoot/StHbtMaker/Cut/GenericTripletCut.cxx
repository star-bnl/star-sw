/***************************************************************************
 *
 * $Id: GenericTripletCut.cxx,v 1.6 2001/06/03 21:05:07 willson Exp $
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
 * Revision 1.6  2001/06/03 21:05:07  willson
 * Cuts on entrance separation
 *
 * Revision 1.5  2000/08/15 23:24:24  lisa
 * Fixed small typo in preprocessor directive ifdef
 *
 * Revision 1.4  2000/08/08 23:39:31  laue
 * Updated for standalone version
 *
 * Revision 1.3  2000/04/12 01:53:35  willson
 * Initial Installation - Comments Added
 *
 * 
 ***************************************************************************/

#include "StHbtMaker/Cut/GenericTripletCut.h"
//#include <cstdio>

#ifdef __ROOT__
ClassImp(GenericTripletCut)
#endif

//__________________
GenericTripletCut::GenericTripletCut(float EntSepCut){
  mEntSepCut = EntSepCut;
  mNTripletsPassed = mNTripletsFailed = 0;
}
//__________________
//GenericTripletCut::~GenericTripletCut(){
//  /* no-op */
//}
//__________________
bool GenericTripletCut::Pass(const StHbtTriplet* Triplet){
  bool temp = true;
  if (mEntSepCut) temp = (Triplet->NominalTpcEntranceSeparation()>mEntSepCut);
  temp ? mNTripletsPassed++ : mNTripletsFailed++;
  return temp;
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

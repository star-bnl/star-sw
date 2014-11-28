/***************************************************************************
 *
 * $Id: qualityPairCut.cxx,v 1.2 2000/07/31 01:19:24 lisa Exp $
 *
 * Author: Randy Wells, Ohio State, rcwells@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   Checks the Quality factor between two tracks ... if to high get thrown away
 *
 ***************************************************************************
 *
 * $Log: qualityPairCut.cxx,v $
 * Revision 1.2  2000/07/31 01:19:24  lisa
 * add PairCut which contains collection of PairCuts - also 3D bertsch-pratt CorrFctn
 *
 * Revision 1.1  2000/04/05 18:56:18  rcwells
 * Adding class qualityPairCut
 *
 * Revision 1.3  2000/01/25 17:35:02  laue
 * I. In order to run the stand alone version of the StHbtMaker the following
 * changes have been done:
 * a) all ClassDefs and ClassImps have been put into #ifdef __ROOT__ statements
 * b) unnecessary includes of StMaker.h have been removed
 * c) the subdirectory StHbtMaker/doc/Make has been created including everything
 * needed for the stand alone version
 *
 * II. To reduce the amount of compiler warning
 * a) some variables have been type casted
 * b) some destructors have been declared as virtual
 *
 * Revision 1.2  1999/07/06 22:33:21  lisa
 * Adjusted all to work in pro and new - dev itself is broken
 *
 * Revision 1.1.1.1  1999/06/29 16:02:56  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#include "StHbtMaker/Cut/qualityPairCut.h"
#include <string>
#include <cstdio>

#ifdef __ROOT__
ClassImp(qualityPairCut)
#endif

//__________________
qualityPairCut::qualityPairCut(){
  mNPairsPassed = mNPairsFailed = 0;
}
//__________________
//qualityPairCut::~qualityPairCut(){
//  /* no-op */
//}
//__________________
bool qualityPairCut::Pass(const StHbtPair* pair){
  double qual = pair->quality();
  bool temp = ( (qual>mQualCutLo) &&
		(qual<mQualCutHi) );

  temp ? mNPairsPassed++ : mNPairsFailed++;
  return temp;
}
//__________________
StHbtString qualityPairCut::Report(){
  string Stemp = "Quality Pair Cut\n";
  char Ctemp[100];
  sprintf(Ctemp,"Range of cut:\t%E ... \t%E\n",mQualCutLo,mQualCutHi);
  Stemp += Ctemp;
  sprintf(Ctemp,"Number of pairs which passed:\t%ld  Number which failed:\t%ld\n",mNPairsPassed,mNPairsFailed);
  Stemp += Ctemp;
  StHbtString returnThis = Stemp;
  return returnThis;
}
//__________________
void qualityPairCut::SetQualityCut(const double& QualCutLo, const double& QualCutHi) {
  mQualCutLo = QualCutLo;
  mQualCutHi = QualCutHi;
}
//__________________

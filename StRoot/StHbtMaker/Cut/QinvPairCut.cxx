/***************************************************************************
 *
 * $Id: QinvPairCut.cxx,v 1.1 2000/09/14 18:36:59 lisa Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   cut on Qinvariant
 *
 ***************************************************************************
 *
 * $Log: QinvPairCut.cxx,v $
 * Revision 1.1  2000/09/14 18:36:59  lisa
 * Added Qinv and ExitSep pair cuts and BPLCMSFrame3DCorrFctn_SIM CorrFctn
 *
 *
 **************************************************************************/

#include "StHbtMaker/Cut/QinvPairCut.h"
#include <string>
#include <cstdio>

#ifdef __ROOT__
ClassImp(QinvPairCut)
#endif

//__________________
QinvPairCut::QinvPairCut(){
  mNPairsPassed = mNPairsFailed = 0;
}
//__________________
//QinvPairCut::~QinvPairCut(){
//  /* no-op */
//}
//__________________
bool QinvPairCut::Pass(const StHbtPair* pair){
  double Qinv = fabs(pair->qInv());
  bool temp = ( (Qinv>mQinvLo) &&
		(Qinv<mQinvHi) );

  temp ? mNPairsPassed++ : mNPairsFailed++;
  return temp;
}
//__________________
StHbtString QinvPairCut::Report(){
  string Stemp = "Qinvariant Pair Cut\n";
  char Ctemp[100];
  sprintf(Ctemp,"Range of cut:\t%E ... \t%E\n",mQinvLo,mQinvHi);
  Stemp += Ctemp;
  sprintf(Ctemp,"Number of pairs which passed:\t%ld  Number which failed:\t%ld\n",mNPairsPassed,mNPairsFailed);
  Stemp += Ctemp;
  StHbtString returnThis = Stemp;
  return returnThis;
}
//__________________
void QinvPairCut::SetQinvRange(const double& Lo, const double& Hi) {
  mQinvLo = Lo;
  mQinvHi = Hi;
}
//__________________

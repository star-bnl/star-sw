/***************************************************************************
 *
 * $Id: anglePairCut.cxx,v 1.1 2001/11/05 16:14:16 rcwells Exp $
 *
 * Author: Randall Wells, Ohio State, rcwells@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   cut on the angle of the pair relative to the reaction plane 
 *
 ***************************************************************************
 *
 * $Log: anglePairCut.cxx,v $
 * Revision 1.1  2001/11/05 16:14:16  rcwells
 * Adding anglePairCut class to cut on pair emission angle
 *
 *
 **************************************************************************/

#include "StHbtMaker/Cut/anglePairCut.h"
#include <string>
#include <cstdio>
#include "StHbtMaker/Infrastructure/StHbtReactionPlaneAnalysis.h"

#ifdef __ROOT__
ClassImp(anglePairCut)
#endif

//__________________
anglePairCut::anglePairCut(){
  mNPairsPassed = mNPairsFailed = 0;
  mBetaT = 0;
  mBetaL = 0;
  mBetaT2 = 0;
  mBetaL2 = 0;
  mBetaTL = 0;
}
//__________________
anglePairCut::anglePairCut(char* title){
  mNPairsPassed = mNPairsFailed = 0;
  char TitT[100] = "betaT";
  strcat(TitT,title);
  mBetaT = new StHbt1DHisto(TitT,"Transverse Velocity",100,0,1);
  char TitL[100] = "betaL";
  strcat(TitL,title);
  mBetaL = new StHbt1DHisto(TitL,"Longitudinal Velocity",100,0,1);
  char TitT2[100] = "betaT2";
  strcat(TitT2,title);
  mBetaT2 = new StHbt1DHisto(TitT2,"Transverse Velocity Squared",100,0,1);
  char TitL2[100] = "betaL2";
  strcat(TitL2,title);
  mBetaL2 = new StHbt1DHisto(TitL2,"Longitudinal Velocity Squared",100,0,1);
  char TitTL[100] = "betaTL";
  strcat(TitTL,title);
  mBetaTL = new StHbt1DHisto(TitTL,"Transverse*Longitudinal Velocity",100,0,1);
}
//__________________
//anglePairCut::~anglePairCut(){
//  /* no-op */
//}
//__________________
bool anglePairCut::Pass(const StHbtPair* pair){
  if ( (mAngle1[0]<0.0)&&(mAngle2[0]<0.0) ) return true; // If both angles are <0.0 always return true
  bool temp = false;
  bool temp1 = false;
  bool temp2 = false;
  double pxTotal = pair->fourMomentumSum().x();
  double pyTotal = pair->fourMomentumSum().y();
  double angle = atan2(pyTotal,pxTotal)*180.0/pi;
  //cout << "Angle: " << pxTotal << " " << pyTotal << " " << angle << endl;
  if (angle<0.0) angle+=360.0;
  //cout << "Angle:(2) " << angle << endl;
  StHbtReactionPlaneAnalysis* RPanal = (StHbtReactionPlaneAnalysis*) myAnalysis;
  double RPangle = RPanal->ReactionPlane()*180.0/pi;
  double angleDifference = angle-RPangle;
  //cout << "Angle:(3) " << RPangle << " " << angleDifference << endl;
  if (angleDifference<0.0) angleDifference+=360.0;
  //cout << "Angle:(4) " << angleDifference << endl;
  // Test without 2nd sector
  if ( (mAngle2[0]>360.0) && (mAngle2[1]>360.0) ) {
    if (mAngle1[0]<mAngle1[1]) {
      temp1 = ( (mAngle1[0]<angleDifference) &&
	       (angleDifference<mAngle1[1]) );
    }
    else {
      temp1 = ( (mAngle1[0]<angleDifference) ||
	       (angleDifference<mAngle1[1]) );
    }
  }
  // End test without 2nd sector
  // Test with 2nd sector
  if ( (mAngle2[0]<360.0) || (mAngle2[1]<360.0) ) {
    if (mAngle1[0]<mAngle1[1]) {
      temp1 = ( (mAngle1[0]<angleDifference) &&
	       (angleDifference<mAngle1[1]) );
      if (mAngle2[0]<mAngle2[1]) {
	temp2 = ( (mAngle2[0]<angleDifference) &&
		  (angleDifference<mAngle2[1]) );
      }
      else {
	temp2 = ( (mAngle2[0]<angleDifference) ||
		  (angleDifference<mAngle2[1]) );
      }
    }
    else {
      temp1 = ( (mAngle1[0]<angleDifference) ||
	       (angleDifference<mAngle1[1]) );
      if (mAngle2[0]<mAngle2[1]) {
	temp2 = ( (mAngle2[0]<angleDifference) &&
		 (angleDifference<mAngle2[1]) );
      }
      else {
	temp2 = ( (mAngle2[0]<angleDifference) ||
		 (angleDifference<mAngle2[1]) );
      }
    }
  }
  // End test with 2nd sector
  temp = temp1 || temp2;
  if ( temp && mBetaT  ) { // if passed cut and histos exist
    double pT = pair->fourMomentumSum().perp();
    double pZ = pair->fourMomentumSum().pz();
    double e = pair->fourMomentumSum().e();
    double e2 = e*e;
    mBetaT->Fill(pT/e);
    mBetaL->Fill(pZ/e);
    mBetaT2->Fill( pT*pT/(e2) );
    mBetaL2->Fill( pZ*pZ/(e2) );
    mBetaTL->Fill( pT*pZ/(e2) );
  }
  temp ? mNPairsPassed++ : mNPairsFailed++;
  return temp;
}
//__________________
StHbtString anglePairCut::Report(){
  string Stemp = "Angle Pair Cut - total dummy-- always returns true\n";
  char Ctemp[100];
  sprintf(Ctemp,"Number of pairs which passed:\t%ld  Number which failed:\t%ld\n",mNPairsPassed,mNPairsFailed);
  Stemp += Ctemp;
  StHbtString returnThis = Stemp;
  return returnThis;
}
//__________________
double anglePairCut::EmissionAngle(const StHbtPair *pair) {
  double pxTotal = pair->fourMomentumSum().x();
  double pyTotal = pair->fourMomentumSum().y();
  double angle = atan2(pyTotal,pxTotal)*180.0/pi;
  if (angle<0.0) angle+=360.0;
  StHbtReactionPlaneAnalysis* RPanal = (StHbtReactionPlaneAnalysis*) myAnalysis;
  double RPangle = RPanal->ReactionPlane()*180.0/pi;
  double angleDifference = angle-RPangle;
  if (angleDifference<0.0) angleDifference+=360.0;
  return angleDifference;
}











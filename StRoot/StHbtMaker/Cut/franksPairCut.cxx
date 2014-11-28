//#include "StHbtMaker/Infrastructure/StHbtPairResonanceInfo.hh"
#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
#include "StHbtMaker/Cut/franksPairCut.h"
#include "SystemOfUnits.h"
#include <string>
#include <cstdio>

#ifdef __ROOT__
ClassImp(franksPairCut)
#endif

#define __2POWER16__ 65536

//__________________
  franksPairCut::franksPairCut() /* : mResonanceInfoOn(false) */ {
  mQuality[0] = -1.; mQuality[1] = +1.;
  mKt[0] = -1e9; mKt[1]= +1e9;
  mPt[0] = -1e9; mPt[1]= +1e9;
  mOpeningAngle[0] = -1e9; mOpeningAngle[1]= +1e9;
  mRapidity[0] = -1e9; mRapidity[1]= +1e9;
  mEta[0] = -1e9; mEta[1]= +1e9;
  mQinv[0] = -1e9; mQinv[1]= +1e9;
  mEntranceSeparation[0] = -1e9; mEntranceSeparation[1]= +1e9;
  mDecayLength[0] = -1e9; mDecayLength[1]= 1e9;
  mAngleToPrimaryVertex[0] = -1e9; mAngleToPrimaryVertex[1]= 1e9;
  mDcaToPrimaryVertex[0] = -1e9; mDcaToPrimaryVertex[1]= 1e9;
  mDcaOfDaughters[0] = -1e9; mDcaOfDaughters[1]= 1e9;
  mNPairsPassed = mNPairsFailed = 0;
  mIdenticalMother =0;
}
//__________________
franksPairCut::franksPairCut(const franksPairCut& c) : StHbtPairCut(c) /* :  ,  mResonanceInfoOn(false) */ {
#ifdef STHBTDEBUG
  cout << " franksPairCut::franksPairCut(const franksPairCut& c) " << endl;
#endif
  mNPairsPassed = mNPairsFailed = 0;
  mPrimaryVertex = c.mPrimaryVertex;
  mQuality[0] = c.mQuality[0];
  mQuality[1] = c.mQuality[1];
  mKt[0] = c.mKt[0];
  mKt[1] = c.mKt[1];
  mPt[0] = c.mPt[0];
  mPt[1] = c.mPt[1];
  mOpeningAngle[0] = c.mOpeningAngle[0];
  mOpeningAngle[1] = c.mOpeningAngle[1];
  mQinv[0] = c.mQinv[0];
  mQinv[1] = c.mQinv[1];
  mRapidity[0] = c.mRapidity[0];
  mRapidity[1] = c.mRapidity[1];
  mEta[0] = c.mEta[0];
  mEta[1] = c.mEta[1];
  mEntranceSeparation[0] = c.mEntranceSeparation[0];
  mEntranceSeparation[1] = c.mEntranceSeparation[1];
  mDecayLength[0] = c.mDecayLength[0];
  mDecayLength[1] = c.mDecayLength[1];
  mAngleToPrimaryVertex[0] = c.mAngleToPrimaryVertex[0];
  mAngleToPrimaryVertex[1] = c.mAngleToPrimaryVertex[1];
  mDcaToPrimaryVertex[0] = c.mDcaToPrimaryVertex[0];
  mDcaToPrimaryVertex[1] = c.mDcaToPrimaryVertex[1];
  mDcaOfDaughters[0] = c.mDcaOfDaughters[0];
  mDcaOfDaughters[1] = c.mDcaOfDaughters[1];
  mIdenticalMother = c.mIdenticalMother;
}
//__________________
//franksPairCut::~franksPairCut(){
//  /* no-op */
//}  
//__________________
bool franksPairCut::Pass(const StHbtPair* pair){
  if ( !(pair->quality() >= mQuality[0]  && pair->quality() <= mQuality[1] ) ) 
    return leave(false);
  if ( !(pair->kT() >= mKt[0]  && pair->kT() <= mKt[1] ) ) 
    return leave(false);
  if ( !(pair->fourMomentumSum().perp() >= mPt[0]  && pair->fourMomentumSum().perp() <= mPt[1] ) ) 
    return leave(false);
  if ( !(pair->OpeningAngle() >= mOpeningAngle[0]  && pair->OpeningAngle() <= mOpeningAngle[1] )  ) 
    return leave(false);
  if ( !(pair->fourMomentumSum().rapidity() >= mRapidity[0]  && pair->fourMomentumSum().rapidity() <= mRapidity[1]) ) 
    return leave(false);
  if ( !(pair->fourMomentumSum().pseudoRapidity() >= mEta[0]  && pair->fourMomentumSum().pseudoRapidity() <= mEta[1] )  ) 
    return leave(false);
  if ( !(fabs(pair->qInv()) >= mQinv[0]  && fabs(pair->qInv()) <= mQinv[1])  ) 
    return leave(false);
  if ( !(pair->NominalTpcEntranceSeparation() >= mEntranceSeparation[0]  && pair->NominalTpcEntranceSeparation() <= mEntranceSeparation[1] )
       ) return leave(false);
  
  /*if (mResonanceInfoOn) {
    //cout << " fix this " << endl; 
    pair->CalculateResonanceInfo(&mPrimaryVertex, 0.25*tesla);
    //cout << " mDecayLength "  << pair->ResonanceInfo()->mDecayLength << endl;
    if ( !(pair->ResonanceInfo()->mDecayLength >= mDecayLength[0]  && 
	   pair->ResonanceInfo()->mDecayLength <= mDecayLength[1]) ) 
      return leave(false);
    //cout << " mDcaOfDaughters "  << pair->ResonanceInfo()->mDcaOfDaughters << endl;
    if ( !(pair->ResonanceInfo()->mDcaOfDaughters >= mDcaOfDaughters[0]  && 
	   pair->ResonanceInfo()->mDcaOfDaughters <= mDcaOfDaughters[1]) ) 
      return leave(false);
    
    //cout << "mAngleToPrimaryVertex " << pair->ResonanceInfo()->mAngleToPrimaryVertex << endl;
    if ( !(pair->ResonanceInfo()->mAngleToPrimaryVertex >= mAngleToPrimaryVertex[0]  && 
	   pair->ResonanceInfo()->mAngleToPrimaryVertex <= mAngleToPrimaryVertex[1]) ) 
      return leave(false);
    
    //cout << "mDcaToPrimaryVertex " << pair->ResonanceInfo()->mDcaToPrimaryVertex << endl;
    if ( !(pair->ResonanceInfo()->mDcaToPrimaryVertex >= mDcaToPrimaryVertex[0]  && 
	   pair->ResonanceInfo()->mDcaToPrimaryVertex <= mDcaToPrimaryVertex[1]) ) 
      return leave(false);
    
	   }*/

  if (mIdenticalMother)
    if(  !((int)(pair->track1()->TrackId()/__2POWER16__) != (int)(pair->track2()->TrackId()/__2POWER16__) ) ) 
      return leave(false);
  
  return leave(true);
}

//__________________
bool franksPairCut::leave(bool b){
   b ? mNPairsPassed++ : mNPairsFailed++;
   return b;
}

//__________________
StHbtString franksPairCut::Report(){
  string Stemp = "Franks Pair Cut have to finish cut and report; this is just a test - \n";
  char Ctemp[100];
  sprintf(Ctemp,"Number of pairs which passed:\t%ld  Number which failed:\t%ld\n",mNPairsPassed,mNPairsFailed);
  Stemp += Ctemp;
  sprintf(Ctemp,"quality: %f  -- %f\n",mQuality[0],mQuality[1]);
  Stemp += Ctemp;
  sprintf(Ctemp,"kT: %f  -- %f\n",mKt[0],mKt[1]);
  Stemp += Ctemp;
  sprintf(Ctemp,"pT: %f  -- %f\n",mPt[0],mPt[1]);
  Stemp += Ctemp;
  sprintf(Ctemp,"opening angle: %f  -- %f\n",mOpeningAngle[0],mOpeningAngle[1]);
  Stemp += Ctemp;
  sprintf(Ctemp,"pair qinv: %f  -- %f\n",mQinv[0],mQinv[1]);
  Stemp += Ctemp;
  sprintf(Ctemp,"pair rapidity: %f  -- %f\n",mRapidity[0],mRapidity[1]);
  Stemp += Ctemp;
  sprintf(Ctemp,"pair eta: %f  -- %f\n",mEta[0],mEta[1]);
  Stemp += Ctemp;
  sprintf(Ctemp,"EntranceSeparation: %f  -- %f\n",mEntranceSeparation[0],mEntranceSeparation[1]);
  Stemp += Ctemp;
  sprintf(Ctemp,"DecayLength: %f  -- %f\n",mDecayLength[0],mDecayLength[1]);
  Stemp += Ctemp;
  sprintf(Ctemp,"AngleToPrimaryVertex: %f  -- %f\n",mAngleToPrimaryVertex[0],mAngleToPrimaryVertex[1]);
  Stemp += Ctemp;
  sprintf(Ctemp,"DcaToPrimaryVertex: %f  -- %f\n",mDcaToPrimaryVertex[0],mDcaToPrimaryVertex[1]);
  Stemp += Ctemp;
  StHbtString returnThis = Stemp;
  return returnThis;
}
 

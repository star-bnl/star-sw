#include "StHbtMaker/Cut/franksXiCut.h"
#include <cstdio>

#ifdef __ROOT__ 
ClassImp(franksXiCut)
#endif


franksXiCut::franksXiCut(){
  mNPassed = mNFailed = 0;
  
  mV0MassRange[0] =0;
  mV0MassRange[1]=10000;
  
  mdcaV0daughters[0]=0; 
  mdcaV0daughters[1]=1000;
  
  mdcaV0ToPrimVertex[0]=0;
  mdcaV0ToPrimVertex[1]=1000;

  mdecayLengthV0[0]=0;
  mdecayLengthV0[1]=10000;
  
  mtpcHitsPos[0]=0;
  mtpcHitsPos[1]=1000;
  
  mtpcHitsNeg[0]=0;
  mtpcHitsNeg[1]=1000;
  
  
  mdcaPosToPrimVertex[0]=0;
  mdcaPosToPrimVertex[1]=1000;
  
  mdcaNegToPrimVertex[0]=0;
  mdcaNegToPrimVertex[1]=10000;
  
  mptArmV0[0]=0;
  mptArmV0[1]=100;
  
  malphaV0[0]=-10;
  malphaV0[1]=10;

  mChargedEdx=0;
  mdEdx[0]=0;
  mdEdx[1]=-10;
  mdEdx[0]=0;
  mdEdx[1]=-10;

  mPt[0]=0; 
  mPt[1]=100000;

  mRapidity[0]=-100000;
  mRapidity[1]=100000;

  V0Type = "K0Short";
  mMass  = 0.498;


}
//------------------------------
//franksXiCut::~franksXiCut(){
//  /* noop */
//}
//------------------------------
bool franksXiCut::Pass(const StHbtXi* XI){
  int inMassRange;

#ifdef STHBTDEBUG  
  cout << endl;
  cout << " * dcaV0Daughters " << XI->dcaV0Daughters();
  cout << " * dcaV0ToPrimVertex " << XI->dcaV0ToPrimVertex();
  cout << " * decayLengthV0 " << XI->decayLengthV0();
  cout << " * tpcHitsPos " << XI->tpcHitsPos();
  cout << " * tpcHitsNeg " << XI->tpcHitsNeg();
  cout << " * dcaPosToPrimVertex " << XI->dcaPosToPrimVertex();
  cout << " * dcaNegToPrimVertex " << XI->dcaNegToPrimVertex();
  cout << " * ptArmV0 " << XI->ptArmV0();
  cout << " * alphaV0 " << XI->alphaV0();
  cout << " * dEdxPos " << XI->dedxPos();
  cout << " * dEdxNeg " << XI->dedxNeg();
  cout << endl;
#endif


  inMassRange=0;
  // Find out what particle is desired

  if( strstr(V0Type,"k") || strstr(V0Type,"K")){
     if( XI->massK0Short() < (mV0MassRange[1]) && 
	 XI->massK0Short() > (mV0MassRange[0]) ) inMassRange=1;
  }
  else if( (strstr(V0Type,"anti") || strstr(V0Type,"ANTI"))){
     if( XI->massAntiLambda() < (mV0MassRange[1]) && 
	 XI->massAntiLambda() > (mV0MassRange[0]) ) inMassRange=1;
  }
  else if( (strstr(V0Type,"ambda") || strstr(V0Type,"AMBDA"))){
     if( XI->massLambda() < (mV0MassRange[1]) && 
	 XI->massLambda() > (mV0MassRange[0]) ) inMassRange=1;
  }



  bool goodPID = ( inMassRange &&
		  (XI->dcaV0Daughters()   > mdcaV0daughters[0]) &&
                  (XI->dcaV0Daughters()   < mdcaV0daughters[1]) &&
                  (XI->dcaV0ToPrimVertex()   > mdcaV0ToPrimVertex[0]) &&
                  (XI->dcaV0ToPrimVertex()   < mdcaV0ToPrimVertex[1]) &&
                  (XI->decayLengthV0() > mdecayLengthV0[0]) &&
                  (XI->decayLengthV0() < mdecayLengthV0[1]) &&
                  (XI->tpcHitsPos()   > mtpcHitsPos[0]) &&
                  (XI->tpcHitsPos()   < mtpcHitsPos[1]) &&
                  (XI->tpcHitsNeg()   > mtpcHitsNeg[0]) &&
                  (XI->tpcHitsNeg()   < mtpcHitsNeg[1]) &&
                  (XI->dcaPosToPrimVertex()   > mdcaPosToPrimVertex[0]) &&
                  (XI->dcaPosToPrimVertex()   < mdcaPosToPrimVertex[1]) &&
                  (XI->dcaNegToPrimVertex()   > mdcaNegToPrimVertex[0]) &&
                  (XI->dcaNegToPrimVertex()   < mdcaNegToPrimVertex[1]) && 
                  (XI->ptArmV0()   > mptArmV0[0]) &&
                  (XI->ptArmV0()   < mptArmV0[1]) &&
                  (XI->alphaV0()   > malphaV0[0]) &&
                  (XI->alphaV0()   < malphaV0[1]));

  if(goodPID){
    if( mChargedEdx <0){
      goodPID = ( (XI->dedxNeg() > (mdEdx[0]*XI->ptNeg()+mdEdx[1])) &&
		  (XI->dedxNeg() > (mdEdx[2]*XI->ptNeg()+mdEdx[3])));
	}
    if( mChargedEdx > 0){
      goodPID = ( (XI->dedxPos() > (mdEdx[0]*XI->ptPos()+mdEdx[1])) &&
		  (XI->dedxPos() > (mdEdx[2]*XI->ptPos()+mdEdx[3])));
    }
  }

  if (goodPID){
    float TEnergy = sqrt((XI->ptotV0())*(XI->ptotV0())+mMass*mMass);
    float TRapidity = 0.5*log((TEnergy+XI->momV0().z())/
			    (TEnergy-XI->momV0().z()));

    float Pt = XI->ptV0();


    
#ifdef STHBTDEBUG  
    cout << " * Pt " << Pt;
    cout << " * mPt[0] " << mPt[0];
    cout << " * mPt[1] " << mPt[1];
    cout << " * TRapidity " << TRapidity;
    cout << " * mRapidity[0] " << mRapidity[0];
    cout << " * mRapidity[1] " << mRapidity[1];
    cout << " * Pt " << (Pt > mPt[0]) && (Pt < mPt[1]);
    cout << " * y " << (TRapidity > mRapidity[0]) && (TRapidity < mRapidity[1]);
    cout << endl;
#endif

    bool goodXI=
      ((Pt             > mPt[0]) &&
       (Pt             < mPt[1]) &&
       (TRapidity      > mRapidity[0]) &&
       (TRapidity      < mRapidity[1]));

    goodXI ? mNPassed++ : mNFailed++;
    return (goodXI);
  }
  else{
    mNFailed++;
    return (goodPID);
  }
}
//------------------------------
StHbtString franksXiCut::Report(){
  string Stemp;
  char Ctemp[100];
  sprintf(Ctemp,"--franksXiCut--\n Particle mass:\t%E\n",this->Mass());
  Stemp=Ctemp;
  sprintf(Ctemp,"V0 mass range:\t%E - %E\n",mV0MassRange[0],
	  mV0MassRange[1]);
  Stemp+=Ctemp;
  sprintf(Ctemp,"dcaV0daughters:\t%E - %E\n",mdcaV0daughters[0],
mdcaV0daughters[1]);
  Stemp+=Ctemp;
  sprintf(Ctemp,"dcaV0ToPrimVertex:\t%E - %E\n",mdcaV0ToPrimVertex[0],
mdcaV0ToPrimVertex[1]);
  Stemp+=Ctemp;
  sprintf(Ctemp,"decayLengthV0:\t%E - %E\n",mdecayLengthV0[0],
mdecayLengthV0[1]);
  Stemp+=Ctemp;
  sprintf(Ctemp,"tpcHitsPos:\t%d - %d\n",mtpcHitsPos[0],mtpcHitsPos[1]);
  Stemp+=Ctemp;
  sprintf(Ctemp,"tpcHitsNeg:\t%d - %d\n",mtpcHitsNeg[0],mtpcHitsNeg[1]);
  Stemp+=Ctemp;
  sprintf(Ctemp,"dcaPosToPrimVertex:\t%E - %E\n",mdcaPosToPrimVertex[0],
mdcaPosToPrimVertex[1]);
  Stemp+=Ctemp;
  sprintf(Ctemp,"dcaNegToPrimVertex:\t%E - %E\n",mdcaNegToPrimVertex[0],
mdcaNegToPrimVertex[1]);
  Stemp+=Ctemp;
  sprintf(Ctemp,"dedx>:\t%E pt+%E and \t%E pt+ %E for Charge %E\n ",mdEdx[0],mdEdx[1],mdEdx[2],mdEdx[3],mChargedEdx);
  Stemp+=Ctemp;
  sprintf(Ctemp,"ptArmV0:\t%E - %E\n",mptArmV0[0],mptArmV0[1]);
  Stemp+=Ctemp;
  sprintf(Ctemp,"alphaV0:\t%E - %E\n",malphaV0[0],malphaV0[1]);
  Stemp+=Ctemp;
  sprintf(Ctemp,"Particle pT:\t%E - %E\n",mPt[0],mPt[1]);
  Stemp+=Ctemp;
  sprintf(Ctemp,"Particle rapidity:\t%E - %E\n",mRapidity[0],mRapidity[1]);
  Stemp+=Ctemp;
  sprintf(Ctemp,"Number of V0s which passed:\t%ld  Number which failed:\t%ld\n",mNPassed,mNFailed);
  Stemp += Ctemp;
  StHbtString returnThis = Stemp;
  return returnThis;
}


/***************************************************************************
 *
 * $Id: franksXiCut.cxx,v 1.1 2001/09/05 20:41:25 laue Exp $
 *
 * Authors: Franki Laue, BNL, laue@bnl.gov
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *
 ***************************************************************************
 *
 * $Log: franksXiCut.cxx,v $
 * Revision 1.1  2001/09/05 20:41:25  laue
 * Updates of the hbtMuDstTree microDSTs
 *
 *
 **************************************************************************/

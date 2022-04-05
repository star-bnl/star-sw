#include "StHbtMaker/Cut/franksXiCut.h"
#include <cstdio>

#ifdef __ROOT__ 
ClassImp(franksXiCut)
#endif


franksXiCut::franksXiCut(){
  mNXisPassed = mNXisFailed = 0;
  
  mXiMassRange[0] =0;
  mXiMassRange[1]=10000;

  mOmegaMassRange[0] =0;
  mOmegaMassRange[1]=10000;
  
  mdcaXidaughters[0]=0; 
  mdcaXidaughters[1]=1000;
  
  mdcaXiToPrimVertex[0]=0;
  mdcaXiToPrimVertex[1]=1000;

  mdecayLengthXi[0]=0;
  mdecayLengthXi[1]=10000;
  
  mtpcHitsBac[0]=0;
  mtpcHitsBac[1]=1000;
  
  mdcaBacToPrimVertex[0]=0;
  mdcaBacToPrimVertex[1]=10000;
  
  mptArmXi[0]=0;
  mptArmXi[1]=100;
  
  malphaXi[0]=-10;
  malphaXi[1]=10;

  mChargedEdx=0;
  mdEdx[0]=0;
  mdEdx[1]=-10;
  mdEdx[0]=0;
  mdEdx[1]=-10;

  mPt[0]=0; 
  mPt[1]=100000;

  mRapidity[0]=-100000;
  mRapidity[1]=100000;

  mMass  = 1.32131;


}
//------------------------------
//franksXiCut::~franksXiCut(){
//  /* noop */
//}
//------------------------------
bool franksXiCut::Pass(const StHbtXi* Xi){
  int inMassRange;

#ifdef STHBTDEBUG  
  cout << endl;
  cout << " * dcaXiDaughters " << Xi->dcaXiDaughters();
  cout << " * dcaXiToPrimVertex " << Xi->dcaXiToPrimVertex();
  cout << " * decayLengthXi " << Xi->decayLengthXi();
  cout << " * tpcHitsBac " << Xi->tpcHitsBac();
  cout << " * dcaNegToPrimVertex " << Xi->dcaBacToPrimVertex();
  cout << " * ptArmXi " << Xi->ptArmXi();
  cout << " * alphaXi " << Xi->alphaXi();
  cout << " * dEdxBac " << Xi->dedxBac();
  cout << endl;
#endif


  inMassRange=0;
  // Find out what particle is desired

  if ( (Xi->massXi() > mXiMassRange[0]) && (Xi->massXi() < mXiMassRange[1]) ) inMassRange=1;
  if ( (Xi->massOmega() > mOmegaMassRange[0]) && (Xi->massOmega() < mOmegaMassRange[1]) ) inMassRange=1;
  

  bool goodPID = ( inMassRange &&
		  (Xi->dcaXiDaughters()   > mdcaXidaughters[0]) &&
                  (Xi->dcaXiDaughters()   < mdcaXidaughters[1]) &&
                  (Xi->dcaXiToPrimVertex()   > mdcaXiToPrimVertex[0]) &&
                  (Xi->dcaXiToPrimVertex()   < mdcaXiToPrimVertex[1]) &&
                  (Xi->decayLengthXi() > mdecayLengthXi[0]) &&
                  (Xi->decayLengthXi() < mdecayLengthXi[1]) &&
                  (Xi->tpcHitsBac()   > mtpcHitsBac[0]) &&
                  (Xi->tpcHitsBac()   < mtpcHitsBac[1]) &&
                  (Xi->dcaBacToPrimVertex()   > mdcaBacToPrimVertex[0]) &&
                  (Xi->dcaBacToPrimVertex()   < mdcaBacToPrimVertex[1]) &&
                  (Xi->ptArmXi()   > mptArmXi[0]) &&
                  (Xi->ptArmXi()   < mptArmXi[1]) &&
                  (Xi->alphaXi()   > malphaXi[0]) &&
                  (Xi->alphaXi()   < malphaXi[1]));

  if(goodPID){
    if( mChargedEdx <0){
      goodPID = (Xi->dedxNeg() > (mdEdx[0]*Xi->ptNeg()+mdEdx[1]));
	}
    if( mChargedEdx > 0){
      goodPID = (Xi->dedxPos() > (mdEdx[0]*Xi->ptPos()+mdEdx[1]));
    }
  }

  if (goodPID){
    float TEnergy = ::sqrt((Xi->ptotXi())*(Xi->ptotXi())+mMass*mMass);
    float TRapidity = 0.5*::log((TEnergy+Xi->momXi().z())/
			    (TEnergy-Xi->momXi().z()));

    float Pt = Xi->ptXi();


    
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

    bool goodXi=
      ((Pt             > mPt[0]) &&
       (Pt             < mPt[1]) &&
       (TRapidity      > mRapidity[0]) &&
       (TRapidity      < mRapidity[1]));

    goodXi ? mNXisPassed++ : mNXisFailed++;
    return (goodXi);
  }
  else{
    mNXisFailed++;
    return (goodPID);
  }
}
//------------------------------
StHbtString franksXiCut::Report(){
  string Stemp;
  char Ctemp[100];
  sprintf(Ctemp,"--franksXiCut--\n Particle mass:\t%E\n",this->Mass());
  Stemp=Ctemp;
  sprintf(Ctemp,"Xi mass range:\t%E - %E\n",mXiMassRange[0],
	  mXiMassRange[1]);
  Stemp+=Ctemp;
  sprintf(Ctemp,"dcaXidaughters:\t%E - %E\n",mdcaXidaughters[0],
mdcaXidaughters[1]);
  Stemp+=Ctemp;
  sprintf(Ctemp,"dcaXiToPrimVertex:\t%E - %E\n",mdcaXiToPrimVertex[0],
mdcaXiToPrimVertex[1]);
  Stemp+=Ctemp;
  sprintf(Ctemp,"decayLengthXi:\t%E - %E\n",mdecayLengthXi[0],
mdecayLengthXi[1]);
  Stemp+=Ctemp;
  sprintf(Ctemp,"tpcHitsBac:\t%d - %d\n",mtpcHitsBac[0],mtpcHitsBac[1]);
  Stemp+=Ctemp;
  sprintf(Ctemp,"dcaBacToPrimVertex:\t%E - %E\n",mdcaBacToPrimVertex[0],
mdcaBacToPrimVertex[1]);
  Stemp+=Ctemp;
  sprintf(Ctemp,"dedx>:\t%E pt+%E for Charge %E\n ",mdEdx[0],mdEdx[1],mChargedEdx);
  Stemp+=Ctemp;
  sprintf(Ctemp,"ptArmXi:\t%E - %E\n",mptArmXi[0],mptArmXi[1]);
  Stemp+=Ctemp;
  sprintf(Ctemp,"alphaXi:\t%E - %E\n",malphaXi[0],malphaXi[1]);
  Stemp+=Ctemp;
  sprintf(Ctemp,"Particle pT:\t%E - %E\n",mPt[0],mPt[1]);
  Stemp+=Ctemp;
  sprintf(Ctemp,"Particle rapidity:\t%E - %E\n",mRapidity[0],mRapidity[1]);
  Stemp+=Ctemp;
  sprintf(Ctemp,"Number of Xis which passed:\t%ld  Number which failed:\t%ld\n",mNXisPassed,mNXisFailed);
  Stemp += Ctemp;
  StHbtString returnThis = Stemp;
  return returnThis;
}

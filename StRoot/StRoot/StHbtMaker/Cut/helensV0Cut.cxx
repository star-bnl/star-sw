/***************************************************************************
 *
 * $Id: helensV0Cut.cxx,v 1.11 2003/09/02 17:58:21 perev Exp $
 *
 * Authors: Helen Caines, Tom Humanic, Ohio State, humanic@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *    a V0 particle cut that selects on phasespace, particle type, etc..  
 *
 ***************************************************************************
 *
 * $Log: helensV0Cut.cxx,v $
 * Revision 1.11  2003/09/02 17:58:21  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.10  2001/09/28 20:41:19  lisa
 * fixes to V0 object and cut
 *
 * Revision 1.9  2000/10/09 21:56:16  laue
 * Helens new cuts
 *
 * Revision 1.7  2000/03/17 17:22:53  laue
 * Roberts new three particle correlations implemented.
 *
 * Revision 1.6  2000/03/16 01:57:17  laue
 * Copy constructor added to some cuts
 *
 * Revision 1.5  2000/02/11 18:02:26  laue
 * Debug output removed
 *
 * Revision 1.4  2000/02/01 00:31:27  laue
 * *** empty log message ***
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
 * Revision 1.2  1999/10/05 11:37:38  lisa
 * Helens realistic V0Cut and Franks memory-sealed McReader
 *
 * Revision 1.1  1999/09/23 23:28:03  lisa
 * add helensV0Cut  AND  rename mikes and franks ParticleCuts to TrackCuts  AND  update documentation
 *
 *
 **************************************************************************/

#include "StHbtMaker/Cut/helensV0Cut.h"
#include <cstdio>

#ifdef __ROOT__ 
ClassImp(helensV0Cut)
#endif


helensV0Cut::helensV0Cut(){
  mNV0sPassed = mNV0sFailed = 0;
  
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
  mdEdx[2]=0;
  mdEdx[3]=-10;

  mPt[0]=0; 
  mPt[1]=100000;

  mRapidity[0]=-100000;
  mRapidity[1]=100000;

  V0Type = "K0Short";
  mMass  = 0.498;


}
//------------------------------
//helensV0Cut::~helensV0Cut(){
//  /* noop */
//}
//------------------------------
bool helensV0Cut::Pass(const StHbtV0* V0){
  int inMassRange;

#ifdef STHBTDEBUG  
  cout << endl;
  cout << " * dcaV0Daughters " << V0->dcaV0Daughters();
  cout << " * dcaV0ToPrimVertex " << V0->dcaV0ToPrimVertex();
  cout << " * decayLengthV0 " << V0->decayLengthV0();
  cout << " * tpcHitsPos " << V0->tpcHitsPos();
  cout << " * tpcHitsNeg " << V0->tpcHitsNeg();
  cout << " * dcaPosToPrimVertex " << V0->dcaPosToPrimVertex();
  cout << " * dcaNegToPrimVertex " << V0->dcaNegToPrimVertex();
  cout << " * ptArmV0 " << V0->ptArmV0();
  cout << " * alphaV0 " << V0->alphaV0();
  cout << " * dEdxPos " << V0->dedxPos();
  cout << " * dEdxNeg " << V0->dedxNeg();
  cout << endl;
#endif


  inMassRange=0;
  // Find out what particle is desired

  if( strstr(V0Type,"k") || strstr(V0Type,"K")){
     if( V0->massK0Short() < (mV0MassRange[1]) && 
	 V0->massK0Short() > (mV0MassRange[0]) ) inMassRange=1;
  }
  else if( (strstr(V0Type,"anti") || strstr(V0Type,"ANTI"))){
     if( V0->massAntiLambda() < (mV0MassRange[1]) && 
	 V0->massAntiLambda() > (mV0MassRange[0]) ) inMassRange=1;
  }
  else if( (strstr(V0Type,"ambda") || strstr(V0Type,"AMBDA"))){
     if( V0->massLambda() < (mV0MassRange[1]) && 
	 V0->massLambda() > (mV0MassRange[0]) ) inMassRange=1;
  }



  bool goodPID = ( inMassRange &&
		  (V0->dcaV0Daughters()   > mdcaV0daughters[0]) &&
                  (V0->dcaV0Daughters()   < mdcaV0daughters[1]) &&
                  (V0->dcaV0ToPrimVertex()   > mdcaV0ToPrimVertex[0]) &&
                  (V0->dcaV0ToPrimVertex()   < mdcaV0ToPrimVertex[1]) &&
                  (V0->decayLengthV0() > mdecayLengthV0[0]) &&
                  (V0->decayLengthV0() < mdecayLengthV0[1]) &&
                  (V0->tpcHitsPos()   > mtpcHitsPos[0]) &&
                  (V0->tpcHitsPos()   < mtpcHitsPos[1]) &&
                  (V0->tpcHitsNeg()   > mtpcHitsNeg[0]) &&
                  (V0->tpcHitsNeg()   < mtpcHitsNeg[1]) &&
                  (V0->dcaPosToPrimVertex()   > mdcaPosToPrimVertex[0]) &&
                  (V0->dcaPosToPrimVertex()   < mdcaPosToPrimVertex[1]) &&
                  (V0->dcaNegToPrimVertex()   > mdcaNegToPrimVertex[0]) &&
                  (V0->dcaNegToPrimVertex()   < mdcaNegToPrimVertex[1]) && 
                  (V0->ptArmV0()   > mptArmV0[0]) &&
                  (V0->ptArmV0()   < mptArmV0[1]) &&
                  (V0->alphaV0()   > malphaV0[0]) &&
                  (V0->alphaV0()   < malphaV0[1]));

  if(goodPID && mChargedEdx){
    if( mChargedEdx <0){
      goodPID = ( (V0->dedxNeg() > (mdEdx[0]*V0->ptNeg()+mdEdx[1])) &&
		  (V0->dedxNeg() > (mdEdx[2]*V0->ptNeg()+mdEdx[3])));
	}
    if( mChargedEdx > 0){
      goodPID = ( (V0->dedxPos() > (mdEdx[0]*V0->ptPos()+mdEdx[1])) &&
		  (V0->dedxPos() > (mdEdx[2]*V0->ptPos()+mdEdx[3])));
    }
  }

  if (goodPID){
    float TEnergy = ::sqrt((V0->ptotV0())*(V0->ptotV0())+mMass*mMass);
    float TRapidity = 0.5*::log((TEnergy+V0->momV0().z())/
			    (TEnergy-V0->momV0().z()));

    float Pt = V0->ptV0();


    
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

    bool goodV0=
      ((Pt             > mPt[0]) &&
       (Pt             < mPt[1]) &&
       (TRapidity      > mRapidity[0]) &&
       (TRapidity      < mRapidity[1]));

    goodV0 ? mNV0sPassed++ : mNV0sFailed++;
    return (goodV0);
  }
  else{
    mNV0sFailed++;
    return (goodPID);
  }
}
//------------------------------
StHbtString helensV0Cut::Report(){
  string Stemp;
  char Ctemp[100];
  sprintf(Ctemp,"--helensV0Cut--\n Particle mass:\t%E\n",this->Mass());
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
  sprintf(Ctemp,"Number of V0s which passed:\t%ld  Number which failed:\t%ld\n",mNV0sPassed,mNV0sFailed);
  Stemp += Ctemp;
  StHbtString returnThis = Stemp;
  return returnThis;
}

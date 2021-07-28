/***************************************************************************
 *
 * $Id: franksV0Cut.cxx,v 1.2 2003/09/02 17:58:21 perev Exp $
 *
 * Authors: Frank Laue, Ohio State, laue@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *    a V0 particle cut that selects on phasespace, particle type, etc..  
 *
 ***************************************************************************
 *
 * $Log: franksV0Cut.cxx,v $
 * Revision 1.2  2003/09/02 17:58:21  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.1  2000/08/28 22:32:04  laue
 * NEW: a V0 cut that allows to reject V0 pairs with share a track
 *
 *
 **************************************************************************/

#include "StHbtMaker/Cut/franksV0Cut.h"
#include "StarClassLibrary/SystemOfUnits.h"
#include <cstdio>

#ifdef __ROOT__ 
ClassImp(franksV0Cut)
#endif


franksV0Cut::franksV0Cut() : mNV0sPassed(0), mNV0sFailed(0) {

}
//------------------------------
//franksV0Cut::~franksV0Cut(){
//  /* noop */
//}
//------------------------------

void franksV0Cut::EventBegin(const StHbtEvent* ev) {
  mPrimaryVertex = ev->PrimVertPos();
}



bool franksV0Cut::Pass(const StHbtV0* V0){
  double degree=3.1415927/180.;
  int inMassRange;
    
#ifdef STHBTDEBUG  
    cout << endl;
    cout << " v0 " << V0 << endl;
    cout << " * dcaV0Daughters " << V0->dcaV0Daughters();
    cout << " * dcaV0ToPrimVertex " << V0->dcaV0ToPrimVertex();
    cout << " * pV0ToPrimVertexAngle " << V0->pV0ToPrimVertexAngle();
    cout << " * decayLengthV0 " << V0->decayLengthV0();
    cout << " * tpcHitsPos " << V0->tpcHitsPos();
    cout << " * tpcHitsNeg " << V0->tpcHitsNeg();
    cout << " * dcaPosToPrimVertex " << V0->dcaPosToPrimVertex();
    cout << " * dcaNegToPrimVertex " << V0->dcaNegToPrimVertex();
    cout << " * ptArmV0 " << V0->ptArmV0();
    cout << " * alphaV0 " << V0->alphaV0();
#endif
    
    
    inMassRange=0;
    // Find out what particle is desired
    
    if( strstr(V0Type,"k") || strstr(V0Type,"K")){
	if( V0->massK0Short() < (mMass+mV0MassRange[1]) && 
	    V0->massK0Short() > (mMass+mV0MassRange[0]) ) inMassRange=1;
    }
    else if( (strstr(V0Type,"anti") || strstr(V0Type,"ANTI"))){
	if( V0->massAntiLambda() < (mMass+mV0MassRange[1]) && 
	    V0->massAntiLambda() > (mMass+mV0MassRange[0]) ) inMassRange=1;
    }
    else if( (strstr(V0Type,"ambda") || strstr(V0Type,"AMBDA"))){
	if( V0->massLambda() < (mMass+mV0MassRange[1]) && 
	    V0->massLambda() > (mMass+mV0MassRange[0]) ) inMassRange=1;
    }
    
#ifdef STHBTDEBUG  
    cout << " inMassRange " << inMassRange << endl;
#endif
    
    double angle = (V0->decayVertexV0() - mPrimaryVertex).angle(V0->momV0()) / degree;
#ifdef STHBTDEBUG  
    cout << " decayVertexV0 " << V0->decayVertexV0() << endl;
    cout << " mPrimaryVertex " << mPrimaryVertex << endl; 
    cout << " angle " << angle << endl;
#endif

    bool goodPID = ( inMassRange &&
		     (V0->dcaV0Daughters()   > mdcaV0daughters[0]) &&
		     (V0->dcaV0Daughters()   < mdcaV0daughters[1]) &&
		     (V0->dcaV0ToPrimVertex()   > mdcaV0ToPrimVertex[0]) &&
		     (V0->dcaV0ToPrimVertex()   < mdcaV0ToPrimVertex[1]) &&
		     (angle   > mpV0ToPrimVertexAngle[0]) &&
		     (angle   < mpV0ToPrimVertexAngle[1]) &&
		     (V0->decayLengthV0() > mdecayLengthV0[0]) &&
		     (V0->decayLengthV0() < mdecayLengthV0[1]) &&
		     (V0->tpcHitsPos()    > mtpcHitsPos[0]) &&
		     (V0->tpcHitsPos()    < mtpcHitsPos[1]) &&
		     (V0->tpcHitsNeg()    > mtpcHitsNeg[0]) &&
		     (V0->tpcHitsNeg()    < mtpcHitsNeg[1]) &&
		     (V0->dcaPosToPrimVertex()   > mdcaPosToPrimVertex[0]) &&
		     (V0->dcaPosToPrimVertex()   < mdcaPosToPrimVertex[1]) &&
		     (V0->dcaNegToPrimVertex()   > mdcaNegToPrimVertex[0]) &&
		     (V0->dcaNegToPrimVertex()   < mdcaNegToPrimVertex[1]) && 
		     (V0->ptArmV0()   > mptArmV0[0]) &&
		     (V0->ptArmV0()   < mptArmV0[1]) &&
		     (V0->alphaV0()   > malphaV0[0]) &&
		     (V0->alphaV0()   < malphaV0[1]));
    

#ifdef STHBTDEBUG  
    cout << " goodPID " << goodPID << endl;
#endif
    
    if (goodPID){
#ifdef STHBTDEBUG  
	cout << "now calculate rapidity" << endl;
#endif
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
StHbtString franksV0Cut::Report(){
  string Stemp;
  char Ctemp[100];
  sprintf(Ctemp,"--franksV0Cut--\n Particle mass:\t%E\n",this->Mass());
  Stemp=Ctemp;
  sprintf(Ctemp,"dcaV0daughters:\t%E - %E\n",mdcaV0daughters[0],
mdcaV0daughters[1]);
  Stemp+=Ctemp;
  sprintf(Ctemp,"dcaV0ToPrimVertex:\t%E - %E\n",mdcaV0ToPrimVertex[0],
mdcaV0ToPrimVertex[1]);
  Stemp+=Ctemp;
  sprintf(Ctemp,"pV0ToPrimVertexAngle:\t%E - %E\n",mpV0ToPrimVertexAngle[0],mpV0ToPrimVertexAngle[1]);
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

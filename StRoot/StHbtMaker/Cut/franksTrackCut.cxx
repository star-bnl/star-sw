/***************************************************************************
 *
 *  
 *
 * Author: Frank Laue, Ohio State, laue@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *    a simple particle cut that selects on phasespace, #hits, DCA, and PID          
 *
 ***************************************************************************
 *
 * 
 **************************************************************************/

#include "StHbtMaker/Cut/franksTrackCut.h"
#include <cstdio>

#ifdef __ROOT__
  ClassImp(franksTrackCut)
#endif

franksTrackCut::franksTrackCut(){
  mNTracksPassed = mNTracksFailed = 0;
  mNSigmaElectron[0] = -1e9;
  mNSigmaElectron[1] = +1e9;
}
//------------------------------
franksTrackCut::franksTrackCut(franksTrackCut& c) : StHbtTrackCut(c) {
  mNTracksPassed = mNTracksFailed = 0;
  mCharge = c.mCharge;
  mNSigmaElectron[0] = c.mNSigmaElectron[0];
  mNSigmaElectron[1] = c.mNSigmaElectron[1];
  mNSigmaPion[0] = c.mNSigmaPion[0];
  mNSigmaPion[1] = c.mNSigmaPion[1];
  mNSigmaKaon[0] = c.mNSigmaKaon[0];
  mNSigmaKaon[1] = c.mNSigmaKaon[1];
  mNSigmaProton[0] = c.mNSigmaProton[0];
  mNSigmaProton[1] = c.mNSigmaProton[1];
  mNHits[0] = c.mNHits[0];
  mNHits[1] = c.mNHits[1];
  mP[0] = c.mP[0];
  mP[1] = c.mP[1];
  mPt[0] = c.mPt[0];
  mPt[1] = c.mPt[1];
  mRapidity[0] = c.mRapidity[0];
  mRapidity[1] = c.mRapidity[1];
  mDCA[0] = c.mDCA[0];
  mDCA[1] = c.mDCA[1];
  mNTracksPassed=0;
  mNTracksFailed=0;
#ifdef STHBTDEBUG
  cout << " franksTrackCut::franksTrackCut(franksTrackCut& c) " << endl;
#endif
}

//------------------------------
franksTrackCut::~franksTrackCut(){
}

//------------------------------
bool franksTrackCut::Pass(const StHbtTrack* track){
  // cout << " *** franksTrackCut::Pass(const StHbtTrack* track) " << endl;
  float TEnergy = sqrt(track->P().mag2()+mMass*mMass);
  float TRapidity = 0.5*log((TEnergy+track->P().z())/
			    (TEnergy-track->P().z()));

#ifdef STHBTDEBUG
  cout << 
    track->NSigmaElectron() << " " <<
    track->NSigmaPion() << " " <<
    track->NSigmaKaon() << " " <<
    track->NSigmaProton() << " " << 
    track->DCAxy() << " " << 
    track->NHits() << " " <<  
    track->P().mag() << " " << 
    track->Pt() << " " <<  
    TRapidity << " " << 
    track->Charge() << " "; 
#endif

   bool goodPID = (
		   (track->NSigmaPion()  >= mNSigmaPion[0]) &&
		   (track->NSigmaPion()   <= mNSigmaPion[1]) &&
		   (track->NSigmaKaon()   >= mNSigmaKaon[0]) &&
		   (track->NSigmaKaon()   <= mNSigmaKaon[1]) &&
		   (track->NSigmaProton() >= mNSigmaProton[0]) &&
		   (track->NSigmaProton() <= mNSigmaProton[1]) &&
		   (track->NSigmaElectron() >= mNSigmaElectron[0]) &&
		   (track->NSigmaElectron() <= mNSigmaElectron[1]) &&
		   (track->Charge() == mCharge || mCharge==0 )
		   );
#ifdef STHBTDEBUG
   cout  << mNSigmaElectron[0] << " << " << track->NSigmaElectron() << " << " << mNSigmaElectron[1] << " ";
   cout << (track->NSigmaElectron() >= mNSigmaElectron[0]) << (track->NSigmaElectron() <= mNSigmaElectron[1]) << endl;
#endif

  bool goodTrack=
    ((track->DCAxy()  >= mDCA[0]) &&
     (track->DCAxy()  <= mDCA[1]) &&
     (track->NHits() >= mNHits[0]) &&
     (track->NHits() <= mNHits[1]) &&
     (track->P().mag() >= mP[0]) &&
     (track->P().mag() <= mP[1]) &&
     (track->Pt()    >= mPt[0]) &&
     (track->Pt()    <= mPt[1]) &&
     (TRapidity      >= mRapidity[0]) &&
     (TRapidity      <= mRapidity[1]));
  
    
#ifdef STHBTDEBUG
  cout << " goodPID=" << goodPID << " ";
  cout << " goodTrack=" << goodTrack << " ";
  cout << endl;
#endif

  (goodTrack && goodPID) ? mNTracksPassed++ : mNTracksFailed++;

    return (goodPID && goodTrack);
}
//------------------------------
StHbtString franksTrackCut::Report(){
  string Stemp;
  char Ctemp[100];
  sprintf(Ctemp,"\nParticle mass:\t%E",mMass);
  Stemp+=Ctemp;
  sprintf(Ctemp,"\nParticle charge:\t%d",mCharge);
  Stemp+=Ctemp;
  sprintf(Ctemp,"\nParticle Nsigma from pion:\t%E - %E",mNSigmaPion[0],mNSigmaPion[1]);
  Stemp+=Ctemp;
  sprintf(Ctemp,"\nParticle Nsigma from kaon:\t%E - %E",mNSigmaKaon[0],mNSigmaKaon[1]);
  Stemp+=Ctemp;
  sprintf(Ctemp,"\nParticle Nsigma from proton:\t%E - %E",mNSigmaProton[0],mNSigmaProton[1]);
  Stemp+=Ctemp;
  sprintf(Ctemp,"\nParticle Nsigma from electron:\t%E - %E",mNSigmaElectron[0],mNSigmaElectron[1]);
  Stemp+=Ctemp;
  sprintf(Ctemp,"\nParticle #hits:\t%d - %d",mNHits[0],mNHits[1]);
  Stemp+=Ctemp;
  sprintf(Ctemp,"\nParticle p:\t%E - %E",mP[0],mP[1]);
  Stemp+=Ctemp;
  sprintf(Ctemp,"\nParticle pT:\t%E - %E",mPt[0],mPt[1]);
  Stemp+=Ctemp;
  sprintf(Ctemp,"\nParticle rapidity:\t%E - %E",mRapidity[0],mRapidity[1]);
  Stemp+=Ctemp;
  sprintf(Ctemp,"\nParticle DCA:\t%E - %E",mDCA[0],mDCA[1]);
  Stemp+=Ctemp;
  sprintf(Ctemp,"\nNumber of tracks which passed:\t%ld  Number which failed:\t%ld",mNTracksPassed,mNTracksFailed);
  Stemp += Ctemp;
  StHbtString returnThis = Stemp;
  return returnThis;
}


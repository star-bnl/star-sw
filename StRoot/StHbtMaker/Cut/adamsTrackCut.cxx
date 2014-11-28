/***************************************************************************
 *
 * $Id: 
 *
 * Author: Frank Laue, Ohio State, laue@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *    a simple particle cut that selects on phasespace, #hits, DCA, and PID          
 *
 ***************************************************************************
 *
 * $Log:
 **************************************************************************/

#include "StHbtMaker/Cut/adamsTrackCut.h"
#include <cstdio>

#ifdef __ROOT__
ClassImp(adamsTrackCut)
#endif
  
  //#define STHBTDEBUG
  
adamsTrackCut::adamsTrackCut(): franksTrackCut() {
  mPIDPThreshold = 0;
}
//------------------------------
adamsTrackCut::adamsTrackCut(adamsTrackCut& c) : franksTrackCut(c) {
  mPIDPThreshold = c.mPIDPThreshold;
#ifdef STHBTDEBUG
  cout << " adamsTrackCut::adamsTrackCut(adamsTrackCut& c) " << endl;
#endif
}

//------------------------------
adamsTrackCut::~adamsTrackCut(){
}

//------------------------------
bool adamsTrackCut::Pass(const StHbtTrack* track){
  //   cout << " *** adamsTrackCut::Pass(const StHbtTrack* track) " << endl;

  float TEnergy = ::sqrt(track->P().mag2()+mMass*mMass);
  float TRapidity = 0.5*::log((TEnergy+track->P().z())/
			    (TEnergy-track->P().z()));

#ifdef STHBTDEBUG
  cout << 
    track->NSigmaElectron() << " " <<
    track->NSigmaPion() << " " <<
    track->NSigmaKaon() << " " <<
    track->NSigmaProton() << " " << 
    track->DCAxy() << " " << 
    track->DCAxyGlobal() << " " << 
    track->NHits() << " " <<  
    track->NHitsDedx() << " " <<  
    track->P().mag() << " " << 
    track->Pt() << " " <<  
    TRapidity << " " << 
    track->Charge() << " ";
#endif

  bool goodPID;
  // Use different PIDs for P below and above threshold
  if (track->P().mag() < mPIDPThreshold)
    {
      // Use NSigma
      goodPID = (
		 (track->NSigmaElectron() >= mNSigmaElectron[0]) &&
		 (track->NSigmaElectron() <= mNSigmaElectron[1]) &&
		 (track->NSigmaPion()   >= mNSigmaPion[0]) &&
		 (track->NSigmaPion()   <= mNSigmaPion[1]) &&
		 (track->NSigmaKaon()   >= mNSigmaKaon[0]) &&
		 (track->NSigmaKaon()   <= mNSigmaKaon[1]) &&
		 (track->NSigmaProton() >= mNSigmaProton[0]) &&
		 (track->NSigmaProton() <= mNSigmaProton[1]) &&
		 !( (track->NSigmaElectron() > mNSigmaAntiElectron[0]) &&
		    (track->NSigmaElectron() < mNSigmaAntiElectron[1]) ) &&
		 !( (track->NSigmaPion()  > mNSigmaAntiPion[0]) &&
		    (track->NSigmaPion()   < mNSigmaAntiPion[1]) ) &&
		 !( (track->NSigmaKaon()   > mNSigmaAntiKaon[0]) &&
		    (track->NSigmaKaon()   < mNSigmaAntiKaon[1]) ) &&
		 !( (track->NSigmaProton() > mNSigmaAntiProton[0]) &&
		    (track->NSigmaProton() < mNSigmaAntiProton[1]) ) &&
		 (track->Charge() == mCharge || mCharge==0 )
		 );
    }
  else 
    {
      // Use PID Probability
      goodPID = (
		 (track->PidProbElectron() >= mPidProbElectron[0]) &&
		 (track->PidProbElectron() <= mPidProbElectron[1]) &&
		 (track->PidProbPion()   >= mPidProbPion[0]) &&
		 (track->PidProbPion()   <= mPidProbPion[1]) &&
		 (track->PidProbKaon()   >= mPidProbKaon[0]) &&
		 (track->PidProbKaon()   <= mPidProbKaon[1]) &&
		 (track->PidProbProton() >= mPidProbProton[0]) &&
		 (track->PidProbProton() <= mPidProbProton[1]) &&
		 (track->Charge() == mCharge || mCharge==0 )
		 );
    }
  
#ifdef STHBTDEBUG
   cout  << mNSigmaElectron[0] << " << " << track->NSigmaElectron() << " << " << mNSigmaElectron[1] << " ";
   cout << (track->NSigmaElectron() >= mNSigmaElectron[0]) << (track->NSigmaElectron() <= mNSigmaElectron[1]) << endl;
#endif

   bool goodTrack=( true &&
     (track->DCAxy()  >= mDCA[0]) &&
     (track->DCAxy()  <= mDCA[1]) &&
     (track->DCAxyGlobal()  >= mDCAGlobal[0]) &&
     (track->DCAxyGlobal()  <= mDCAGlobal[1]) &&
     (track->NHits() >= mNHits[0]) &&
     (track->NHits() <= mNHits[1]) &&
     (track->NHitsDedx() >= mNdEdxHits[0]) &&
     (track->NHitsDedx() <= mNdEdxHits[1]) &&
      (track->P().mag() >= mP[0]) &&
      (track->P().mag() <= mP[1]) &&
      (track->Pt()    >= mPt[0]) &&
      (track->Pt()    <= mPt[1]) &&
      (track->P().x() >= mPx[0]) &&
      (track->P().x() <= mPx[1]) &&
      (track->P().y() >= mPy[0]) &&
      (track->P().y() <= mPy[1]) &&
      (track->P().z() >= mPz[0]) &&
      (track->P().z() <= mPz[1]) &&
      (track->P().pseudoRapidity() >= mEta[0]) &&
      (track->P().pseudoRapidity() <= mEta[1]) &&
      (TRapidity      >= mRapidity[0]) &&
      (TRapidity      <= mRapidity[1])
);
  
    
#ifdef STHBTDEBUG
  cout << " goodPID=" << goodPID << " ";
  cout << " goodTrack=" << goodTrack << " ";
  cout << endl;
#endif

  (goodTrack && goodPID) ? mNTracksPassed++ : mNTracksFailed++;

    return (goodPID && goodTrack);
}
//------------------------------
StHbtString adamsTrackCut::Report()  {
  return franksTrackCut::Report();
}


ostrstream* adamsTrackCut::finalReport() const{
  return franksTrackCut::finalReport();
}


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

  /*
    cout << 
    track->NSigmaPion() << " " <<
    track->NSigmaKaon() << " " <<
    track->NSigmaProton() << " " <<
    track->DCAxy() << " " <<
    track->NHits() << " " << 
    track->P().mag() << " " <<
    track->Pt() << " " << 
    TRapidity << " " <<
    track->Charge() << " " <<
    endl;
  */

  bool goodPID = ((track->NSigmaPion()   >= mNSigmaPion[0]) &&
                  (track->NSigmaPion()   <= mNSigmaPion[1]) &&
                  (track->NSigmaKaon()   >= mNSigmaKaon[0]) &&
                  (track->NSigmaKaon()   <= mNSigmaKaon[1]) &&
                  (track->NSigmaProton() >= mNSigmaProton[0]) &&
                  (track->NSigmaProton() <= mNSigmaProton[1]) &&
                  (track->Charge() == mCharge || mCharge==0 ));

  if (goodPID){
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

    
    //goodTrack = goodTrack && ( fabs(track->NSigmaPion()) > 1/ abs(track->P()) );

    goodTrack ? mNTracksPassed++ : mNTracksFailed++;
    // cout << " *** franksTrackCut: now fill *** " << endl;
    //    Fill(track,goodTrack); // fill monitor histograms 
    return (goodTrack);
  }
  else{
    mNTracksFailed++;
    //    Fill(track,goodPID); // fill monitor histograms 
    return (goodPID);
  }
}
//------------------------------
StHbtString franksTrackCut::Report(){
  string Stemp;
  char Ctemp[100];
  sprintf(Ctemp,"\nParticle mass:\t%E",this->Mass());
  Stemp+=Ctemp;
  sprintf(Ctemp,"\nParticle charge:\t%d",mCharge);
  Stemp+=Ctemp;
  sprintf(Ctemp,"\nParticle Nsigma from pion:\t%E - %E",mNSigmaPion[0],mNSigmaPion[1]);
  Stemp+=Ctemp;
  sprintf(Ctemp,"\nParticle Nsigma from kaon:\t%E - %E",mNSigmaKaon[0],mNSigmaKaon[1]);
  Stemp+=Ctemp;
  sprintf(Ctemp,"\nParticle Nsigma from proton:\t%E - %E",mNSigmaProton[0],mNSigmaProton[1]);
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


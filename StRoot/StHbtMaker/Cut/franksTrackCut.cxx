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
                  (track->Charge() == mCharge));

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
  sprintf(Ctemp,"Particle mass:\t%E\n",this->Mass());
  Stemp=Ctemp;
  sprintf(Ctemp,"Particle charge:\t%d\n",mCharge);
  Stemp=Ctemp;
  sprintf(Ctemp,"Particle Nsigma from pion:\t%E - %E\n",mNSigmaPion[0],mNSigmaPion[1]);
  Stemp+=Ctemp;
  sprintf(Ctemp,"Particle Nsigma from kaon:\t%E - %E\n",mNSigmaKaon[0],mNSigmaKaon[1]);
  Stemp+=Ctemp;
  sprintf(Ctemp,"Particle Nsigma from proton:\t%E - %E\n",mNSigmaProton[0],mNSigmaProton[1]);
  Stemp+=Ctemp;
  sprintf(Ctemp,"Particle #hits:\t%d - %d\n",mNHits[0],mNHits[1]);
  Stemp+=Ctemp;
  sprintf(Ctemp,"Particle p:\t%E - %E\n",mP[0],mP[1]);
  Stemp+=Ctemp;
  sprintf(Ctemp,"Particle pT:\t%E - %E\n",mPt[0],mPt[1]);
  Stemp+=Ctemp;
  sprintf(Ctemp,"Particle rapidity:\t%E - %E\n",mRapidity[0],mRapidity[1]);
  Stemp+=Ctemp;
  sprintf(Ctemp,"Particle DCA:\t%E - %E\n",mDCA[0],mDCA[1]);
  Stemp+=Ctemp;
  sprintf(Ctemp,"Number of tracks which passed:\t%ld  Number which failed:\t%ld\n",mNTracksPassed,mNTracksFailed);
  Stemp += Ctemp;
  StHbtString returnThis = Stemp;
  return returnThis;
}


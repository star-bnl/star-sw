/***************************************************************************
 *
 * $Id: mikesParticleCut.cxx,v 1.4 1999/07/27 20:21:08 lisa Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *    a simple particle cut that selects on phasespace, #hits, DCA, and PID          
 *
 ***************************************************************************
 *
 * $Log: mikesParticleCut.cxx,v $
 * Revision 1.4  1999/07/27 20:21:08  lisa
 * Franks fixes of StTrack and subsequent changes to particleCut and EventReader
 *
 * Revision 1.3  1999/07/19 14:24:04  hardtke
 * modifications to implement uDST
 *
 * Revision 1.2  1999/07/06 22:33:21  lisa
 * Adjusted all to work in pro and new - dev itself is broken
 *
 * Revision 1.1.1.1  1999/06/29 16:02:56  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#include "StHbtMaker/Cut/mikesParticleCut.h"
#include <cstdio>

ClassImp(mikesParticleCut)

mikesParticleCut::mikesParticleCut(){
  mNTracksPassed = mNTracksFailed = 0;
}
//------------------------------
//mikesParticleCut::~mikesParticleCut(){
//  /* noop */
//}
//------------------------------
bool mikesParticleCut::Pass(const StHbtTrack* track){
  static int trackCount=0;


  /*
    cout << endl;
    cout << "#track " << trackCount++;
    cout << " * pion " << (track->NSigmaPion() > mNSigmaPion[0]) && (track->NSigmaPion() < mNSigmaPion[1]);
    cout << " * kaon " << (track->NSigmaKaon() > mNSigmaKaon[0]) && (track->NSigmaKaon() < mNSigmaKaon[1]);
    cout << " * proton " << (track->NSigmaProton() > mNSigmaProton[0]) && (track->NSigmaProton() < mNSigmaProton[1]);
    cout << " * charge " << (track->Charge() == mCharge);
  */
  bool goodPID = ((track->NSigmaPion()   > mNSigmaPion[0]) &&
                  (track->NSigmaPion()   < mNSigmaPion[1]) &&
                  (track->NSigmaKaon()   > mNSigmaKaon[0]) &&
                  (track->NSigmaKaon()   < mNSigmaKaon[1]) &&
                  (track->NSigmaProton() > mNSigmaProton[0]) &&
                  (track->NSigmaProton() < mNSigmaProton[1]) &&
                  (track->Charge() == mCharge));


  if (goodPID){
    float TEnergy = sqrt(track->P().mag2()+mMass*mMass);
    float TRapidity = 0.5*log((TEnergy+track->P().z())/
			    (TEnergy-track->P().z()));

    float Pt = sqrt((track->P().x())*(track->P().x())+
                    (track->P().y())*(track->P().y()));


    
    /*
      cout << " * DCAxy " << (track->DCAxy()  > mDCA[0]) && (track->DCAxy()  < mDCA[1]);
      cout << " * mDCA[0] " << mDCA[0];
      cout << " * mDCA[1] " << mDCA[1];
      cout << " * track->DCAxy " << track->DCAxy();
      cout << " * NHits " <<  (track->NHits() > mNHits[0]) && (track->NHits() < mNHits[1]); 
      cout << " * Pt " << (Pt > mPt[0]) && (Pt < mPt[1]);
      cout << " * y " << (TRapidity > mRapidity[0]) && (TRapidity < mRapidity[1]);
      cout << endl;
    */

    bool goodTrack=
      ((track->DCAxy()  > mDCA[0]) &&
       (track->DCAxy()  < mDCA[1]) &&
       (track->NHits() > mNHits[0]) &&
       (track->NHits() < mNHits[1]) &&
       (Pt             > mPt[0]) &&
       (Pt             < mPt[1]) &&
       (TRapidity      > mRapidity[0]) &&
       (TRapidity      < mRapidity[1]));

    goodTrack ? mNTracksPassed++ : mNTracksFailed++;
    return (goodTrack);
  }
  else{
    mNTracksFailed++;
    return (goodPID);
  }
}
//------------------------------
StHbtString mikesParticleCut::Report(){
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

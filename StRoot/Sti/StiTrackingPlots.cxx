/*
 * $Id: StiTrackingPlots.cxx,v 2.2 2003/03/13 18:59:15 pruneau Exp $
 *
 *
 * $Log: StiTrackingPlots.cxx,v $
 * Revision 2.2  2003/03/13 18:59:15  pruneau
 * various updates
 *
 * Revision 2.1  2003/03/12 16:36:03  andrewar
 * Trackng plots package added. Sti tracks are histogrammed at the Sti level
 * alowing comparison to MuDst and StEvent tracks.
 *
 */

#include <iostream>
#include <string>

#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TFile.h"

#include "Sti/StiDefaultTrackFilter.h"
#include "Sti/StiTrackContainer.h"
#include "Sti/StiTrack.h"
#include "Sti/StiTrackNode.h"
#include "Sti/StiKalmanTrack.h"
#include "Sti/StiKalmanTrackNode.h"
#include "Sti/StiTrackingPlots.h"

StiTrackingPlots::StiTrackingPlots()
{
  cout <<"StiTrackingPlots::StiTrackingPlots() -I- Started"<<endl;
  //Standard plots def.
  add( numTracks=new TH1D("numTracks","Number of tracks in Container", 128,0.,10000.) );
  add( radLengthZ   =new TH2D("radLengthZ","Absorption Length (%) vs. Z",400,-200,200,  100,0,100)  );
  add( radLengthPhi =new TH2D("radLengthPhi","Absorption Length (%) vs. Phi",128,0,360, 256,0,1) );
  add( radLengthEta =new TH2D("radLengthEta","Absorption Length (%) vs. Eta",128,-2,2, 256,0,1)  );
  add( _eta    = new TH1D("eta","Track Eta",200,-2,2) );
  add( _phi    = new TH1D("phi","Track Phi",100,-3.1415927,3.1415927) );
  add( _pt     = new TH1D("pt", "pt",       100,0., 5.) );
  add( mCurv  = new TH3D("mCurv","Curvature v. Eta and Pt", 256,-100,100,128,-2,2,128,0,30));
  add( mHeli  = new TH3D("mHeli","Helicity v. Eta and Pt", 3,-1,1,128,-2,2,128,0,30));
  add( mMomX  = new TH3D("mMomX","Momentum (X) v. Eta and Phi",  256,0,30,128,-2,2,128,0,360));
  add( mMomY  = new TH3D("mMomY","Momentum (Y) v. Eta and Phi",  256,0,30,128,-2,2,128,0,360));
  add( mMomZ  = new TH3D("mMomZ","Momentum (Z) v. Eta and Phi",  256,0,30,128,-2,2,128,0,360));
  add( mPhase = new TH3D("mPhase","Phase v. Eta and Pt",  256,0,30,128,-2,2,128,0,360));
  add( globalDca = new TH1D("globalDca","Global DCA", 160, -20,20) );	 
  cout <<"StiTrackingPlots::StiTrackingPlots() -I- Done"<<endl;
}
  
StiTrackingPlots::~StiTrackingPlots()
{
  cout << "StiTrackingPlots::~StiTrackingPlots() -I- Done"<<endl;
}

///this method opens an output file, then writes hists to it,
///then closes the file - use writeHists(file) if you want to
///add the hists to an already open file
void StiTrackingPlots::writeHists()
{
  const char* tName = mOutFile.c_str();
  TFile *newFile = new TFile(tName,"RECREATE");
  write();
  newFile->Close();
  delete newFile;
}

void StiTrackingPlots::writeHists(TFile *outFile)
{//this method writes hists in file <outFile>

  outFile->cd();
  write();
}

void StiTrackingPlots::fillStandardPlots(StiTrackContainer *mTrackStore)
{
  cout <<"StiTrackingPlots::fillStandardPlots: "
       <<"Storing plot values for event."<<endl;

  numTracks->Fill(mTrackStore->getTrackCount(mFilter));

  //loop over tracks
  for (TrackMap::const_iterator trackIt = mTrackStore->begin(); 
       trackIt!=mTrackStore->end();
       ++trackIt)
    {
      const StiKalmanTrack* kTrack = static_cast<const StiKalmanTrack*>((*trackIt).second);
      if(!kTrack) continue; //not for anything other than KalmanTracks
      if(kTrack->getFitPointCount()<15) continue;

      _eta->Fill(kTrack->getPseudoRapidity());
      _phi->Fill(kTrack->getPhi());
      _pt->Fill(kTrack->getPt());
      globalDca->Fill(kTrack->getDca());

      double phi = kTrack->getPhi();
      double eta = kTrack->getPseudoRapidity();
      double pt =  kTrack->getPt();
      double p[3], e[3];
      kTrack->getMomentum(p,e);
      

      radLengthPhi->Fill(kTrack->getTrackRadLength(),phi);
      radLengthEta->Fill(kTrack->getTrackRadLength(),eta);

      mCurv->Fill(kTrack->getCurvature(), eta, pt);
      //mHeli->Fill(kTrack->getHelicity(), eta, pt);
      mMomX->Fill(p[0],eta,phi);
      mMomY->Fill(p[1],eta,phi);
      mMomZ->Fill(p[2],eta,phi);
      //mPhase->Fill(kTrack->);

    }//end loop over tracks
}


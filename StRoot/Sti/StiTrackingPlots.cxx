/*
 * $Id: StiTrackingPlots.cxx,v 2.1 2003/03/12 16:36:03 andrewar Exp $
 *
 *
 * $Log: StiTrackingPlots.cxx,v $
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
  //Standard plots def.
  numTracks= new TH1D("numTracks","Number of tracks in Container",
		      128,0.,10000.);

  //Rad length map
  radLengthZ = new TH2D("radLengthZ","Absorption Length (%) vs. Z",
		       401,-200,200, 256,0,100);
  radLengthPhi = new TH2D("radLengthPhi","Absorption Length (%) vs. Phi",
		       128,0,360, 256,0,1);
  radLengthEta = new TH2D("radLengthEta","Absorption Length (%) vs. Eta",
		       128,-2,2, 256,0,1);

  //track kinematics
  mEta   = new TH1D("mEta","Track Eta",1024,-2,2);

  mCurv  = new TH3D("mCurv","Curvature v. Eta and Pt",
		    256,-100,100,128,-2,2,128,0,30);
  mHeli  = new TH3D("mHeli","Helicity v. Eta and Pt",
		    3,-1,1,128,-2,2,128,0,30);
  mMomX  = new TH3D("mMomX","Momentum (X) v. Eta and Phi",
		    256,0,30,128,-2,2,128,0,360);
  mMomY  = new TH3D("mMomY","Momentum (Y) v. Eta and Phi",
		    256,0,30,128,-2,2,128,0,360);
  mMomZ  = new TH3D("mMomZ","Momentum (Z) v. Eta and Phi",
		    256,0,30,128,-2,2,128,0,360);
  mPhase = new TH3D("mPhase","Phase v. Eta and Pt",
		    256,0,30,128,-2,2,128,0,360);

  globalDca = new TH1D("globalDca","Global DCA", 128,-20,20);
	 
  cout <<"Track Plots Set up."<<endl;

}

StiTrackingPlots::~StiTrackingPlots()
{
  //Standard plots def.
  //  delete numTracks;

//    delete radLengthZ;
//    delete radLengthPhi;
//    delete radLengthEta;


  cout <<"Track Plots cleaned up."<<endl;

}

void StiTrackingPlots::writeHists()
{
  //this method opens an output file, then writes hists to it,
  //then closes the file - use writeHists(file) if you want to
  //add the hists to an already open file
  const char* tName = mOutFile.c_str();
  TFile *newFile = new TFile(tName,"RECREATE");
  //writeHists(newFile);
  numTracks->Write();

  mEta->Write();
  //tRap->Write();

  //rad length maps
  radLengthZ->Write();
  radLengthPhi->Write();
  radLengthEta->Write();

  mCurv->Write();
  mHeli->Write();
  mMomX->Write();
  mMomY->Write();
  mMomZ->Write();
  mPhase->Write();

  globalDca->Write();


  newFile->Close();
  delete newFile;
}

void StiTrackingPlots::writeHists(TFile *outFile)
{//this method writes hists in file <outFile>

  outFile->cd();
  numTracks->Write();

  //rad length maps
  radLengthZ->Write();
  radLengthPhi->Write();
  radLengthEta->Write();
  
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
      mEta->Fill(kTrack->getPseudoRapidity());
      globalDca->Fill(kTrack->getInnerMostNode()->_x);
      //tRap->Fill(kTrack->getRapidity());

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


/*
 * $Id: StiTrackingPlots.cxx,v 2.5 2003/03/31 17:19:02 pruneau Exp $
 *
 *
 * $Log: StiTrackingPlots.cxx,v $
 * Revision 2.5  2003/03/31 17:19:02  pruneau
 * various
 *
 * Revision 2.4  2003/03/14 19:02:21  pruneau
 * various minor updates
 *
 * Revision 2.3  2003/03/13 21:21:29  pruneau
 * getPhase() fixed. MUST inclde -helicity()*pi/2
 *
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
  : HistogramGroup()
{ 
  initialize();
}
 
StiTrackingPlots::StiTrackingPlots(const string & name, const string & description)
  : HistogramGroup(name,description)
{ 
  initialize();
}
 

void StiTrackingPlots::initialize()
{
  cout <<"StiTrackingPlots::StiTrackingPlots() -I- Started"<<endl;
  numTracks=book("numTracks","Number of tracks in Container", 128,0.,10000.);
  radLengthZ   =book("radLengthZ","Absorption Length (%) vs. Z",400,-200,200,  100,0,100);
  radLengthPhi =book("radLengthPhi","Absorption Length (%) vs. Phi",128,0,360, 256,0,1);
  radLengthEta =book("radLengthEta","Absorption Length (%) vs. Eta",128,-2,2, 256,0,1);
  _eta    = book("eta","Track Eta",200,-2,2);
  _etaPlus    = book("etaPlus","Track Eta +",200,-2,2);
  _etaMinus   = book("etaMinus","Track Eta -",200,-2,2);
  _phi        = book("phi","Track Phi",100,-3.1415927,3.1415927);
  _phiPlus    = book("phiPlus","Track Phi+",100,-3.1415927,3.1415927);
  _phiMinus   = book("phiMinus","Track Phi-",100,-3.1415927,3.1415927);
  _pt         = book("pt", "pt",      250,0., 5.);
  _ptPlus     = book("ptPlus", "ptPlus",  250,0., 5.);
  _ptMinus    = book("ptMinus", "ptMinus", 250,0., 5.);
  // mCurv  = new TH3D("mCurv","Curvature v. Eta and Pt", 256,-100,100,128,-2,2,128,0,30));
  // mHeli  = new TH3D("mHeli","Helicity v. Eta and Pt", 3,-1,1,128,-2,2,128,0,30));
  // mMomX  = new TH3D("mMomX","Momentum (X) v. Eta and Phi",  256,0,30,128,-2,2,128,0,360));
  // mMomY  = new TH3D("mMomY","Momentum (Y) v. Eta and Phi",  256,0,30,128,-2,2,128,0,360));
  // mMomZ  = new TH3D("mMomZ","Momentum (Z) v. Eta and Phi",  256,0,30,128,-2,2,128,0,360));
  // mPhase = new TH3D("mPhase","Phase v. Eta and Pt",  256,0,30,128,-2,2,128,0,360));
  _globalDca = book("globalDca","Global DCA", 160, 0,20);	  
  _globalDcaPlus = book("globalDcaPlus","Global DCAPlus", 160, 0,20);	 
  _globalDcaMinus = book("globalDcaMinus","Global DCA Minus", 160, 0,20);	 
  _dca40 = book("dca40","DCA N>=40", 160, 0,20);	 
  _dca40Plus  = book("dca40Plus","DCA N>=40 Plus", 160, 0,20);	 
  _dca40Minus = book("dca40Minus","DCA N>=40 Minus", 160, 0,20);
  
  _nptsVsPt = book("nptsVsPt","nptsVsPt",40, 0., 4., 50, 0., 50.);
  _nptsVsPtPlus = book("nptsVsPtPlus","nptsVsPtPlus",40, 0., 4., 50, 0., 50.);
  _nptsVsPtMinus = book("nptsVsPtMinus","nptsVsPtMinus",40, 0., 4., 50, 0., 50.);
  
  _nptsVsEta = book("nptsVsEta","nptsVsEta",               40, -2., 2., 50, 0., 50.);
  _nptsVsEtaPlus = book("nptsVsEtaPlus","nptsVsEtaPlus",   40, -2., 2., 50, 0., 50.);
  _nptsVsEtaMinus = book("nptsVsEtaMinus","nptsVsEtaMinus",40, -2., 2., 50, 0., 50.);
  
  _nptsVsPhi = book("nptsVsPhi","nptsVsPhi",100,-3.1415927,3.1415927, 50, 0., 50.);
  _nptsVsPhiPlus = book("nptsVsPhiPlus","nptsVsPhiPlus",100,-3.1415927,3.1415927, 50, 0., 50.);
  _nptsVsPhiMinus = book("nptsVsPhiMinus","nptsVsPhiMinus",100,-3.1415927,3.1415927, 50, 0., 50.);
  
  cout <<"StiTrackingPlots::StiTrackingPlots() -I- Done"<<endl;
}
  
StiTrackingPlots::~StiTrackingPlots()
{
  cout << "StiTrackingPlots::~StiTrackingPlots() -I- Done"<<endl;
}




void StiTrackingPlots::fill(StiTrackContainer *mTrackStore)
{
  //cout <<"StiTrackingPlots::fill() -I- Filling histos" <<endl;

  //numTracks->Fill(mTrackStore->getTrackCount(mFilter));

  //loop over tracks
  for (TrackMap::const_iterator trackIt = mTrackStore->begin(); 
       trackIt!=mTrackStore->end();
       ++trackIt)
    {
      const StiTrack* track = (*trackIt).second;
      if(!track) continue; 
      double nPts = track->getPointCount();
      if(nPts<15) continue;
      double phi = track->getPhi();
      double eta = track->getPseudoRapidity();
      double pt  = track->getPt();
      double dca = track->getDca();
      double p[3], e[3];
      track->getMomentum(p,e);
      
      _eta->Fill(eta);
      _phi->Fill(phi);
      _pt->Fill(pt);
      _globalDca->Fill(dca);
      if (nPts>=40)
	_dca40->Fill(dca); 
      _nptsVsPt->Fill(pt,nPts);
      _nptsVsEta->Fill(eta,nPts);
      _nptsVsPhi->Fill(phi,nPts);

      //radLengthPhi->Fill(track->getTrackRadLength(),phi);
      //radLengthEta->Fill(track->getTrackRadLength(),eta);

      //mCurv->Fill(track->getCurvature(), eta, pt);
      //mHeli->Fill(track->getHelicity(), eta, pt);
      //mMomX->Fill(p[0],eta,phi);
      //mMomY->Fill(p[1],eta,phi);
      //mMomZ->Fill(p[2],eta,phi);
      //mPhase->Fill(track->);

      if (track->getCharge()>0) 
	{
	  _etaPlus->Fill(track->getPseudoRapidity());
	  _phiPlus->Fill(track->getPhi());
	  _ptPlus->Fill(track->getPt());
	  _globalDcaPlus->Fill(track->getDca());
	  if (nPts>=40)
	    _dca40Plus->Fill(dca);
	  _nptsVsPtPlus->Fill(pt,nPts);
	  _nptsVsEtaPlus->Fill(eta,nPts);
	  _nptsVsPhiPlus->Fill(phi,nPts);
	}
      else
	{
	  _etaMinus->Fill(track->getPseudoRapidity());
	  _phiMinus->Fill(track->getPhi());
	  _ptMinus->Fill(track->getPt());
	  _globalDcaMinus->Fill(track->getDca());
	  if (nPts>=40)
	    _dca40Minus->Fill(dca);
	  _nptsVsPtMinus->Fill(pt,nPts);
	  _nptsVsEtaMinus->Fill(eta,nPts);
	  _nptsVsPhiMinus->Fill(phi,nPts);
	}

    }//end loop over tracks
  //cout <<"StiTrackingPlots::fill() -I- Done Filling histos" <<endl;

}


/*
 * $Id: StiTrackingPlots.cxx,v 2.9 2003/07/22 17:16:29 pruneau Exp $
 *
 *
 * $Log: StiTrackingPlots.cxx,v $
 * Revision 2.9  2003/07/22 17:16:29  pruneau
 * various
 *
 * Revision 2.8  2003/05/01 20:46:52  pruneau
 * changed error parametrization
 *
 * Revision 2.7  2003/04/29 18:48:34  pruneau
 * *** empty log message ***
 *
 * Revision 2.6  2003/04/04 14:44:22  pruneau
 * Fix to the hit error calculator and the getCharge methods.
 *
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
#include "TProfile.h"
#include "TProfile2D.h"
#include "TFile.h"

#include "Sti/StiDefaultTrackFilter.h"
#include "Sti/StiTrackContainer.h"
#include "Sti/StiTrack.h"
#include "Sti/StiTrackNode.h"
#include "Sti/StiKalmanTrack.h"
#include "Sti/StiKalmanTrackNode.h"
#include "Sti/StiTrackingPlots.h"
#include "Sti/StiToolkit.h"
#include "Sti/StiHitContainer.h"

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
  _radLengthVsPhi =book("radLengthVsPhi","Absorption Length (%) vs. Phi",120,-180.,180., 400,0,0.2);
  _radLengthVsEta =book("radLengthVsEta","Absorption Length (%) vs. Eta",60,-1.5,1.5, 400,0,0.2);
  _radLengthVsPhiProf =bookProfile("radLengthVsPhiProf","Absorption Length Profile(%) vs. Phi",120,-180.,180.);
  _radLengthVsEtaProf =bookProfile("radLengthVsEtaProf","Absorption Length Profile(%) vs. Eta",60,-1.5,1.5);
  _eta        = book("eta",     "Track Eta",  200, -2.,2.);
  _etaPlus    = book("etaPlus", "Track Eta +",200, -2.,2.);
  _etaMinus   = book("etaMinus","Track Eta -",200, -2.,2.);
  _phi        = book("phi",     "Track Phi",  100,-3.1415927,3.1415927);
  _phiPlus    = book("phiPlus", "Track Phi+", 100,-3.1415927,3.1415927);
  _phiMinus   = book("phiMinus","Track Phi-", 100,-3.1415927,3.1415927);
  _pt         = book("pt",     "pt",      250,0., 5.);
  _ptPlus     = book("ptPlus", "ptPlus",  250,0., 5.);
  _ptMinus    = book("ptMinus","ptMinus", 250,0., 5.);
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
  
  _nptsVsPt      = book("nptsVsPt",     "nptsVsPt",     40, 0., 4., 50, 0., 50.);
  _nptsVsPtPlus  = book("nptsVsPtPlus", "nptsVsPtPlus", 40, 0., 4., 50, 0., 50.);
  _nptsVsPtMinus = book("nptsVsPtMinus","nptsVsPtMinus",40, 0., 4., 50, 0., 50.);
  
  _nptsVsEta      = book("nptsVsEta",     "nptsVsEta",     40, -2., 2., 50, 0., 50.);
  _nptsVsEtaPlus  = book("nptsVsEtaPlus", "nptsVsEtaPlus", 40, -2., 2., 50, 0., 50.);
  _nptsVsEtaMinus = book("nptsVsEtaMinus","nptsVsEtaMinus",40, -2., 2., 50, 0., 50.);
  _nptsVsEtaPtGt200 = book("nptsVsEtaPtGt200",     "nptsVsEtaPtGt200",     40, -2., 2., 50, 0., 50.);
  _nptsVsEtaPtLt200 = book("nptsVsEtaPtLt200",     "nptsVsEtaPtLt200",     40, -2., 2., 50, 0., 50.);

  _nptsVsPhi = book("nptsVsPhi","nptsVsPhi",100,-3.1415927,3.1415927, 50, 0., 50.);
  _nptsVsPhiPlus = book("nptsVsPhiPlus","nptsVsPhiPlus",100,-3.1415927,3.1415927, 50, 0., 50.);
  _nptsVsPhiMinus = book("nptsVsPhiMinus","nptsVsPhiMinus",100,-3.1415927,3.1415927, 50, 0., 50.);
  
  _xLastHitVsXLastNode = book("xLastHitVsXLastNode","xLastHitVsXLastNode",200,0.,200.,200,0.,200.);
  _xLastHitVsXLastNode1 = book("xLastHitVsXLastNode1","xLastHitVsXLastNode1",200,0.,200.,200,0.,200.);
  _xLastHitVsXLastNode2 = book("xLastHitVsXLastNode2","xLastHitVsXLastNode2",200,0.,200.,200,0.,200.);

  _chi2 = book("chi2","chi2",100,0.,10.);
  _chi2VsNpts = book("chi2VsNpts","chi2VsNpts",50,0.,50.,100,0.,10.);

  _yVsXPosEta = book("yVsXPosEta","yVsXPosEta",800,-200.,200., 200,-200.,200.);
  _yVsXNegEta = book("yVsXNegEta","yVsXNegEta",800,-200.,200., 200,-200.,200.);

  //cout <<"StiTrackingPlots::StiTrackingPlots() -I- Done"<<endl;
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
  for (TrackToTrackMap::const_iterator trackIt = mTrackStore->begin(); 
       trackIt!=mTrackStore->end();
       ++trackIt)
    {
      const StiTrack* track = (*trackIt).second;
      const StiKalmanTrack * kTrack = dynamic_cast<const StiKalmanTrack *>(track);
      if(!track) continue; 
      double nPts = track->getPointCount();
      if(nPts<15) continue;
      double phi = track->getPhi();
      double eta = track->getPseudoRapidity();
      double thePt  = track->getPt();
      double dca = track->getDca();
      //double p[3], e[3];
      //track->getMomentum(p,e);
      
      _eta->Fill(eta);
      _phi->Fill(phi);
      _pt->Fill(thePt);
      _globalDca->Fill(dca);
      if (nPts>=40) _dca40->Fill(dca); 
      _nptsVsPt->Fill(thePt,nPts);
      _nptsVsEta->Fill(eta,nPts);
      _nptsVsPhi->Fill(phi,nPts);
      if (thePt>0.2)
	_nptsVsEtaPtGt200->Fill(eta,nPts);
      else
	_nptsVsEtaPtLt200->Fill(eta,nPts);
      if (nPts>5)
	{
	  double chi2 = track->getChi2()/(nPts-5);
	  _chi2->Fill(chi2);
	  _chi2VsNpts->Fill(nPts,chi2);
	}
      if (kTrack)
	{
	  //cout << "StiTrackPlots::fill() -I- kTrack OK";
	  double x1 = kTrack->getInnerMostNode()->_x;
	  double z = kTrack->getInnerMostNode()->_p1;
	  //cout << " x1:"<<x1;
	  double x2 = kTrack->getInnerMostHitNode()->_x;
	  //cout << " x2:"<<x2;
	  _xLastHitVsXLastNode->Fill(x1,x2);
	  if (fabs(eta)<0.4)
	    _xLastHitVsXLastNode1->Fill(x1,x2);
	  else if (fabs(eta)<0.8 && fabs(eta)>0.5 )
	    _xLastHitVsXLastNode2->Fill(x1,x2);
	  
	  StiKalmanTrackNode * aNode = static_cast<StiKalmanTrackNode *>(kTrack->getInnerMostNode());
	  double x3 = aNode->_x;

	  if(nPts>40 && x2<2. && x3<5. && fabs(z)<50. && thePt>0.4)
	    {
	      double radLength = kTrack->getTrackRadLength();
	      if (fabs(eta)<0.4)
		{
		  _radLengthVsPhi->Fill(180.*phi/3.1415,radLength);
		  _radLengthVsPhiProf->Fill(180.*phi/3.1415,radLength);
		}
	      _radLengthVsEta->Fill(eta,radLength);
	      _radLengthVsEtaProf->Fill(eta,radLength);
	    }
	  //cout << " - Filled OK";
	}
      //else
	//cout << "StiTrackPlots::fill() -W- kTrack==0"<<endl;

      if (track->getCharge()>0) 
	{
	  _etaPlus->Fill(eta);
	  _phiPlus->Fill(phi);
	  _ptPlus->Fill(thePt);
	  _globalDcaPlus->Fill(dca);
	  if (nPts>=40) _dca40Plus->Fill(dca);
	  _nptsVsPtPlus->Fill(thePt,nPts);
	  _nptsVsEtaPlus->Fill(eta,nPts);
	  _nptsVsPhiPlus->Fill(phi,nPts);
	}
      else
	{
	  _etaMinus->Fill(eta);
	  _phiMinus->Fill(phi);
	  _ptMinus->Fill(thePt);
	  _globalDcaMinus->Fill(dca);
	  if (nPts>=40) _dca40Minus->Fill(dca);
	  _nptsVsPtMinus->Fill(thePt,nPts);
	  _nptsVsEtaMinus->Fill(eta,nPts);
	  _nptsVsPhiMinus->Fill(phi,nPts);
	}

    }//end loop over tracks
  //cout <<"StiTrackingPlots::fill() -I- Done Filling histos" <<endl;

  StiHitContainer * hitContainer = StiToolkit::instance()->getHitContainer();
  HitVectorType hits = hitContainer->getAllHits();
  HitVectorType::const_iterator iter;
  for (iter=hits.begin();iter!=hits.end();++iter)
    {
      double x = (*iter)->x_g();
      double y = (*iter)->y_g();
      double z = (*iter)->z_g();
      if (z>0)
	_yVsXPosEta->Fill(x,y);
      else
	_yVsXNegEta->Fill(x,y);
    }
}
  


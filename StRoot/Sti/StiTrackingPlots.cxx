/*
 * $Id: StiTrackingPlots.cxx,v 2.13 2003/11/24 18:37:20 andrewar Exp $
 *
 *
 * $Log: StiTrackingPlots.cxx,v $
 * Revision 2.13  2003/11/24 18:37:20  andrewar
 * Reduced number of bins in 3D hists. There were memory problems.
 *
 * Revision 2.12  2003/11/14 22:31:57  andrewar
 * Added isPrimary() cut so only Primary tracks are histogrammed
 * (not a mix of Primary and global).
 *
 * Revision 2.11  2003/09/02 17:59:42  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 2.10  2003/07/30 19:19:23  pruneau
 * sigh
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

#include "Stiostream.h"
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

  mGDcavNptsvEtaA = book("mGDcavNptsvEtaA",
				"Global DCA vs. Npts vs. Eta, All charges",
				100,0.,20.,6,0.,645.,30,-2.,2.);
  //mPDcavNptsvEtaA = book("mPDcavNptsvEtaA",
  //			"Primary DCA vs. Npts vs. Eta, All charges",
  //			100,0.,20.,645,0.,645.,30,-2.0,2.);
  mGDcavNptsvPtA = book("mGDcavNptsvPtA",
				"Global DCA vs. Npts vs. Pt, All charges",
				100,0.,20.,6,0.,645.,10,0.,5.);
  //mPDcavNptsvPtA = book("mPDcavNptsvPtA",
  //			"Global DCA vs. Npts vs. Pt, All charges",
  //			100,0.,20.,645,0.,645.,50,0.,5.);
  
  mNptsvPtvEtaA = book("mNptsvPtvEtaA",
				"Npts vs. Pt. vs. Eta, All charges",
				6,0.,645.,30,-2.,2., 50,0.,5.);
  mGDcavEtavPtA = book("mGDcavEtavPtA",
		       "Global DCA vs. Eta vs. Pt",
       		      100,0.,20.,10,-2.,2.,10,0.,5.);
  //mPDcavEtavPtA= book("mPDcavEtavPtA",
  //		      "Primary DCA vs. Eta vs. Pt"
  //		      100,0,20.,20,-2.,2.,50,0.,5.);

  mGDcavNptsvEtaP = book("mGDcavNptsvEtaP",
				"Global DCA vs. Npts vs. Eta, P All charges",
				20,0.,20.,6,0.,645.,15,-2.,2.);
  //mPDcavNptsvEtaP = book("mPDcavNptsvEtaP",
  //			"Primary DCA vs. Npts vs. Eta, All charges",
  //			100,0.,20.,645,0.,645.,30,-2.0,2.);
  mGDcavNptsvPtP = book("mGDcavNptsvPtP",
				"Global DCA vs. Npts vs. Pt, All charges",
				20,0.,20.,6,0.,645.,10,0.,5.);
  //mPDcavNptsvPtP = book("mPDcavNptsvPtP",
  //			"Global DCA vs. Npts vs. Pt, All charges",
  //			100,0.,20.,645,0.,645.,50,0.,5.);
  
  mNptsvPtvEtaP = book("mNptsvPtvEtaP",
				"Npts vs. Pt. vs. Eta, All charges",
				645,0.,645.,4,-2.,2., 10,0.,5.);
  mGDcavEtavPtP = book("mGDcavEtavPtP",
		       "Global DCA vs. Eta vs. Pt",
		       10,0,20.,4,-2.,2.,10,0.,5.);
  //mPDcavEtavPtP= book("mPDcavEtavPtP",
  //		      "Primary DCA vs. Eta vs. Pt"
  //		      100,0,20.,20,-2.,2.,50,0.,5.);

  mGDcavNptsvEtaM = book("mGDcavNptsvEtaM",
				"Global DCA vs. Npts vs. Eta, P All charges",
				10,0.,20.,6,0.,645.,4,-2.,2.);
  //mPDcavNptsvEtaM = book("mPDcavNptsvEtaM",
  //			"Primary DCA vs. Npts vs. Eta, All charges",
  //			100,0.,20.,645,0.,645.,30,-2.0,2.);
  mGDcavNptsvPtM = book("mGDcavNptsvPtM",
				"Global DCA vs. Npts vs. Pt, All charges",
				10,0.,20.,6,0.,645.,10,0.,5.);
  //mPDcavNptsvPtM = book("mPDcavNptsvPtM",
  //			"Global DCA vs. Npts vs. Pt, All charges",
  //			100,0.,20.,645,0.,645.,50,0.,5.);
  
  mNptsvPtvEtaM = book("mNptsvPtvEtaM",
  			"Npts vs. Pt. vs. Eta, All charges",
  			6,0.,645.,4,-2.,2., 10,0.,5.);
  mGDcavEtavPtM = book("mGDcavEtavPtM",
		       "Global DCA vs. Eta vs. Pt",
       		      100,0,20.,20,-2.,2.,50,0.,5.);
  //mPDcavEtavPtM= book("mPDcavEtavPtM",
  //			      "Primary DCA vs. Eta vs. Pt"
  //			      100,0,20.,20,-2.,2.,50,0.,5.);

  
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
      if(!kTrack->isPrimary()) continue;
      double nPts = track->getPointCount();
      if(nPts<5) continue;
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

      int tpcPoints = (kTrack->getNodes(1)).size();
      int svtPoints = (kTrack->getNodes(2)).size();
      int mPts=100*svtPoints+tpcPoints;
      
      mGDcavNptsvEtaA->Fill(dca,mPts,eta);
      mGDcavNptsvPtA->Fill(dca,mPts, thePt);
      mNptsvPtvEtaA->Fill(mPts,thePt,eta);
      mGDcavEtavPtA->Fill(dca,eta,thePt);

      if(track->getCharge())
	{//charge > 0
	  mGDcavNptsvEtaP->Fill(dca,mPts,eta);
	  mGDcavNptsvPtP->Fill(dca,mPts, thePt);
	  mNptsvPtvEtaP->Fill(mPts,thePt,eta);
	  mGDcavEtavPtP->Fill(dca,eta,thePt);
	}
      else
	{//charge < 0
	  mGDcavNptsvEtaM->Fill(dca,mPts,eta);
	  mGDcavNptsvPtM->Fill(dca,mPts, thePt);
	  mNptsvPtvEtaM->Fill(mPts,thePt,eta);
	  mGDcavEtavPtM->Fill(dca,eta,thePt);
	}
      

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
	  //cout << " x1:"<<x1;
	  double x2 = kTrack->getInnerMostHitNode()->_x;
	  //cout << " x2:"<<x2;
	  _xLastHitVsXLastNode->Fill(x1,x2);
	  if (fabs(eta)<0.4)
	    _xLastHitVsXLastNode1->Fill(x1,x2);
	  else if (fabs(eta)<0.8 && fabs(eta)>0.5 )
	    _xLastHitVsXLastNode2->Fill(x1,x2);
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

}


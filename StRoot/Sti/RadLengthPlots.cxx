#include "RadLengthPlots.h"
#include "Sti/StiDefaultTrackFilter.h"
#include "Sti/StiTrackContainer.h"
#include "Sti/StiTrack.h"
#include "Sti/StiTrackNode.h"
#include "Sti/StiKalmanTrack.h"
#include "Sti/StiKalmanTrackNode.h"

RadLengthPlots::RadLengthPlots(const string & name, const string & description)
  : StiHistograms(name,description)
{ 
  initialize();
}
 

void RadLengthPlots::initialize()
{
  cout <<"RadLengthPlots::RadLengthPlots() -I- Started"<<endl;
  _radLengthVsPhi =book("radLengthVsPhi","Absorption Length (%) vs. Phi",120,-180.,180., 400,0,0.2);
  _radLengthVsEta =book("radLengthVsEta","Absorption Length (%) vs. Eta",60,-1.5,1.5, 400,0,0.2);
  _radLengthVsPhiProf =bookProfile("radLengthVsPhiProf","Absorption Length Profile(%) vs. Phi",120,-180.,180.);
  _radLengthVsEtaProf =bookProfile("radLengthVsEtaProf","Absorption Length Profile(%) vs. Eta",60,-1.5,1.5);
}
  
RadLengthPlots::~RadLengthPlots()
{
  cout << "RadLengthPlots::~RadLengthPlots() -I- Done"<<endl;
}

void RadLengthPlots::fill(StiTrackContainer *mTrackStore)
{
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
      double dca = track->getDca(); if(dca){}
      if (kTrack)
	{
	  StiKalmanTrackNode * innerMostNode = static_cast<StiKalmanTrackNode *>(kTrack->getInnerMostNode());
	  double x1 = innerMostNode->_x; if(x1){}
	  double z  = innerMostNode->_p1;
	  double x2 = kTrack->getInnerMostHitNode()->_x;
	  StiKalmanTrackNode * secNode =  static_cast<StiKalmanTrackNode *>(innerMostNode->getParent());
	  double x3 = secNode->_x;if(x3){}
	  if(nPts>40 && x2<2. && fabs(z)<50. && thePt>0.4)
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
	}
    }
}


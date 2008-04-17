/***************************************************************************
 *
 * $Id: StTpcFourPMaker.cxx,v 1.2 2008/04/17 20:12:18 tai Exp $
 * 
 * Author: Thomas Henry February 2003
 ***************************************************************************
 *
 * Description:  Maker which creates a list of Four Momentums from the TPC
 * orresponding to charged particles.
 *
 ***************************************************************************
 *
 * Revision 1.0  2003/02/20 thenry
 *
 **************************************************************************/
#include <string>
#include <iostream>

#include "StChain.h"
#include "StDetectorId.h"
#include "StEventTypes.h"
#include "StMessMgr.h"
#include "StIOMaker/StIOMaker.h"
#include "StEvent.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StJetMaker/StMuTrackFourVec.h"

#include "StEmcClusterCollection.h"
#include "StMuDSTMaker/COMMON/StMuEmcCollection.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StEmcPoint.h"
#include "StJetMaker/StFourPMakers/StTpcFourPMaker.h"

ClassImp(StTpcFourPMaker)
  
StTpcFourPMaker::StTpcFourPMaker(const char* name, StMuDstMaker* uDstMaker) 
  : StFourPMaker(name, uDstMaker){
}

Int_t StTpcFourPMaker::Make() {
  cout <<" Start StTpcFourPMaker :: "<< GetName() <<" mode="<<m_Mode<<endl;   

  // TODO: construct tracks out of (primary) tracks. 
  StMuDst* uDst = muDst->muDst();

  // Add TPC tracks
  int nTracks = uDst->numberOfPrimaryTracks();
  tracks.clear();
  for(int i = 0; i < nTracks; i++)
  {
    StMuTrack *t = uDst->primaryTracks(i);
    if(t->flag()<=0) continue;
    if(t->eta() < GetEtaLow()) continue;
    if(t->eta() > GetEtaHigh()) continue;
    if(static_cast<double>(t->nHits())
       /static_cast<double>(t->nHitsPoss()) < .51)
      continue;
    StThreeVectorF mom = t->momentum();
    float mommag2 = mom.mag2();
    float ee = ::sqrt(me*me + mommag2);
    float epr = ::sqrt(mpr*mpr + mommag2);
    float epi = ::sqrt(mpi*mpi + mommag2);
    float ek = ::sqrt(mk*mk + mommag2);
    float probPion, probKaon, probProton, probElectron;
    if(!t) { 
      probPion = 1.0; 
      probKaon = probProton = probElectron = 0; 
      continue; }
    probPion = t->pidProbPion();
    if((probPion < 0) || (probPion > 1)) probPion = 0;
    probKaon = t->pidProbKaon();
    if((probKaon < 0) || (probKaon > 1)) probKaon = 0;
    probProton = t->pidProbProton();
    if((probProton < 0) || (probProton > 1)) probProton = 0;
    probElectron = t->pidProbElectron();
    if((probElectron < 0) || (probElectron > 1)) probElectron = 0;
    if((probPion == 0) && (probKaon == 0) && 
       (probProton == 0) && (probElectron == 0))
      {
	probPion = 1.0;
	return kStOK;
      }
    double sum = probPion + probKaon + probProton + probElectron;
    if(fabs(sum-1.0) < .01)
      {
	probPion /= sum;
	probKaon /= sum;
	probProton /= sum;
	probElectron /= sum;
      }
    
    float energy = probElectron*ee + probProton*epr + probPion*epi 
      + probKaon*ek;
    // float energy = t->pidProbElectron()*ee + t->pidProbProton()*epr +
    //t->pidProbPion()*epi + t->pidProbKaon()*ek;
    StLorentzVectorF P(energy, mom);
    StMuTrackFourVec& track = tPile[i];

    track.Init(t, P, i, kTpcId);
    //cout <<"StTpc4P\tInitTrack kTpcId:\t"<<track<<endl;
    tracks.push_back(&track);
  }

  return kStOk;
}




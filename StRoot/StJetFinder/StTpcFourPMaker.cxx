/***************************************************************************
 *
 * $Id: StTpcFourPMaker.cxx,v 1.4 2003/05/09 20:48:19 thenry Exp $
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
#include <string.h>
#include <iostream.h>

#include "StChain.h"
#include "StEventTypes.h"
#include "StMessMgr.h"
#include "StIOMaker/StIOMaker.h"
#include "StEvent.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StSpinMaker/StMuTrackFourVec.h"

#include "StEmcClusterCollection.h"
#include "StMuDSTMaker/COMMON/StMuEmcCollection.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StEmcPoint.h"
#include "StTpcFourPMaker.h"

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
    StThreeVectorF mom = t->momentum();
    float mass = t->pidProbElectron()*me + t->pidProbProton()*mpr +
      t->pidProbPion()*mpi + t->pidProbKaon()*mk;
    StLorentzVectorF P(sqrt(mass*mass + mom.mag2()), mom);
    StMuTrackFourVec& track = tPile[i];
    track.Init(t, P, i);
    tracks.push_back(&track);
  }

  return kStOk;
}




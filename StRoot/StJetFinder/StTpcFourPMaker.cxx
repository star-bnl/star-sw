/***************************************************************************
 *
 * $Id: StTpcFourPMaker.cxx,v 1.1 2003/04/04 21:36:06 thenry Exp $
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
#include "../StSpinMaker/StMuTrackFourVec.h"

#include "StEmcClusterCollection.h"
#include "StMuDSTMaker/COMMON/StMuEmcCollection.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StEmcPoint.h"
#include "StTpcFourPMaker.h"

ClassImp(StTpcFourPMaker)
  
StTpcFourPMaker::StTpcFourPMaker(const char* name, StMuDstMaker* uDstMaker) 
  : StFourPMaker(name, uDstMaker){
    tracks = new StMuTrackFourVec[MAXTRACKS];
    nTracks = 0;
}

Int_t StTpcFourPMaker::Make() {
  cout <<" Start StTpcFourPMaker :: "<< GetName() <<" mode="<<m_Mode<<endl;   

  // TODO: construct tracks out of (primary) tracks. 
  StMuDst* uDst = muDst->muDst();

  // Add TPC tracks
  nTracks = uDst->numberOfPrimaryTracks();
  for(int i = 0; i < nTracks; i++)
  {
    StMuTrack *t = uDst->primaryTracks(i);
    StThreeVectorF mom = t->momentum();
    float mass = t->pidProbElectron()*me + t->pidProbProton()*mp +
      t->pidProbPion()*mpi + t->pidProbKaon()*mk;
    StLorentzVectorF P(sqrt(mass*mass + mom.mag2()), mom);
    tracks[i].Init(t, P, i);
  }

  return kStOk;
}



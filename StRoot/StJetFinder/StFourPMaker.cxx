/***************************************************************************
 *
 * $Id: StFourPMaker.cxx,v 1.1 2003/04/04 21:34:59 thenry Exp $
 * 
 * Author: Thomas Henry February 2003
 ***************************************************************************
 *
 * Description:  Maker which creates a list of Four Momentums (base class)
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
#include "StFourPMaker.h"

ClassImp(StFourPMaker)
  
StFourPMaker::StFourPMaker(const char* name, StMuDstMaker* uDstMaker) 
  : StMaker(name), muDst(uDstMaker){
    tracks = NULL;
    nTracks = 0;
    muEmcCol = new StMuEmcCollection();
    me = .000511;
    mp = .9383;
    mpi = .1396;
    mk = .4937;
}

StMuEmcCollection* StFourPMaker::getStMuEmcCollection(void)
{
  if(muDst)
  {
    StMuDst* uDst = muDst->muDst();
    muEmcCol = uDst->emcCollection();
  }
  return muEmcCol;
}

Int_t StFourPMaker::Init() 
{
  return StMaker::Init();
}

Int_t StFourPMaker::Make() {
  cout <<" Start StFourPMaker :: "<< GetName() <<" mode="<<m_Mode<<endl;   

  // This class does nothing!

  return kStOk;
}

Int_t StFourPMaker::Finish()
{
  //StMaker::Finish();
  return StMaker::Finish();
}


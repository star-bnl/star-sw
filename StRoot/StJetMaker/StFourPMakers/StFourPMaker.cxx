// $Id: StFourPMaker.cxx,v 1.4 2008/05/08 21:11:49 tai Exp $
#include <string>
#include <iostream>

#include "StChain.h"
#include "StEventTypes.h"
#include "StMessMgr.h"
#include "StIOMaker/StIOMaker.h"
#include "StEvent.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StJetMaker/StMuTrackFourVec.h"

#include "StEmcClusterCollection.h"
#include "StMuDSTMaker/COMMON/StMuEmcCollection.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StEmcPoint.h"
#include "StJetMaker/StFourPMakers/StFourPMaker.h"

ClassImp(StFourPMaker)
  
StFourPMaker::StFourPMaker(const char* name, StMuDstMaker* uDstMaker) 
  : StMaker(name)
  , eta_high_lim(2.0)
  , eta_low_lim(-2.0)
{
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

void StFourPMaker::Clear(const Option_t* o)
{
    tracks.clear();
    return StMaker::Clear();
}

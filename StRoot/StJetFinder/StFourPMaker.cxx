/***************************************************************************
 *
 * $Id: StFourPMaker.cxx,v 1.6 2003/09/11 18:14:04 thenry Exp $
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
#include "StSpinMaker/StMuTrackFourVec.h"

#include "StEmcClusterCollection.h"
#include "StMuDSTMaker/COMMON/StMuEmcCollection.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StEmcPoint.h"
#include "StFourPMaker.h"

ClassImp(StFourPMaker)
  
StFourPMaker::StFourPMaker(const char* name, StMuDstMaker* uDstMaker) 
  : StMaker(name), muDst(uDstMaker), me(0.000511), mpr(0.9383), 
    mpi(0.1396), mk(0.4937), eta_high_lim(1.6), eta_low_lim(-1.6){
    muEmcCol = new StMuEmcCollection();
    //me = .000511;	
    //mpr = .9383;
    //mpi = .1396;
    //mk = .4937;
    eta_high_lim = 1.6;
    eta_low_lim = -1.6;
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





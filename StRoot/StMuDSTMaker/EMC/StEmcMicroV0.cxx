//###########################################################
// EMC Micro Event
// Author: Alexandre A. P. Suaide
// initial version 08/2001
//
// See README for details
//########################################################### 
#include "StEmcMicroV0.h"

ClassImp(StEmcMicroV0);

StEmcMicroV0::StEmcMicroV0()
{
  mTracks[0]=NULL;
  mTracks[1]=NULL;
  mX=0;
  mY=0;
  mZ=0;
}
StEmcMicroV0::~StEmcMicroV0()
{
  for(Int_t i=0;i<2;i++) if (mTracks[i]) delete mTracks[i];
}
StEmcMicroV0::StEmcMicroV0(StEmcMicroV0 * v0)
{
  mX=v0->getVertexX();
  mY=v0->getVertexY();
  mZ=v0->getVertexZ();
  
  for(Int_t i=0;i<2;i++)
  {
    StEmcMicroTrack *tr =v0->getDaughter(i);
    if(tr) mTracks[i] = new StEmcMicroTrack(tr);    
  }
}



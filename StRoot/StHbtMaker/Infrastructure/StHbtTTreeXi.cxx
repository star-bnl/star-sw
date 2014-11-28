/***********************************************************************
 *
 * $Id: StHbtTTreeXi.h,v 1.0 1999/09/07
 *
 * Authors: Frank Laue, BNL, laue@bnl.gov
 *
 ***********************************************************************/

#include "StHbtTTreeXi.h"
#include "StHbtEvent.hh"
#include "StHbtV0.hh"
#include "StHbtXi.hh"


//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
StHbtTTreeXi::StHbtTTreeXi(const StHbtEvent* event, const StHbtXi* xi) : StHbtTTreeV0(event, xi) {
  mCharge = xi->mCharge;      
  mDecayVertexXiX = xi->mDecayVertexXi.x();
  mDecayVertexXiY = xi->mDecayVertexXi.y();
  mDecayVertexXiZ = xi->mDecayVertexXi.z();
  
  mDcaXiDaughters = xi->mDcaXiDaughters;
  mDcaBachelorToPrimVertex = xi->mDcaBachelorToPrimVertex;
  mDcaXiToPrimVertex = xi->mDcaXiToPrimVertex;
  mMomBachelorX = xi->mMomBachelor.x();
  mMomBachelorY = xi->mMomBachelor.y();
  mMomBachelorZ = xi->mMomBachelor.z();
  
  mKeyBachelor = xi->mKeyBachelor;
  mTopologyMapBachelor[0] = xi->mTopologyMapBachelor[0];
  mTopologyMapBachelor[1] = xi->mTopologyMapBachelor[1];
  
  mChi2Xi = xi->mChi2Xi;
  mClXi = xi->mChi2Xi;
  mChi2Bachelor = xi->mChi2Bachelor;
  mClBachelor = xi->mClBachelor;
  
  mDedxBachelor = xi->mDedxBachelor;
  mNumDedxBachelor = xi->mNumDedxBachelor;
  
}

ClassImp(StHbtTTreeXi)

/***********************************************************************
 *
 * $Log: StHbtTTreeXi.cxx,v $
 * Revision 1.2  2001/12/05 15:10:34  laue
 * Boris' updates (mainly access functions)
 *
 *
 *
 ***********************************************************************/

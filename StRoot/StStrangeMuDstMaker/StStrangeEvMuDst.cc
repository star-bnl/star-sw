/***********************************************************************
 *
 * $Id: StStrangeEvMuDst.cc,v 1.2 2000/03/29 20:52:13 genevb Exp $
 *
 * Authors: Gene Van Buren, UCLA, 24-Mar-2000
 *          Peter G. Jones, University of Birmingham, 19-Aug-1999
 *
 ***********************************************************************
 *
 * Description: Strangeness event micro dst class
 *
 ***********************************************************************
 *
 * $Log: StStrangeEvMuDst.cc,v $
 * Revision 1.2  2000/03/29 20:52:13  genevb
 * Added StKinkMuDst, replaced arrays
 *
 * Revision 1.1  2000/03/29 03:10:07  genevb
 * Introduction of Strangeness Micro DST package
 *
 *
 ***********************************************************************/
#include "StStrangeEvMuDst.hh"
#include "StPrimaryVertex.h"
ClassImp(StStrangeEvMuDst)

StStrangeEvMuDst::StStrangeEvMuDst() { 
}

void StStrangeEvMuDst::Fill(StPrimaryVertex* primaryVertex) {

  mRun = 0;
  mEvent = 0;

  mPrimaryVertexX = primaryVertex->position().x();
  mPrimaryVertexY = primaryVertex->position().y();
  mPrimaryVertexZ = primaryVertex->position().z();
}

StStrangeEvMuDst::~StStrangeEvMuDst() {
}

Float_t StStrangeEvMuDst::primaryVertex(Int_t n) {
  switch (n) {
    case (2): return mPrimaryVertexZ;
    case (1): return mPrimaryVertexY;
    default : return mPrimaryVertexX;
  }
}

/***********************************************************************
 *
 * $Id: StStrangeEvMuDst.cc,v 2.0 2000/06/02 22:11:54 genevb Exp $
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
 * Revision 2.0  2000/06/02 22:11:54  genevb
 * New version of Strangeness micro DST package
 *
 * Revision 1.3  2000/03/31 03:20:24  jones
 * Added topology map to V0/Xi; access funcs for each data member
 *
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


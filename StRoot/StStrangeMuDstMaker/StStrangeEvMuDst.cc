/***********************************************************************
 *
 * $Id: StStrangeEvMuDst.cc,v 1.1 2000/03/29 03:10:07 genevb Exp $
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

  mPrimaryVertex[0] = primaryVertex->position().x();
  mPrimaryVertex[1] = primaryVertex->position().y();
  mPrimaryVertex[2] = primaryVertex->position().z();
}

StStrangeEvMuDst::~StStrangeEvMuDst() {
}


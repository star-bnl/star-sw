/***********************************************************************
 *
 * $Id: StEvMiniDst.cc,v 1.2 1999/11/19 19:44:47 genevb Exp $
 *
 * Author: Peter G. Jones, University of Birmingham, 04-Jun-1999
 *
 ***********************************************************************
 *
 * Description: Event mini dst class
 *
 ***********************************************************************
 *
 * $Log: StEvMiniDst.cc,v $
 * Revision 1.2  1999/11/19 19:44:47  genevb
 * Modified for StEvent 2.0
 *
 * Revision 1.1  1999/09/02 09:04:55  jones
 * Added StEvMiniDst class, New file handling, Partially implemented TTrees
 *
 *
 ***********************************************************************/
#include "StEvMiniDst.hh"
#include "StPrimaryVertex.h"
ClassImp(StEvMiniDst)

StEvMiniDst::StEvMiniDst() { 
}

StEvMiniDst::StEvMiniDst(StPrimaryVertex* primaryVertex) {

  mRun = 0;
  mEvent = 0;

  mPrimaryVertex[0] = primaryVertex->position().x();
  mPrimaryVertex[1] = primaryVertex->position().y();
  mPrimaryVertex[2] = primaryVertex->position().z();
}

StEvMiniDst::~StEvMiniDst() {
}


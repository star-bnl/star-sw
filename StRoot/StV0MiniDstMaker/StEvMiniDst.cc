/***********************************************************************
 *
 * $Id: StEvMiniDst.cc,v 1.1 1999/09/02 09:04:55 jones Exp $
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
 * Revision 1.1  1999/09/02 09:04:55  jones
 * Added StEvMiniDst class, New file handling, Partially implemented TTrees
 *
 *
 ***********************************************************************/
#include "StEvMiniDst.hh"
#include "StVertex.h"
ClassImp(StEvMiniDst)

StEvMiniDst::StEvMiniDst() { 
}

StEvMiniDst::StEvMiniDst(StVertex* primaryVertex) {

  mRun = 0;
  mEvent = 0;

  mPrimaryVertex[0] = primaryVertex->position().x();
  mPrimaryVertex[1] = primaryVertex->position().y();
  mPrimaryVertex[2] = primaryVertex->position().z();
}

StEvMiniDst::~StEvMiniDst() {
}


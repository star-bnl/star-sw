/***********************************************************************
 *
 * $Id: StStrangeEvMuDst.cc,v 3.1 2000/09/07 02:22:56 genevb Exp $
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
 * Revision 3.1  2000/09/07 02:22:56  genevb
 * Now using STAR standard uncorrected primary track multiplicity
 *
 * Revision 3.0  2000/07/14 12:56:49  genevb
 * Revision 3 has event multiplicities and dedx information for vertex tracks
 *
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
#include "StEventTypes.h"
#include "StuRefMult.hh"

ClassImp(StStrangeEvMuDst)

StStrangeEvMuDst::StStrangeEvMuDst() { 
}

void StStrangeEvMuDst::Fill(StEvent& event) {

  mRun = event.runId();
  mEvent = event.id();
  
  mGlobalTracks = 0;
  StSPtrVecTrackNode& theNodes = event.trackNodes();
  for (unsigned int i=0; i<theNodes.size(); i++) {
    for (unsigned int j=0; j<theNodes[i]->entries(global); j++) {
      if (theNodes[i]->track(global,j)->flag() > 0)
        mGlobalTracks++;
    }
  }
  mPrimaryTracks = uncorrectedNumberOfPrimaries(event);

  StPrimaryVertex* primaryVertex = event.primaryVertex();
  if( primaryVertex ) {
    mPrimaryVertexX = primaryVertex->position().x();
    mPrimaryVertexY = primaryVertex->position().y();
    mPrimaryVertexZ = primaryVertex->position().z();
  } else {
    mPrimaryVertexX = 0.;
    mPrimaryVertexY = 0.;
    mPrimaryVertexZ = 0.;
  }
}

StStrangeEvMuDst::~StStrangeEvMuDst() {
}


/***************************************************************************
 *
 * $Id: StarMuDst.cxx,v 1.1 2002/03/05 15:41:08 jeromel Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 *
 ***************************************************************************/

#include "StarMuDst.h"

#include "StarMuDstMaker.h"
#include "StarMuDebug.h"
#include "TClonesArray.h"
#include "TTree.h"


TClonesArray* StarMuDst::arrays[__NARRAYS__] = {0,0,0,0,0,0,0,0,0};
TClonesArray* StarMuDst::strangeArrays[__NSTRANGEARRAYS__] = {0,0,0,0,0,0,0,0,0,0,0};


StarMuDst::StarMuDst() {
  DEBUGMESSAGE("");
  /* no-op */
}

void StarMuDst::unset() {
  for ( int i=0; i<__NARRAYS__; i++) {
    arrays[i] = 0;
  }
  for ( int i=0; i<__NSTRANGEARRAYS__; i++) {
    strangeArrays[i] = 0;
  }
}

void StarMuDst::set(StarMuDstMaker* maker) {
  DEBUGMESSAGE2("");
  if (!maker) { DEBUGVALUE(maker); return;}
  for ( int i=0; i<__NARRAYS__; i++) {
    arrays[i] = maker->mArrays[i];
  }
  for ( int i=0; i<__NSTRANGEARRAYS__; i++) {
    strangeArrays[i] = maker->mStrangeArrays[i];
  }
}

ClassImp(StarMuDst)

/***************************************************************************
 *
 * $Log: StarMuDst.cxx,v $
 * Revision 1.1  2002/03/05 15:41:08  jeromel
 * First version of Frank's Commone MicroDST.
 *
 *
 **************************************************************************/

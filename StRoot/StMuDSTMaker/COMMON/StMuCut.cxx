/***************************************************************************
 *
 * $Id: StMuCut.cxx,v 1.5 2004/11/24 15:28:02 jeromel Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 *
 ***************************************************************************/

#include "StMuCut.h"
#include "Stiostream.h"

ClassImp(StMuCut)

StMuCut::StMuCut(){
  int i;
  for (i=0; i < 2 ; i++){
    mNStEvent[i]    = 0;
    mNStTrack[i]    = 0;
    mNStV0Vertex[i] = 0;
    mNStXiVertex[i] = 0;
    mNStKinkMuDst[i]= 0;
    mNStV0MuDst[i]  = 0;
    mNStXiMuDst[i]  = 0;
    mNStKinkMuDst[i]= 0;
  }
};

/***************************************************************************
 *
 * $Log: StMuCut.cxx,v $
 * Revision 1.5  2004/11/24 15:28:02  jeromel
 * Un-initialized arrays leads to wrong counter (fixed)
 *
 * Revision 1.4  2003/09/02 17:58:44  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.3  2002/09/11 21:02:41  laue
 * added cut on track encoded method for ITTF
 *
 * Revision 1.2  2002/05/04 23:56:29  laue
 * some documentation added
 *
 * Revision 1.1  2002/03/08 17:04:17  laue
 * initial revision
 *
 *
 **************************************************************************/

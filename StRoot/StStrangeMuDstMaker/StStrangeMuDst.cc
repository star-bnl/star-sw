/***********************************************************************
 *
 * $Id: StStrangeMuDst.cc,v 3.1 2001/11/05 23:41:06 genevb Exp $
 * $Log: StStrangeMuDst.cc,v $
 * Revision 3.1  2001/11/05 23:41:06  genevb
 * Add more dEdx, B field info, careful of changes to TTree unrolling
 *
 * Revision 3.0  2000/07/14 12:56:49  genevb
 * Revision 3 has event multiplicities and dedx information for vertex tracks
 *
 * Revision 2.0  2000/06/05 05:19:43  genevb
 * New version of Strangeness micro DST package
 *
 *
 ***********************************************************************
 *
 * Description: Strangeness micro dst base class
 *
 ***********************************************************************/
#include "StStrangeMuDst.hh"
#include "TClass.h"
#include "StObject.h"
#include "TStreamerInfo.h"

ClassImp(StStrangeMuDst)
ClassImp(StStrangeAssoc)

StStrangeAssoc::StStrangeAssoc(Int_t indexRecoArray, Int_t indexMcArray)
{
  mIndexRecoArray = indexRecoArray;
  mIndexMcArray   = indexMcArray;
}

/***********************************************************************
 *
 * $Id: StStrangeMuDst.cc,v 3.0 2000/07/14 12:56:49 genevb Exp $
 * $Log: StStrangeMuDst.cc,v $
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

ClassImp(StStrangeMuDst)
ClassImp(StStrangeAssoc)

StStrangeAssoc::StStrangeAssoc(Int_t indexRecoArray, Int_t indexMcArray)
{
  mIndexRecoArray = indexRecoArray;
  mIndexMcArray   = indexMcArray;
}

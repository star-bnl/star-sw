/***************************************************************************
 *
 * $Id: StTpcHit.cxx,v 2.19 2011/10/17 00:13:49 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTpcHit.cxx,v $
 * Revision 2.19  2011/10/17 00:13:49  fisyak
 * Add handles for IdTruth info
 *
 * Revision 2.18  2011/03/31 19:25:13  fisyak
 * Keep ADC value for cluster
 *
 * Revision 2.17  2010/03/26 13:47:29  fisyak
 * Add methods to modify hit content
 *
 * Revision 2.16  2010/03/05 16:30:19  fisyak
 * Add hit id
 *
 * Revision 2.15  2009/11/23 16:34:07  fisyak
 * Cleanup, remove dependence on dst tables, clean up software monitors
 *
 * Revision 2.14  2009/11/10 00:40:18  ullrich
 * Changed print-out format.
 *
 * Revision 2.13  2007/10/03 21:47:35  ullrich
 * Added several new member to hold hit length info.
 *
 * Revision 2.12  2004/08/06 15:37:09  fisyak
 * Add clster id
 *
 * Revision 2.11  2004/07/15 16:36:25  ullrich
 * Removed all clone() declerations and definitions. Use StObject::clone() only.
 *
 * Revision 2.10  2004/01/13 21:01:08  fisyak
 * Add Truth and Quality information from simulation
 *
 * Revision 2.9  2001/04/05 04:00:57  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.8  2001/03/24 03:34:59  perev
 * clone() -> clone() const
 *
 * Revision 2.7  2000/07/28 23:29:42  calderon
 * Added handling of Fit Flag: use this flag to tell if the point
 * is used in the fit.
 *
 * Revision 2.6  2000/06/01 21:39:07  ullrich
 * Added member mFlag and access member flag() and setFlag().
 *
 * Revision 2.5  1999/12/01 15:56:28  ullrich
 * Renamed xxxInCluster() methods to xxxInHit()
 *
 * Revision 2.4  1999/11/11 10:19:52  ullrich
 * Inlined sector() and padrow().
 *
 * Revision 2.3  1999/11/09 19:35:25  ullrich
 * Memory now allocated using StMemoryPool via overloaded new/delete
 *
 * Revision 2.2  1999/11/04 21:41:00  ullrich
 * Added missing default constructor
 *
 * Revision 2.1  1999/10/28 22:27:07  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.0  1999/10/12 18:42:48  ullrich
 * Completely Revised for New Version
 *
 **************************************************************************/
#include "StTpcHit.h"
#include "StTrack.h"
static const char rcsid[] = "$Id: StTpcHit.cxx,v 2.19 2011/10/17 00:13:49 fisyak Exp $";

StMemoryPool StTpcHit::mPool(sizeof(StTpcHit));

ClassImp(StTpcHit)

void StTpcHit::setExtends(Float_t cl_x, Float_t cl_t, Short_t mnpad, Short_t mxpad, Short_t mntmbk, Short_t mxtmbk) {
  setPadTmbk(cl_x, cl_t);
  Short_t pad  = TMath::Nint(mMcl_x/64.);
  Short_t time = TMath::Nint(mMcl_t/64.);
  mMinpad = pad - mnpad;
  mMaxpad = mxpad - pad;
  mMintmbk = time - mntmbk;
  mMaxtmbk = mxtmbk - time;
}

ostream&  operator<<(ostream& os, const StTpcHit& v)
{
    return os << Form("Tpc s/r %3i/%3i ",v.sector(),v.padrow())
	      << *((StHit *)&v)
	      << Form(" pmin/max %3i/%3i np %2i tmin/max %3i/%3i nt %2i tm %6.2f pad %6.2f adc %4i",
		      (Int_t)  v.minPad(), (Int_t)  v.maxPad(),(Int_t) v.padsInHit(), 
		      (Int_t) v.minTmbk(), (Int_t) v.maxTmbk(),(Int_t) v.timeBucketsInHit(),
		      v.timeBucket(),v.pad(), v.adc());  
}
void   StTpcHit::Print(Option_t *option) const {cout << *this << endl;}

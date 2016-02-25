/***************************************************************************
 *
 * $Id: StHit.cxx,v 2.28 2016/02/25 17:07:14 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sept 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StHit.cxx,v $
 * Revision 2.28  2016/02/25 17:07:14  ullrich
 * Removed implentation of detector() making class pure abstract.
 *
 * Revision 2.27  2015/10/09 17:46:14  ullrich
 * Changed type of mIdTruth from ushort to int.
 *
 * Revision 2.26  2012/10/23 20:18:33  fisyak
 * Add/modify print outs
 *
 * Revision 2.25  2012/01/24 03:05:08  perev
 * Cleanup
 *
 * Revision 2.24  2011/10/17 00:13:49  fisyak
 * Add handles for IdTruth info
 *
 * Revision 2.23  2011/01/21 18:30:45  fisyak
 * fix setFlag with UShort_t
 *
 * Revision 2.22  2009/11/23 16:34:06  fisyak
 * Cleanup, remove dependence on dst tables, clean up software monitors
 *
 * Revision 2.21  2009/11/10 00:40:17  ullrich
 * Changed print-out format.
 *
 * Revision 2.20  2006/02/14 21:04:31  perev
 * WarnOff
 *
 * Revision 2.19  2006/01/19 21:49:41  ullrich
 * Changed order of initializer in constructor.
 *
 * Revision 2.18  2005/07/19 21:34:10  perev
 * quality ==> qaTruth to avoid misleading
 *
 * Revision 2.17  2005/07/06 18:57:48  fisyak
 * Add StHit print out
 *
 * Revision 2.16  2004/10/13 16:10:52  ullrich
 * Fixed bug in operator==
 *
 * Revision 2.15  2004/08/06 15:37:09  fisyak
 * Add clster id
 *
 * Revision 2.14  2004/07/30 22:28:31  fisyak
 * Add transient pointer to next Hit
 *
 * Revision 2.13  2004/07/15 16:36:24  ullrich
 * Removed all clone() declerations and definitions. Use StObject::clone() only.
 *
 * Revision 2.12  2004/03/30 15:59:08  calderon
 * Added method to set mFitFlag (new chain no longer uses tables, so must set
 * this by hand).
 *
 * Revision 2.11  2004/01/13 21:01:32  fisyak
 * Add Truth and Quality information from simulation
 *
 * Revision 2.10  2001/04/05 04:00:51  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.9  2001/03/24 03:34:50  perev
 * clone() -> clone() const
 *
 * Revision 2.8  2001/03/06 21:04:30  ullrich
 * Modified detector() method. Replaced switch
 * statement by simple static_cast.
 *
 * Revision 2.7  2000/12/08 20:21:07  genevb
 * Changed kTofPatchId -> kTofId
 *
 * Revision 2.6  2000/07/28 23:29:42  calderon
 * Added handling of Fit Flag: use this flag to tell if the point
 * is used in the fit.
 *
 * Revision 2.5  2000/07/28 19:49:28  akio
 * Change in Detector Id for Endcap SMD
 *
 * Revision 2.4  2000/06/07 09:43:17  ullrich
 * Changed return type of flag() to unsigned int
 *
 * Revision 2.3  2000/06/01 21:38:53  ullrich
 * Added member mFlag and access member flag() and setFlag().
 *
 * Revision 2.2  2000/05/19 18:33:14  ullrich
 * Minor changes (add const) to cope with modified StArray.
 *
 * Revision 2.1  1999/10/28 22:25:47  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.0  1999/10/12 18:42:17  ullrich
 * Completely Revised for New Version
 *
 **************************************************************************/
#include "StHit.h"
#include "StTrack.h"
#include "StTrackNode.h"
#include "StTrackDetectorInfo.h"

static const char rcsid[] = "$Id: StHit.cxx,v 2.28 2016/02/25 17:07:14 ullrich Exp $";

ClassImp(StHit)

//______________________________________________________________________________
StHit::StHit()
{
    mHardwarePosition	= 0;
    mCharge 		= 0;
    mTrackRefCount 	= 0;
    mFlag = mFitFlag 	= 0;
    mIdTruth = mQuality = 0;
    mId 		= 0;
    mNextHit 		= 0;
}

//______________________________________________________________________________
StHit::StHit(const StThreeVectorF& p,
             const StThreeVectorF& e,
             unsigned int hp, float q, unsigned char c, int idTruth, unsigned short quality, unsigned short id)
            :StMeasuredPoint(p),mHardwarePosition(hp),mPositionError(e),mCharge(q),
             mId(id),mIdTruth(idTruth),mQuality(quality),mFitFlag(0),mTrackRefCount(c),mFlag(0),mNextHit(0)
{
}

//______________________________________________________________________________
StHit::~StHit() { /* noop */ }

//______________________________________________________________________________
int StHit::operator==(const StHit& h) const
{
    return h.mPosition         == mPosition &&
           h.mPositionError    == h.mPositionError &&
           h.mCharge           == mCharge &&
           h.mHardwarePosition == mHardwarePosition &&
           h.mFlag             == mFlag &&
           h.mIdTruth          == mIdTruth &&
           h.mQuality          == mQuality &&
           h.mId               == mId &&
           h.mNextHit          == mNextHit;
}

//______________________________________________________________________________
int StHit::operator!=(const StHit& h) const
{
    return !(*this == h);  // use operator==()
}

//______________________________________________________________________________
void StHit::setCharge(float val) { mCharge = val; }

//______________________________________________________________________________
void StHit::setTrackReferenceCount(unsigned char val) { mTrackRefCount = val; }
    
//______________________________________________________________________________
void StHit::setFitFlag(unsigned char val) { mFitFlag = val; }
    
//______________________________________________________________________________
void StHit::setHardwarePosition(unsigned int val) { mHardwarePosition = val; }

//______________________________________________________________________________
void StHit::setPositionError(const StThreeVectorF& e) { mPositionError = e; }
    
//______________________________________________________________________________
float StHit::charge() const { return mCharge; }

//______________________________________________________________________________
unsigned int StHit::flag() const { return static_cast<unsigned int>(mFlag); }

//______________________________________________________________________________
int StHit::usedInFit() const { return static_cast<int>(mFitFlag); }

//______________________________________________________________________________
unsigned int StHit::trackReferenceCount() const { return static_cast<unsigned int>(mTrackRefCount); }

//______________________________________________________________________________
StThreeVectorF StHit::positionError() const { return mPositionError; }
   
//______________________________________________________________________________
StMatrixF StHit::covariantMatrix() const
{
    // for now the diagonal elements is all we have
    StMatrixF m(3,3);
    m(1,1) = mPositionError.x()*mPositionError.x();
    m(2,2) = mPositionError.y()*mPositionError.y();
    m(3,3) = mPositionError.z()*mPositionError.z();
    return m;
}


//______________________________________________________________________________
void StHit::setIdTruth(int idtru,int qatru) 
{
    if (qatru==0) qatru = (idtru>>16);
    idtru    = idtru&((1<<16)-1);
    mIdTruth = idtru;
    mQuality = (UShort_t) qatru;
}

//______________________________________________________________________________
int StHit::idTruth() const
{
  return mIdTruth;
}

//______________________________________________________________________________
ostream&  operator<<(ostream& os, const StHit& v)
{
  os << Form("id %5i ",v.id()) << *((const StMeasuredPoint *)&v );
  if (v.charge() > 1) os << Form(" q(ADC) %6.1f", v.charge());
  else                os << Form(" q(keV) %6.2f", 1e6*v.charge());
  os << Form(" idT %5i qa %3i fl%3i us %1i",v.idTruth(), v.qaTruth(), v.flag(), v.usedInFit());
  return os;
}

//______________________________________________________________________________
void StHit::Print(Option_t *option) const {cout << *this << endl;}

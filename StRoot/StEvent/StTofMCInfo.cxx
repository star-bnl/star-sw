/***************************************************************************
 *
 * $Id: StTofMCInfo.cxx,v 2.3 2003/09/02 17:58:06 perev Exp $
 *
 * Author: Wei-Ming Zhang, April 2001 
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTofMCInfo.cxx,v $
 * Revision 2.3  2003/09/02 17:58:06  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 2.2  2003/05/21 18:23:18  ullrich
 * Major Revision of ToF classes (F. Geurts)
 *
 * Revision 2.1  2001/04/27 21:40:34  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include <Stiostream.h>
#include "StTofMCInfo.h"

static const char rcsid[] = "$Id: StTofMCInfo.cxx,v 2.3 2003/09/02 17:58:06 perev Exp $";

ClassImp(StTofMCInfo)

StTofMCInfo::StTofMCInfo()
    : mTrkId(0), mGId(0),    mNHits(0),    mNPhe(0), mDe(0),   mPTot(0),  
    mDs(0),   mSLength(0), mPmLength(0), mTof(0),  mTime(0), mMTime(0),   
    mMTimeL(0)                                              {/* noop */ }

StTofMCInfo::StTofMCInfo(int trkId,   int gId,       int nHits, 
			 float de,    int nPhe,      float pTot, 
			 float ds,    float sLength, float pmLenght, 
			 float tof,   float time,    float mTime,    
			 float mTimeL)
    : mTrkId(trkId),       mGId(gId),   mNHits(nHits), mNPhe(nPhe), 
      mDe(de),             mPTot(pTot), mDs(ds),       mSLength(sLength), 
      mPmLength(pmLenght), mTof(tof),   mTime(time),   mMTime(mTime), 
      mMTimeL(mTimeL)                                         {/* noop */ }

ostream& operator<<(ostream& os, const StTofMCInfo& MCInfo)
{
    return (os << "  trkId= " << MCInfo.mTrkId << ", gId=  " << MCInfo.mGId 
	    << ", nHits= " << MCInfo.mNHits << ", nPhe= " << MCInfo.mNPhe  
	    << ", tof: "   << MCInfo.mTof); 
}

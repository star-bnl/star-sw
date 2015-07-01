/*!
 * \class StTofMCInfo 
 * \author Wei-Ming Zhang, April 2001 
 */
/***************************************************************************
 *
 * $Id: StTofMCInfo.h,v 2.3 2003/05/21 18:23:18 ullrich Exp $
 *
 * Author: Wei-Ming Zhang, April 2001 
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTofMCInfo.h,v $
 * Revision 2.3  2003/05/21 18:23:18  ullrich
 * Major Revision of ToF classes (F. Geurts)
 *
 * Revision 2.2  2002/02/22 22:56:51  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.1  2001/04/27 21:40:34  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StTofMCInfo_hh
#define StTofMCInfo_hh

#include "StObject.h"

class StTofMCInfo : public StObject {
public:
    StTofMCInfo();
    StTofMCInfo(int,   int,   int,   float, int,   float, 
		float, float, float, float, float, float,    
		float);

    int operator==(const StTofMCInfo& MCInfo) const;
    int operator!=(const StTofMCInfo& MCInfo) const;

public:
    Int_t   mTrkId;
    Int_t   mGId;
    Int_t   mNHits;
    Int_t   mNPhe; 
    Float_t mDe;
    Float_t mPTot;
    Float_t mDs;
    Float_t mSLength;
    Float_t mPmLength;
    Float_t mTof;
    Float_t mTime;
    Float_t mMTime;
    Float_t mMTimeL;
    
    ClassDef(StTofMCInfo,2)
};

inline int StTofMCInfo::operator==(const StTofMCInfo& MCInfo) const
{
    return ((mTrkId == MCInfo.mTrkId) && (mGId  == MCInfo.mGId) &&
            (mNHits == MCInfo.mNHits) && (mNPhe == MCInfo.mNPhe)); 
}

inline int StTofMCInfo::operator!=(const StTofMCInfo& MCInfo) const
{
    return !(*this == MCInfo);  // use operator==()
}

ostream& operator<<(ostream& os, const StTofMCInfo& MCInfo);

#endif

/***************************************************************************
 *
 * $Id: StTofMCSlat.h,v 2.1 2001/04/26 01:07:42 ullrich Exp $
 *
 * Author: Wei-Ming Zhang, April 2001 
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTofMCSlat.h,v $
 * Revision 2.1  2001/04/26 01:07:42  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StTofMCSlat_hh
#define StTofMCSlat_hh

#include "StTofSlat.h"

struct StructTofMCInfo {
    StructTofMCInfo()
      : mTrkId(0), mGId(0),    mNHits(0),    mNPhe(0), mDe(0),   mPTot(0),  
        mDs(0),   mSLength(0), mPmLength(0), mTof(0),  mTime(0), mMTime(0),   
        mMTimeL(0)                                              {/* nopt*/ }

    StructTofMCInfo(int trkId,   int gId,       int nHits, 
                    float de,    int nPhe,      float pTot, 
                    float ds,    float sLength, float pmLenght, 
                    float tof,   float time,    float mTime,    
                    float mTimeL)
      : mTrkId(trkId),       mGId(gId),   mNHits(nHits), mNPhe(nPhe), 
        mDe(de),             mPTot(pTot), mDs(ds),       mSLength(sLength), 
        mPmLength(pmLenght), mTof(tof),   mTime(time),   mMTime(mTime), 
        mMTimeL(mTimeL)                                         {/* nopt*/ }

    int operator==(const StructTofMCInfo& MCInfo) const;
    int operator!=(const StructTofMCInfo& MCInfo) const;

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
};

inline int StructTofMCInfo::operator==(const StructTofMCInfo& MCInfo) const
{
    return ((mTrkId == MCInfo.mTrkId) && (mGId  == MCInfo.mGId) &&
            (mNHits == MCInfo.mNHits) && (mNPhe == MCInfo.mNPhe)); 
}

inline int StructTofMCInfo::operator!=(const StructTofMCInfo& MCInfo) const
{
    return !(*this == MCInfo);  // use operator==()
}

ostream& operator<<(ostream& os, const StructTofMCInfo& MCInfo);


class StTofMCSlat : public StTofSlat {
public:
    StTofMCSlat();
    StTofMCSlat(const StructTofMCInfo&);
    // StTofMCSlat(const StTofMCSlat&);            use default
    // StTofMCSlat& operator=(const StTofMCSlat&); use default
    ~StTofMCSlat();
    
    int operator==(const StTofMCSlat&) const;
    int operator!=(const StTofMCSlat&) const;

    const StructTofMCInfo&  mcInfo() const;

    void                setMCInfo(const StructTofMCInfo&);

    void                setNHits(int nHits);
    void                setNPhe(int nPhe);
    void                setDe(float de);
    void                setDs(float ds);
    void                setTof(float tof);

protected:
    StructTofMCInfo     mTofMCInfo; 

    ClassDef(StTofMCSlat,1)
};

inline const StructTofMCInfo&
StTofMCSlat::mcInfo() const
{
    return mTofMCInfo;
}

inline void
StTofMCSlat::setMCInfo(const StructTofMCInfo& MCInfo)
{
    mTofMCInfo = MCInfo;
}

inline void
StTofMCSlat::setNHits(int nHits)
{
    mTofMCInfo.mNHits = nHits;
}

inline void
StTofMCSlat::setNPhe(int nPhe)
{
    mTofMCInfo.mNPhe = nPhe;
}

inline void
StTofMCSlat::setDe(float de)
{
    mTofMCInfo.mDe = de;
}


inline void
StTofMCSlat::setDs(float ds)
{
    mTofMCInfo.mDs = ds;
}


inline void
StTofMCSlat::setTof(float tof)
{
    mTofMCInfo.mTof = tof;
}

ostream& operator<<(ostream& os, const StTofMCSlat&);
#endif

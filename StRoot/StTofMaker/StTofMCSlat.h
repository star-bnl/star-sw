/*
 * $Id: StTofMCSlat.h,v 1.1 2001/04/24 20:27:38 wzhang Exp $ 
 *
 * Author: Wei-Ming Zhang, April 2001
 ***************************************************************************
 *
 * Description:
 * Inherited from StTofSlat with MC Information, a structure, added.
 *
 ***************************************************************************
 *
 * $Log: StTofMCSlat.h,v $
 * Revision 1.1  2001/04/24 20:27:38  wzhang
 * First release
 *
 *
 **************************************************************************/
#ifndef StTofMCSlat_hh
#define StTofMCSlat_hh

#include "StTofSlat.h"

// 
struct StructTofMCInfo {
    StructTofMCInfo()
      : mTrkId(0), mGId(0),    mNHits(0),    mNPhe(0), mDe(0),   mPTot(0),  
        mDs(0),   mSLength(0), mPmLength(0), mTof(0),  mTime(0), mMTime(0),   
        mMTimeL(0)                                              {/* nopt*/ }

    StructTofMCInfo(Int_t trkId,   Int_t gId,       Int_t nHits, 
                    Float_t de,    Int_t nPhe,      Float_t pTot, 
                    Float_t ds,    Float_t sLength, Float_t pmLenght, 
                    Float_t tof,   Float_t time,    Float_t mTime,    
                    Float_t mTimeL)
      : mTrkId(trkId),       mGId(gId),   mNHits(nHits), mNPhe(nPhe), 
        mDe(de),             mPTot(pTot), mDs(ds),       mSLength(sLength), 
        mPmLength(pmLenght), mTof(tof),   mTime(time),   mMTime(mTime), 
        mMTimeL(mTimeL)                                         {/* nopt*/ }

    Int_t operator==(const StructTofMCInfo& MCInfo) const;
    Int_t operator!=(const StructTofMCInfo& MCInfo) const;

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

inline Int_t StructTofMCInfo::operator==(const StructTofMCInfo& MCInfo) const
{
    return ((mTrkId == MCInfo.mTrkId) && (mGId  == MCInfo.mGId) &&
            (mNHits == MCInfo.mNHits) && (mNPhe == MCInfo.mNPhe)); 
}

inline Int_t StructTofMCInfo::operator!=(const StructTofMCInfo& MCInfo) const
{
    return !(*this == MCInfo);  // use operator==()
}


ostream& operator<<(ostream& os, const StructTofMCInfo& MCInfo);

class StTofMCSlat : public StTofSlat {
public:
    StTofMCSlat();
    StTofMCSlat(StructTofMCInfo MCInfo);
    // StTofMCSlat(const StTofMCSlat&);            use default
    // StTofMCSlat& operator=(const StTofMCSlat&); use default
    ~StTofMCSlat();
    
    Int_t operator==(const StTofMCSlat&) const;
    Int_t operator!=(const StTofMCSlat&) const;

    StructTofMCInfo     MCInfo()            const;

    void                setMCInfo(StructTofMCInfo MCInfo);

//  reset MCInfo members for mult-hits slat
    void                setNHits(Int_t nHits);
    void                setNPhe(Int_t nPhe);
    void                setDe(Float_t de);
    void                setDs(Float_t ds);
    void                setTof(Float_t tof);

protected:

    StructTofMCInfo     mTofMCInfo; 

    ClassDef(StTofMCSlat,1)
};

inline StructTofMCInfo
StTofMCSlat::MCInfo() const
{
    return mTofMCInfo;
}

inline void
StTofMCSlat::setMCInfo(StructTofMCInfo MCInfo)
{
    mTofMCInfo = MCInfo;
}

inline void
StTofMCSlat::setNHits(Int_t nHits)
{
    mTofMCInfo.mNHits = nHits;
}

inline void
StTofMCSlat::setNPhe(Int_t nPhe)
{
    mTofMCInfo.mNPhe = nPhe;
}

inline void
StTofMCSlat::setDe(Float_t de)
{
    mTofMCInfo.mDe = de;
}


inline void
StTofMCSlat::setDs(Float_t ds)
{
    mTofMCInfo.mDs = ds;
}


inline void
StTofMCSlat::setTof(Float_t tof)
{
    mTofMCInfo.mTof = tof;
}

ostream& operator<<(ostream& os, const StTofMCSlat& MCSlat);
#endif

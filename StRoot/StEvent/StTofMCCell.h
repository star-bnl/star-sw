/***************************************************************************
 *
 * $Id: StTofMCCell.h,v 2.1 2003/05/21 18:24:20 ullrich Exp $
 *
 * Author: F. Geurts, May 2003
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTofMCCell.h,v $
 * Revision 2.1  2003/05/21 18:24:20  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StTofMCCell_hh
#define StTofMCCell_hh

#include "StTofCell.h"
#include "StTofMCInfo.h"

class StTofMCCell : public StTofCell {
public:
    StTofMCCell();
    StTofMCCell(const StTofMCInfo&);
    ~StTofMCCell();
    
    int operator==(const StTofMCCell&) const;
    int operator!=(const StTofMCCell&) const;

    const StTofMCInfo&  mcInfo() const;

    void  setMCInfo(const StTofMCInfo&);

    void  setNHits(int nHits);
    void  setNPhe(int nPhe);
    void  setDe(float de);
    void  setDs(float ds);
    void  setTof(float tof);

protected:
    StTofMCInfo  mTofMCInfo; 

    ClassDef(StTofMCCell,1)
};

inline const StTofMCInfo&
StTofMCCell::mcInfo() const
{
    return mTofMCInfo;
}

inline void
StTofMCCell::setMCInfo(const StTofMCInfo& MCInfo)
{
    mTofMCInfo = MCInfo;
}

inline void
StTofMCCell::setNHits(int nHits)
{
    mTofMCInfo.mNHits = nHits;
}

inline void
StTofMCCell::setNPhe(int nPhe)
{
    mTofMCInfo.mNPhe = nPhe;
}

inline void
StTofMCCell::setDe(float de)
{
    mTofMCInfo.mDe = de;
}


inline void
StTofMCCell::setDs(float ds)
{
    mTofMCInfo.mDs = ds;
}


inline void
StTofMCCell::setTof(float tof)
{
    mTofMCInfo.mTof = tof;
}

ostream& operator<<(ostream& os, const StTofMCCell&);

#endif

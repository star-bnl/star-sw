/*!
 * \class StTofCell 
 * \author F. Geurts, May 2003
 */
/***************************************************************************
 *
 * $Id: StTofCell.h,v 2.1 2003/05/21 18:24:20 ullrich Exp $
 *
 * Author: F. Geurts, May 2003
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTofCell.h,v $
 * Revision 2.1  2003/05/21 18:24:20  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StTofCell_hh
#define StTofCell_hh

#include "StObject.h"

class StTrack;

class StTofCell : public StObject {
public:
    StTofCell();
    StTofCell(int, int, int, int, int, StTrack*);
    ~StTofCell();
    
    int operator==(const StTofCell&) const;
    int operator!=(const StTofCell&) const;
    
    int trayIndex() const;
    int moduleIndex() const;
    int cellIndex() const;
    int adc() const;
    int tdc() const;

    StTrack* associatedTrack();
    const StTrack* associatedTrack() const;
    
    void      setTrayIndex(int);
    void      setModuleIndex(int);
    void      setCellIndex(int);
    void      setAdc(int);
    void      setTdc(int);
    void      setAssociatedTrack(StTrack*);

protected:
    Int_t    mTrayIndex;
    Int_t    mModuleIndex;
    Int_t    mCellIndex;
    Int_t    mAdc;
    Int_t    mTdc;
    StTrack* mAssociatedTrack;   //$LINK

    ClassDef(StTofCell,1)
};


inline void
StTofCell::setTrayIndex(int trayId)
{
    mTrayIndex = trayId;
}

inline void
StTofCell::setModuleIndex(int moduleId)
{
    mModuleIndex = moduleId;
}

inline void
StTofCell::setCellIndex(int cellId)
{
    mCellIndex = cellId;
}

inline void
StTofCell::setAdc(int rawAdc)
{
    mAdc = rawAdc;
}

inline void
StTofCell::setTdc(int rawTdc)
{
    mTdc = rawTdc;
}

inline int
StTofCell::trayIndex() const
{
    return mTrayIndex;
}

inline int
StTofCell::moduleIndex() const
{
    return mModuleIndex;
}

inline int
StTofCell::cellIndex() const
{
    return mCellIndex;
}

inline int
StTofCell::adc()  const
{
    return mAdc;
}

inline int
StTofCell::tdc()  const
{
    return mTdc;
}

#endif

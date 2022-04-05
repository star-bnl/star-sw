/*!
 * \class StTofCell 
 * \author F. Geurts, May 2003
 */
/***************************************************************************
 *
 * $Id: StTofCell.h,v 2.8 2008/03/31 20:09:35 ullrich Exp $
 *
 * Author: F. Geurts, May 2003
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTofCell.h,v $
 * Revision 2.8  2008/03/31 20:09:35  ullrich
 * Changed typr of mLeadingEdgeTime and mTrailingEdgeTime from float to double
 *
 * Revision 2.7  2007/09/19 17:32:18  ullrich
 * New member (mLeadingEdgeTime,  mTrailingEdgeTime) and related functions and updates added.
 *
 * Revision 2.6  2004/06/14 23:54:38  jeromel
 * Corrected typo
 *
 * Revision 2.5  2004/06/11 19:36:48  ullrich
 * Added implementation of zHit().
 *
 * Revision 2.4  2004/02/05 17:59:31  ullrich
 * Changed $LINK to StLink mechanism and add new member.
 *
 * Revision 2.3  2003/08/28 23:24:17  jeromel
 * Modif in class
 *
 * Revision 2.2  2003/08/05 17:12:32  ullrich
 * Added position() methods and member.
 *
 * Revision 2.1  2003/05/21 18:24:20  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StTofCell_hh
#define StTofCell_hh

#include "StObject.h"
#include "StThreeVectorD.hh"
#include "StContainers.h"

class StTrack;

class StTofCell : public StObject {
public:
    StTofCell();
    StTofCell(int, int, int, int, int, int, StTrack*,
	    float, int, const StThreeVectorD&);
    StTofCell(int, int, int, int, StTrack*,
	    float, int, const StThreeVectorD&);
    ~StTofCell();
    
    int operator==(const StTofCell&) const;
    int operator!=(const StTofCell&) const;
    
    int                   trayIndex() const;
    int                   moduleIndex() const;
    int                   cellIndex() const;
    int                   daqIndex() const;
    int                   adc() const;
    int                   tdc() const;
    double                leadingEdgeTime() const;
    double                trailingEdgeTime() const;
    float                 tot() const;
    StTrack*              associatedTrack();
    const StTrack*        associatedTrack() const;
    float                 zHit() const;
    int                   matchFlag() const;
    const StThreeVectorD& position() const;
    
    void      setTrayIndex(int);
    void      setModuleIndex(int);
    void      setCellIndex(int);
    void      setDaqIndex(int);
    void      setAdc(int);
    void      setTdc(int);
    void      setLeadingEdgeTime(double);
    void      setTrailingEdgeTime(double);
    void      setAssociatedTrack(StTrack*);
    void      setZHit(float);
    void      setMatchFlag(int);
    void      setPosition(const StThreeVectorD&);

protected:
    Int_t    mTrayIndex;
    Int_t    mModuleIndex;
    Int_t    mCellIndex;
    Int_t    mDaqIndex;
    Int_t    mAdc;
    Int_t    mTdc;
    Double_t mLeadingEdgeTime;
    Double_t mTrailingEdgeTime;
    //    StTrack* mAssociatedTrack;   //$LINK
#ifdef __CINT__
    StObjLink        mAssociatedTrack;		
#else
    StLink<StTrack>  mAssociatedTrack;		
#endif //__CINT__
    Float_t  mZhit;
    Int_t    mMatchFlag;
    StThreeVectorD mPosition;

    ClassDef(StTofCell,4)
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
StTofCell::setDaqIndex(int daqId)
{
    mDaqIndex = daqId;
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

inline void
StTofCell::setLeadingEdgeTime(double val)
{
    mLeadingEdgeTime = val;
}

inline void
StTofCell::setTrailingEdgeTime(double val)
{
    mTrailingEdgeTime = val;
}

inline void
StTofCell::setZHit(float zhit) {mZhit = zhit;}

inline void
StTofCell::setMatchFlag(int flag)
{
  mMatchFlag = flag;
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
StTofCell::daqIndex() const
{
    return mDaqIndex;
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

inline double 
StTofCell::leadingEdgeTime()  const
{
    return mLeadingEdgeTime;
}

inline double 
StTofCell::trailingEdgeTime()  const
{
    return mTrailingEdgeTime;
}

inline float
StTofCell::tot()  const
{
    return mTrailingEdgeTime - mLeadingEdgeTime;
}

inline int
StTofCell::matchFlag() const
{
  return mMatchFlag;
}

inline float
StTofCell::zHit() const
{
  return mZhit;
}

#endif

/*!
 * \class StHit 
 * \author Thomas Ullrich, Jan 1999
 */
/***************************************************************************
 *
 * $Id: StHit.h,v 2.30 2016/02/25 17:07:14 ullrich Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StHit.h,v $
 * Revision 2.30  2016/02/25 17:07:14  ullrich
 * Removed implentation of detector() making class pure abstract.
 *
 * Revision 2.29  2015/10/09 17:46:14  ullrich
 * Changed type of mIdTruth from ushort to int.
 *
 * Revision 2.28  2012/01/24 03:04:22  perev
 * Cleanup
 *
 * Revision 2.27  2011/10/17 00:13:49  fisyak
 * Add handles for IdTruth info
 *
 * Revision 2.26  2011/05/12 22:25:48  fisyak
 * Restore hit errors as persistent, add sort to TpcHit
 *
 * Revision 2.25  2011/01/21 18:30:45  fisyak
 * fix setFlag with UShort_t
 *
 * Revision 2.24  2011/01/21 16:14:40  fisyak
 * change mFlag type from UChar_t to UShort_t (bug #2058)
 *
 * Revision 2.23  2009/11/10 00:40:17  ullrich
 * Changed print-out format.
 *
 * Revision 2.22  2008/04/02 20:43:58  fisyak
 * Don't store  mPositionError in file
 *
 * Revision 2.21  2006/01/19 21:50:02  ullrich
 * Made detector() virtual method.
 *
 * Revision 2.20  2005/12/07 19:03:23  perev
 * mId short ==> int
 *
 * Revision 2.19  2005/07/19 21:34:24  perev
 * quality ==> qaTruth to avoid misleading
 *
 * Revision 2.18  2005/07/06 18:57:48  fisyak
 * Add StHit print out
 *
 * Revision 2.17  2005/01/26 23:04:12  perev
 * const for Sthit* nextHit()
 *
 * Revision 2.16  2004/08/19 21:58:53  ullrich
 * Fixed omitted implementation of nextHit().
 *
 * Revision 2.15  2004/08/18 19:00:19  ullrich
 * Added access function hardwarePosition().
 *
 * Revision 2.14  2004/08/06 15:37:09  fisyak
 * Add clster id
 *
 * Revision 2.13  2004/07/30 22:28:31  fisyak
 * Add transient pointer to next Hit
 *
 * Revision 2.12  2004/07/15 16:36:24  ullrich
 * Removed all clone() declerations and definitions. Use StObject::clone() only.
 *
 * Revision 2.11  2004/03/30 15:59:08  calderon
 * Added method to set mFitFlag (new chain no longer uses tables, so must set
 * this by hand).
 *
 * Revision 2.10  2004/01/13 21:01:32  fisyak
 * Add Truth and Quality information from simulation
 *
 * Revision 2.9  2002/02/22 22:56:48  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.8  2001/04/25 17:46:56  jeromel
 * Remove last change. There is another way to fix the problems seen (the right way
 * actually) which Thomas will take care off.
 *
 * Revision 2.7  2001/04/25 15:57:22  jeromel
 * Fixed cint problem with StContainers.h
 *
 * Revision 2.6  2001/04/05 04:00:38  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.5  2001/03/24 03:34:50  perev
 * clone() -> clone() const
 *
 * Revision 2.4  2000/07/28 23:29:42  calderon
 * Added handling of Fit Flag: use this flag to tell if the point
 * is used in the fit.
 *
 * Revision 2.3  2000/06/07 09:43:21  ullrich
 * Changed return type of flag() to unsigned int
 *
 * Revision 2.2  2000/06/01 21:38:56  ullrich
 * Added member mFlag and access member flag() and setFlag().
 *
 * Revision 2.1  1999/10/28 22:25:50  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.0  1999/10/12 18:42:21  ullrich
 * Completely Revised for New Version
 *
 **************************************************************************/
#ifndef StHit_hh
#define StHit_hh

#include "StMeasuredPoint.h"
#include "StEnumerations.h"
#include "StContainers.h"

class StTrackNode;
class StTrack;

class StHit : public StMeasuredPoint {
public:
    StHit();
    StHit(const StThreeVectorF& position,
          const StThreeVectorF& errors,
          unsigned int   hardware,
	      float          charge,
	      unsigned char  trakRefCount = 0,
	      int            idTruth=0,
          unsigned short quality=0, 
          unsigned short id =0);
    // StHit(const StHit&);            use default
    // StHit& operator=(const StHit&); use default
    ~StHit();

    int operator==(const StHit&) const;
    int operator!=(const StHit&) const;
    
    float           charge() const;
    unsigned int    trackReferenceCount() const;
    unsigned int    flag() const;
    StThreeVectorF  positionError() const;     // overwrite inherited
    StMatrixF       covariantMatrix() const;   // overwrite inherited
    int             usedInFit() const;
    int             idTruth() const;
    int             qaTruth() const { return mQuality; }
    int  	        id()      const;
    const StHit*    nextHit() const;
    unsigned int    hardwarePosition() const;
    
    
    void setCharge(float);
    void setFlag(unsigned short val) { mFlag = val; }
    void setFitFlag(unsigned char);
    void setTrackReferenceCount(unsigned char);
    void setHardwarePosition(unsigned int);
    void setPositionError(const StThreeVectorF&);
    void setId(int Id)			{mId = Id;}
    void setIdTruth(int idtru, int qatru=0);
    void SetNextHit(StHit *next = 0) 	{mNextHit = next;}

    virtual StDetectorId   detector() const = 0;
    virtual void           Print(Option_t *option="") const;
    
protected:
    unsigned int bits(unsigned int, unsigned int) const;
    
    UInt_t         mHardwarePosition;
    StThreeVectorF mPositionError; 
    Float_t        mCharge;
    Int_t          mId;
    Int_t          mIdTruth; // simulation track id 
    UShort_t       mQuality; // quality of this information (percentage of charge produced by mIdTruth)
    UChar_t        mFitFlag;
    UChar_t        mTrackRefCount;
    UShort_t       mFlag;
    StHit*         mNextHit; //!
    ClassDef(StHit,8)
};

inline unsigned int StHit::bits(unsigned int bit, unsigned int nbits) const
{
    return (mHardwarePosition>>bit) & ~(~0UL<<nbits);
}

inline unsigned int    StHit::hardwarePosition() const {return mHardwarePosition;}
inline int  	       StHit::id()      const {return mId;}
inline const StHit*    StHit::nextHit() const {return mNextHit;}

ostream&              operator<<(ostream& os, StHit const & v);
#endif

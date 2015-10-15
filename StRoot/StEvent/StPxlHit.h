/*!
 * \class StPxlHit 
 * \authors S. MArgetis, J. Bouchet, Jan 2013
 * \Initial Revision.
 */
/***************************************************************************
 * 
 * $Id: StPxlHit.h,v 2.4 2015/05/13 18:05:25 ullrich Exp $
 *
 * Author: S. Margetis, J. Bouchet, Jan 2013
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StPxlHit.h,v $
 * Revision 2.4  2015/05/13 18:05:25  ullrich
 * New constructors for setting local hit position, proper initialization
 * of all data member, modified existing constructor, new getter and
 * setter for local hit coordinates.
 *
 * Revision 2.3  2014/04/10 16:00:13  jeromel
 * Changes to inlcude Ist structure (Thomas OK-ed / may revisit some comments)
 *
 * Revision 2.2  2013/06/09 22:04:42  ullrich
 * Modified layer() method.
 *
 * Revision 2.1  2013/03/05 14:40:40  ullrich
 * Initial Revision.
 * 
 **************************************************************************/
#ifndef StPxlHit_hh
#define StPxlHit_hh
#include "StHit.h"
#include "StMemoryPool.hh"
#include "StEnumerations.h"

class StPxlHit : public StHit
{
public:
    StPxlHit();
    StPxlHit(const StThreeVectorF& position,
             const StThreeVectorF& error,
             UInt_t hwPosition, Float_t charge, 
             unsigned char trackRefCount = 0) :  StHit(position, error, hwPosition, charge, trackRefCount) {}
    StPxlHit(const double localPos[3], UInt_t sector, UInt_t ladder,
             UInt_t sensor, const StThreeVectorF& position, const StThreeVectorF& error,
             UInt_t hwPosition, Float_t charge, UChar_t trackRefCount = 0,
             Int_t idTruth=0, UShort_t quality=0, UShort_t id=0);
    StPxlHit(const double localPos[3], UInt_t sector, UInt_t ladder,
             UInt_t sensor, Int_t idTruth);
    StPxlHit(Float_t meanRow, Float_t meanColumn, UInt_t sector, UInt_t ladder,
             UInt_t sensor);
    ~StPxlHit();
    
    virtual StDetectorId detector() const;
    
    UChar_t sector() const;
    UChar_t ladder() const;
    UChar_t sensor() const;
    Float_t meanRow() const;
    Float_t meanColumn() const;
    UChar_t nRawHits() const;
    UChar_t layer() const ;
    
    Float_t localPosition(UInt_t) const;
    const float* localPosition() const;
    void  setLocalPosition(float, float, float);
    void  setLocalY(Float_t y);
    
    void setSector(UChar_t);
    void setLadder(UChar_t);
    void setSensor(UChar_t);
    void setDetectorId(StDetectorId);
    void setMeanRow(float);
    void setMeanColumn(float);
    void setNRawHits(UChar_t);
    
    virtual bool isSortable() const;
    
    void* operator new(size_t sz,void *p)     { return p;}
    void* operator new(size_t)     { return mPool.alloc(); }
    void  operator delete(void* p) { mPool.free(p); }
    
protected:
    UChar_t mSector; //Sector : 1-10
    UChar_t mLadder; //Ladder : 1-4 
    UChar_t mSensor; //Sensor : 1-10
    Float_t mMeanRow; // mean row : mean row of the pxl cluster
    Float_t mMeanColumn; // mean column : mean column of the pxl cluster
    UChar_t mNRawHits; // nRawHits
    //local position of hit inside the wafer 
    // 3D : u,v,TPS
    Float_t mLocalPosition[3];
    
    // this has to go once the playing and testing is over.
    // should be hard wired in member function.
    StDetectorId mDetectorId;
    
    static StMemoryPool mPool;  //!
    
    ClassDef(StPxlHit,1)
};

ostream& operator<<(ostream&, const StPxlHit&);

inline UChar_t StPxlHit::sector() const {return mSector;}
inline UChar_t StPxlHit::ladder() const {return mLadder;}
inline UChar_t StPxlHit::sensor() const {return mSensor;}
inline Float_t StPxlHit::meanRow() const {return mMeanRow;}
inline Float_t StPxlHit::meanColumn()  const {return mMeanColumn;}
inline UChar_t StPxlHit::nRawHits() const {return mNRawHits;}
inline UChar_t StPxlHit::layer() const {return (mLadder==1)? 1 : 2;}
inline const Float_t* StPxlHit::localPosition() const { return mLocalPosition; }
inline void StPxlHit::setLocalY(Float_t y) { mLocalPosition[1] = y; }
inline void StPxlHit::setSector(UChar_t v) {mSector = v;}
inline void StPxlHit::setLadder(UChar_t v) {mLadder = v;}
inline void StPxlHit::setSensor(UChar_t v) {mSensor = v;}
inline void StPxlHit::setMeanRow(Float_t v) {mMeanRow = v;}
inline void StPxlHit::setMeanColumn(Float_t v) {mMeanColumn = v;}
inline void StPxlHit::setNRawHits(UChar_t v) {mNRawHits = v;}

inline bool StPxlHit::isSortable() const { return true; }

#endif

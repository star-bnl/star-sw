/*!
 * \class StPxlHit 
 * \authors S. MArgetis, J. Bouchet, Jan 2013
 * \Initial Revision.
 */
/***************************************************************************
 * 
 * $Id: StPxlHit.h,v 2.1 2013/03/05 14:40:40 ullrich Exp $
 *
 * Author: S. Margetis, J. Bouchet, Jan 2013
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StPxlHit.h,v $
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
             unsigned int hwPosition, float charge, 
             unsigned char trackRefCount = 0);
    ~StPxlHit();
    
    StDetectorId detector() const;
    
    unsigned char sector() const;
    unsigned char ladder() const;
    unsigned char sensor() const;
    float meanRow() const;
    float meanColumn() const;
    unsigned char nRawHits() const;
    unsigned char layer() const ;
    
    float localPosition(unsigned int) const;
    void setLocalPosition(float, float, float);
    
    void setSector(unsigned char);
    void setLadder(unsigned char);
    void setSensor(unsigned char);
    void setDetectorId(StDetectorId);
    void setMeanRow(float);
    void setMeanColumn(float);
    void setNRawHits(unsigned char);
    
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

inline unsigned char StPxlHit::sector() const {return mSector;}
inline unsigned char StPxlHit::ladder() const {return mLadder;}
inline unsigned char StPxlHit::sensor() const {return mSensor;}
inline float StPxlHit::meanRow() const {return mMeanRow;}
inline float StPxlHit::meanColumn()  const {return mMeanColumn;}
inline unsigned char StPxlHit::nRawHits() const {return mNRawHits;}
inline unsigned char StPxlHit::layer() const {return (mLadder==4)? 1 : 2;}

inline void StPxlHit::setSector(unsigned char v) {mSector = v;}
inline void StPxlHit::setLadder(unsigned char v) {mLadder = v;}
inline void StPxlHit::setSensor(unsigned char v) {mSensor = v;}
inline void StPxlHit::setMeanRow(float v) {mMeanRow = v;}
inline void StPxlHit::setMeanColumn(float v) {mMeanColumn = v;}
inline void StPxlHit::setNRawHits(unsigned char v) {mNRawHits = v;}

inline bool StPxlHit::isSortable() const { return true; }

#endif

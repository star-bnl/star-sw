/*!
 * \class StPxlHit 
 * \authors S. MArgetis, J. Bouchet, Jan 2013
 * \Initial Revision.
 */
/***************************************************************************
 * 
 * $Id: StPxlHit.h,v 2.6 2017/05/04 00:58:28 perev Exp $
 *
 * Author: S. Margetis, J. Bouchet, Jan 2013
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StPxlHit.h,v $
 * Revision 2.6  2017/05/04 00:58:28  perev
 * Cleanup
 *
 * Revision 2.5  2016/02/25 17:10:20  ullrich
 * Implemented detector() which is now a pure abstract method in StHit.
 *
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
    StPxlHit(const double localPos[3], unsigned int sector, unsigned int ladder,
             unsigned int sensor, const StThreeVectorF& position, const StThreeVectorF& error,
             unsigned int hwPosition, float charge, unsigned char trackRefCount = 0,
             unsigned short idTruth=0, unsigned short quality=0, unsigned short id=0);
    StPxlHit(const double localPos[3], unsigned int sector, unsigned int ladder,
             unsigned int sensor, unsigned short idTruth);
    StPxlHit(float meanRow, float meanColumn, unsigned int sector, unsigned int ladder,
             unsigned int sensor);
    ~StPxlHit();
    
    StDetectorId detector() const;
    
    unsigned int sector() const;
    unsigned int ladder() const;
    unsigned int sensor() const;
    float meanRow() const;
    float meanColumn() const;
    unsigned int nRawHits() const;
    unsigned int layer() const ;
    
    float localPosition(unsigned int) const;
    const float* localPosition() const;
    void  setLocalPosition(float, float, float);
    void  setLocalY(float y);
    
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

inline unsigned int StPxlHit::sector() const {return mSector;}
inline unsigned int StPxlHit::ladder() const {return mLadder;}
inline unsigned int StPxlHit::sensor() const {return mSensor;}
inline float StPxlHit::meanRow() const {return mMeanRow;}
inline float StPxlHit::meanColumn()  const {return mMeanColumn;}
inline unsigned int StPxlHit::nRawHits() const {return mNRawHits;}
inline unsigned int StPxlHit::layer() const {return (mLadder==1)? 1 : 2;}
inline const Float_t* StPxlHit::localPosition() const { return mLocalPosition; }
inline void StPxlHit::setLocalY(float y) { mLocalPosition[1] = y; }
inline void StPxlHit::setSector(unsigned char v) {mSector = v;}
inline void StPxlHit::setLadder(unsigned char v) {mLadder = v;}
inline void StPxlHit::setSensor(unsigned char v) {mSensor = v;}
inline void StPxlHit::setMeanRow(float v) {mMeanRow = v;}
inline void StPxlHit::setMeanColumn(float v) {mMeanColumn = v;}
inline void StPxlHit::setNRawHits(unsigned char v) {mNRawHits = v;}

inline bool StPxlHit::isSortable() const { return true; }

#endif

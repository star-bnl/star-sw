/***************************************************************************
 *
 * $Id: StFpsSlat.h,v 2.2 2015/10/21 14:45:55 ullrich Exp $
 *
 * Author: Jingguo Ma, Akio Ogawa, Sep 2015
 ***************************************************************************
 *
 * Description: StFpsSlat is data for individual FPS slat 
 *
 ***************************************************************************
 *
 * $Log: StFpsSlat.h,v $
 * Revision 2.2  2015/10/21 14:45:55  ullrich
 * Moved 7 members out of the schema.
 *
 * Revision 2.1  2015/09/01 18:26:45  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StFpsSlat_hh
#define StFpsSlat_hh

#include "Stiostream.h"
#include "StObject.h"
#include "StContainers.h"
#include "StEnumerations.h"

class StFpsSlat : public StObject {
public:
    StFpsSlat();
    StFpsSlat(int slatid, float mip);
    ~StFpsSlat();
    
    int slatId() const;
    float mip() const;
    
    void setSlatId(int);
    void setMip(float);
    
    unsigned int  nPoint(int type) const;           // 0 for closest, 1/2/3 for 2nd/3rd/4th closest, 5 for eithe
    StPtrVecFmsPoint& point(int type);              // return fmsPoints pointing to the slat
    const StPtrVecFmsPoint& point(int type) const;
    void addPoint(StFmsPoint* point, int type);
    
    void print(int option=0) const;
    
protected:
    Int_t    mSlatId; //!
    Float_t  mMip;    //!
    UInt_t   mNPoint[kFpsNCandidate+1];   //! 0 for closest, 1/2/3 for 2nd/3rd/4th closest, 5 for either
    StPtrVecFmsPoint mPoint1;    //! closest points
    StPtrVecFmsPoint mPoint2;    //! 2nd closest points
    StPtrVecFmsPoint mPoint3;    //! 3rd closest points
    StPtrVecFmsPoint mPoint4;    //! 4th closest points

    ClassDef(StFpsSlat,2)
};

inline int StFpsSlat::slatId() const {return mSlatId;}
inline float StFpsSlat::mip()  const {return mMip;}
inline void StFpsSlat::setSlatId(int slatid) {mSlatId=slatid;}
inline void StFpsSlat::setMip(float mip) {mMip=mip;} 
inline unsigned int StFpsSlat::nPoint(int type) const {return mNPoint[type];}

#endif

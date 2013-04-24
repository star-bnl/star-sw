/*! \class StFgtPoint
 \brief Represents a point in the FGT 
 
 */
/***************************************************************************
 *
 * $Id: StFgtPoint.h,v 2.4 2013/04/24 17:35:09 ullrich Exp $
 * Author: S. Gliske, Oct 2011
 *
 ***************************************************************************
 *
 * Description: data for individual ``point'' on the FGT, i.e. a pair
 * of 1D clusters.  Note, if errors during construction, the key will
 * be set to -999.  Need to check this after constructing.
 *
 ***************************************************************************
 *
 * $Log: StFgtPoint.h,v $
 * Revision 2.4  2013/04/24 17:35:09  ullrich
 * Implemented detector() and removed redundant semicolons.
 *
 * Revision 2.3  2013/01/08 19:53:15  ullrich
 * Added comparison operators.
 *
 * Revision 1.3  2012/12/11 00:13:10  avossen
 * update of StFgtPoint
 *
 * Revision 1.4  2012/03/07 19:20:43  sgliske
 * updated based on reviewer comments
 *
 * Revision 1.3  2012/03/06 22:28:01  sgliske
 * asserts removed in StFgtPoint constructor.
 * Now must check key after constructing
 *
 * Revision 1.2  2011/11/01 18:42:57  sgliske
 * Added ::Clear(Option_t*) and few other missing things to FGT containers
 *
 * Revision 1.1  2011/10/31 21:51:30  sgliske
 * creation: StEvent containers, take 2
 *
 *
 **************************************************************************/
#ifndef _ST_FGT_POINT_H_
#define _ST_FGT_POINT_H_

#include "StHit.h"
#include "StFgtHit.h"

class StFgtPoint : public StHit {
public:
    // constructors
    StFgtPoint();
    StFgtPoint( StFgtHit* hit1, StFgtHit* hit2, int key, int rank );
    
    // StFgtPoint(const StFgtPoint&);             --> use default
    // StFgtPoint& operator=(const StFgtPoint&);  --> use default
    const bool operator < (const StFgtPoint& rhs) const;
    const bool operator > (const StFgtPoint& rhs) const;
    const bool operator >= (const StFgtPoint& rhs) const;
    const bool operator <= (const StFgtPoint& rhs) const;
    // deconstructor
    ~StFgtPoint();

    StDetectorId detector() const;
    void setHardwarePosition(short disc, short quad);

    // other accessors
    int getKey();
    int getDisc();
    int getQuad();
    const StFgtHit* getHitR() const;
    const StFgtHit* getHitPhi() const;
    
    float getPositionR() const;
    float getPositionPhi() const;
    float getChargeAsymmetry() const;
    int getRank() const;
    void setRank(int rank);
    
protected:
    // data members
    Int_t   mKey;                         // unique label
    Float_t mChargeAsymmetry;
    Int_t   mRank;
    
    StFgtHit *mHitR;   //! do not stream pointers
    StFgtHit *mHitPhi;  //!
    
private:   
    ClassDef(StFgtPoint,2);
}; 


// inline functions

inline StFgtPoint::StFgtPoint() : StHit(), mHitR(0), mHitPhi(0) { 
    // nothing else
}

inline int StFgtPoint::getDisc() {
    return static_cast< int >(mHardwarePosition/8);
}

inline int StFgtPoint::getQuad() {
    return static_cast< int >((mHardwarePosition/2)%4);
}

inline StDetectorId StFgtPoint::detector() const {return kFgtId;}

inline int StFgtPoint::getKey() {
    return mKey;
}

inline const StFgtHit* StFgtPoint::getHitR() const {
    return mHitR;
}

inline const StFgtHit* StFgtPoint::getHitPhi() const {
    return mHitPhi;
}

inline float StFgtPoint::getPositionR() const {
    return mHitR->getPositionR();
}

inline float StFgtPoint::getPositionPhi() const {
    return mHitPhi->getPositionPhi();
}

inline float StFgtPoint::getChargeAsymmetry() const {
    return mChargeAsymmetry;
}

inline int StFgtPoint::getRank() const {
    return mRank;
}

inline void StFgtPoint::setRank(int rank) {
    mRank=rank;
}

inline const bool StFgtPoint::operator >(const StFgtPoint& rhs) const {
    return mRank>rhs.mRank;
}

inline const bool StFgtPoint::operator >=(const StFgtPoint& rhs) const {
    return mRank>=rhs.mRank;
}

inline const bool StFgtPoint::operator <(const StFgtPoint& rhs) const {
    return mRank<rhs.mRank;
}

inline const bool StFgtPoint::operator <=(const StFgtPoint& rhs) const {
    return mRank<=rhs.mRank;
}

inline void StFgtPoint::setHardwarePosition(short disc, short quad){
  mHardwarePosition = disc*4+quad+1;
}

#endif

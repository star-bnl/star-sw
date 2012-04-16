/***************************************************************************
 *
 * $Id: StFgtHit.h,v 2.1 2012/04/16 20:20:49 ullrich Exp $
 * Author: S. Gliske, Oct 2011
 *
 ***************************************************************************
 *
 * Description: data for individual ``hit'' on the FGT, i.e. a 1D cluster.
 *
 ***************************************************************************
 *
 * $Log: StFgtHit.h,v $
 * Revision 2.1  2012/04/16 20:20:49  ullrich
 * Initial Revision
 *
 *
 **************************************************************************/

#ifndef _ST_FGT_HIT_H_
#define _ST_FGT_HIT_H_

#include <map>

#include "StHit.h"
#include "StFgtStrip.h"

// Note: not const StFgtStrip, so the clustering can modify the
// strips.  Clustering algos require these to be ordered by geoId.
// Note also, the stripWeightMap_t is only used in memory, and is not
// streamed.
typedef std::map< StFgtStrip*, float, stripPtrLessThan > stripWeightMap_t;

class StFgtHit : public StHit {
public:
    // constructors
    StFgtHit( int key = -1, int centralStripGeoId = -1, float charge = 0, 
             short disc = -1, short quad = -1, char layer = ' ',
             float rPos = 0, float rErr = 10000, float phiPos = 0, float phiErr = 10000, float zPos = 0, float zErr = 10000 );
    // StFgtHit(const StFgtHit&);             --> use default
    // StFgtHit& operator=(const StFgtHit&);  --> use default
    
    // deconstructor
    ~StFgtHit();
    
    // accessors/modifiers for the map
    stripWeightMap_t& getStripWeightMap();
    const stripWeightMap_t& getStripWeightMap() const;
    
    // modifer
    void setHardwareId( short disc, short quad, char layer );
    
    // other accessors
    int getKey() const;
    int getDisc() const;
    int getQuad() const;
    char getLayer() const;
    int getCentralStripGeoId() const;
    float getPositionR() const;
    float getPositionPhi() const;
    float getPositionZ() const;
    float getErrorR() const;
    float getErrorPhi() const;
    float getErrorZ() const;
    // note: no getCharge, as already have charge() defined through parent StHit
    float getChargeUncert() const;
    short getMaxAdc() const;
    
    // modifiers
    void setCentralStripGeoId( int geoId );
    void setPositionR( float position );
    void setPositionPhi( float position );
    void setPositionZ( float position );
    void setErrorR( float error );
    void setErrorPhi( float error );
    void setErrorZ( float error );
    void setDisc( short disc );
    void setQuad( short quad );
    void setLayer( char layer );
    void setChargeUncert( float sigma );
    
protected:
    void update2error();                          // set x,y part of inherited mPositionError
    
    
protected:
    // data members
    Int_t   mKey;                                 // unique label
    Float_t mR, mErrR, mPhi, mErrPhi;             // r, phi, z position and error
    Int_t   mCentralStripGeoId;                   // obvious
    Float_t mChargeUncert;                        // uncertanity on the charge
    // for keeping track of which strips constribute to which cluster (not persistant)
    stripWeightMap_t mStripWeightMap;             //! 
    
private:   
    ClassDef(StFgtHit,1);
}; 


// inline functions

inline short StFgtHit::getMaxAdc() const {
    short mMaxAdc = -1;
    
    for(stripWeightMap_t::const_iterator it=mStripWeightMap.begin(); it != mStripWeightMap.end(); it++ ){
        short adcVal = it->first->getMaxAdc();
        if( adcVal > mMaxAdc ) 
            mMaxAdc = adcVal;
    };
    
    return mMaxAdc;
}

inline int StFgtHit::getDisc() const {
    return static_cast< int >(mHardwarePosition/8);
};

inline int StFgtHit::getQuad() const {
    return static_cast< int >((mHardwarePosition/2)%4);
};

inline char StFgtHit::getLayer() const {
    return (mHardwarePosition % 2) ? 'R' : 'P';
};

inline int StFgtHit::getKey() const {
    return mKey;
};

inline stripWeightMap_t& StFgtHit::getStripWeightMap() {
    return mStripWeightMap;
};

inline const stripWeightMap_t& StFgtHit::getStripWeightMap() const {
    return mStripWeightMap;
};

inline void StFgtHit::setHardwareId( short disc, short quad, char layer ){
    mHardwarePosition = disc*8+quad*2+(layer=='R');
};

inline int StFgtHit::getCentralStripGeoId() const {
    return mCentralStripGeoId;
};

inline float StFgtHit::getPositionR() const {
    return mR;
};

inline float StFgtHit::getPositionPhi() const {
    return mPhi;
};

inline float StFgtHit::getPositionZ() const {
    return mPosition.z();
};

inline float StFgtHit::getErrorR() const {
    return mErrR;
};

inline float StFgtHit::getErrorPhi() const {
    return mErrPhi;
};

inline float StFgtHit::getErrorZ() const {
    return mPositionError.z();
};

inline void StFgtHit::setCentralStripGeoId( int geoId ){
    mCentralStripGeoId = geoId;
};

inline void StFgtHit::setDisc( short disc ){
    short quad = getQuad();
    char layer = getLayer();
    
    setHardwareId( disc, quad, layer );
};

inline void StFgtHit::setQuad( short quad ){
    short disc = getDisc();
    char layer = getLayer();
    
    setHardwareId( disc, quad, layer );
};

inline void StFgtHit::setLayer( char layer ){
    short disc = getDisc();
    short quad = getQuad();
    
    setHardwareId( disc, quad, layer );
};

inline void StFgtHit::setPositionR( float position ){
    mR = position;
    mPosition.setX( mR*cos( mPhi ) );
    mPosition.setY( mR*sin( mPhi ) );
};

inline void StFgtHit::setPositionPhi( float position ){
    mPhi = position;
    mPosition.setX( mR*cos( mPhi ) );
    mPosition.setY( mR*sin( mPhi ) );
};

inline void StFgtHit::setPositionZ( float position ){
    mPosition.setZ( position );
};

inline void StFgtHit::setErrorR( float error ){
    mErrR = error;
    update2error();
};

inline void StFgtHit::setErrorPhi( float error ){
    mErrPhi = error;
    update2error();
};

inline void StFgtHit::setErrorZ( float error ){
    mPositionError.setZ( error );
};

// charge uncertainty

inline float StFgtHit::getChargeUncert() const { return mChargeUncert; };
inline void StFgtHit::setChargeUncert( float sigma ){ mChargeUncert = sigma; };

#endif

/***************************************************************************
 *
 * $Id: StFgtStrip.h,v 2.1 2012/04/16 20:20:49 ullrich Exp $
 * Author: S. Gliske, Oct 2011
 *
 ***************************************************************************
 *
 * Description: data for individual strip of the FGT.
 *
 ***************************************************************************
 *
 * $Log: StFgtStrip.h,v $
 * Revision 2.1  2012/04/16 20:20:49  ullrich
 * Initial Revision
 *
 *
 **************************************************************************/

#ifndef _ST_FGT_STRIP_H_
#define _ST_FGT_STRIP_H_

#include "StObject.h"
#include "StEnumerations.h"

class StFgtStrip : public StObject {    
public:
    // constructors
    StFgtStrip();
    StFgtStrip(const StFgtStrip&);
    StFgtStrip& operator=( const StFgtStrip& );
    
    // deconstructor
    ~StFgtStrip();
    
    // accessors
    int   getGeoId()   const;
    short getAdc( int tb = -1 ) const;
    short getMaxAdc() const;
    short getClusterSeedType() const;
    float getCharge()  const;
    float getChargeUncert() const;
    void  getElecCoords( int& rdo, int& arm,  int& apv,  int& chan ); 
    float getPed() const;
    float getPedErr() const;
    bool  chargeValid() const;
    
    // modifiers
    void setGeoId  ( int geoId );
    void setAdc    ( short adc, int tb = -1 );
    void setMaxAdc(short adc);
    void setClusterSeedType( short seedType );
    void setCharge ( float charge );
    void setChargeUncert( float chargeUncert);
    void setElecCoords( int rdo, int arm,  int apv,  int chan ); 
    void setPed(float ped);
    void setPedErr(float pedErr);
    void invalidateCharge();
    
    // query default time bin
    static void setDefaultTimeBin( int tb );
    static int getDefaultTimeBin();
    
protected:
    // data members
    Int_t   mGeoId;                // indexing: 6 disk * 4 quad * 2 planes * 720 strips
    Short_t mAdc[kFgtNumTimeBins]; // note "StRoot/RTS/src/DAQ_FGT/daq_fgt.h" uses UShort_t
    Short_t mMaxAdc;               // max over the time bins
    Short_t mClusterSeedType;      // See types in StFgtConsts.h
    Float_t mCharge;               // before GEM, units (C), relation: ADC = ped + charge*gain(r,phi,disc)
    Float_t mChargeUncert;
    Int_t   mRdo, mArm, mApv, mChan; // elec coords, straight out of the DAQ file
    Float_t mPed;
    Float_t mPedErr;
    
    static Int_t mDefaultTimeBin;
    
    // to signify an invalid value of the charge
    enum { kInvalidChargeValue = -10000 };
    
private:   
    ClassDef(StFgtStrip,1);
};

// Functor for sorting the strips in the strip weight map.
struct stripPtrLessThan {
    bool operator() (const StFgtStrip* strip1, const StFgtStrip* strip2) const;
};

// inline functions

// accessors

inline int   StFgtStrip::getGeoId()           const { return mGeoId; };
inline short StFgtStrip::getMaxAdc()          const { return mMaxAdc; };
inline short StFgtStrip::getClusterSeedType() const { return mClusterSeedType; };
inline float StFgtStrip::getCharge()          const { return mCharge; };
inline float StFgtStrip::getChargeUncert()    const { return mChargeUncert; };

inline void    StFgtStrip::getElecCoords( int& rdo, int& arm,  int& apv,  int& chan ){ rdo = mRdo; arm = mArm; apv = mApv; chan = mChan; };

inline float StFgtStrip::getPed()             const { return mPed; };
inline float StFgtStrip::getPedErr()          const { return mPedErr; };
inline bool  StFgtStrip::chargeValid()        const { return mCharge != 0 && mCharge != kInvalidChargeValue; };

inline short StFgtStrip::getAdc( int tb ) const {
    return mAdc[ (tb < 0 || tb >= kFgtNumTimeBins) ? mDefaultTimeBin : tb ];
};

// modifiers

inline void StFgtStrip::setGeoId( int geoId )                 { mGeoId = geoId; };
inline void StFgtStrip::setMaxAdc( short adc )                { mMaxAdc=adc; };
inline void StFgtStrip::setClusterSeedType( short seedType)   { mClusterSeedType=seedType; };
inline void StFgtStrip::setCharge ( float charge )            { mCharge = charge; };
inline void StFgtStrip::setChargeUncert ( float chargeUncert ){ mChargeUncert = chargeUncert; };

inline void StFgtStrip::setElecCoords( int rdo, int arm,  int apv,  int chan ){ mRdo = rdo; mArm = arm; mApv = apv; mChan = chan; };

inline void StFgtStrip::setPed(float ped)                     { mPed=ped; }
inline void StFgtStrip::setPedErr(float pedErr)               { mPedErr=pedErr; }
inline void StFgtStrip::invalidateCharge()                      { mCharge = kInvalidChargeValue; };

inline void StFgtStrip::setAdc( short adc, int tb ) {
    mAdc[ (tb < 0 || tb >= kFgtNumTimeBins) ? mDefaultTimeBin : tb ] = adc;
    if( adc > mMaxAdc )
        mMaxAdc=adc;
};

// static default time bin

inline int StFgtStrip::getDefaultTimeBin()          { return mDefaultTimeBin; };
inline void StFgtStrip::setDefaultTimeBin( int tb ) { mDefaultTimeBin = tb; };

#endif

/***************************************************************************
 *
 * Authors: K.S. Engle and Richard Witt (witt@usna.edu), Jan 2013
 * based on StFgtStrip
 *
 ***************************************************************************
 *
 * Description: data for individual strip of the GMT.
 *
 ***************************************************************************/

#ifndef _ST_GMT_STRIP_H_
#define _ST_GMT_STRIP_H_

#include "StObject.h"
#include "StEnumerations.h"
#include "St_base/StMessMgr.h"

class StGmtStrip : public StObject {    
public:
    // constructors
    StGmtStrip();
    StGmtStrip(const StGmtStrip&);
    StGmtStrip& operator=( const StGmtStrip& );
    
    // deconstructor
    ~StGmtStrip();
    
    // accessors
    int   getGeoId()   const;
    int   getModule()   const;
    int   getCoordNum()   const;
    int  isY()   const;
    int  isC()   const;
    float getPosition()   const;
    short getAdc( int tb = -1 ) const;
    short getPedSubtractedAdc( int tb = -1 ) const;
    short getMaxAdc() const;
    short getMaxPedSubtractedAdc() const;
    short getMaxAdcTB() const;
    short getMaxPedSubtractedAdcTB() const;
//     short getClusterSeedType() const;
    float getCharge()  const;
    float getChargeUncert() const;
    void  getElecCoords( int& rdo, int& arm,  int& apv,  int& chan ); 
    float getPed() const;
    float getPedStdDev() const;
    float getPedErr() const;
    bool  chargeValid() const;
    int   getRdo()   const;
    int   getArm()   const;
    int   getApv()   const;
    int   getChannel()   const;
    
    // modifiers
    void setGeoId  ( int geoId );
    void setModule  ( int module );
    void setCoordNum  ( int coord );
    void setIsY  ( int isY );
    void setIsC  ( int isC );
    void setPosition  ( float position );
    void setAdc    ( short adc, int tb = -1 );
    void setPedSubtractedAdc    ( short adc, int tb = -1 );
    void setMaxAdc(short adc);
    void setMaxPedSubtractedAdc(short adc);
//     void setClusterSeedType( short seedType );
    void setCharge ( float charge );
    void setChargeUncert( float chargeUncert);
    void setElecCoords( int rdo, int arm,  int apv,  int chan ); 
    void setPed(float ped);
    void setPedStdDev(float pedStdDev);
    void setPedErr(float pedErr);
    void invalidateCharge();
    
    // query default time bin
    static void setDefaultTimeBin( int tb );
    static int getDefaultTimeBin();
    void     Print(Option_t *option="") const;
protected:
    // data members
    Int_t   mGeoId;                // indexing: 8 modules * 2 APV * 128 channels = 2048
    Int_t   mModule;                // indexing: 8 modules
    Int_t   mCoordNum;                // 0-127 in each dimension (X and Y)
    Int_t   mIsY;                  // is it a pad (as opposed to a strip)?
    Float_t mPosition;                  // coordinate position relative to local origin (in module)
    Short_t mAdc[kGmtNumTimeBins]; // note "StRoot/RTS/src/DAQ_GMT/daq_gmt.h" uses UShort_t
    Short_t mPedSubtractedAdc[kGmtNumTimeBins]; // note "StRoot/RTS/src/DAQ_GMT/daq_gmt.h" uses UShort_t
    Short_t mMaxAdc;               // max over the time bins
    Short_t mMaxPedSubtractedAdc;               // max over the time bins
    Short_t mMaxAdcTB;               // max over the time bins
    Short_t mMaxPedSubtractedAdcTB;               // max over the time bins
//     Short_t mClusterSeedType;      // See types in StGmtConsts.h
    Float_t mCharge;               // before GEM, units (C), relation: ADC = ped + charge*gain(r,phi,disc)
    Float_t mChargeUncert;
    Int_t   mRdo, mArm, mApv, mChan; // elec coords, straight out of the DAQ file
    Float_t mPed;
    Float_t mPedStdDev; // standard deviation
    Float_t mPedErr;  // RMS
    
    Int_t mIsC; //is used in a cluster ?

    static Int_t mDefaultTimeBin;
    
    // to signify an invalid value of the charge
    enum { kInvalidChargeValue = -10000 };
    
private:   
    ClassDef(StGmtStrip,1)
};
ostream&              operator<<(ostream& os, StGmtStrip const & v);

// Functor for sorting the strips in the strip weight map.
struct gmtStripPtrLessThan {
    bool operator() (const StGmtStrip* strip1, const StGmtStrip* strip2) const;
};

// inline functions

// accessors

inline int   StGmtStrip::getGeoId()           const { return mGeoId; };
inline int   StGmtStrip::getModule()           const { return mModule; };
inline int   StGmtStrip::getCoordNum()           const { return mCoordNum; };
inline int   StGmtStrip::isY()           const { return mIsY; };
inline int   StGmtStrip::isC()           const { return mIsC; };
inline float   StGmtStrip::getPosition()           const { return mPosition; };
inline short StGmtStrip::getMaxAdc()          const { return mMaxAdc; };
inline short StGmtStrip::getMaxPedSubtractedAdc()          const { return mMaxPedSubtractedAdc; };
inline short StGmtStrip::getMaxAdcTB()          const { return mMaxAdcTB; };
inline short StGmtStrip::getMaxPedSubtractedAdcTB()          const { return mMaxPedSubtractedAdcTB; };
// inline short StGmtStrip::getClusterSeedType() const { return mClusterSeedType; };
inline float StGmtStrip::getCharge()          const { return mCharge; };
inline float StGmtStrip::getChargeUncert()    const { return mChargeUncert; };

inline void    StGmtStrip::getElecCoords( int& rdo, int& arm,  int& apv,  int& chan ){ rdo = mRdo; arm = mArm; apv = mApv; chan = mChan; };

inline float StGmtStrip::getPed()             const { return mPed; };
inline float StGmtStrip::getPedStdDev()             const { return mPedStdDev; };
inline float StGmtStrip::getPedErr()          const { return mPedErr; };
inline bool  StGmtStrip::chargeValid()        const { return mCharge != 0 && mCharge != kInvalidChargeValue; };

inline short StGmtStrip::getAdc( int tb ) const {
    return mAdc[ (tb < 0 || tb >= kGmtNumTimeBins) ? mDefaultTimeBin : tb ];
};
inline short StGmtStrip::getPedSubtractedAdc( int tb ) const {
    return mPedSubtractedAdc[ (tb < 0 || tb >= kGmtNumTimeBins) ? mDefaultTimeBin : tb ];
};

inline int   StGmtStrip::getRdo()           const { return mRdo; };
inline int   StGmtStrip::getArm()           const { return mArm; };
inline int   StGmtStrip::getApv()           const { return mApv; };
inline int   StGmtStrip::getChannel()           const { return mChan; };

// modifiers

inline void StGmtStrip::setGeoId( int geoId )                 { mGeoId = geoId; };
inline void StGmtStrip::setModule( int module )                 { mModule = module; };
inline void StGmtStrip::setCoordNum( int coord )                 { mCoordNum = coord; };
inline void StGmtStrip::setIsY( int isy )                 { mIsY = isy; };
inline void StGmtStrip::setIsC( int isy )                 { mIsC = isy; };
inline void StGmtStrip::setPosition( float position )                 { mPosition = position; };
inline void StGmtStrip::setMaxAdc( short adc )                { mMaxPedSubtractedAdc=adc; };
// inline void StGmtStrip::setClusterSeedType( short seedType)   { mClusterSeedType=seedType; };
inline void StGmtStrip::setCharge ( float charge )            { mCharge = charge; };
inline void StGmtStrip::setChargeUncert ( float chargeUncert ){ mChargeUncert = chargeUncert; };

inline void StGmtStrip::setElecCoords( int rdo, int arm,  int apv,  int chan ){ mRdo = rdo; mArm = arm; mApv = apv; mChan = chan; };

inline void StGmtStrip::setPed(float ped)                     { mPed=ped; }
inline void StGmtStrip::setPedStdDev(float pedsigma)                     { mPedStdDev=pedsigma; }
inline void StGmtStrip::setPedErr(float pedErr)               { mPedErr=pedErr; }
inline void StGmtStrip::invalidateCharge()                      { mCharge = kInvalidChargeValue; };

inline void StGmtStrip::setAdc( short adc, int tb ) {
    mAdc[ (tb < 0 || tb >= kGmtNumTimeBins) ? mDefaultTimeBin : tb ] = adc;
    if( adc > mMaxAdc ) {
        mMaxAdc=adc;
        mMaxAdcTB=tb;
    }
    //LOG_INFO << "SETADC : " << adc << "," << tb << "\t=> " << mMaxAdc << "," << mMaxAdcTB << endm;
};

inline void StGmtStrip::setPedSubtractedAdc( short adc, int tb ) {
    mPedSubtractedAdc[ (tb < 0 || tb >= kGmtNumTimeBins) ? mDefaultTimeBin : tb ] = adc;
    if( adc > mMaxPedSubtractedAdc ) {
        mMaxPedSubtractedAdc=adc;
        mMaxPedSubtractedAdcTB=tb;
    }
    //LOG_INFO << "SETADCPED : " << adc << "," << tb << "\t=> " << mMaxPedSubtractedAdc << "," << mMaxPedSubtractedAdcTB << endm;
};

// static default time bin

inline int StGmtStrip::getDefaultTimeBin()          { return mDefaultTimeBin; };
inline void StGmtStrip::setDefaultTimeBin( int tb ) { mDefaultTimeBin = tb; };

#endif

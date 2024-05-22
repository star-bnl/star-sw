/**
 * \class StGmtStrip
 * \brief Holds data for the strip in GMT
 * 
 * Data for an individual strip in GMT (based on StFgtStrip).
 *
 * \author K.S. Engle, Jan. 2013
 * \author Richard Witt (witt@usna.edu), Jan. 2013
 * \author Grigory Nigmatkulov (nigmatkulov@gmail.com), Dec. 2020
 */

#ifndef StGmtStrip_hh
#define StGmtStrip_hh

// STAR headers
#include "StObject.h"
#include "St_base/StMessMgr.h"
#include "StEnumerations.h"

//________________
class StGmtStrip : public StObject {    
 public:
  /// Constructor
  StGmtStrip();
  /// Copy constructor
  StGmtStrip(const StGmtStrip&);
  /// Assignment operator
  StGmtStrip& operator=( const StGmtStrip& );
  /// Destructor
  ~StGmtStrip();
  /// Print strip information (parameters)
  void Print(Option_t *option="") const;

  //
  // Getters
  //

  /// Detector ID (8 modules * 2 APV * 128 channels)
  Int_t getGeoId() const      { return mGeoId; };
  /// Module ID (8 modules in total)
  Int_t getModule() const     { return mModule; }
  /// Coordinate (0-127)
  Int_t getCoordNum() const   { return mCoordNum; }
  /// Is it a pad?
  Int_t isY() const           { return mIsY; }
  /// Is used in a cluster
  Int_t  isC()   const        { return mIsC; }
  /// Coordinate position relative to local origin (in module)
  Float_t getPosition() const { return mPosition; }
  /// ADC in a strip for a given time bin
  Short_t getAdc( Int_t tb = -1 ) const
  { return mAdc[ (tb < 0 || tb >= kGmtNumTimeBins) ? mDefaultTimeBin : tb ]; }
  /// Pedestal subtracted ADC for a give time bin
  Short_t getPedSubtractedAdc( Int_t tb = -1 ) const
  { return mPedSubtractedAdc[ (tb < 0 || tb >= kGmtNumTimeBins) ? mDefaultTimeBin : tb ]; }  
  /// Maximal ADC over the time bins
  Short_t getMaxAdc() const   { return mMaxAdc; }
  /// Maximal pedestal subtraced ADC over the time bins
  Short_t getMaxPedSubtractedAdc() const { return mMaxPedSubtractedAdc; }
  /// Maximal over the time bins
  Short_t getMaxAdcTB() const { return mMaxAdcTB; }
  /// Maximal over the time bins
  Short_t getMaxPedSubtractedAdcTB() const { return mMaxPedSubtractedAdcTB; }

  /// Charge before GEM (in C)
  Float_t getCharge() const   { return mCharge; }
  /// Charge uncertainty
  Float_t getChargeUncert() const { return mChargeUncert; }
  /// Coordinates from electronics
  void  getElecCoords( Int_t& rdo, Int_t& arm,  Int_t& apv,  Int_t& chan )
  { rdo = mRdo; arm = mArm; apv = mApv; chan = mChan; }
  /// Pedestal
  Float_t getPed() const       { return mPed; }
  /// Pedestal standard deviation
  Float_t getPedStdDev() const { return mPedStdDev; }
  /// Pedestal error
  Float_t getPedErr() const    { return mPedErr; }
  /// Check if charge is valid
  Bool_t  chargeValid() const
  { return mCharge != 0 && mCharge != kInvalidChargeValue; }
  /// RDO number
  Int_t getRdo() const         { return mRdo; }
  /// Arm
  Int_t getArm() const         { return mArm; }
  /// Apv
  Int_t getApv() const         { return mApv; }
  /// Channel number
  Int_t getChannel() const     { return mChan; }
  /// Default time bin
  static Int_t getDefaultTimeBin() { return mDefaultTimeBin; }

  //
  // Setters
  //
    
  /// Set detector GeoId
  void setGeoId( Int_t geoId )       { mGeoId = geoId; }
  /// Set module
  void setModule( Int_t module )     { mModule = module; }
  /// Set coordinate
  void setCoordNum( Int_t coord )    { mCoordNum = coord; }
  /// Set is it a pad
  void setIsY( Int_t isY )           { mIsY = isY; }
  /// Set is used in a cluster
  void setIsC( Int_t isC )           { mIsC = isC; }
  /// Set position relative to local origin (in module)
  void setPosition( Float_t position ) { mPosition = position; }
  /// Set ADC for the given time bucket
  void setAdc( Short_t adc, Int_t tb = -1 ) {
    mAdc[ (tb < 0 || tb >= kGmtNumTimeBins) ? mDefaultTimeBin : tb ] = adc;
    if( adc > mMaxAdc ) { mMaxAdc=adc; mMaxAdcTB=tb; }
  }
  /// Set pedestal stubtracted ADC for the given time bucket
  void setPedSubtractedAdc( Short_t adc, Int_t tb = -1 ) {
    mPedSubtractedAdc[ (tb < 0 || tb >= kGmtNumTimeBins) ? mDefaultTimeBin : tb ] = adc;
    if( adc > mMaxPedSubtractedAdc ) { mMaxPedSubtractedAdc=adc; mMaxPedSubtractedAdcTB=tb; }
  }
  /// Set maximal ADC over time buckets
  void setMaxAdc(Short_t adc)        { mMaxAdc=adc; }
  /// Set maximal pedestal subtracked ADC over time buckets
  void setMaxPedSubtractedAdc(Short_t adc) { mMaxPedSubtractedAdc = adc; }

  /// Set charge before the GEM (in C)
  void setCharge( Float_t charge )   { mCharge = charge; }
  /// Set charge uncertainty
  void setChargeUncert( Float_t chargeUncert) { mChargeUncert = chargeUncert; }
  /// Set coordinates from electronics
  void setElecCoords( Int_t rdo, Int_t arm,  Int_t apv,  Int_t chan )
  { mRdo = rdo; mArm = arm; mApv = apv; mChan = chan; }
  /// Set pedestal
  void setPed(Float_t ped)           { mPed=ped; }
  /// Set pedestal standard deviation
  void setPedStdDev(Float_t pedStdDev) { mPedStdDev=pedStdDev; }
  /// Set pedestal error
  void setPedErr(Float_t pedErr)     { mPedErr=pedErr; }
  /// Set charge to the invalid state
  void invalidateCharge()            { mCharge = kInvalidChargeValue; };
  /// Set default time bin
  static void setDefaultTimeBin( Int_t tb ) { mDefaultTimeBin = tb; }

 protected:
  /// Indexing: 8 modules * 2 APV * 128 channels = 2048
  Int_t   mGeoId;
  /// Indexing: 8 modules
  Int_t   mModule;
  /// 0-127 in each dimension (X and Y)
  Int_t   mCoordNum;
  /// Is it a pad (as opposed to a strip)?
  Int_t   mIsY;
  /// Coordinate position relative to local origin (in module)
  Float_t mPosition;
  /// ADC in a strip. Note "StRoot/RTS/src/DAQ_GMT/daq_gmt.h" uses UShort_t
  Short_t mAdc[kGmtNumTimeBins];
  /// ADC after pedestal subtraction
  /// Note "StRoot/RTS/src/DAQ_GMT/daq_gmt.h" uses UShort_t
  Short_t mPedSubtractedAdc[kGmtNumTimeBins]; 
  /// Maximal ADC over the time bins
  Short_t mMaxAdc;
  /// Maximal pedestal subtracted ADC over the time bins
  Short_t mMaxPedSubtractedAdc;
  /// Maximal over the time bins
  Short_t mMaxAdcTB;
  /// Max over the time bins
  Short_t mMaxPedSubtractedAdcTB;
  /// Charge before GEM, units (C)
  /// relation: ADC = ped + charge*gain(r,phi,disc)
  Float_t mCharge;
  /// Charge uncertainty
  Float_t mChargeUncert;

  // elec coords, straight out of the DAQ file

  /// RDO number
  Int_t mRdo;
  Int_t mArm; 
  Int_t mApv;
  /// Channel number
  Int_t mChan;

  /// Pedestal
  Float_t mPed;
  /// Pedestal standard deviation
  Float_t mPedStdDev;
  /// Pedestal RMS
  Float_t mPedErr;  
  // Is used in a cluster ?
  Int_t mIsC; 
  /// Time bin
  static Int_t mDefaultTimeBin;
    
  /// To signify an invalid value of the charge
  enum { kInvalidChargeValue = -10000 };
    
 private:   
  ClassDef(StGmtStrip,1)
};

ostream& operator<<(ostream& os, StGmtStrip const & v);

// Functor for sorting the strips in the strip weight map.
struct gmtStripPtrLessThan {
  bool operator() (const StGmtStrip* strip1, const StGmtStrip* strip2) const;
};


#endif // #define StGmtStrip_hh

/**
 * \class StGmtHit
 * \brief Holds data for the hit in GMT
 * 
 * Data for an individual ``hit'' in GMT, i.e. a 1D cluster (based on StFgtHit).
 *
 * \author K.S. Engle, Jan. 2013
 * \author Richard Witt (witt@usna.edu), Jan. 2013
 * \author Grigory Nigmatkulov (nigmatkulov@gmail.com), Dec. 2020
 */

#ifndef StGmtHit_hh
#define StGmtHit_hh

// C++ headers
#include <map>

// StEvent headers
#include "StHit.h"
#include "StGmtStrip.h"

//________________
class StGmtHit : public StHit {
  public:
    /// Constructor
    StGmtHit( Int_t key = -1, Int_t module = -1, Float_t adcX = 0, 
            Float_t adcY = 0, Float_t dadcX = 0, Float_t dadcY = 0,
            Float_t localX = 0, Float_t localY = 0,
            Float_t localXErr = 10000, Float_t localYErr = 10000,
            Float_t sigmaX = 0, Float_t sigmaY = 0,
            Float_t sigmaXErr = 10000, Float_t sigmaYErr = 10000);
    /// Destructor
    ~StGmtHit();

    /// Print hit information (parameters)
    void Print(Option_t *option="") const;

    //
    // Getters
    //

    /// Unique detector ID
    virtual StDetectorId detector() const { return kGmtId; }

    /// Key
    Int_t getKey() const         { return mKey; }
    /// Module 
    Int_t getModule() const      { return hardwarePosition() - 1; }
    /// ADC in X
    Float_t getAdcX() const      { return mAdcX; }
    /// ADC error in X
    Float_t getErrorAdcX() const { return mdAdcX; }
    /// ADC in Y
    Float_t getAdcY() const      { return mAdcY; }
    /// ADC error in Y
    Float_t getErrorAdcY() const { return mdAdcY; }
    /// Local X coordinate
    Float_t getLocalX() const    { return position().x(); }
    /// Local X coordinate error
    Float_t getErrorLocalX() const { return positionError().x(); }
    /// Local Y coordinate
    Float_t getLocalY() const    { return position().y(); }
    /// Local Y coordinate error
    Float_t getErrorLocalY() const { return positionError().y(); }
    /// Position in X
    Float_t getSigmaX() const    { return mSigmaX; }
    /// Position error in X
    Float_t getErrorSigmaX() const { return mErrSigmaX; }
    /// Position in Y
    Float_t getSigmaY() const    { return mSigmaY; }
    /// Position error in X
    Float_t getErrorSigmaY() const { return mErrSigmaY; }
    /// Volume ID
    Int_t volumeID() const       { return 0; }

    //
    // Setters
    //

    /// Set ADC in X
    void setAdcX( Float_t adc )          { mAdcX = adc; }
    /// Set ADC error in X
    void setErrorAdcX( Float_t error )   { mdAdcX = error; }
    /// Set ADC in Y
    void setAdcY( Float_t adc )          { mAdcY = adc; }
    /// Set ADC error in Y
    void setErrorAdcY( Float_t error )   { mdAdcY = error; }
    /// Set local X
    void setLocalX( Float_t position )   { mPosition.setX(position); }
    /// Set local X error
    void setErrorLocalX( Float_t error ) { mPositionError.setX(error); }
    /// Set local Y
    void setLocalY( Float_t position )   { mPosition.setY(position); }
    /// Set local Y error
    void setErrorLocalY( Float_t error ) { mPositionError.setY(error); }
    /// Set local X
    void setSigmaX( Float_t sigma )      { mSigmaX = sigma; }
    /// Set local X error
    void setErrorSigmaX( Float_t error ) { mErrSigmaX = error; }
    /// Set local Y
    void setSigmaY( Float_t sigma )      { mSigmaY = sigma; }
    /// Set local Y error
    void setErrorSigmaY( Float_t error ) { mErrSigmaY = error; }
    /// Set module ID
    void setModule( Short_t module )     { setHardwarePosition(module+1); }    

 protected:
    /// Unique label
    Int_t   mKey;
    /// ADC counts in X
    Float_t mAdcX;
    /// ADC counts in Y
    Float_t mAdcY;
    /// ADC counts in X
    Float_t mdAdcX;
    /// ADC counts in Y
    Float_t mdAdcY;
    /// Position in local X
    Float_t mSigmaX;
    /// Position error in local X
    Float_t mErrSigmaX;
    /// Position in local Y
    Float_t mSigmaY;
    /// Position error in local Y
    Float_t mErrSigmaY;
    /// Uncertanity on the charge for keeping track of which 
    /// strips constribute to which cluster (not persistant)
    Float_t mChargeUncert;
    
  private:   
    ClassDef(StGmtHit,1)
};

ostream& operator<<(ostream& os, StGmtHit const & v);

#endif // #define StGmtHit_hh

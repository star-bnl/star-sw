/***************************************************************************
 *
 * Authors: K.S. Engle and Richard Witt (witt@usna.edu), Jan 2013
 * based on StFgtHit
 *
 ***************************************************************************
 *
 * Description: data for individual ``hit'' on the GMT, i.e. a 1D cluster.
 *
 ***************************************************************************/

#ifndef _ST_GMT_HIT_H_
#define _ST_GMT_HIT_H_

#include <map>

#include "StHit.h"
#include "StGmtStrip.h"

class StGmtHit : public StHit {
public:
    // constructors
    StGmtHit( Int_t key = -1, Int_t module = -1, 
	      Float_t adcX = 0, Float_t adcY = 0,
	      Float_t dadcX = 0, Float_t dadcY = 0,
	      Float_t localX = 0, Float_t localY = 0,
	      Float_t localXErr = 10000, Float_t localYErr = 10000,
	      Float_t sigmaX = 0, Float_t sigmaY = 0,
	      Float_t sigmaXErr = 10000, Float_t sigmaYErr = 10000) : 
      StHit( StThreeVectorF(localX,localY,0), StThreeVectorF(localXErr,localYErr,0), module+1, 0.), mKey(key), 
      mAdcX(adcX), mAdcY(adcY), mdAdcX(dadcX), mdAdcY(dadcY), 
      mSigmaX(sigmaX), mErrSigmaX(sigmaXErr), mSigmaY(sigmaY), mErrSigmaY(sigmaYErr) {}
    // deconstructor
    ~StGmtHit() {}
    virtual StDetectorId detector() const 		{return kGmtId;};   
    // accessors/modifiers for the map
    // modifer
    // other accessors
    Int_t getKey() const {return mKey;}
    Int_t getModule() const {return hardwarePosition() - 1; }
    Float_t getAdcX() const {return mAdcX;}
    Float_t getErrorAdcX() const {return mdAdcX;}
    Float_t getAdcY() const {return mAdcY;}
    Float_t getErrorAdcY() const {return mdAdcY;}
    Float_t getLocalX() const {return position().x();}
    Float_t getErrorLocalX() const {return positionError().x();}
    Float_t getLocalY() const {return position().y();}
    Float_t getErrorLocalY() const {return positionError().y();}
    Float_t getSigmaX() const {return mSigmaX;}
    Float_t getErrorSigmaX() const {return mErrSigmaX;}
    Float_t getSigmaY() const {return mSigmaY;}
    Float_t getErrorSigmaY() const {return mErrSigmaY;}

    // note: no getCharge, as already have charge() defined through parent StHit
//     Short_t getMaxAdc() const;
    
    // modifiers
    void setAdcX( Float_t adc ) {mAdcX = adc;}
    void setErrorAdcX( Float_t error ) {mdAdcX = error;}
    void setAdcY( Float_t adc ) {mAdcY = adc;}
    void setErrorAdcY( Float_t error ) {mdAdcY = error;}
    void setLocalX( Float_t position ) {mPosition.setX(position);}
    void setErrorLocalX( Float_t error ) {mPositionError.setX(error);}
    void setLocalY( Float_t position ) {mPosition.setY(position);}
    void setErrorLocalY( Float_t error ) {mPositionError.setY(error);}
    void setSigmaX( Float_t sigma ) {mSigmaX = sigma;}
    void setErrorSigmaX( Float_t error ) {mErrSigmaX = error;}
    void setSigmaY( Float_t sigma ) {mSigmaY = sigma;}
    void setErrorSigmaY( Float_t error ) {mErrSigmaY = error;}
    void setModule( Short_t module ) {setHardwarePosition(module+1);}
    void     Print(Option_t *option="") const;
protected:
    // data members
    Int_t   mKey;                                 // unique label
    Float_t mAdcX, mAdcY;     // Adc counts in X and Y 
    Float_t mdAdcX, mdAdcY;     // Adc counts in X and Y 
    Float_t mSigmaX, mErrSigmaX;     // local x position and error
    Float_t mSigmaY, mErrSigmaY;     // local y position and error

    Float_t mChargeUncert;                        // uncertanity on the charge
    // for keeping track of which strips constribute to which cluster (not persistant)
    
private:   
    ClassDef(StGmtHit,2)
}; 
ostream&   operator<<(ostream& os, StGmtHit const & v);
#endif

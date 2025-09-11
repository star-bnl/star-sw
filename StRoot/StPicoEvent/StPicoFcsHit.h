/***************************************************************************
 *
 * Author: jdb, Feb 2022
 ***************************************************************************
 *
 * Description: StPicoFcsHit stores the Fcs Hits
 *
 **************************************************************************/
#ifndef StPicoFcsHit_hh
#define StPicoFcsHit_hh

#include <TObject.h>
#include <vector>
#include "TVector3.h"
#include "TLorentzVector.h"

class StPicoFcsHit : public TObject {

public:
    /// Constructor
    StPicoFcsHit();
    /// Copy constructor
    StPicoFcsHit(const StPicoFcsHit &fwdTrack);
    /// Destructor
    virtual ~StPicoFcsHit();

    virtual void Print(const Char_t *option = "") const;    
    unsigned short detectorId() const { return mDetectorId; } 
    Int_t id() const { return mId; }    // Id of the hit winthin detectorId
    float energy() const { return mFourMomentumT; } // Energy
    const TLorentzVector fourMomentum() const { return TLorentzVector( mFourMomentumX, mFourMomentumY, mFourMomentumZ, mFourMomentumT ); } // Hit four-momentum (px, py, pz, E)

    void setDetectorId(unsigned short detector) { mDetectorId=(UShort_t)detector; }
    void setId(int id) { mId = (UShort_t)id; }
    void setFourMomentum(float px, float py, float pz, float e) { mFourMomentumX = px; mFourMomentumY = py; mFourMomentumZ = pz; mFourMomentumT = e; }
    void setFourMomentum(TLorentzVector p4) { mFourMomentumX = p4.X(); mFourMomentumY = p4.Y(); mFourMomentumZ = p4.Z(); mFourMomentumT = p4.T(); }
    
protected:
    UShort_t mDetectorId;      // DetectorId
    UShort_t mId;              // Id of the hit within a detectorId
    Float_t mFourMomentumX;  // Four momentum component X
    Float_t mFourMomentumY;  // Four momentum component Y
    Float_t mFourMomentumZ;  // Four momentum component Z
    Float_t mFourMomentumT;  // Four momentum component T

    ClassDef(StPicoFcsHit, 1)
};

#endif


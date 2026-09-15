/***************************************************************************
 *
 * Author: jdb, Feb 2022
 ***************************************************************************
 *
 * Description: StPicoFwdVertex stores the Forward tracks built from Fst and Ftt
 *
 **************************************************************************/
#ifndef StPicoFwdVertex_hh
#define StPicoFwdVertex_hh

#include <TObject.h>
#include <vector>
#include "TVector3.h"
#include <climits>

class StPicoFwdVertex : public TObject {
    

public:
    /// Constructor
    StPicoFwdVertex(  ) : TObject(), mId(0), mNumberOfTracks(0), mChi2(-999), mPositionX(-999), mPositionY(-999), mPositionZ(-999) { /* empty */ }
    /// Copy constructor
    StPicoFwdVertex(const StPicoFwdVertex &fwdVertex){
        this->mId = fwdVertex.mId;
        this->mNumberOfTracks = fwdVertex.mNumberOfTracks;
        this->mChi2 = fwdVertex.mChi2;
        this->mPositionX = fwdVertex.mPositionX;
        this->mPositionY = fwdVertex.mPositionY;
        this->mPositionZ = fwdVertex.mPositionZ;
    }
    /// Destructor
    virtual ~StPicoFwdVertex() {}

    virtual void Print(const Char_t *option = "") const {
        printf("StPicoFwdVertex: Position = (%.3f, %.3f, %.3f) cm, Chi2 = %.3f, Number of Tracks = %u\n",
               mPositionX, mPositionY, mPositionZ, mChi2, mNumberOfTracks);
    }
    /// Return unique Id of the vertex
    Int_t   id() const              { return mId; }
    /// Return chi2 of the track
    Float_t chi2() const            { return mChi2; }
    /// Position X
    Float_t positionX() const       { return mPositionX; }
    /// Position Y
    Float_t positionY() const       { return mPositionY; }
    /// Position Z
    Float_t positionZ() const       { return mPositionZ; }
    /// Helper function to get position as a TVector3
    TVector3 position() const       { return TVector3(mPositionX, mPositionY, mPositionZ); }

    // Setters
    void setId(Int_t id)            { mId = id; }
    void setChi2(Float_t chi2)      { mChi2 = chi2; }
    void setPositionX(Float_t x)    { mPositionX = x; }
    void setPositionY(Float_t y)    { mPositionY = y; }
    void setPositionZ(Float_t z)    { mPositionZ = z; }
    void setPosition(Float_t x, Float_t y, Float_t z) { setPositionX(x); setPositionY(y); setPositionZ(z); }
    void setNumberOfTracks(Int_t n) { mNumberOfTracks = n; }
    
protected:

    // Track quality and convergence
    /// Unique track ID
    UShort_t mId;
    /// Number of tracks associated with the vertex
    UChar_t mNumberOfTracks;
    /// Chi2 of the track
    Float_t mChi2;
    /// x Position cm
    Float_t mPositionX;
    /// y Position cm
    Float_t mPositionY;
    /// z Position cm
    Float_t mPositionZ;

    ClassDef(StPicoFwdVertex,1)

};

#endif


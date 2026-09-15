/***************************************************************************
 *
 * Author: jdb, Feb 2022
 ***************************************************************************
 *
 * Description: StPicoFwdTrack stores the Forward tracks built from Fst and Ftt
 *
 **************************************************************************/
#ifndef StPicoFwdTrack_hh
#define StPicoFwdTrack_hh

#include <TObject.h>
#include <vector>
#include "TVector3.h"
#include <climits>



class StPicoFwdTrack : public TObject {
    

public:
    enum StPicoFwdTrackType { kGlobal=0, kBeamlineConstrained=1, kPrimaryVertexConstrained=2, kForwardVertexConstrained=3 };
    /// Constructor
    StPicoFwdTrack(  );
    /// Copy constructor
    StPicoFwdTrack(const StPicoFwdTrack &fwdTrack);
    /// Destructor
    virtual ~StPicoFwdTrack();

    virtual void Print(const Char_t *option = "") const;
    /// Return unique Id of the track
    Int_t   id() const              { return mId; }
    /// Return chi2 of the track
    Float_t chi2() const            { return mChi2; }
    /// Return p-value of the track
    Float_t pVal() const            { return mPVal / 10000.f; }
    /// Return momentum (GeV/c)
    TVector3 momentum() const { return TVector3( mMomentumX, mMomentumY, mMomentumZ ); }
    /// Return charge of the track (encoded in nHitsFit as: nHitsFit * charge)
    Short_t charge() const                 { return (mNumberOfFitPoints > 0) ? 1 : -1; }
    /// Quality of the fit (from status)
    bool   didFitConverge() const { return (mStatus >= 1); }
    bool   didFitConvergeFully() const { return (mStatus >= 2); }
    /// Return if the track fit converged
    Char_t  status() const            { return mStatus; }
    /// Index of the corresponding MC track
    Int_t idTruth() const                  { return mIdTruth; }
    /// Qualtiy of the MC track
    Int_t qaTruth() const                  { return mQATruth; }

    // Number of fit points used by GenFit
    Int_t   numberOfFitPoints() const { return mNumberOfFitPoints; }
    // unsigned int   numberOfPossibleFitPoints() const;

    // Number of points used in the track seed step
    Int_t   numberOfSeedPoints() const { return mNumberOfSeedPoints; }

    // Projected Projection at the ECal
    TVector3 ecalProjection() const { return TVector3( ((float)mECalX)/100.f, ((float)mECalY)/100.f, ((float)mECalZ)/10.f ); }
    // Projected Projection at the HCal
    TVector3 hcalProjection() const { return TVector3( ((float)mHCalX)/100.f, ((float)mHCalY)/100.f, ((float)mHCalZ)/10.f ); }

    // DCA to primary vertex
    Float_t dcaXY() const { return mDCAXY; }
    Float_t dcaZ() const { return mDCAZ; }
    // Index of the primary vertex used in the fit
    UChar_t vertexIndex() const {
        // extract bits 7…2:
        return (mVtxIndex >> 2) & 0x3F;
    }
    UChar_t trackType() const {
        // extract bits 1…0:
        return mVtxIndex & 0x03;
    }
    // Index of the corresponding Global Track if Primary, BLC, or FwdVertex constrained tracks
    UShort_t globalTrackIndex() const { return mGlobalTrackIndex; }
    bool isGlobalTrack() const { return (trackType() == StPicoFwdTrack::kGlobal); }
    bool isBeamLineConstrainedTrack() const { return (trackType() == StPicoFwdTrack::kBeamlineConstrained); }
    bool isPrimaryTrack() const { return (trackType() == StPicoFwdTrack::kPrimaryVertexConstrained); }
    bool isFwdVertexConstrainedTrack() const { return (trackType() == StPicoFwdTrack::kForwardVertexConstrained); }

     // access ecal match indices
     UChar_t ecalMatchIndex( Int_t i ) const { return (i < (Int_t)mEcalMatchIndex.size()) ? mEcalMatchIndex[i] : 0; }
     Int_t numberOfEcalMatchIndices() const { return (Int_t)mEcalMatchIndex.size(); }
     // access hcal match indices
     UChar_t hcalMatchIndex( Int_t i ) const { return (i < (Int_t)mHcalMatchIndex.size()) ? mHcalMatchIndex[i] : 0; }
     Int_t numberOfHcalMatchIndices() const { return (Int_t)mHcalMatchIndex.size(); }

    void setId( Int_t id) { mId = (UShort_t)id; }
    void setMomentum( TVector3 mom ) { mMomentumX = mom.X(); mMomentumY = mom.Y(); mMomentumZ = mom.Z(); }
    void setMomentum( double px, double py, double pz ) { mMomentumX = (Float_t)px; mMomentumY = (Float_t)py; mMomentumZ = (Float_t)pz; }
    void setStatus( UChar_t status ) { mStatus = status;}
    void setNumberOfSeedPoints( Int_t lNumberOfSeedPoints ) { mNumberOfSeedPoints = (UChar_t)lNumberOfSeedPoints;}
    void setNumberOfFitPoints( Int_t lNumberOfFitPoints ) { mNumberOfFitPoints = (Char_t)lNumberOfFitPoints;}
    void setChi2(Float_t chi2) { mChi2 = chi2; }
    void setPVal(Float_t pval);
    void addEcalCluster( UChar_t index ) { mEcalMatchIndex.push_back(index); }
    void addHcalCluster( UChar_t index ) { mHcalMatchIndex.push_back(index); }
    void setECalProjection( Float_t x, Float_t y, Float_t z ) { mECalX = (Short_t)(x*100.f); mECalY = (Short_t)(y*100.f); mECalZ = (Short_t)(z*10.f); }
    void setHCalProjection( Float_t x, Float_t y, Float_t z ) { mHCalX = (Short_t)(x*100.f); mHCalY = (Short_t)(y*100.f); mHCalZ = (Short_t)(z*10.f); }
    /// Set index of the corresonding MC track
    void setMcTruth(Int_t index, Int_t qa)   { mIdTruth = (UShort_t)index; mQATruth = (UShort_t)qa; }
    void setDca( Float_t dcaX, Float_t dcaY, Float_t dcaZ ) { mDCAXY =sqrt(dcaX*dcaX + dcaY*dcaY); mDCAZ = dcaZ; }
    void setVtxIndexRaw( UChar_t vtxIndex ) { mVtxIndex = vtxIndex; }
    void setGlobalTrackIndex( UShort_t index ) { mGlobalTrackIndex = index; }


protected:

    // Track quality and convergence
    /// Unique track ID
    UShort_t mId;
    /// Number of points used in seed
    UChar_t mNumberOfSeedPoints;
    /// Charge * nHitsFit
    Char_t mNumberOfFitPoints;
    /// Chi2 of the track
    Float_t mChi2;
    
    /// Px momentum (GeV/c)
    Float_t mMomentumX;
    /// Py momentum (GeV/c)
    Float_t mMomentumY;
    /// Pz momentum (GeV/c)
    Float_t mMomentumZ;
    /// convergence (0=no, 1=converge, 2=converge fully)
    UChar_t mStatus;
    /// DCA to primary vertex (XY)
    Float_t mDCAXY;
    /// DCA to primary vertex (Z)
    Float_t mDCAZ;
    /// Index of primary vertex used in fit (first 6 bits ) AND track type in last 2 bits
    /// Track Type: 0=Global, 1=BLC, 2=Primary, 3=FwdVertex (See StFwdTrack::TrackType)
    /// Global tracks set mVtxIndex to 2^6 - 1 = 63
    UChar_t mVtxIndex;
    /// USHRT_MAX if global tracks
    /// Index of the corresponding Global Track if BLC tracks
    /// Index of the corresponding BLC Track if primary tracks
    UShort_t mGlobalTrackIndex;

    /// ecal match index
    std::vector<UChar_t> mEcalMatchIndex;
    /// hcal match index
    std::vector<UChar_t> mHcalMatchIndex;

    /// MC track id
    UShort_t mIdTruth;
    /// MC track quality (percentage of hits coming from corresponding MC track)
    UShort_t mQATruth;

    /// p-value of the track fit (x10000)
    UShort_t mPVal; 

    /// ECal and HCal track projection positions
    /// These are packed 
    Short_t mECalX; 
    Short_t mECalY;
    Short_t mECalZ;
    Short_t mHCalX;
    Short_t mHCalY;
    Short_t mHCalZ;

    ClassDef(StPicoFwdTrack,1)

};

#endif


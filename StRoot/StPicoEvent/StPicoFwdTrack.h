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
#include "TRefArray.h"


class StPicoFwdTrack : public TObject {

public:
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
    Float_t chi2() const            { return mChi2 / 1000.f; }
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

    void setId( Int_t id) { mId = (UShort_t)id; }
    void setMomentum( TVector3 mom ) { mMomentumX = mom.X(); mMomentumY = mom.Y(); mMomentumZ = mom.Z(); }
    void setMomentum( double px, double py, double pz ) { mMomentumX = (Float_t)px; mMomentumY = (Float_t)py; mMomentumZ = (Float_t)pz; }
    void setStatus( UChar_t status ) { mStatus = status;}
    void setNumberOfSeedPoints( Int_t lNumberOfSeedPoints ) { mNumberOfSeedPoints = (UChar_t)lNumberOfSeedPoints;}
    void setNumberOfFitPoints( Int_t lNumberOfFitPoints ) { mNumberOfFitPoints = (Char_t)lNumberOfFitPoints;}
    void setChi2(Float_t chi2);
    void addEcalCluster( UChar_t index ) { mEcalMatchIndex.push_back(index); }
    void addHcalCluster( UChar_t index ) { mHcalMatchIndex.push_back(index); }
    /// Set index of the corresonding MC track
    void setMcTruth(Int_t index, Int_t qa)   { mIdTruth = (UShort_t)index; mQATruth = (UShort_t)qa; }
    
protected:

    // Track quality and convergence
    /// Unique track ID
    UShort_t mId;
    /// Number of points used in seed
    UChar_t mNumberOfSeedPoints;
    /// Charge * nHitsFit
    Char_t mNumberOfFitPoints;
    /// Chi2 of the track (encoding = chi2*1000)
    UShort_t mChi2;

    /// Px momentum (GeV/c)
    Float_t mMomentumX;
    /// Py momentum (GeV/c)
    Float_t mMomentumY;
    /// Pz momentum (GeV/c)
    Float_t mMomentumZ;
    /// convergence (0=no, 1=converge, 2=converge fully)
    UChar_t mStatus;

    /// ecal match index
    std::vector<UChar_t> mEcalMatchIndex;
    /// hcal match index
    std::vector<UChar_t> mHcalMatchIndex;

    /// MC track id
    UShort_t mIdTruth;
    /// MC track quality (percentage of hits coming from corresponding MC track)
    UShort_t mQATruth;
    
    ClassDef(StPicoFwdTrack,2)

};

#endif


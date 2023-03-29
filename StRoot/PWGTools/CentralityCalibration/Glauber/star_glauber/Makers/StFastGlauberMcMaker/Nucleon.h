
#ifndef __Nucleon_h__
#define __Nucleon_h__

#include "TVector3.h"

//____________________________________________________________________________________________________
// Class Nucleon: Nucleon class
class Nucleon {
  public:
    /// Default constructor
    /// (r, theta, phi) are the spherical coordinates of nucleon
    /// (Theta, Phi) are the orientation of nuclei, used to rotate nucleons with respect to those angles
    /// Rotation will be done by 1) Theta, and then 2) Phi by default
    /// if isThetaFirst = kFALSE, rotate Phi first, then Theta.
    Nucleon() ;
    virtual ~Nucleon(); /// Default destructor

    void Reset() ; /// Reset all data members
    void Set(const Double_t r, const Double_t theta, const Double_t phi,
        const Double_t impactParameter, const Double_t Theta, const Double_t Phi,
        const Bool_t isThetaFirst = kTRUE) ;

    Double_t GetX() const ; /// Get x position
    Double_t GetY() const ; /// Get y position
    Double_t GetZ() const ; /// Get z position
    Double_t GetXYZ(const Char_t* name="X") const ; /// Get x/y/z or their product (see source)
    Double_t GetPhi() const ; /// Get azimuthal angle of nucleon
    Double_t GetR() const ; /// Get radius

    UInt_t GetNpart()          const ; /// Get Npart(x,y)
    UInt_t GetNcoll()          const ; /// Get Ncoll(x,y)
    Double_t GetMultiplicity() const ; /// Get Multiplicity(x,y)
    Double_t GetWeight(const UInt_t weightId=0) const ; /// Get weight factor to calculate average quantities

    void IncrementNpart() ; /// Increment Npart(x,y)
    void IncrementNcoll() ; /// Increment Ncoll(x,y)
    void SetMultiplicity(const Double_t val) ; /// Set multiplicity

    const TVector3& GetVector() const ; /// Get (x,y,z) vector

  private:
    TVector3 mPosition     ; /// (x,y,z) position of nucleon
    UInt_t mNpart          ; /// Npart(x,y)
    UInt_t mNcoll          ; /// Ncoll(x,y)
    Double_t mMultiplicity ; /// Multiplicity(x,y)

    ClassDef(Nucleon, 1)
};

inline void Nucleon::IncrementNpart() { mNpart++ ; }
inline void Nucleon::IncrementNcoll() { mNcoll++ ; }
inline UInt_t Nucleon::GetNpart()          const { return mNpart ; }
inline UInt_t Nucleon::GetNcoll()          const { return mNcoll ; }
inline Double_t Nucleon::GetMultiplicity() const { return mMultiplicity ; }
inline void Nucleon::SetMultiplicity(const Double_t val) { mMultiplicity = val ; }

#endif


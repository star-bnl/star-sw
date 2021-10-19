
#include <assert.h>
#include "Nucleon.h"

ClassImp(Nucleon)

//____________________________________________________________________________________________________
// Default constructor
Nucleon::Nucleon()
{
  Reset() ;
}

//____________________________________________________________________________________________________
// Default destructor
Nucleon::~Nucleon()
{
}

//____________________________________________________________________________________________________
void Nucleon::Reset()
{
  mPosition.SetMagThetaPhi(-9999., -9999., -9999.);
  mNpart = 0;
  mNcoll = 0;
  mMultiplicity = 0.0;
}

//____________________________________________________________________________________________________
void Nucleon::Set(const Double_t r, const Double_t theta, const Double_t phi, 
    const Double_t impactParameter, const Double_t Theta, const Double_t Phi,
    const Bool_t isThetaFirst)
{
  /// Initial position of nucleon without rotation
  mPosition.SetMagThetaPhi(r, theta, phi);

  /// Rotate angles if Theta and Phi are non-zero
  if( Theta != 0.0 && Phi != 0.0 ){
    if(isThetaFirst){
      /// Rotate Theta, then Phi
      mPosition.RotateY(Theta);
      mPosition.RotateZ(Phi);
    }
    else{
      /// Rotate Phi, then Theta
      mPosition.RotateZ(Phi);
      mPosition.RotateY(Theta);
    }
  }

  mPosition.SetX( mPosition.X() + impactParameter );
  mNpart = 0;
  mNcoll = 0;
  mMultiplicity = 0.0;
}


//____________________________________________________________________________________________________
Double_t Nucleon::GetX() const
{
  return mPosition.X() ;
}

//____________________________________________________________________________________________________
Double_t Nucleon::GetY() const
{
  return mPosition.Y() ;
}

//____________________________________________________________________________________________________
Double_t Nucleon::GetZ() const
{
  return mPosition.Z() ;
}

//____________________________________________________________________________________________________
Double_t Nucleon::GetPhi() const
{
  return mPosition.Phi() ;
}

//____________________________________________________________________________________________________
Double_t Nucleon::GetR() const
{
  return mPosition.Mag() ;
}

//____________________________________________________________________________________________________
Double_t Nucleon::GetXYZ(const Char_t* name) const
{
  /// Get each x/y/z position, or product of them
  /// Input argument 'name' is case insensitive
  const TString type(name);

  if( type.CompareTo("x", TString::kIgnoreCase) == 0 )        return GetX() ;
  else if( type.CompareTo("y", TString::kIgnoreCase) == 0 )   return GetY() ;
  else if( type.CompareTo("z", TString::kIgnoreCase) == 0 )   return GetZ() ;
  else if( type.CompareTo("xx", TString::kIgnoreCase) == 0 )  return GetX() * GetX() ;
  else if( type.CompareTo("yy", TString::kIgnoreCase) == 0 )  return GetY() * GetY() ;
  else if( type.CompareTo("xy", TString::kIgnoreCase) == 0 )  return GetX() * GetY() ;
  else if( type.CompareTo("xxx", TString::kIgnoreCase) == 0 ) return GetX() * GetX() * GetX() ;
  else if( type.CompareTo("yyy", TString::kIgnoreCase) == 0 ) return GetY() * GetY() * GetY() ;
  else if( type.CompareTo("xxy", TString::kIgnoreCase) == 0 ) return GetX() * GetX() * GetY() ;
  else if( type.CompareTo("xyy", TString::kIgnoreCase) == 0 ) return GetX() * GetY() * GetY() ;
  else{
    Error("IcGenerator::GetXYZ", "Invalid name for sum, name=%s", name);
    assert(0);
  }

  // Never happen
  Error("Nucleon::GetXYZ", "Something is wrong");
  assert(0);
}


//____________________________________________________________________________________________________
const TVector3& Nucleon::GetVector() const
{
  return mPosition ;
}

//____________________________________________________________________________________________________
Double_t Nucleon::GetWeight(const UInt_t weightId) const
{
  /// Get multiplicity weight, npart, ncoll or multiplicity

  switch ( weightId ) {
    case 0: return 1.0 ;               // Npart weight
    case 1: return GetNcoll() ;        // Ncoll weight
    case 2: return GetMultiplicity() ; // Multiplicity weight
    default:
      Warning("Nucleon::GetWeight", "Invalid weightId, id=%3d. Return unit weight", weightId);
      return 1.0;
  }

  // Never happen
  Error("Nucleon::GetWeight", "Something is wrong");
  assert(0);
}


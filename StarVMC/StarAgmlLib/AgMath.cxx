#include "AgMath.h"
#include "TMath.h"

Int_t iand(Int_t x, Int_t y){ return x&y; }
Int_t ior (Int_t x, Int_t y){ return x|y; }
Int_t ieor(Int_t x, Int_t y){ return x^y; }

Int_t nint(Float_t  x){ return (Int_t)( (x>0)? x+0.5 : x-0.5 ); }
Int_t nint(Double_t x){ return (Int_t)( (x>0)? x+0.5 : x-0.5 ); }

Double_t tand(Double_t x){ return TMath::Tan(x*degrad); }
Double_t cosd(Double_t x){ return TMath::Cos(x*degrad); }
Double_t sind(Double_t x){ return TMath::Sin(x*degrad); }

Int_t mod(const Int_t &a, const Int_t &b)
{
	return a%b;
}
Float_t mod( const Float_t &a, const Float_t &b)
{
	return a - b*(Int_t)(a/b);
}

Float_t operator*( const Float_t &A, const AgPower<Int_t> &power )
{
  return TMath::Power(A,power.value);
}
Float_t operator*( const Float_t &A, const AgPower<Float_t> &power )
{
  return TMath::Power(A,power.value);
}

#ifndef __AgMath_h__
#define __AgMath_h__

#include "TObject.h"
#include "TMath.h"
#include <iostream>

// Provides single point to define and/or redefine mathematical
// keywords used in AgSTAR syntax.  i.e. cos, sin, cosh, tanh, ...

//
// Define some often used constants
//
const Double_t pi     = TMath::Pi();
const Double_t twopi  = 2.0 * pi;
const Double_t inch   = 2.5400;
const Double_t mil    = inch * 1.0E-3;
const Double_t mil_p  = mil; // ??? used in pipegeo
const Double_t cm     = 1.000;
const Double_t mm     = cm * 1.0E-1;
const Double_t degrad = TMath::Pi() / 180.0;
// doing the right way results in div by zero in rootcint...
const Double_t raddeg = 5.72957795130823229e+01;
const Double_t GeV    = 1.000;
const Double_t MeV    = 0.001 * GeV;
const Double_t keV    = 0.001 * MeV;

//
// Prevent some potentially nasty user-code conflicts with math.h, unitstd.h, ...
//
#define j0 _j0_
#define j1 _j1_
#define jn _jn_

#define y0 _y0_
#define y1 _y1_
#define yn _yn_

#define dup _dup_

#define tanf _tanf_
#define fpos _fpos_
//
// Implement some bitwise functions from fortran land
//
Int_t iand(Int_t x, Int_t y);
Int_t ior(Int_t x, Int_t y);
Int_t ieor(Int_t x, Int_t y);

Int_t nint(Double_t x);
Int_t nint(Float_t x);

//
// Fortran abs is fabs
//
#define abs(x) TMath::Abs(x)

// VAX-Fortran (omfg) trig functions taking arguement in degrees
Double_t tand(Double_t x);
Double_t cosd(Double_t x);
Double_t sind(Double_t x);
#if 0
template <typename T> T max(const T &a, const T &b ){ return TMath::Max(a,b); }
template <typename T> T min(const T &a, const T &b ){ return TMath::Min(a,b); }
#endif
Int_t mod(const Int_t &a, const Int_t &b);
Float_t mod(const Float_t &a, const Float_t &b);

#define cos(x) TMath::Cos( (Double_t) x )
#define sin(x) TMath::Sin( (Double_t) x )
#define tan(x) TMath::Tan( (Double_t) x )
#define exp(x) TMath::Exp( (Double_t) x )

// Overload the ^ operator


template<class T>
class AgPower
{
 public:
  AgPower(T a){ value=a; }
  T value;
};

Float_t operator*( const Float_t &A, const AgPower<Int_t> &power );
Float_t operator*( const Float_t &A, const AgPower<Float_t> &power );

//Float_t operator^(const Float_t &a, const Float_t &b);
using namespace std;
#endif
 

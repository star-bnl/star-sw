#ifndef TGeVSimParams_h
#define TGeVSimParams_h

//
// Params HEADER documentation goes here
//
// MSD
//

#include "TROOT.h"

// Create a class to store the parameters of each particle type for each event
class TGeVSimParams : public TObject {   
 public:
  TGeVSimParams();
  TGeVSimParams(Int_t p,Int_t mod,Float_t t,Float_t sy,Float_t ev,Float_t st,
		Float_t v1,Float_t v2,Float_t m); 
  ~TGeVSimParams() {}

  //void Print();
  void Print(Option_t* option="") const;  // argument to remove compiler warning about overloaded virtual functions 
  void PrintHeader();
  //void Clear();
  void Clear(Option_t* option="");  // argument to remove compiler warning about overloaded virtual functions 

  Int_t   PDG;
  Int_t   Model;
  Float_t Temp;
  Float_t SigmaY;
  Float_t ExpVel;
  Float_t SigmaTemp;
  Float_t V1Scalar;  // Flow can be set as a funct of pt and y and can be different
  Float_t V2Scalar;  // for each particle, so we can only store the scalars
  Int_t   Mult;

  ClassDef(TGeVSimParams, 1);
};

#endif



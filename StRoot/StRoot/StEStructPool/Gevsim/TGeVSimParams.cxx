////////////////////////////////////////////////////////////////////////////////
//
// Params Source File Documentation is HERE
//
// MSD
//
////////////////////////////////////////////////////////////////////////////////


#include "TGeVSimParams.h"
#include <iostream>

ClassImp(TGeVSimParams);

/////////////////////

TGeVSimParams::TGeVSimParams() {
  PDG = 0;
  Model = 0;
  Temp = 0;
  SigmaY = 0;
  ExpVel = 0;
  SigmaTemp = 0;
  V1Scalar = 0;
  V2Scalar = 0;
  Mult = 0;
}

/////////////////////

TGeVSimParams::TGeVSimParams(Int_t p,Int_t mod,Float_t t,Float_t sy,Float_t ev,Float_t st,
			 Float_t v1,Float_t v2,Float_t m) {
  PDG = p;
  Model = mod;
  Temp = t;
  SigmaY = sy;
  ExpVel = ev;
  SigmaTemp = st;
  V1Scalar = v1;
  V2Scalar = v2;
  Mult = (Int_t)m;
}

/////////////////////

void TGeVSimParams::Print(Option_t* option) const {

  printf("%d\t%d\t%d\t%4.3f\t%4.3f\t%4.3f\t%4.3f\t%4.3f\t%4.3f\n", 
	 PDG, Model, Mult, Temp, SigmaTemp, SigmaY, ExpVel, V1Scalar, V2Scalar);
}

/////////////////////

void TGeVSimParams::PrintHeader() {

  printf("PDG\tModel\tMult\tTemp\tSigTemp\tSigmaY\tExpVel\tV1sc\tV2sc\n");
}

/////////////////////

void TGeVSimParams::Clear(Option_t* option) {
  PDG = 0;
  Model = 0;
  Temp = 0;
  SigmaY = 0;
  ExpVel = 0;
  SigmaTemp = 0;
  V1Scalar = 0;
  V2Scalar = 0;
  Mult = 0;
}

/////////////////////

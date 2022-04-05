#ifndef TGeVSimEvent_h
#define TGeVSimEvent_h

///////////////////////////////////////////////////////////
//
// HEADER doc here
//
// MSD
//
////////////////////////////////////////////////////////////

#include "TROOT.h"
#include "TClonesArray.h"
#include "TObjArray.h"

class TClonesArray;

class TGeVSimEvent : public TObject {
 
private:
  
  Float_t evPsi;         // Reaction Plane Angle
  Int_t evNum;           // Event Number  
 

 public:
  
  //////////////////////////////////////////////////////////////

  TGeVSimEvent() {evNum = 0; evParts = 0;} // Default for use by ROOT.  Do not use.
  TGeVSimEvent(Int_t numParts);
  ~TGeVSimEvent(); 
  
  ////////////////////////////////////////////////////////////

  Float_t Psi() const {return evPsi;}
  void SetPsi(Float_t psi) {evPsi = psi;} 

  Float_t EventNumber() const {return evNum;}
  void SetEventNumber(Int_t num) {evNum = num;}

  Double_t MeanPt() const;
  Int_t GetMult() const;

  void NextEvent();

  void Print(Option_t* option="") const;  // argument to remove compiler warning about overloaded virtual functions

  TClonesArray *evParts;  // The particles in this event
  TObjArray *evParams;    // The parameters of each particle type, holds TGeVSimParams objects

  ClassDef(TGeVSimEvent, 1)

};

/////////////////////////////////////////////////

#endif



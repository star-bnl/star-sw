////////////////////////////////////////////////////////////////////////////////
//
// Source File Documentation is HERE
//
// MSD
//
////////////////////////////////////////////////////////////////////////////////


#include "TGeVSimEvent.h"
#include "TParticle.h"
#include "TGeVSimParams.h"
//#include <iostream>
#include "Stiostream.h"

ClassImp(TGeVSimEvent);

////////////////////////////////////////////////////////////////////////////////

TGeVSimEvent::TGeVSimEvent(Int_t numParts) {
  //
  // Constructor Documentation:
  //
  // Sets some defaults, declares the TClonesArray
  //

  evNum = 1;

  evParts = new TClonesArray("TParticle", numParts);
  evParams = new TObjArray(10, 0);

}

////////////////////////////////////////////////////////////////////////////////

TGeVSimEvent::~TGeVSimEvent() {
  //
  // This is the destructor
  //

  if (evParts) delete evParts;
}

////////////////////////////////////////////////////////////////////////////////

Double_t TGeVSimEvent::MeanPt() const {
  //
  // MeanPt doc
  //

  Double_t sum = 0;

  for(Int_t i=0; i<evParts->GetEntries(); i++) {
    TParticle *p = (TParticle*)evParts->At(i);
    sum += p->Pt();
  }
  
  sum /= evParts->GetEntries();
  return sum;
}

////////////////////////////////////////////////////////////////////////////////

Int_t TGeVSimEvent::GetMult() const {
  //
  // GetMult doc
  //

  Int_t mult;
  
  if (evParts) {
    mult = evParts->GetEntriesFast();
  }
  else {
    Error("GetMult","No array defined");
    mult = -1;
  }

  return mult;
  
}

////////////////////////////////////////////////////////////////////////////////

void TGeVSimEvent::NextEvent() {
  //
  // NextEvent doc
  // Reset everything for the next event
  //

  //evNum++;
  evParts->Clear();
  evParams->Clear();
}

////////////////////////////////////////////////////////////////////////////////
  
void TGeVSimEvent::Print(Option_t* option) const {
  //
  // Print doc
  //

  cout << "Event Number " << evNum << endl;
  cout << "Multiplicity: " << GetMult() << endl;
  //cout << "Reaction Plane: " << evPsi*180/3.14 << endl;
  printf("Reaction Plane: %3.2f\n", evPsi*180/3.142);
  cout << "Mean Pt: " << MeanPt() << endl; 

  cout << "Particle Parameters:" << endl;
  for(Int_t i=0; i<evParams->GetEntries(); i++)
    {
      TGeVSimParams *par = (TGeVSimParams*)evParams->At(i);
      if (i==0) par->PrintHeader();
      par->Print();
    }
  cout << endl << endl;
  
}

////////////////////////////////////////////////////////////////////////////////
  


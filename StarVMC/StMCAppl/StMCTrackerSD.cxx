// $Id: StMCTrackerSD.cxx,v 1.1.1.1 2004/07/17 20:02:55 perev Exp $
//
// Geant4 ExampleN02 adapted to Virtual Monte Carlo 
//
/// Id: ExN02TrackerSD.cc,v 1.6 2002/01/09 17:24:10 ranjard Exp 
// GEANT4 tag Name: geant4-04-00-patch-02 
//
// by Ivana Hrivnacova, 21.4.2002

#include "Riostream.h"

#include <TVirtualMC.h>
#include <TLorentzVector.h>
#include <TTree.h>

#include "StMCTrackerSD.h"
#include "StMCTrackerHit.h"
#include "StMCRootManager.h"

ClassImp(StMCTrackerSD)

using namespace std;

//_____________________________________________________________________________
StMCTrackerSD::StMCTrackerSD(const char* name)
  : TNamed(name, ""),
    fTrackerCollection(0),
    fVerboseLevel(1)
{
  fTrackerCollection = new TClonesArray("StMCTrackerHit");
}

//_____________________________________________________________________________
StMCTrackerSD::StMCTrackerSD()
  : TNamed(),
    fTrackerCollection(0),
    fVerboseLevel(1)
{}

//_____________________________________________________________________________
StMCTrackerSD::~StMCTrackerSD()
{}

//
// private methods
//

//_____________________________________________________________________________
StMCTrackerHit* StMCTrackerSD::AddHit()
{
// Creates a new hit in the TClonesArray.
// ---

  TClonesArray& ref = *fTrackerCollection;
  Int_t size = ref.GetEntriesFast();

  return new(ref[size]) StMCTrackerHit();
}

//
// public methods
//

//_____________________________________________________________________________
void StMCTrackerSD::Initialize()
{
// Registers hits collection in Root manager;
// sets sensitive volumes.
// ---
  
  Register();
  
  fSensitiveVolumeID = gMC->VolId("CHMB");
}

//_____________________________________________________________________________
Bool_t StMCTrackerSD::ProcessHits()
{
// Creates hits (in stepping).
// ---

  Int_t copyNo;
  Int_t id = gMC->CurrentVolID(copyNo);

  if (id != fSensitiveVolumeID) return false;

  Double_t edep = gMC->Edep();

  if (edep==0.) return false;

  StMCTrackerHit* newHit = AddHit();

  // Track ID
  newHit->SetTrackID  (gMC->GetStack()->GetCurrentTrackNumber());

  // Chamber no
  newHit->SetChamberNb(copyNo);

  // Energy deposit
  newHit->SetEdep     (edep);

  // Position
  TLorentzVector pos;
  gMC->TrackPosition(pos);
  newHit->SetPos (TVector3(pos.X(), pos.Y(), pos.Z()));
  
  //newHit->Print();
  //newHit->Draw();

  return true;
}

//_____________________________________________________________________________
void StMCTrackerSD::EndOfEvent()
{
// Prints hits collection (if verbose)
// and deletes hits afterwards.
// ---

  if (fVerboseLevel>0)  Print();
    
  // Reset hits collection
  fTrackerCollection->Delete();  
}

//_____________________________________________________________________________
void StMCTrackerSD::Register()
{
// Registers the hits collection in Root manager.
// ---
  
  StMCRootManager::Instance()->Register("hits", &fTrackerCollection);
}

//_____________________________________________________________________________
void StMCTrackerSD::Print(const Option_t*) const
{
// Prints the hits collection.
// ---
  
   Int_t nofHits = fTrackerCollection->GetEntriesFast();
   /*     
   cout << "\n-------->Hits Collection: in this event they are " << nofHits 
        << " hits in the tracker chambers: " << endl;
	    
   for (Int_t i=0; i<nofHits; i++) (*fTrackerCollection)[i]->Print();          
   */
}

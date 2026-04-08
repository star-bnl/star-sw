#include "G4ParticleTypes.hh"
#include "G4HCofThisEvent.hh"
#include "G4TouchableHistory.hh"
#include "G4Track.hh"
#include "G4Step.hh"
#include "G4ThreeVector.hh"
#include "G4SDManager.hh"
#include "G4VProcess.hh"
#include "G4ios.hh"
#include "G4Tubs.hh"

#include "RHICfFCSD.hh"

//////////
/// Constructor and Destructor
RHICfFCSD::RHICfFCSD(G4String name): G4VSensitiveDetector(name)
{
  G4String HCname;
  collectionName.insert(HCname=name);
}

RHICfFCSD::~RHICfFCSD()
{
}

//////////
/// Initialize
void RHICfFCSD::Initialize(G4HCofThisEvent* HCTE)
{
  /// Create hit collection
  hitsColl = new FCHitsCollection(SensitiveDetectorName, collectionName[0]); 
  
  /// Push H.C. to "Hit Collection of This Event"
  G4int hcid = GetCollectionID(0);
  HCTE->AddHitsCollection(hcid, hitsColl);

  /// Clear energy deposit buffer
  edep.clear();
  edep.resize(ntower);
}

//////////
/// ProcessHits
G4bool RHICfFCSD::ProcessHits(G4Step* astep, G4TouchableHistory*)
{
  /// Get step information from "PreStepPoint"
  const G4StepPoint* preStepPoint=astep->GetPreStepPoint();
  G4TouchableHistory* touchable = (G4TouchableHistory*)(preStepPoint->GetTouchable());

  G4int itower=touchable->GetReplicaNumber(0);

  /// Accumulate energy deposit in each scintillator
  edep[itower]+=astep->GetTotalEnergyDeposit()/CLHEP::MeV;

  return true;
}

//////////
/// EndOfEvent
void RHICfFCSD::EndOfEvent(G4HCofThisEvent* )
{
  /// Make hits and push them to "Hit Coleltion"
  for(int itower=0; itower<ntower; itower++) {
    RHICfFCHit* ahit=new RHICfFCHit(itower, edep[itower]);
    hitsColl->insert(ahit);
  }
}

//////////
/// DrawAll
void RHICfFCSD::DrawAll()
{
}

//////////
/// PrintAll
void RHICfFCSD::PrintAll()
{
  hitsColl->PrintAllHits();
}

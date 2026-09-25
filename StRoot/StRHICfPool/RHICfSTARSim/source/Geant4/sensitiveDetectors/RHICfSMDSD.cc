#include "G4ParticleTypes.hh"
#include "G4HCofThisEvent.hh"
#include "G4TouchableHistory.hh"
#include "G4Step.hh"
#include "G4ThreeVector.hh"
#include "G4SDManager.hh"
#include "G4VProcess.hh"
#include "G4ios.hh"
#include "G4Tubs.hh"

#include "RHICfSMDSD.hh"

//////////
/// Constructor and Destructor
RHICfSMDSD::RHICfSMDSD(G4String name): G4VSensitiveDetector(name)
{
  G4String HCname;
  collectionName.insert(HCname=name);
}

RHICfSMDSD::~RHICfSMDSD()
{
}

//////////
/// Initialize
void RHICfSMDSD::Initialize(G4HCofThisEvent* HCTE)
{
  /// Create hit collection
  hitsColl = new SMDHitsCollection(SensitiveDetectorName, collectionName[0]); 
  
  /// Push H.C. to "Hit Collection of This Event"
  G4int hcid = GetCollectionID(0);
  HCTE->AddHitsCollection(hcid, hitsColl);

  /// Clear energy deposit buffer
  edep.clear();
  edep.resize(nxy);

  for(int ixy=0; ixy<nxy; ixy++){ 
    edep[ixy].resize(nsmd[ixy]);
  }
}

//////////
/// ProcessHits
G4bool RHICfSMDSD::ProcessHits(G4Step* astep, G4TouchableHistory*)
{
  /// Get step information from "PreStepPoint"
  const G4StepPoint* preStepPoint=astep->GetPreStepPoint();
  G4TouchableHistory* touchable = (G4TouchableHistory*)(preStepPoint->GetTouchable());

  G4int ixy=-1;
  G4int ismd;
  /// PHENIX
  if(touchable->GetVolume(0)->GetName()=="Vol-smdh_PV" ||
     touchable->GetVolume(0)->GetName()=="Vol-smdv_PV") {
    if(touchable->GetVolume(0)->GetName()=="Vol-smdh_PV") ixy=0;
    if(touchable->GetVolume(0)->GetName()=="Vol-smdv_PV") ixy=1;
    ismd=touchable->GetReplicaNumber(0);
  }
  /// STAR
  if(touchable->GetVolume(0)->GetName()=="Vol-smdh-strip_PV" ||
     touchable->GetVolume(0)->GetName()=="Vol-smdv-strip_PV") {
    if(touchable->GetVolume(0)->GetName()=="Vol-smdh-strip_PV") {
      ixy=0;
      ismd=int(touchable->GetReplicaNumber(0)/4);
    }else if(touchable->GetVolume(0)->GetName()=="Vol-smdv-strip_PV") {
      ixy=1;
      ismd=int(touchable->GetReplicaNumber(0)/3);
    }
  }

  /// Accumulate energy deposit in each scintillator
  edep[ixy][ismd]+=astep->GetTotalEnergyDeposit();

  return true;
}

//////////
/// EndOfEvent
void RHICfSMDSD::EndOfEvent(G4HCofThisEvent* )
{
  /// Make hits and push them to "Hit Coleltion"
  for(int ixy=0; ixy<nxy; ixy++) {
    for(int ismd=0; ismd<nsmd[ixy]; ismd++) {
      RHICfSMDHit* ahit=new RHICfSMDHit(ixy, ismd, edep[ixy][ismd]);
      hitsColl->insert(ahit);
    }
  }
}

//////////
/// DrawAll
void RHICfSMDSD::DrawAll()
{
}

//////////
/// PrintAll
void RHICfSMDSD::PrintAll()
{
  hitsColl->PrintAllHits();
}

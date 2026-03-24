#include "G4ParticleTypes.hh"
#include "G4HCofThisEvent.hh"
#include "G4TouchableHistory.hh"
#include "G4Step.hh"
#include "G4ThreeVector.hh"
#include "G4SDManager.hh"
#include "G4ios.hh"

#include "RHICfDetectorConstruction.hh"
#include "RHICfGSOplateSD.hh"

//////////
/// Constructor and Destructor
RHICfGSOplateSD::RHICfGSOplateSD(G4String name): G4VSensitiveDetector(name)
{
  G4String HCname;
  collectionName.insert(HCname=name);
}

RHICfGSOplateSD::~RHICfGSOplateSD()
{
  fmap->Close();
}

//////////
/// Initialize
void RHICfGSOplateSD::Initialize(G4HCofThisEvent* HCTE)
{
  /// Create hit collection
  hitsColl = new GSOplateHitsCollection(SensitiveDetectorName, collectionName[0]); 
  
  /// Push H.C. to "Hit Collection of This Event"
  G4int hcid = GetCollectionID(0);
  HCTE->AddHitsCollection(hcid, hitsColl);

  /// Clear energy deposit buffer
  edep.clear();
  edep_truth.clear();

  edep.resize(ntower);
  edep_truth.resize(ntower);

  for(int itower=0; itower<ntower; itower++) {
    edep[itower].resize(nplate);
    edep_truth[itower].resize(nplate);

  }

}

//////////
/// ProcessHits
G4bool RHICfGSOplateSD::ProcessHits(G4Step* astep, G4TouchableHistory*)
{
  /// Get step information from "PreStepPoint"
  const G4StepPoint* preStepPoint = astep->GetPreStepPoint();
  G4TouchableHistory* touchable = (G4TouchableHistory*)(preStepPoint->GetTouchable());

  /// Accumulate energy deposit in each scintillator
  G4int itower=-1;
  G4String ctower=touchable->GetVolume(0)->GetName();
  if(ctower=="Vol-gso-scintillator-small_PV") itower=0;
  if(ctower=="Vol-gso-scintillator-large_PV") itower=1;
  G4int iplate=touchable->GetReplicaNumber(1);

  G4ThreeVector coord = preStepPoint->GetPosition();
  const G4AffineTransform transformation = touchable->GetHistory()->GetTopTransform();
  G4ThreeVector localPosition = transformation.TransformPoint(coord);

  G4double localx=localPosition.x()*CLHEP::mm;
  G4double localy=localPosition.y()*CLHEP::mm;

  edep_truth[itower][iplate]+=astep->GetTotalEnergyDeposit();
  /// Rotate -90degrees
  edep[itower][iplate]+=astep->GetTotalEnergyDeposit()*gmap[itower][iplate]->Interpolate(localy,-localx);

  return true;
}

//////////
/// EndOfEvent
void RHICfGSOplateSD::EndOfEvent(G4HCofThisEvent* )
{
  /// Make hits and push them to "Hit Coleltion"
  for(int itower=0; itower<ntower; itower++) {
    for(int iplate=0; iplate<nplate; iplate++) {
      RHICfGSOplateHit* ahit=new RHICfGSOplateHit(itower, iplate, edep_truth[itower][iplate], edep[itower][iplate]);
      hitsColl->insert(ahit);
    }
  }
}

//////////
/// DrawAll
void RHICfGSOplateSD::DrawAll()
{
}

//////////
/// PrintAll
void RHICfGSOplateSD::PrintAll()
{
  hitsColl->PrintAllHits();
}

void RHICfGSOplateSD::SetTables(TString atables)
{
  fmap=new TFile(Form("%s/GSOplateMap.root", atables.Data()));
  char gname[256];
  for(int itower=0; itower<ntower; itower++) {
    for(int iplate=0; iplate<nplate; iplate++) {
      sprintf(gname,"map_tow%d_plate%02d",itower,iplate);
      gmap[itower][iplate]=(TGraph2D*)fmap->Get(gname);
    }
  }
}

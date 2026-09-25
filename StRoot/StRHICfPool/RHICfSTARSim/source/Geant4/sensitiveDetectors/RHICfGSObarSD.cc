#include "G4ParticleTypes.hh"
#include "G4HCofThisEvent.hh"
#include "G4TouchableHistory.hh"
#include "G4Step.hh"
#include "G4ThreeVector.hh"
#include "G4SDManager.hh"
#include "G4ios.hh"

#include "RHICfDetectorConstruction.hh"
#include "RHICfGSObarSD.hh"

//////////
/// Constructor and Destructor
RHICfGSObarSD::RHICfGSObarSD(G4String name): G4VSensitiveDetector(name)
{
  G4String HCname;
  collectionName.insert(HCname=name);
}

RHICfGSObarSD::~RHICfGSObarSD()
{
  fatt->Close();
}

//////////
/// Initialize
void RHICfGSObarSD::Initialize(G4HCofThisEvent* HCTE)
{
  /// Create hit collection
  hitsColl = new GSObarHitsCollection(SensitiveDetectorName, collectionName[0]); 
  /// Push H.C. to "Hit Collection of This Event"
  G4int hcid = GetCollectionID(0);
  HCTE->AddHitsCollection(hcid, hitsColl);

  /// Clear energy deposit buffer
  edep.clear();
  edep_truth.clear();
  edep.resize(nside);
  edep_truth.resize(ntower);

  for(int itower=0; itower<ntower; itower++) {
    edep[itower].resize(nbelt);
    edep_truth[itower].resize(nbelt);

    for(int ibelt=0; ibelt<nbelt; ibelt++) {
      edep[itower][ibelt].resize(nxy);
      edep_truth[itower][ibelt].resize(nxy);

      for(int ixy=0; ixy<nxy; ixy++) {
        edep[itower][ibelt][ixy].resize(nbar[itower]);
        edep_truth[itower][ibelt][ixy].resize(nbar[itower]);
      }
    }
  }
}

//////////
/// ProcessHits
G4bool RHICfGSObarSD::ProcessHits(G4Step* astep, G4TouchableHistory*)
{
  /// Get step information from "PreStepPoint"
  const G4StepPoint* preStepPoint = astep->GetPreStepPoint();
  G4TouchableHistory* touchable = (G4TouchableHistory*)(preStepPoint->GetTouchable());

  /// Accumulate energy deposit in each scintillator
  G4int itower=-1;
  G4String ctower=touchable->GetVolume(0)->GetName();
  if(ctower=="Vol-gsobar-small_PV") itower=0;
  if(ctower=="Vol-gsobar-large_PV") itower=1;
  G4int ibelt=touchable->GetReplicaNumber(2);
  G4int ixy=touchable->GetReplicaNumber(1);
  G4int ibar=touchable->GetReplicaNumber(0);

  G4ThreeVector coord = preStepPoint->GetPosition();
  const G4AffineTransform transformation = touchable->GetHistory()->GetTopTransform();
  G4ThreeVector localPosition = transformation.TransformPoint(coord);

  G4double localx=localPosition.x()*CLHEP::mm;
  G4double localy=localPosition.y()*CLHEP::mm;
  /// Flip coordinates

  if(ixy==0) localx=localx*(-1);

  // ====== NOTE: The edep_truth and edep are reversed (Found date: Dec/2024)
  // origin code
  // edep[itower][ibelt][ixy][ibar]+=astep->GetTotalEnergyDeposit();
  // edep_truth[itower][ibelt][ixy][ibar] +=astep->GetTotalEnergyDeposit()*gatt[itower][ibelt][ixy][ibar]->Eval(localx);
  // =============================================================================

  edep_truth[itower][ibelt][ixy][ibar]+=astep->GetTotalEnergyDeposit();
  edep[itower][ibelt][ixy][ibar] +=astep->GetTotalEnergyDeposit()*gatt[itower][ibelt][ixy][ibar]->Eval(localx);
  
  return true;
}

//////////
/// EndOfEvent
void RHICfGSObarSD::EndOfEvent(G4HCofThisEvent* )
{
  /// Make hits and push them to "Hit Coleltion"
  for(int itower=0; itower<ntower; itower++) {
    for(int ibelt=0; ibelt<nbelt; ibelt++) {
      for(int ixy=0; ixy<nxy; ixy++) {
        for(int ibar=0; ibar<nbar[itower]; ibar++) {
          RHICfGSObarHit* ahit=new RHICfGSObarHit(itower, ibelt, ixy, ibar, edep[itower][ibelt][ixy][ibar], edep_truth[itower][ibelt][ixy][ibar]);
          hitsColl->insert(ahit);
        }
      }
    }
  }
}

//////////
/// DrawAll
void RHICfGSObarSD::DrawAll()
{
}

//////////
/// PrintAll
void RHICfGSObarSD::PrintAll()
{
  hitsColl->PrintAllHits();
}

void RHICfGSObarSD::SetTables(TString atables)
{
  fatt=new TFile(Form("%s/GSObarAttenuation.root", atables.Data()));
  char gname[256];
  for(int itower=0; itower<ntower; itower++) {
    for(int ibelt=0; ibelt<nbelt; ibelt++) {
      for(int ixy=0; ixy<nxy; ixy++) {
	for(int ibar=0; ibar<nbar[itower]; ibar++) {
	  sprintf(gname,"Tow%d_Belt%d_XY%d_bar%02d",itower,ibelt,ixy,ibar);
	  gatt[itower][ibelt][ixy][ibar]=(TGraph*)fatt->Get(gname);
	}
      }
    }
  }
}

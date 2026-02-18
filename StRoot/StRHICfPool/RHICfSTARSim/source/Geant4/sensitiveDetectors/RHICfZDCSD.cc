#include "G4ParticleTypes.hh"
#include "G4HCofThisEvent.hh"
#include "G4TouchableHistory.hh"
#include "G4Step.hh"
#include "G4ThreeVector.hh"
#include "G4SDManager.hh"
#include "G4VProcess.hh"
#include "G4ios.hh"
#include "G4Tubs.hh"

#include "RHICfZDCSD.hh"

//////////
/// Constructor and Destructor
RHICfZDCSD::RHICfZDCSD(G4String name): G4VSensitiveDetector(name)
{
  G4String HCname;
  collectionName.insert(HCname=name);
}

RHICfZDCSD::~RHICfZDCSD()
{
  fdata->Close();
}

//////////
/// Initialize
void RHICfZDCSD::Initialize(G4HCofThisEvent* HCTE)
{
  /// Create hit collection
  hitsColl = new ZDCHitsCollection(SensitiveDetectorName, collectionName[0]); 
  
  /// Push H.C. to "Hit Collection of This Event"
  G4int hcid = GetCollectionID(0);
  HCTE->AddHitsCollection(hcid, hitsColl);

  /// Clear energy deposit buffer
  nphoton.clear();
  nphoton.resize(nzdc);
  nphoton2.clear();
  nphoton2.resize(nzdc);
  edep.clear();
  edep.resize(nzdc);
}

//////////
/// ProcessHits
G4bool RHICfZDCSD::ProcessHits(G4Step* astep, G4TouchableHistory*)
{
  G4Track* track=astep->GetTrack();
  /// Get step information from "PreStepPoint"
  const G4StepPoint* preStepPoint=astep->GetPreStepPoint();
  G4TouchableHistory* touchable = (G4TouchableHistory*)(preStepPoint->GetTouchable());

  G4int izdc=touchable->GetReplicaNumber(2);
  bool bOptics=false;
  if(bOptics) {
    /// Accumulate Cherenkov light that reach the upper end of the fiber
    G4ParticleDefinition* particleType=track->GetDefinition();
    if(particleType==G4OpticalPhoton::OpticalPhotonDefinition()) {
      G4String proc=track->GetCreatorProcess()->G4VProcess::GetProcessName();
      if(proc=="Cerenkov") {
	/// Check boundary
	G4VPhysicalVolume* prePV=preStepPoint->GetPhysicalVolume();
	const G4StepPoint* postStepPoint=astep->GetPostStepPoint();
	G4VPhysicalVolume* postPV=postStepPoint->GetPhysicalVolume();

	G4ThreeVector global=postStepPoint->GetPosition();
	const G4AffineTransform transform=touchable->GetHistory()->GetTopTransform();
	G4ThreeVector local=transform.TransformPoint(global);
	if(prePV->GetName()=="Vol-fibr_PV" && postPV->GetName()=="Vol-zdc_PV") {
	  if(fabs(local.z())==fiberlength) {
	    if(local.z()>0) nphoton[izdc]++; /// upper bound, count it
	    track->SetTrackStatus(fStopAndKill);
	  }
	}
      }
    }
  }else{
    G4ParticleDefinition* particleType=track->GetDefinition();
    if(particleType->GetPDGCharge()!=0) {
      /// Check boundary
      G4VPhysicalVolume* prePV=preStepPoint->GetPhysicalVolume();
      const G4StepPoint* postStepPoint=astep->GetPostStepPoint();
      //      G4VPhysicalVolume* postPV=postStepPoint->GetPhysicalVolume();

      if(prePV->GetName()=="Vol-fibr_PV" &&
	 preStepPoint->GetStepStatus()==fGeomBoundary) {
	G4double beta=preStepPoint->GetBeta();

	G4ThreeVector coord1=preStepPoint->GetPosition();
	G4ThreeVector coord2=postStepPoint->GetPosition();
	const G4AffineTransform transform=touchable->GetHistory()->GetTopTransform();
	G4ThreeVector local1=transform.TransformPoint(coord1);
	G4ThreeVector local2=transform.TransformPoint(coord2);
	G4double theta=(local2-local1).getTheta()/CLHEP::pi*180.;

	if(beta>1./1.49 && theta>0 && theta<90) {
	  if(particleType==G4Electron::ElectronDefinition() ||
	     particleType==G4Positron::PositronDefinition()) {
          nphoton2[izdc]=nphoton2[izdc]+felectron->Interpolate(theta,beta)*fabs(particleType->GetPDGCharge());
      }else{
        nphoton2[izdc]=nphoton2[izdc]+fmuon->Interpolate(theta,beta)*fabs(particleType->GetPDGCharge());
      }
    }
      }
    }
  }

  /// Accumulate energy deposit in each scintillator
  edep[izdc]+=astep->GetTotalEnergyDeposit();


  return true;
}

//////////
/// EndOfEvent
void RHICfZDCSD::EndOfEvent(G4HCofThisEvent* )
{
  /// Make hits and push them to "Hit Coleltion"
  for(int izdc=0; izdc<nzdc; izdc++) {
    nphoton[izdc]=(int)nphoton2[izdc];
    
    RHICfZDCHit* ahit=new RHICfZDCHit(izdc, nphoton[izdc], edep[izdc]);
    hitsColl->insert(ahit);
  }
}

//////////
/// DrawAll
void RHICfZDCSD::DrawAll()
{
}

//////////
/// PrintAll
void RHICfZDCSD::PrintAll()
{
  hitsColl->PrintAllHits();
}

void RHICfZDCSD::SetFiberLength(G4double afiberlength)
{
  fiberlength=afiberlength;
}


void RHICfZDCSD::SetTables(TString atables)
{
  /// Read table for optical photon calculation (response function)
  fdata=new TFile(Form("%s/ZDClightyield.root", atables.Data()));
  felectron=(TGraph2D*)fdata->Get("electron");
  fmuon=(TGraph2D*)fdata->Get("muon");
}

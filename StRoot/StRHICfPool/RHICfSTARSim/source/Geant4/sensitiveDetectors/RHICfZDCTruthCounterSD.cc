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

#include "RHICfZDCTruthCounterSD.hh"

//////////
/// Constructor and Destructor
RHICfZDCTruthCounterSD::RHICfZDCTruthCounterSD(G4String name): G4VSensitiveDetector(name)
{
  mIncidentTrackID = -1;
  mIncidentPosition[0] = -999.;
  mIncidentPosition[1] = -999.;
  mIncidentEnergy = -1.;
}

RHICfZDCTruthCounterSD::~RHICfZDCTruthCounterSD()
{
}

//////////
/// Initialize
void RHICfZDCTruthCounterSD::Initialize(G4HCofThisEvent* HCTE)
{
}

//////////
/// ProcessHits
G4bool RHICfZDCTruthCounterSD::ProcessHits(G4Step* astep, G4TouchableHistory*)
{
  G4Track* track = astep -> GetTrack();
  double energy = track->GetKineticEnergy()*0.001; // [GeV]
  if(energy < 0.1){return true;}

  mIncidentTrackID = track -> GetTrackID();
  const G4StepPoint* preStepPoint = astep -> GetPreStepPoint();
  mIncidentPosition[0] = preStepPoint -> GetPosition().x();
  mIncidentPosition[1] = preStepPoint -> GetPosition().y();
  mIncidentEnergy = energy;

  return true;
}

void RHICfZDCTruthCounterSD::EndOfEvent(G4HCofThisEvent* )
{
}

int RHICfZDCTruthCounterSD::GetIncidentTrackID()
{
  int id = mIncidentTrackID;
  mIncidentTrackID = -1;
  return id;
}

double RHICfZDCTruthCounterSD::GetIncidentPosX()
{
  double x = mIncidentPosition[0];
  mIncidentPosition[0] = -999.;
  return x;
}

double RHICfZDCTruthCounterSD::GetIncidentPosY()
{
  double y = mIncidentPosition[1];
  mIncidentPosition[1] = -999.;
  return y;
}

double RHICfZDCTruthCounterSD::GetIncidentEnergy()
{
  double energy = mIncidentEnergy;
  mIncidentEnergy = 0.;
  return energy;
}
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

#include "RHICfTruthCounterSD.hh"

//////////
/// Constructor and Destructor
RHICfTruthCounterSD::RHICfTruthCounterSD(G4String name): G4VSensitiveDetector(name)
{
  mIncidentTowerIdx = -1;
  mIncidentTrackID = -1;
  mIncidentPosition[0] = -999.;
  mIncidentPosition[1] = -999.;
  mIncidentEnergy = -1;
}

RHICfTruthCounterSD::~RHICfTruthCounterSD()
{
}

//////////
/// Initialize
void RHICfTruthCounterSD::Initialize(G4HCofThisEvent* HCTE)
{
}

//////////
/// ProcessHits
G4bool RHICfTruthCounterSD::ProcessHits(G4Step* astep, G4TouchableHistory*)
{
  G4Track* track = astep -> GetTrack();
  double energy = track->GetKineticEnergy()*0.001; // [GeV]
  if(energy < 0.1){return true;}

  const G4StepPoint* preStepPoint=astep->GetPreStepPoint();
  G4TouchableHistory* touchable = (G4TouchableHistory*)(preStepPoint->GetTouchable());

  mIncidentTowerIdx = touchable->GetReplicaNumber(0);
  mIncidentTrackID = track -> GetTrackID();
  mIncidentPosition[0] = preStepPoint -> GetPosition().x();
  mIncidentPosition[1] = preStepPoint -> GetPosition().y();
  mIncidentEnergy = energy;

  return true;
}

void RHICfTruthCounterSD::EndOfEvent(G4HCofThisEvent* )
{
}

int RHICfTruthCounterSD::GetIncidentTowerIdx()
{
  int idx = mIncidentTowerIdx;
  mIncidentTowerIdx = -1;
  return idx;
}

int RHICfTruthCounterSD::GetIncidentTrackID()
{
  int id = mIncidentTrackID;
  mIncidentTrackID = -1;
  return id;
}

double RHICfTruthCounterSD::GetIncidentPosX()
{
  double x = mIncidentPosition[0];
  mIncidentPosition[0] = -999.;
  return x;
}

double RHICfTruthCounterSD::GetIncidentPosY()
{
  double y = mIncidentPosition[1];
  mIncidentPosition[1] = -999.;
  return y;
}

double RHICfTruthCounterSD::GetIncidentEnergy()
{
  double energy = mIncidentEnergy;
  mIncidentEnergy = 0.;
  return energy;
}
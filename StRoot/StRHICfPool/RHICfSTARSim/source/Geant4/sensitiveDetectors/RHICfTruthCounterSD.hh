#ifndef RHICfTruthCounterSD_H
#define RHICfTruthCounterSD_H 1

#include "G4VSensitiveDetector.hh"

#include <vector>
#include "RHICfSimPar.hh"

class G4Step;
class G4HCofThisEvent;
class G4TouchableHistory;

class RHICfTruthCounterSD: public G4VSensitiveDetector
{
public:
  RHICfTruthCounterSD(const G4String name);
  virtual ~RHICfTruthCounterSD();

  virtual G4bool ProcessHits(G4Step* aStep, G4TouchableHistory* ROhist);
  virtual void Initialize(G4HCofThisEvent* HCTE);
  virtual void EndOfEvent(G4HCofThisEvent* HCTE);

  int GetIncidentTowerIdx();
  int GetIncidentTrackID();
  double GetIncidentPosX();
  double GetIncidentPosY();
  double GetIncidentEnergy();

  private:
    int mIncidentTowerIdx;
    int mIncidentTrackID;
    double mIncidentPosition[2];
    double mIncidentEnergy;
};

#endif

#ifndef RHICfZDCTruthCounterSD_H
#define RHICfZDCTruthCounterSD_H 1

#include "G4VSensitiveDetector.hh"

#include <vector>
#include "RHICfSimPar.hh"

class G4Step;
class G4HCofThisEvent;
class G4TouchableHistory;

class RHICfZDCTruthCounterSD: public G4VSensitiveDetector
{
  public:
    RHICfZDCTruthCounterSD(const G4String name);
    virtual ~RHICfZDCTruthCounterSD();

    virtual G4bool ProcessHits(G4Step* aStep, G4TouchableHistory* ROhist);
    virtual void Initialize(G4HCofThisEvent* HCTE);
    virtual void EndOfEvent(G4HCofThisEvent* HCTE);

    int GetIncidentTrackID();  
    double GetIncidentPosX();
    double GetIncidentPosY();
    double GetIncidentEnergy();

  private:
    int mIncidentTrackID;
    double mIncidentPosition[2];
    double mIncidentEnergy;
};

#endif

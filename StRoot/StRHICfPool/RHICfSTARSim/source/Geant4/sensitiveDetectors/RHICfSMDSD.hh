#ifndef RHICFSMDSD_H
#define RHICFSMDSD_H 1

#include "G4VSensitiveDetector.hh"

#include <vector>

#include "RHICfSMDHit.hh"
#include "RHICfSimPar.hh"

class G4Step;
class G4HCofThisEvent;
class G4TouchableHistory;

class RHICfSMDSD: public G4VSensitiveDetector
{
public:
  RHICfSMDSD(const G4String name);
  virtual ~RHICfSMDSD();

  virtual G4bool ProcessHits(G4Step* aStep, G4TouchableHistory* ROhist);
  virtual void Initialize(G4HCofThisEvent* HCTE);
  virtual void EndOfEvent(G4HCofThisEvent* HCTE);

  virtual void DrawAll();
  virtual void PrintAll(); 
  
private:
  /// Buffer for energy deposit
  std::vector<std::vector<double> > edep;
  SMDHitsCollection*  hitsColl;
};

#endif

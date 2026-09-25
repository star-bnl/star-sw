#ifndef RHICFFCSD_H
#define RHICFFCSD_H 1

#include "G4VSensitiveDetector.hh"

#include <vector>

#include "RHICfFCHit.hh"
#include "RHICfSimPar.hh"

class G4Step;
class G4HCofThisEvent;
class G4TouchableHistory;

class RHICfFCSD: public G4VSensitiveDetector
{
public:
  RHICfFCSD(const G4String name);
  virtual ~RHICfFCSD();

  virtual G4bool ProcessHits(G4Step* aStep, G4TouchableHistory* ROhist);
  virtual void Initialize(G4HCofThisEvent* HCTE);
  virtual void EndOfEvent(G4HCofThisEvent* HCTE);

  virtual void DrawAll();
  virtual void PrintAll(); 
  
private:
  /// Buffer for energy deposit
  std::vector<double> edep;
  FCHitsCollection*  hitsColl;
};

#endif

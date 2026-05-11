#ifndef RHICFGSOPLATESD_H
#define RHICFGSOPLATESD_H 1

#include "G4VSensitiveDetector.hh"

#include "TFile.h"
#include "TGraph2D.h"
#include "TString.h"

#include <vector>

#include "RHICfGSOplateHit.hh"
#include "RHICfSimPar.hh"

class G4Step;
class G4HCofThisEvent;
class G4TouchableHistory;

class RHICfGSOplateSD: public G4VSensitiveDetector
{
public:
  RHICfGSOplateSD(const G4String name);
  virtual ~RHICfGSOplateSD();

  virtual G4bool ProcessHits(G4Step* astep, G4TouchableHistory* ROhist);
  virtual void Initialize(G4HCofThisEvent* HCTE);
  virtual void EndOfEvent(G4HCofThisEvent* HCTE);

  virtual void DrawAll();
  virtual void PrintAll(); 

  void SetTables(TString atables);
  std::vector<std::vector<double> > GetEdeposit_truth() {return edep_truth;};
  std::vector<std::vector<double> > GetEdeposit() {return edep;};

private:
  /// Buffer for energy deposit
  std::vector<std::vector<double> > edep_truth;
  std::vector<std::vector<double> > edep;
  GSOplateHitsCollection*  hitsColl;

  TFile* fmap;
  TGraph2D* gmap[ntower][nplate];
};

#endif

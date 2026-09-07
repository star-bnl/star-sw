#ifndef RHICFGSOBARSD_H
#define RHICFGSOBARSD_H 1

#include "G4VSensitiveDetector.hh"

#include "TFile.h"
#include "TGraph.h"
#include "TString.h"

#include <vector>

#include "RHICfGSObarHit.hh"
#include "RHICfSimPar.hh"

class G4Step;
class G4HCofThisEvent;
class G4TouchableHistory;

class RHICfGSObarSD: public G4VSensitiveDetector
{
public:
  RHICfGSObarSD(const G4String name);
  virtual ~RHICfGSObarSD();

  virtual G4bool ProcessHits(G4Step* astep, G4TouchableHistory* ROhist);
  virtual void Initialize(G4HCofThisEvent* HCTE);
  virtual void EndOfEvent(G4HCofThisEvent* HCTE);

  virtual void DrawAll();
  virtual void PrintAll(); 

  void SetTables(TString atables);

private:
  /// Buffer for energy deposit
  std::vector<std::vector<std::vector<std::vector<double> > > > edep_truth;
  std::vector<std::vector<std::vector<std::vector<double> > > > edep;
  GSObarHitsCollection*  hitsColl;

  TFile *fatt;
  TGraph *gatt[ntower][nbelt][nxy][40];
};

#endif

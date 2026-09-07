#ifndef RHICFZDCSD_H
#define RHICFZDCSD_H 1

#include "G4VSensitiveDetector.hh"

#include <vector>
#include <fstream>

#include "TFile.h"
#include "TGraph2D.h"
#include "TString.h"

#include "RHICfZDCHit.hh"
#include "RHICfSimPar.hh"

class G4Step;
class G4HCofThisEvent;
class G4TouchableHistory;

class RHICfZDCSD: public G4VSensitiveDetector
{
public:
  RHICfZDCSD(const G4String name);
  virtual ~RHICfZDCSD();

  virtual G4bool ProcessHits(G4Step* aStep, G4TouchableHistory* ROhist);
  virtual void Initialize(G4HCofThisEvent* HCTE);
  virtual void EndOfEvent(G4HCofThisEvent* HCTE);

  virtual void DrawAll();
  virtual void PrintAll(); 

  void SetTables(TString atables);
  void SetFiberLength(G4double afiberlength);
  
private:
  /// Buffer for energy deposit
  std::vector<int> nphoton;
  std::vector<double> nphoton2;
  std::vector<double> edep;
  ZDCHitsCollection*  hitsColl;

  G4double fiberlength;

  TFile *fdata;
  TGraph2D *felectron;
  TGraph2D *fmuon;
};

#endif

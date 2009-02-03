// $Id: StarVMCApplication.cxx,v 1.8 2009/02/03 16:01:05 fisyak Exp $
// Class StarVMCApplication
// ----------------------- 
// Implementation of the TVirtualMCApplication
#include <assert.h>
#include "StarVMCApplication.h"
#include "StarMCHits.h"
#include "TGeoManager.h"
#include "TROOT.h"
#include "TClass.h"
#include "TSystem.h"
#include "TInterpreter.h"
#include "TVirtualMC.h"
#include "TPDGCode.h"
#include "TApplication.h"
#include "TGeant3TGeo.h"

ClassImp(StarVMCApplication);

//_____________________________________________________________________________
StarVMCApplication::StarVMCApplication(const char *name, const char *title) : 
  TVirtualMCApplication(name,title),
  fStack(0),
  fPrimaryGenerator(0),
  fMagField(0),
  fMcHits(0),
  fFieldB(0),
  fDebug(0)
{
  // Standard constructor
  TString program(gSystem->BaseName(gROOT->GetApplication()->Argv(0)));
  assert (! program.BeginsWith("root4star"));
  if (name) {
    // Create a user stack
    fStack = new StarMCStack(100); 
    // Constant magnetic field (in kiloGauss)
    fFieldB = new Double_t[3];
    fFieldB[0] = 0.;
    fFieldB[1] = 0.;
    fFieldB[2] = 5.;
  }
}
//_____________________________________________________________________________
StarVMCApplication::~StarVMCApplication() {  // Destructor  
  delete fStack;
  delete fFieldB;
  delete gMC;
  gMC = 0;
}
//_____________________________________________________________________________
void StarVMCApplication::InitMC(const char* setup) {  // Initialize MC.
  if (setup) {
    gROOT->LoadMacro(setup);
    gInterpreter->ProcessLine("Config()");
  }
  gMC->SetStack(fStack);
  gMC->Init();
  gMC->BuildPhysics(); 
}
//_____________________________________________________________________________
void StarVMCApplication::RunMC(Int_t nofEvents) {    // MC run.
  gMC->ProcessRun(nofEvents);
  FinishRun();
}
//_____________________________________________________________________________
void StarVMCApplication::FinishRun() {    // Finish MC run.
}
//_____________________________________________________________________________
void StarVMCApplication::ConstructGeometry() {    // Initialize geometry
  if (gMC->IsA()->InheritsFrom("TGeant3TGeo")) {
    assert(gGeoManager); 
    gMC->SetRootGeometry();
  }
}
//_____________________________________________________________________________
void StarVMCApplication::InitGeometry() {    
  if (gMC->IsA()->InheritsFrom("TGeant3")) {
    // Set drawing options
  }  
  if (gMC->IsA()->InheritsFrom("TGeant3TGeo")) {
    TGeant3TGeo *geant3 = (TGeant3TGeo *)gMC;
    geant3->Gprint("mate");
    geant3->Gprint("tmed");
  }
  if (fMcHits) fMcHits->Init();
}
//_____________________________________________________________________________
void StarVMCApplication::GeneratePrimaries() {    
  if (fPrimaryGenerator) {
    fPrimaryGenerator->GeneratePrimaries();
  }
  else {
    // Fill the user stack (derived from TVirtualMCStack) with primary particles.
    // ---
    // Track ID (filled by stack)
    Int_t ntr;
    
    // Option: to be tracked
    Int_t toBeDone = 1; 
    
    // Particle type
    //Int_t pdg  = 0;    // geantino
    Int_t pdg  = kProton;
    
    // Polarization
    Double_t polx = 0.; 
    Double_t poly = 0.; 
    Double_t polz = 0.; 
    
    // Position
    Double_t vx  = 0.; 
    Double_t vy  = 0.; 
    Double_t vz = 10.;
    Double_t tof = 0.;
    
    // Energy
    Double_t kinEnergy = 3.0;
    Double_t mass = 0.9382723;
    Double_t e  = mass + kinEnergy;
    
    // Momentum
    Double_t px, py, pz;
    px = 0.; 
    py = 0.; 
    pz = sqrt(e*e - mass*mass); 
    
    // Add particle to stack 
    fStack->PushTrack(toBeDone, -1, pdg, px, py, pz, e, vx, vy, vz, tof, polx, poly, polz, 
		      kPPrimary, ntr, 1., 0);
  }
  Int_t NPrimary = fStack->GetNtrack();
  if (! NPrimary) gMC->StopRun();
}
//_____________________________________________________________________________
void StarVMCApplication::BeginEvent() {    // User actions at beginning of event
  fStack->Reset();
  if (fMcHits) fMcHits->Clear();
}
//_____________________________________________________________________________
void StarVMCApplication::BeginPrimary() {    // User actions at beginning of a primary track
}
//_____________________________________________________________________________
void StarVMCApplication::PreTrack() {    // User actions at beginning of each track
}
//_____________________________________________________________________________
void StarVMCApplication::Stepping() {    // User actions at each step
  if (fMcHits) fMcHits->Step();
}
//_____________________________________________________________________________
void StarVMCApplication::PostTrack() {    // User actions after finishing of each track
  // delete stack only track
  StarMCParticle *current =  fStack->GetCurrentParticle();
  TObjArray *objs = fStack->GetParticles();
  if (objs->IndexOf(current) < objs->LowerBound()) delete current;
}
//_____________________________________________________________________________
void StarVMCApplication::FinishPrimary() {    // User actions after finishing of a primary track
}
//_____________________________________________________________________________
void StarVMCApplication::FinishEvent() {    // User actions after finishing of an event
  if (TString(gMC->GetName()) == "TGeant3") {
    // add scale (1.4)
  }  
  fStack->Print();
  if (fMcHits) fMcHits->FinishEvent(); // add kine info
} 
//_____________________________________________________________________________
void StarVMCApplication::Field(const Double_t* x, Double_t* b) const {
  if (fMagField) {
    fMagField->BField(x,b);
  } else {
    // Uniform magnetic field
    // ---
    for (Int_t i=0; i<3; i++) b[i] = fFieldB[i];
  }
}

// $Id: StarVMCApplication.cxx,v 1.3 2005/06/09 20:13:47 fisyak Exp $
// Class StarVMCApplication
// ----------------------- 
// Implementation of the TVirtualMCApplication
/* Flow diagram:
   Load(); // shared libraries
   GetVMC(); // define gGeoManager
   StarVMCApplication *appl = new StarVMCApplication("StarVMC", "The STAR VMC application");
    virtual void ConstructGeometry() = 0;
    virtual void InitGeometry() = 0;
    virtual void AddParticles() {}
    virtual void GeneratePrimaries() = 0;
    virtual void BeginEvent() = 0;
    virtual void BeginPrimary() = 0;
    virtual void PreTrack() = 0;
    virtual void Stepping() = 0;
    virtual void PostTrack() = 0;
    virtual void FinishPrimary() = 0;
    virtual void FinishEvent() = 0;
    
    virtual Double_t TrackingRmax() const { return DBL_MAX; }
    virtual Double_t TrackingZmax() const { return DBL_MAX; } 
    virtual void     Field(const Double_t* x, Double_t* b) const = 0;
   Geant3TGeo* geant3 = new TGeant3TGeo("C++ Interface to Geant3")

   appl->InitMC();
   ->    gMC->SetStack(fStack);
   ->    gMC->Init();
   ->    gMC->BuildPhysics(); 
  
   appl->RunMC(1);
   
   TGeant3::Init
*  MAIN(user)                                                          *
*  |                                                                   *
*  |-GZEBRA  initialisation   of   ZEBRA  system, dynamic core         *
*  |         allocation                                                *
*  |-UGINIT  (user)                                                    *
*  ||                                                                  *
*  ||- GINIT    initialisation of GEANT3 variables                     *
*  ||- GFFGO    interpretation of data cards                           *
*  ||- GZINIT   initialisation  of ZEBRA  core  divisions and  link    *
*  ||           areas                                                  *
*  ||- GPART    creation of the 'particle' data structure JPART        *
   -->  DefineParticles()
   -->  fApplication->AddParticles();
*  ||- GMATE    creation of the 'material' data structure JMATE        *
   -->  fApplication->ConstructGeometry();
*  ||- GGCLOS   close Geometry package                                 *
*  ||- GPHYSI   preparation of cross-sections and energy loss tables   *
*  |            for all used materials                                 *
   -->  FinishGeometry();
*  ||- <USER>   description  of the geometrical setup,   of the        *
*  ||           sensitive detectors and creation of data structures    *
*  ||           JVOLUM, JTMED, JROTM, JSETS                            *
   -->  fApplication->InitGeometry();
   -->    
*  |                                                                   *
*  |-GRUN (loop over events)                                           *
*  ||                                                                  *
   TGeant3::ProcessRun
   -->  fApplication->BeginEvent();
   -->  ProcessEvent();
*  ||- GTRIGI   initialisation for event processing                    *
         -->  Gtrigi();
*  ||- GTRIG    event processing                                       *
         -->  Gtrigc();
         -->  Gtrig();
*  ||  |                                                               *
*  ||  |- GUKINE (user)  generation (or input)  of  event initial      *
*  ||  |                 kinematics                                    *
       -->    TVirtualMCApplication::Instance()->GeneratePrimaries();
*  ||  |- GUTREV (user)                                                *   
       --> gtreveroot();
*  ||  |   |- GTREVE   (simplified flow for sequential tracking)       *
*  ||  |      |- GSSTAK   store primary tracks in stack                *
*  ||  |      |- Loop over tracks                                      *
*  ||  |        |- GLTRAC   prepare commons for tracking               *
*  ||  |            |- GMEDIA  find current volume /tracking medium    *
*  ||  |        |- GUTRAK (user)                                       *
*  ||  |          |- GTRACK                                            *
*  ||  |            |- GTGAMA/GTELEC/...  tracking   of  particle      *
*  ||  |                                  according to type            *
*  ||  |                 |-   compute physical step size               *
*  ||  |                 |- GTNEXT compute geometrical step size       *
*  ||  |                 |-   propagate (GUSWIM..)                     *
*  ||  |                 |-   test change of volume (GINVOL)           *
gufld  ->      TVirtualMCApplication::Instance()->Field(xdouble,bdouble);
     TVirtualMCApplication::Instance()->PreTrack();

     g3track();

     TVirtualMCApplication::Instance()->PostTrack();
*  ||  |            |- GUSTEP (user) recording of hits in data         *
  if (!gMC->IsTrackOut()) app->Stepping();

*  ||  |                      structure JHITS and of space points      *
*  ||  |                      in structure JXYZ                        *
*  ||  |- GUDIGI computation  of  digitisations  and recording  in     *
*  ||  |         structure JDIGI                                       *
*  ||  |- GUOUT  output of current event                               *
*  ||                                                                  *
*  ||- GTRIGC   clearing of memory for next event                      *
*  |                                                                   *
*  |-UGLAST (user)                                                     *
  fApplication->FinishEvent();
*  ||                                                                  *
*  ||- GLAST    standard GEANT3 termination.                           *
*  |                                                                   *
*  |                                                                   *
*  STOP                                                                *
*/

#include <assert.h>
#include "StarVMCApplication.h"
#include "StarMCHits.h"
#include "TGeoManager.h"
#include "TROOT.h"
#include "TSystem.h"
#include "TInterpreter.h"
#include "TVirtualMC.h"
#include "TPDGCode.h"
#include "TApplication.h"
#include "TGeant3TGeo.h"
#include "StarMagField.h"
#include "StarCallf77.h"
#define agufld  F77_NAME(agufld,AGUFLD)
#define gufld   F77_NAME(gufld,GUFLD)
#define agdetp_new	 F77_NAME(agdetpnew,AGDETPNEW)
#define agdetp_add	 F77_NAME(agdetpadd,AGDETPADD)
R__EXTERN  "C" {
  void type_of_call agufld(Float_t *x, Float_t *bf);
  void type_of_call gufld(Float_t *x, Float_t *bf) {agufld(x,bf);}
  void type_of_call agdetp_add(DEFCHARD name, Float_t* a, Int_t* b DEFCHARL namel) {
    printf("StarVMCApplication agdetp_add(%s,%f,%i) is called\n",name,a[0],b[0]);
    if (TString(name) == "MFLG(1).Bfield=") StarMagField::SetFactor(a[0]);
  }
  void type_of_call agdetp_new(DEFCHARD name DEFCHARL namel) {
    printf("StarVMCApplication agdetp_new(%s) is called\n",name);
  }
}

ClassImp(StarVMCApplication);

//_____________________________________________________________________________
StarVMCApplication::StarVMCApplication(const char *name, const char *title) : 
  TVirtualMCApplication(name,title),
  fStack(0),
  fPrimaryGenerator(0),
  fMagField(0),
  fMcHits(0),
  fFieldB(0) 
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

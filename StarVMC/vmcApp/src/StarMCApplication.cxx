// $Id: StarMCApplication.cxx,v 1.2 2004/07/13 19:10:25 potekhin Exp $
//

#include <iostream.h>

#include "StarMCApplication.h"
#include "StarConfiguration.h"
#include "StarStack.h"
#include "StarDetector.h"
#include "StarKineGenerator.h"
#include "StarVolume.h"

#include <TROOT.h>
#include <TGeoManager.h>
#include <TInterpreter.h>
#include <TVirtualMC.h>
#include <TPDGCode.h>


ClassImp(StarMCApplication)

//_____________________________________________________________________________
  StarMCApplication::StarMCApplication(const char *name, const char *title,
                                     FileMode fileMode) 
    : TVirtualMCApplication(name,title),
      _stack(0),
      _DetectorConstruction(),
      fFieldB(0),
      fRootManager("StarVMC", fileMode),
      _fileBased(0),
      _display(0),
      _finishEventCB(0)
{
  // Standard constructor
  cout<<"StarMCApplication ctor called"<<endl;


  // Create a user stack
  _stack   = new StarStack(100); 
  
  // need a fresh container for the modules
  _modules = new TObjArray(0);
  // ditto, hits
  _hits    = new TObjArray(0);


  _generator = new StarKineGenerator();
  _generator->SetStack(_stack);

  // Constant magnetic field (in kiloGauss)
  fFieldB = new Double_t[3];
  fFieldB[0] = 0.;
  fFieldB[1] = 0.;
  fFieldB[2] = 5.;

  cout<<"StarMCApplication ctor done"<<endl;
}

//_____________________________________________________________________________
StarMCApplication::StarMCApplication()
  : TVirtualMCApplication(),
    _stack(0),
    _DetectorConstruction(),
    fFieldB(0),
    fRootManager(),
    _fileBased(0),
    _display(0),
    _finishEventCB(0)
{    
// Default constructor
// ---
}

//_____________________________________________________________________________
StarMCApplication::~StarMCApplication() 
{
// Destructor  
// ---
  
  delete _stack;
  delete fFieldB;
  delete gMC;
  gMC = 0;
}



//_______________________________________________________________________
void StarMCApplication::SetGenerator(StarGenerator   *generator) {
  if(!_generator) _generator = generator;
}
//_______________________________________________________________________
void StarMCApplication::ResetGenerator(StarGenerator *generator)
{
  if(_generator) {
    if(generator) cerr<<"Replacing generator"<<endl;
    else cerr<<"Replacing generator with NULL"<<endl;
    _generator = generator;
  }
}
//
//_____________________________________________________________________________
void StarMCApplication::setFileBased() {
  _fileBased=1;
}

//_____________________________________________________________________________
void StarMCApplication::RegisterStack()
{
// Registers stack in Root manager.
// ---

  fRootManager.Register("stack", "StarStack", &_stack);   
}  

//*****************************************************************************
//
//
//                              public methods
//
//
//*****************************************************************************

/* TGeant3 has the following sequence of calls,
   in its Init method:

    fApplication->ConstructGeometry();

    FinishGeometry(); // if flagged as importing root geo, imports material and media

    fApplication->InitGeometry();

*/

//_____________________________________________________________________________
void StarMCApplication::ConstructGeometry()
{    
  //

  cout<<"StarMCApplication::ConstructGeometry"<<endl;
  if(!gGeoManager) {
    cout<<"No TGeoManager found!"<<endl;
    exit(-1);
  }

  if(_fileBased) return; // no need to construct (?)

  // Do materials
  cout<<"StarMCApplication::ConstructGeometry DetectorConstruction.ConstructMaterials()"<<endl;
  _DetectorConstruction.ConstructMaterials();
  // Do global geo setup (may change in future)
  cout<<"StarMCApplication::ConstructGeometry DetectorConstructionConstructGeometry()"<<endl;
  _DetectorConstruction.ConstructGeometry();

  // Now do individual detectors
  TIterator*   it = Modules()->MakeIterator();
  StarDetector* d = (StarDetector*) it->Next();

  while(d) {
    d->CreateGeometry();
    d=(StarDetector*) it->Next();
  }

}

//_____________________________________________________________________________
void StarMCApplication::InitGeometry() {    
// Initialize geometry
// ---
  
//  fTrackerSD.Initialize();

  cout<<"StarMCApplication::InitGeometry()"<<endl;

  TGeoVolume* topVolume = gGeoManager->GetTopVolume();
  //  cout<<"Have Top Volume"<<endl;
  //  InspectGeometry(topVolume);
  //  cout<<"inspected"<<endl;


}

//_____________________________________________________________________________
void StarMCApplication::InitMC(void) {

  // This is called from the "driver", i.e. user application
  // A few lines down below, it calls Init method on the
  // TGeant3 object, which in turn operates the detector
  // construction and initialization (above)

  cout<<"StarMCApplication::InitMC"<<endl;

  TString geoFile = StarConfiguration::getGeoFile();

  if(geoFile.Length()>0) {
    setFileBased();
    TGeoManager::Import(geoFile);
  }
  else {   //    cout<<"Internal geo not implemented yet, use external file"<<endl;     exit(-1);

    cout<<"Will create an instance of the TGeoManager"<<endl;
    new TGeoManager("StarTest","Manager");

  }

  //  geant3->SetDEBU(1,2,1);
  //  geant3->SetSWIT(2,2);

  gMC->SetStack(_stack);
  gMC->Init();
  gMC->BuildPhysics(); 
  
  RegisterStack();
}

//_____________________________________________________________________________
void StarMCApplication::InitDisplay() {    
  _display = new StarMCDisplay();
}
//_____________________________________________________________________________
void StarMCApplication::InspectGeometry(TGeoVolume* v) {

  if(v==0) {
    cout<<"zero pointer supplied for top volume!"<<endl;
    return;
  }

  TObjArray* nodes=v->GetNodes();
  if(nodes==0) {
    cout<<"no nodes detected in the top volume!"<<endl;
    return;
  }

  int n = nodes->GetEntries();

  cout<<"Entries: "<<n<<endl;

  if(n>0) {
    for (int i=0;i<n;i++) {
      TObject* myObj = nodes->At(i);
      if(myObj) {
	TGeoNode* myNode = (TGeoNode *) myObj;

	cout<<"At: "<<i<<endl;
	if(myNode) {
	  cout<<myNode->GetName()<<"***"<<endl;
	  InspectGeometry(myNode->GetVolume());
	}
	else
	  cout<<"failed"<<endl;
      }

    }
  }
}

//_____________________________________________________________________________
void StarMCApplication::GeneratePrimaries()
{
  _generator->Generate();
}

//_____________________________________________________________________________
void StarMCApplication::BeginEvent() {    
  // User actions at beginning of event
  // ---
  
  // nothing to be done this example
}

//_____________________________________________________________________________
void StarMCApplication::BeginPrimary() {    
  // User actions at beginning of a primary track
  // ---

  // nothing to be done this example
}

//_____________________________________________________________________________
void StarMCApplication::PreTrack() {    
  // User actions at beginning of each track
  // ---

  // nothing to be done this example
}

//_____________________________________________________________________________
void StarMCApplication::Stepping() { // User actions at each step

  Int_t copyNo;
  Int_t id = gMC->CurrentVolID(copyNo);  //  cout<<"volume "<<id<<"   "<<gMC->VolName(id)<<endl;

  StarVolume* sv =  StarVolume::FindVolume(id);
  if(!sv) {
    cout<<"Fatal error: volume "<<id<<" was not located in the list"<<endl;
    exit(-1);
  }

  if (! sv->IsSensitive()) return;       //  cout<<"volume id: "<<id<<" name "<<sv->GetName()<<endl;

  Double_t edep = gMC->Edep();

  if (edep==0.) return;

  StarHit* newHit = new StarHit(); // = AddHit();

  newHit->SetTrackID (gMC->GetStack()->GetCurrentTrackNumber());
  newHit->SetVolumeID(copyNo);
  newHit->SetEdep    (edep);

  // Position
  TLorentzVector pos;
  gMC->TrackPosition(pos);
  newHit->SetPos (TVector3(pos.X(), pos.Y(), pos.Z()));
  

  AddHit(newHit);

  //newHit->Print();
  //newHit->Draw();

  return;


}

//_____________________________________________________________________________
void StarMCApplication::PostTrack()
{    
  // User actions after finishing of each track
  // ---

  // nothing to be done this example
}

//_____________________________________________________________________________
void StarMCApplication::FinishPrimary()
{    
  // User actions after finishing of a primary track
  // ---
  
  // nothing to be done this example
}

//_____________________________________________________________________________
void StarMCApplication::FinishEvent()
{    
  // User actions after finishing of an event
  // ---
  
  if (TString(gMC->GetName()) == "TGeant3") { gMC->Gdraw("WRLD", 90., 180.);
  }  
 
  fRootManager.Fill();


  cout<<"End Event"<<endl;
  PrintHits();

  if(_display) {
    _display->DrawVolume();
    _display->DrawHits(Hits());
  }

  if(_finishEventCB) _finishEventCB();

  // Clear hits: tbd?
  Hits()->Delete();

  //  _stack->Print();  
  _stack->Reset();
} 

//_____________________________________________________________________________
void StarMCApplication::Field(const Double_t* x, Double_t* b) const {
  // Uniform magnetic field
  // ---
  
   for (Int_t i=0; i<3; i++) b[i] = fFieldB[i];
}

//_____________________________________________________________________________
void  StarMCApplication::ReadEvent(Int_t i) {
  // Reads i-th event and prints hits.    
  // ---
  
  //  fTrackerSD.Register();
  RegisterStack();
  fRootManager.ReadEvent(i);

  _stack->Print();  
  //  fTrackerSD.Print();
}  
//_____________________________________________________________________________
void StarMCApplication::RunMC(Int_t nofEvents) {    
// MC run.
// ---

  gMC->ProcessRun(nofEvents);
  FinishRun();
}

//_____________________________________________________________________________
void StarMCApplication::FinishRun() {    
// Finish MC run.
// ---

  fRootManager.Write();
}

//_____________________________________________________________________________
void StarMCApplication::AddModule(StarModule* m_) {
  if (m_ == 0x0) return;
  _modules->Add(m_);
}
//_____________________________________________________________________________
void StarMCApplication::AddHit(StarHit* h_) {
  if (h_ == 0x0) return;
  _hits->Add(h_);
}

//_____________________________________________________________________________
void StarMCApplication::PrintHits(void) {

  // Now do individual hits
  TIterator*   it = Hits()->MakeIterator();
  StarHit*      h = (StarHit*) it->Next();

  while(h) {
    h->Print();
    h=(StarHit*) it->Next();
  }
}

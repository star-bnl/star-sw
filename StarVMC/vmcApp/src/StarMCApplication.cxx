// $Id: StarMCApplication.cxx,v 1.5 2004/09/02 23:27:31 potekhin Exp $
// $Log: StarMCApplication.cxx,v $
// Revision 1.5  2004/09/02 23:27:31  potekhin
// *** empty log message ***
//
// Revision 1.4  2004/07/16 22:52:35  potekhin
// Incremental changes
//
// Revision 1.3  2004/07/13 19:10:59  potekhin
// Added a log tag
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
                                     FileMode fileMode)  : TVirtualMCApplication(name,title),
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
  _stack   = new StarStack(10000);
  //  _stack->SetDebug(3);
  
  // need a fresh container for the modules
  _modules = new TObjArray(0);
  // ditto, hits
  _hits    = new TObjArray(0);


  _generator = new StarKineGenerator();
  _generator->SetStack(_stack);
  _generator->SetSeed(StarConfiguration::getSeed());

  _generator->Init();

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
void StarMCApplication::SetInterest(const char* processName_) {
  _stack->SetInterest(processName_);
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
    cout<<"*** "<<d->GetName()<<endl;
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
  cout<<"Begin Event"<<endl;
  Hits()->Delete();   //  _stack->Print();  
  Stack()->Reset();
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

  if(_display) { } // prepare tracks to be shown


  if(!sv->IsSensitive()) return;       //  cout<<"volume id: "<<id<<" name "<<sv->GetName()<<endl;

  Double_t edep = gMC->Edep();
  if (edep==0.0) return;

  StarHit* newHit = new StarHit(); // = AddHit();

  newHit->SetTrackID (gMC->GetStack()->GetCurrentTrackNumber());
  newHit->SetVolumeID(id);
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

  // cout<<"Stack current: "<<endl;//_stack->GetCurrentParticle()<<endl;



  // nothing to be done this example
}

//_____________________________________________________________________________
void StarMCApplication::FinishPrimary()
{    
  // User actions after finishing of a primary track
  // ---

  cout<<"Finished primary track"<<endl;
  
  // nothing to be done this example
}

//_____________________________________________________________________________
void StarMCApplication::FinishEvent()
{    
  // User actions after finishing of an event
  // ---
  
  // ??? obsolete ???
  if (TString(gMC->GetName()) == "TGeant3") { gMC->Gdraw("WRLD", 90., 180.);
  }  
 
  //  fRootManager.Fill();


  cout<<"End Event"<<endl;
  //  PrintHits();

  if(_display) {
    _display->DrawVolume();
    _display->DrawHits(Hits());
    _display->Update();
  }

  if(_finishEventCB) _finishEventCB(Hits());

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
  //  RegisterStack();
  //  fRootManager.ReadEvent(i);

  //  cout<<"Read Event"<<endl;
  //  _stack->Print();  
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
void StarMCApplication::Trig(void) {    
// MC run.
// ---
  cout<<"Begin Trig"<<endl;
  BeginEvent();
  gMC->ProcessEvent();
  FinishEvent();
  cout<<"End Trig"<<endl;
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

  //  cout<<"Hits for track "<<h_->GetTrackID()<<endl;
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
//_____________________________________________________________________________
void StarMCApplication::PrintKine(void) {
  Stack()->PrintKine();
}
//_____________________________________________________________________________
void StarMCApplication::PrintVertex(int n_) {

  // Now do individual vertices
  cout<<"Print Vertex"<<endl;

  TObjArray*    vert=Vertices();

  cout<<"number  parent"<<endl;
  if(!vert) {
    cout<<"No vertices found"<<endl;
    return;
  }

  if(n_>=0) { // individual vertex
    StarVertex* v = (StarVertex*) vert->At(n_);
    v->Print();
    return;  // done here
  }

  // Have to do all
  TIterator*      it = vert->MakeIterator();
  StarVertex*      v = (StarVertex*) it->Next();

  while(v) {
    v->Print();
    v=(StarVertex*) it->Next();
  }
}
//_____________________________________________________________________________
void StarMCApplication::PrintParticle(int n_) {

  // Now do individual vertices

  TObjArray*    part=_stack->Kine();

  if(!part) {
    cout<<"No particles found in the event"<<endl;
    return;
  }


  cout<<"id   origin  vertices"<<endl;

  if(n_>=0) { // individual particle
    StarParticle* p = (StarParticle*) part->At(n_);
    if(!p) {
      cout<<"No particle found at "<<n_<<endl;
      return;
    }
    p->Print();
    return;  // done here
  }

  // Have to do all
  TIterator*      it = part->MakeIterator();
  StarParticle*    p = (StarParticle*) it->Next();

  while(p) {
    p->Print();
    p=(StarParticle*) it->Next();
  }
}

#include "AgUStep.h"
#include "StMessMgr.h"
#include "St_geant_Maker/St_geant_Maker.h"
#include "TMath.h"
#include "TLorentzVector.h"

extern "C" {

  void agustep_() {
    if ( AgUStep::Instance() ) {
      (*AgUStep::Instance())();
    }
  };


  struct Agcstep_t {
    
    Float_t vect0[7];
    Float_t vloc0[7];
    Float_t vloc[7];
    Float_t xloc[7];
    Float_t astep;
    Float_t adestep;

  } agcstep_;

};

// .................................................................. Event ...............
Event::Event() : 
  TObject(), 
  idEvent(-1), 
  nTracks(0), 
  nSteps(0),
  tracks(new TClonesArray("Track")),
  steps(new TClonesArray("Step"))
{ 
  Clear(); 
}
//
Track *Event::AddTrack() {

  TLorentzVector p, x;
  AgUStep::geant3->TrackMomentum( p );
  AgUStep::geant3->TrackPosition( x );
 
  TClonesArray &TRACKS = *tracks;

  Track *t = new (TRACKS[nTracks++]) Track();

  t->eta = p.Eta();
  t->phi = p.Phi();

  t->px = p[0];
  t->py = p[1];
  t->pz = p[2];

  t->x = x[0];
  t->y = x[1];
  t->z = x[2];

  t->mass   = AgUStep::geant3->TrackMass();
  t->charge = AgUStep::geant3->TrackCharge();

  //  tracks.Add( t );
  return t;
};
void  Event::Clear( const Option_t *opts ) {
  idEvent = -1;
  nTracks = 0;
  nSteps  = 0;
  tracks->Clear("C");
};
// .................................................................. Track ...............
Track::Track() : TObject(), idTruth(-1), eta(0), phi(0), nSteps(0), steps() { Clear(); } 

Step *Track::AddStep() {

  TLorentzVector x;
  AgUStep::geant3->TrackPosition( x );
  //  nSteps++;
  //  Step *s = new Step();
  Int_t &n = AgUStep::Instance()->mEvent->nSteps;
  TClonesArray &STEPS = *AgUStep::Instance()->mEvent->steps;
  Step *s = new (STEPS[n++]) Step(); 
  s->x = x[0];
  s->y = x[1];
  s->z = x[2];
  s->r = x.Perp();
  s->idStep = n; // ID of tracking step 
  s->idTruth = idTruth;
  this->nSteps++; // Why is this not incremented in the track object?  
  steps.Add(s); 

  //  LOG_INFO << "      New Step at index: " << n << " nSteps=" << nSteps << endm;

  //  nSteps++; // add step
  return s;
}
void  Track::Clear( const Option_t *opts ) {
  idTruth = -1;
  eta = 0;
  phi = 0;
  px = 0; py = 0; pz = 0; 
  x = 0; y = 0; z = 0; mass = 0; charge = 0;
  nSteps=0;
  steps.Clear("");
};
// .................................................................. Step ................
Step::Step() : TObject(), idStep(-1), x(0), y(0), z(0), dEstep(-1), adEstep(-1), step(-1),state(0) { }
void Step::Clear(Option_t *opts)
{
  idStep=-1;
  x=0; y=0; z=0; dEstep=-1; adEstep=-1; step=-1; state=0;
}



// .....................................................................................
#define agcstep agcstep_

TGiant3  *AgUStep::geant3; // G3 VMC
Quest_t  *AgUStep::cquest; // G3 Commons ...
Gclink_t *AgUStep::clink; 
Gcflag_t *AgUStep::cflag; 
Gcvolu_t *AgUStep::cvolu; 
Gcnum_t  *AgUStep::cnum; 
Gcsets_t *AgUStep::csets;
Gckine_t *AgUStep::ckine;
Gcking_t *AgUStep::cking;
Gctrak_t *AgUStep::ctrak;
Gcmate_t *AgUStep::cmate;
Gccuts_t *AgUStep::ccuts;
Gcphys_t *AgUStep::cphys;
Int_t     AgUStep::nlev;

AgUStep *AgUStep::sInstance = 0;
AgUStep *AgUStep::Instance() { 
  if ( 0 == sInstance ) sInstance = new AgUStep();
  return sInstance; 
}

AgUStep::AgUStep() : TNamed("AgUStep","AgSTAR user stepping routine"),
		     mTree(0),
		     mFile(0),
		     mEvent( new Event() ),
		     mTrack(0),
		     idEvent(-1)
{ 
  geant3 = St_geant_Maker::instance()->Geant3();
  cquest = (Quest_t  *) geant3->Quest();
  clink  = (Gclink_t *) geant3->Gclink();
  cflag  = (Gcflag_t *) geant3->Gcflag();
  cvolu  = (Gcvolu_t *) geant3->Gcvolu();
  cnum   = (Gcnum_t  *) geant3->Gcnum();
  csets  = (Gcsets_t *) geant3->Gcsets();
  ckine  = (Gckine_t *) geant3->Gckine();
  cking  = (Gcking_t *) geant3->Gcking();
  ctrak  = (Gctrak_t *) geant3->Gctrak();
  cmate  = (Gcmate_t *) geant3->Gcmate();
  ccuts  = (Gccuts_t *) geant3->Gccuts();
  cphys  = (Gcphys_t *) geant3->Gcphys();

  oldEvent = -999;

};

// Take a step through the G3 geometry
void AgUStep::operator()()
{

  Double_t x = ctrak -> vect[0];
  Double_t y = ctrak -> vect[1];
  Double_t z = ctrak -> vect[2];

  Double_t r = TMath::Sqrt(x*x+y*y);      if (r>50.0) return; // 50 cm limit

  // Get current event
  idEvent = geant3->CurrentEvent();
  
  // Detect new event and reset sums, clear event, etc...
  if ( oldEvent != idEvent )
    {

      if (mTree && idEvent>1) mTree->Fill(); // last event should be filled on finish

      mEvent -> Clear("C"); // clear old event
      aDeStep = 0;          // clear sums etc...
      aStep   = 0;
      idTruth = 0;
      //      LOG_INFO << "New Event " << idEvent << endm;
      mEvent -> idEvent = idEvent; // and set the event number
      oldEvent = idEvent;
    }

  // Detect a new track
  if ( 0 == ctrak->sleng ) 
    {
      aDeStep = 0;
      aStep   = 0;
      idTruth++;
      //      LOG_INFO << "  New Track " << idTruth << endm;

      // Add track to this event
      mTrack = mEvent->AddTrack();
      mTrack->idTruth = idTruth;
      
    }

  
  aDeStep += ctrak->destep;
  aStep   += ctrak->step;

  // Add a new step to the track
  assert(mTrack);

  //  LOG_INFO << "     New Step " << x << " " << y << " " << z << " " << aDeStep << " " << endm;

  Step *mStep = mTrack -> AddStep();
  mStep -> adEstep = aDeStep;
  mStep -> dEstep  = ctrak -> destep;
  mStep -> step = ctrak -> step;
  mStep -> state = ctrak->inwvol;

  
}




// Initialization
void AgUStep::Init( const Char_t *filename )
{
  //  LOG_INFO << "Initialize stepper with filename= " << filename << endm;
  mFile = TFile::Open( filename, "recreate" );
  mTree = new TTree( "stepping", "custom stepping tree" );
  mTree->Branch("Event", &mEvent, 32000, 99); // Add the event to the ttree
}

void AgUStep::Finish()
{
  if (mTree&&mFile) {
    mTree->Fill();

    mFile->cd();
    mTree->Write();
    //  mFile->Write();
    delete mFile;
  }
}

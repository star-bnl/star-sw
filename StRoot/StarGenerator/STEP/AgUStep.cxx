#include "AgUStep.h"
#include "StMessMgr.h"
#include "St_geant_Maker/St_geant_Maker.h"
#include "TMath.h"
#include "TLorentzVector.h"
#include "TGeoManager.h"
#include "TGeoManager.h"

Float_t AgUStep::rmin = 0.0;
Float_t AgUStep::rmax = 200.0;
Float_t AgUStep::zmin =-200.0;
Float_t AgUStep::zmax = 200.0;
Int_t   AgUStep::verbose = 0;
Int_t   AgUStep::mnTruth =  0;
Int_t   AgUStep::mxTruth = -1;

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
Step::Step() : TObject(), 
	       idStep(-1), 
	       idTruth(0),
	       x(0), y(0), z(0), r(0), 
	       state(0), 
	       dEstep(-1), 
	       adEstep(-1), 
	       nStep(-1) ,
	       step(-1) ,
	       dens(0)  ,
	       A(0), 
	       Z(0),
	       isvol(0)
{ 
  Clear(); 
}

void Step::Clear(Option_t *opts)
{
  idStep=-1; idTruth=0; isvol=0;
  x=0; y=0; z=0; dEstep=-1; adEstep=-1; step=-1; state=0; nStep=-1;
  for( Int_t i=0;i<15;i++ ) { vnums[i]=0; cnums[i]=0; }
}

TString Step::path()
{
  TString _path = "";
  if ( gGeoManager ) for ( Int_t i=0;i<15;i++ ) {
      Int_t vid = vnums[i]; if (0==vid) break;
      Int_t cid = cnums[i];
      TGeoVolume *vol = gGeoManager->GetVolume(vid);   if (0==vol) {_path=""; break; }
      TString name = vol->GetName();
      _path += Form("/%s_%i",name.Data(),cid);
    }
  return _path;
}

TString Step::volume()
{
  TString _volume = "";
  if ( gGeoManager ) for ( Int_t i=0;i<15;i++ ) {

      Int_t id = vnums[i]; if (0==id) break;
      TGeoVolume *vol = gGeoManager->GetVolume(id);
      if ( vol ) _volume = vol->GetName();
    }


  return _volume;
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
Gctmed_t *AgUStep::ctmed;

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
		     idEvent(-1),
		     idTruth(0),
		     aDeStep(0.),
		     nStep(0),
		     aStep(0.),
		     vect0{0.,0.,0., 0.,0.,0., 0.}, oldEvent(-1)
  
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
  ctmed  = (Gctmed_t *) geant3->Gctmed();

  oldEvent = -999;

};

// Take a step through the G3 geometry
void AgUStep::operator()()
{

  Double_t x = ctrak -> vect[0];
  Double_t y = ctrak -> vect[1];
  Double_t z = ctrak -> vect[2];

  Double_t _a = cmate->a;
  Double_t _z = cmate->z;
  Double_t _dens = cmate->dens;

  Double_t r = TMath::Sqrt(x*x+y*y);      
  if (r > rmax || TMath::Abs(z) > 200 ) // limited to inner tracking region
    {
      ctrak->istop = 2; // stop the track
      return; // track is exiting region of interest
    }

  // Get current event
  idEvent = geant3->CurrentEvent();
  
  // Detect new event and reset sums, clear event, etc...
  if ( oldEvent != idEvent )
    {

      if (mTree && idEvent>1) mTree->Fill(); // last event should be filled on finish

      mEvent -> Clear("C"); // clear old event
      aDeStep = 0;          // clear sums etc...
      aStep   = 0;
      nStep   = 0;
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
      nStep   = 0;
      idTruth++;
      //      LOG_INFO << "  New Track " << idTruth << endm;

      // Add track to this event
      mTrack = mEvent->AddTrack();
      mTrack->idTruth = idTruth;
      
    }
  


  aDeStep += ctrak->destep;
  aStep   += ctrak->step;
  nStep   ++;

  // Add a new step to the track
  assert(mTrack);

  if ( r < rmin ) return; // track has not entered region of interest
  if ( z < zmin || z > zmax ) return; // outside of region of interest

  Step *mStep = mTrack -> AddStep();

  mStep -> isvol   =  ctmed->isvol;
  if ( ctmed->isvol ) {
    // I cannot rely on this, but we will set isvol to the value
    // of csets->numbv[0]... only correct for sensitive volumes
    // placed w/in the mother volume ...
    mStep->isvol = csets->numbv[0] + 1;
    
    //  LOG_INFO << "Sensitive volume: " << endm;
    //    for ( int i=0;i<20;i++ ) 
    //      LOG_INFO << csets->numbv[i] << endm;
  }

  mStep -> adEstep = aDeStep;
  mStep -> nStep  = nStep;
  mStep -> dEstep  = ctrak -> destep;
  mStep -> step = ctrak -> step;
  mStep -> state = ctrak->inwvol;

  mStep -> A = _a;
  mStep -> Z = _z;
  mStep -> dens = _dens;

  //  if (!gGeoManager) return; // step through before complete init?

  // Print out current path...
  //LOG_INFO << "N level = " << cvolu->nlevel << endm;
  TString path = "";
  TString leaf = "";
  for ( Int_t i=0;i<cvolu->nlevel;i++ )
    {
      path += "/"; 
      Char_t buff[16];
      memcpy( buff, &cvolu->names[i], sizeof(cvolu->names[i]) );
      
      TString volume; for ( Int_t ii=0;ii<4;ii++ ) volume += buff[ii];

      path += volume; path += "_"; 
      path += cvolu->number[i];

      if ( gGeoManager ) {
	UShort_t volumeNumber = gGeoManager->FindVolumeFast(volume)->GetNumber();
	UShort_t copyNumber   = cvolu->number[i];
	mStep->vnums[i] = volumeNumber;
	mStep->cnums[i] = copyNumber;
      }
      else {
	mStep->vnums[i] = 0;
	mStep->cnums[i] = 0;
      }
      leaf = volume;

    }
  
  /////////////////////////////////////////////////////////////////////////
  //
  // At this point the step is considered ended.  We only handle verbose
  // level printouts below.
  //
  //////////////////////////////////////////////////////////////////////////
  if ( mTrack->idTruth < mnTruth || mTrack->idTruth > mxTruth ) return;

  
  if ( verbose ) {
    LOG_INFO << Form("[AgUStep] idtruth=%i %8.4f %8.4f %8.4f %8.4f %4s %4i %8.4f %8.4f %s",
		     mTrack->idTruth,
		     mStep->x,
		     mStep->y,
		     mStep->z,
		     mStep->r,
		     leaf.Data(),
		     mStep->idTruth,
		     aStep,
		     mStep->step,
		     path.Data()
		     ) 
	     << endm;
		     
  }
  
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

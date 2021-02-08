#include "StarPrimaryMaker.h"
ClassImp(StarPrimaryMaker);
//#include "StarGenerator.h"

#include "g2t/St_g2t_particle_Module.h"
#include "tables/St_g2t_event_Table.h"
#include "tables/St_g2t_gepart_Table.h"
#include "tables/St_g2t_vertex_Table.h"
#include "tables/St_g2t_event_Table.h"

#include "StarGenerator.h"
#include "StarCallf77.h" 
#include <iostream>
#include "St_geant_Maker/St_geant_Maker.h"
#include "TGiant3.h"
#include <map>
#include "TString.h"
#include "TSystem.h"

#include "StBFChain.h"
#include "TFile.h"
#include "TTree.h"
#include "TClass.h"

#include "StarGenerator/EVENT/StarGenEvent.h"
#include "StarGenerator/EVENT/StarGenParticle.h"
#include "AgStarReader.h"
#include "StarGenerator/UTIL/StarRandom.h"

#include "StarGenerator/FILT/StarFilterMaker.h"

#include "TMCProcess.h"

#include "tables/St_vertexSeed_Table.h"

using namespace std;

// 1 mm / speed of light
const double mmOverC = 1.0E-3 / TMath::C();

StarPrimaryMaker *fgPrimary      = 0;
// --------------------------------------------------------------------------------------------------------------
StarPrimaryMaker::StarPrimaryMaker()  : 
  StMaker("PrimaryMaker"),
  mNumParticles(0),
  mTree(0),
  mFile(0),
  mTreeName("none"),
  mFileName("none"),
  mStack(0),
  mPrimaryEvent(0),
  mVx(0), mVy(0), mVz(0), mSx(0.1), mSy(0.1), mSz(30.0), mRho(0), mVdxdz(0), mVdydz(0),
  mDoBeamline(0),
  mPtMin(0), mPtMax(-1), mRapidityMin(0), mRapidityMax(-1), mPhiMin(0), mPhiMax(-1), mZMin(-999), mZMax(+999),
  mRunNumber(0),
  mPrimaryVertex(0,0,0,0),
  mFilter(0),mAccepted(0)
{
  assert(fgPrimary == 0); // cannot create more than one primary generator
  fgPrimary = this;

  mStack = new StarParticleStack();
  AgStarReader::Instance().SetStack(mStack);

  // Register the particle database with this maker
  StarParticleData &pdb = StarParticleData::instance();
  //  Shunt( &pdb );
  AddData( &pdb, ".data" );

  SetAttr("FilterKeepHeader", int(1) );

}
// --------------------------------------------------------------------------------------------------------------
StarPrimaryMaker::~StarPrimaryMaker()
{
  fgPrimary = 0;       // Cleanup
  if ( mStack )        delete mStack;
  if ( mFile )         delete mFile;
  /* deleting mTree and mPrimaryEvent cause seg violation here... why? */
}
// --------------------------------------------------------------------------------------------------------------
TParticlePDG *StarPrimaryMaker::pdg( Int_t id ){

  return StarParticleData::instance().GetParticle(id);

}
// --------------------------------------------------------------------------------------------------------------
Int_t StarPrimaryMaker::Init()
{

  //
  // Initialize runtime flags
  //
  mDoBeamline = IAttr("beamline");
  
  
  //
  // Initialize all submakers first
  //
  Int_t result =  StMaker::Init();

  //
  // The filter is properly a maker, so initialize it.  Also create accepted event list.
  // and add it as an object set.
  //
  if (mFilter) { 
    mFilter->Init();
    mAccepted=new TEventList(mFilter->GetName(),"Accepted events");
  }

  //
  // Intialize the TTree with one event branch for each sub generator
  // and one branch for the primary event
  //
  if ( mFileName == "" ) {
    mFileName =   ((StBFChain*)StMaker::GetTopChain())->GetFileOut();
    mFileName.ReplaceAll(".root",".gener.root");
  }

  mFile = TFile::Open( mFileName, "recreate" );
  if ( !mFile ) result = (result<kStWarn)? kStWarn : result;

  mTree = new TTree( "genevents", "TTree containing event generator information" );

  mPrimaryEvent = new StarGenEvent("primaryEvent","Primary Event... particle-wise information from all event generators");
  mTree->Branch("primaryEvent","StarGenEvent",&mPrimaryEvent,64000,99);

  if (mFilter) mFilter->SetEvent(mPrimaryEvent);

  TIter Next( GetMakeList() );
  StarGenerator *generator = 0; 
  StMaker *_maker = 0;

  Int_t id = 0;

  while ( (_maker=(StMaker *)Next()) )
    {

      //
      // Dynamic cast to StarGenerator and punt if it's not one
      //
      generator = dynamic_cast<StarGenerator *>(_maker);
      if ( !generator ) 
	{
	  continue;
	}

      // Set ID of the event generator
      generator -> SetId( id++ );
      

      //
      // By default, connect generator to output tree.  It the IO mode
      // of the generator has been set, skip this step.
      //
      if ( generator->IOmode()==0 ) 
	{
	  generator -> SetOutputTree( mTree );
	}

    }

  return result;
}
// --------------------------------------------------------------------------------------------------------------
Int_t StarPrimaryMaker::Finish()
{

  // Calls finish on all sub makers to collect statisitcs
  StMaker::Finish();

  // Now call finish on the filter
  if ( mFilter ) {

    // Call finish on the generator
    mFilter->Finish();
    
    mAccepted->Print("all");
    mTree->SetEventList( mAccepted );
  }

  if (mFile) 
    { 

      // Add the instance of the particle data so we have a record of
      // the particles used as input to the generator
      TObjArray particles = StarParticleData::instance().GetParticles();
      mTree->GetUserInfo()->Add( &particles );
      //      mTree->GetUserInfo()->Add( &(StarParticleData::instance().GetParticles()) );

      ///\todo add random number state to user info

      TIter Next( GetMakeList() );
      StarGenerator *generator = 0; 
      StMaker *_maker = 0;
      
      //Int_t id = 0;
      
      while ( (_maker=(StMaker *)Next()) )
	{
	  generator = dynamic_cast<StarGenerator *>(_maker);
	  if ( !generator ) 
	    {
	      continue;
	    }
	  StarGenStats stats = generator->Stats();
	  if ( mFilter ) stats.nFilterSeen   = mFilter->numberOfEvents();
	  if ( mFilter ) stats.nFilterAccept = mFilter->acceptedEvents();
	  stats.Dump();
	  stats.Write(); // write to fiel
	}

      mFile -> Write();
      mFile -> Close();
    }
  else
    {
      Warning(GetName(),"Could not write to unopened file");
    }


  return kStOK;
}
// --------------------------------------------------------------------------------------------------------------
Int_t StarPrimaryMaker::Make()
{

  Bool_t go = true;

  while (go) {

    /// Iterate over all generators and execute  PreGenerate()
    PreGenerate();

    /// Iterate over all generators and execute     Generate()
    Generate();

    /// Iterate over all generators and execute PostGenerate()
    PostGenerate();

    /// Register g2t tables
    BuildTables();

    /// Finalize the event
    Finalize();

    /// Apply the event filter (if available)
    if (mFilter)
      {
	mFilter->Make(); // properly this is a maker but we fake it here...
      }

    /// Print the event for debugging purposes
    if ( IAttr("Debug")==1 ) event()->Print("head");

    ///
    /// If the filter resulted in an accept decision, fill the tree and return kStOK
    ///
    if ( event()->GetFilterResult() & ( StarGenEvent::kAccept | StarGenEvent::kFlag ) )
      {

	//
	// Increment the event number in the master event record
	//
	(*event())++;

	//
	// Fill the TTree
	//
	mTree->Fill();

	//
	// Add the event to the accepted event list
	//
	if ( mAccepted ) mAccepted->Enter( mTree->GetEntries() );

	// Break out of loop and accept event.  
	break;
	
      }

    ///
    /// If the filter resulted in a reject decision, fill the tree and try try again.
    /// Clear the particle information if the KeepAll flag has not been set.
    ///
    if ( IAttr( "FilterKeepAll" ) == 0 ) mPrimaryEvent->Clear("part");
    if ( IAttr( "FilterKeepAll" ) || IAttr( "FilterKeepHeader" ) )    mTree->Fill();
    Clear();

    //
    // If enabled, skip rejected events
    //
    if ( IAttr("FilterSkipRejects") ) return kStSKIP; 


  }// infinite loop

  return kStOK;
}
// --------------------------------------------------------------------------------------------------------------
// Intialize for this run
Int_t StarPrimaryMaker::InitRun( Int_t runnumber )
{
  // Set the run number
  mPrimaryEvent->SetRunNumber(runnumber);
  mRunNumber = runnumber;

  return StMaker::InitRun( runnumber );
}

// --------------------------------------------------------------------------------------------------------------
void StarPrimaryMaker::Clear( const Option_t *opts )
{
  mNumParticles = 0;
  mStack->Clear();
  mPrimaryEvent->Clear();
  StMaker::Clear(opts);
  TIter Next( GetMakeList() );
  StarGenerator *generator = 0;
  while ( (generator=(StarGenerator *)Next()) )
    {
      generator->Clear();
    }
}

// --------------------------------------------------------------------------------------------------------------
void StarPrimaryMaker::AddGenerator( StarGenerator *gener )
{
  static Int_t id = 0;
  gener->mId = ++id;
  AddMaker(gener);
}

void StarPrimaryMaker::AddFilter( StarFilterMaker *filter )
{
  mFilter = filter;
  AddData( 0, ".filter" );
  mFilter -> Shunt( GetDataSet( ".filter" ) );
}
// --------------------------------------------------------------------------------------------------------------
Int_t StarPrimaryMaker::PreGenerate()
{ 
  
  // Check for beamline constraint and load values
  if ( mDoBeamline ) 
    {
      TDataSet*      dbDataSet = GetChain()->GetDataBase("Calibrations/rhic/vertexSeed");
      vertexSeed_st* vSeed = 0;
      if ( dbDataSet ) 
	{
	  vSeed = ((St_vertexSeed*) (dbDataSet->FindObject("vertexSeed")))->GetTable();
	}
      if ( vSeed ) {

	mVx    = vSeed->x0;
	mVy    = vSeed->y0;
	mVdxdz = vSeed->dxdz;
	mVdydz = vSeed->dydz;
      }
      else {
	LOG_WARN << "-- Beamline constraint requested but none seen in DB --" << endm;
      }

    }

  if ( mDoBeamline ) 
  LOG_INFO << Form("[%i]Beamline Parameters run=%i vx=%6.4f vy=%6.4f vz=%6.3f dxdz=%6.4f dydz=%6.4f",mDoBeamline,mRunNumber,mVx,mVy,mVz,mVdxdz,mVdydz) << endm;


  TIter Next( GetMakeList() );
  StarGenerator *generator = 0;
  while ( (generator=(StarGenerator *)Next()) )
    {
      generator -> PreGenerateHook();
    }
  
  return kStOK;
}
// --------------------------------------------------------------------------------------------------------------
Int_t StarPrimaryMaker::PostGenerate()
{ 
  
  TIter Next( GetMakeList() );
  StarGenerator *generator = 0;
  while ( (generator=(StarGenerator *)Next()) )
    {
      generator -> PostGenerateHook();
    }
  
  return kStOK;
}
// --------------------------------------------------------------------------------------------------------------
Int_t StarPrimaryMaker::Generate()
{
  TIter Next( GetMakeList() );
  StarGenerator *generator = 0; 
  while ( (generator=(StarGenerator *)Next()) )
    {

      // Generate the event with probability equal to the pileup
      // probability
      Double_t prob = generator -> GetPileup();
      if ( StarRandom::Instance().flat() < prob )
	generator -> Generate(); 

      // Accumulate total number of particles
      mNumParticles += generator->GetNumberOfParticles();

    }  
  return kStOK;
}
// --------------------------------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------------------------------
void StarPrimaryMaker::SetCuts( Double_t ptmin,  Double_t ptmax, 
				    Double_t ymin,   Double_t ymax,
				    Double_t phimin, Double_t phimax,
				    Double_t zmin,   Double_t zmax )
{

  LOG_INFO << "-- StarPrimaryMaker::SetCuts --" << " ptmin=" << ptmin << " ptmax=" << ptmax << endm;
  LOG_INFO << "-- StarPrimaryMaker::SetCuts --" << "  ymin=" <<  ymin << "  ymax=" <<  ymax << endm;
  LOG_INFO << "-- StarPrimaryMaker::SetCuts --" << " phimn=" << phimin << " phimx=" << phimax << endm;
  LOG_INFO << "-- StarPrimaryMaker::SetCuts --" << "  zmin=" <<  zmin << "  zmax=" <<  zmax << endm;

  mPtMin = ptmin; mPtMax = ptmax;
  mRapidityMin = ymin; mRapidityMax = ymax;
  mPhiMin = phimin; mPhiMax = phimax;
  mZMin = zmin; mZMax = zmax;
}
// --------------------------------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------------------------------
Bool_t StarPrimaryMaker::Simulate( StarGenParticle *particle )
{

  Bool_t go = particle->Simulate(); if (!go) return go;

  Double_t px = particle -> GetPx();
  Double_t py = particle -> GetPy();
  Double_t pz = particle -> GetPz();
  Double_t pt = TMath::Sqrt(px*px + py*py);
  TVector3 p( px, py, pz );

  // Check PT range
  if ( pt < mPtMin ) return false;
  if ( mPtMin < mPtMax )
    {
      if ( pt > mPtMax ) return false;
    }

  if ( pt > 0 )
    if ( mRapidityMin < mRapidityMax )
      {
	if ( p.Eta() < mRapidityMin ) return false;
	if ( p.Eta() > mRapidityMax ) return false;
      } // else, no cut
  

  // Extend this ... add phi and z-vertex range cuts.

  return true;

}
// --------------------------------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------------------------------
Int_t StarPrimaryMaker::Finalize()
{

  TIter Next( GetMakeList() );
  StarGenerator *generator = 0; 
  Int_t offset = 0;

  //
  // Loop over all generators and call finalize on them.
  //
  while ( (generator=(StarGenerator *)Next()) )
    {
      generator -> Finalize();
    }
  Next.Reset();

  //
  // Retrieve the particle_st table
  //
  St_particle *table = (St_particle*) GetDataSet("particle");
  if ( !table ) 
    {
      //assert(!mNumParticles);
      Error(GetName(),"No particle table found, but we have particles.");
      return kStFatal;
    }

  //
  // Generate the primary vertex within allowed limits
  //
  TLorentzVector primary = Vertex(); while ( primary.Z() < mZMin || primary.Z() > mZMax ) primary = Vertex();

  mPrimaryVertex = primary;

  //
  // Next loop over all generators and push their tracks onto the particle stack.
  // Add them to the local event record and establish the connection between the
  // particle index on the stack and the particle index in the event record.
  //
  Int_t nstack = 0;
  while ( (generator=(StarGenerator *)Next()) )
    {

      // Obtain the vertex for this event.  If the generator is marked as
      // pileup, sample a new vertex.  Otherwise use the primary vertex
      TLorentzVector vertex = (generator->IsPileup())?	Vertex() : primary;

      StarGenEvent *event = generator->Event();
      Int_t npart = event->GetNumberOfParticles();

      // Set the offset on the event
      event -> SetOffset( offset );

      // cout << "Pushing particles to particle stack" << endl;
      // event->Print();

      Int_t ntrack; // set by stack
      Int_t ndone = 0;

      for ( Int_t i=0; i<npart; i++ )
	{

	  StarGenParticle *particle = (*event)[i];
	  assert(particle);

	  Int_t    toDo = Simulate(particle); //particle->Simulate();

	  Double_t px   = particle->GetPx();
	  Double_t py   = particle->GetPy();
	  Double_t pz   = particle->GetPz();
	  Double_t E    = particle->GetEnergy();
	  Double_t M    = particle->GetMass();
	  Double_t vx   = particle->GetVx() / 10;       // mm --> cm as per the HEPEVT standard  
	  Double_t vy   = particle->GetVy() / 10;       // mm --> cm
	  Double_t vz   = particle->GetVz() / 10;       // mm --> cm 
	  Double_t vt   = particle->GetTof() * mmOverC; // mm/c to s

	  Double_t polx=0, poly=0, polz=0;
	  
	  Int_t    parent  = particle->GetFirstMother();
	  Int_t    parent2 = particle->GetLastMother();
	  Int_t    kid1    = particle->GetFirstDaughter();
	  Int_t    kid2    = particle->GetLastDaughter();
	  Int_t    id      = particle->GetId();
	  Int_t    status  = particle->GetStatus();

	  Double_t weight  = 1.0;

	  //
	  // Except for the first event, offset the particle's start vertex and update the
	  // particle ids
	  //
	  if ( i )
	    {

	      // Offset z-vertex first so that we can apply the beamline
	      // constraint.
	      vz += vertex.Z();

	      // Rotate the particle along the specified beamline
	      RotateBeamline( px, py, pz, E, M, vx, vy, vz, vt );

	      // Smear the vertex in X, Y, and Z. 
	      // NOTE: This is wrong, in the sense that we *should* measure the vertex
	      //       distributions along the beam line.  In practice, users give us
	      //       the x,y and z distributions as measured in the STAR reference
	      //       system.  So long as dxdz and dydz are small, this should all be
	      //       a good approximation.
	      vx += vertex.X();
	      vy += vertex.Y();
	      // vz was already offset
	      vt += vertex.T();


	      // Handle bookkeepting between event generators
	      parent += offset;
	      parent2 += offset;
	      kid1 += offset;
	      kid2 += offset;

	    }




	  // Add the track to the particle stack and register the
	  // track number with the particle
	  mStack -> PushTrack( toDo, parent, id, px, py, pz, E, vx, vy, vz, vt, polx, poly, polz, kPNoProcess, ntrack, weight, status );
	  //	  particle->SetStack( ntrack ); //?????

	  // Add the track to the g2t table iff it is pushed out to the simulator
	  if ( toDo ) table -> AddAt( particle->get_hepevt_address(), nstack );

	  // Add it to the primary event record
	  StarGenParticle *p = mPrimaryEvent->AddParticle( status, id, parent, parent2, kid1, kid2, px, py, pz, E, M, vx, vy, vz, vt );

	  // Associate this event record entry with the particle stack
	  p->SetIndex( particle -> GetIndex() );
	  // Set the primary key
	  p->SetPrimaryKey( i + offset );
	  
	  // If the particle is to be simulated, increment the stack ID
	  if ( toDo ) p->SetStack(  1 + nstack );
	  else        p->SetStack( -1 );

	  if ( toDo ) { nstack++; ndone++; }

	}

      
      // Add the total number of particles 
      // (npart includes the 0th entry 
      // generator record).
      offset += npart;


      //
      // Clear the generator's particle record so that only one
      // such record appears for the user
      //
      event->Clear("part");

    }

  return kStOK;
}

// --------------------------------------------------------------------------------------------------------------
TLorentzVector StarPrimaryMaker::Vertex()
{  
  Double_t x=0,y=0,z=0,t=0;

  TVector2 xy = StarRandom::Instance().gauss2d( mSx, mSy, mRho );
  x = mVx + xy.X();
  y = mVy + xy.Y();
  z = mVz + StarRandom::Instance().gauss( mSz );
  Double_t dist = TMath::Sqrt(x*x+y*y+z*z);
  t = dist / TMath::Ccgs();

  return TLorentzVector(x,y,z,t);
}
// --------------------------------------------------------------------------------------------------------------
void StarPrimaryMaker::BuildTables()
{

  // Loop over all generators and total the number of particles
  TIter Next( GetMakeList() );
  StarGenerator *generator = 0; 
  Int_t sum = 0;

  while ( (generator=(StarGenerator *)Next()) )
    {
      sum += generator -> Event() -> GetNumberOfParticles();
    }
  mNumParticles = sum;

  // Create g2t_event and g2t_particle tables for geant maker.  
  St_g2t_event    *g2t_event    = new St_g2t_event( "event", 1 );
  AddData( g2t_event );
  if ( mNumParticles ) {
    St_particle *g2t_particle = new St_particle( "particle", mNumParticles );
    AddData( g2t_particle );
  }
  // note: m_DataSet owns the new object(s) and is responsible for cleanup
   
}
// --------------------------------------------------------------------------------------------------------------
void StarPrimaryMaker::RotateBeamline( Double_t &px, Double_t &py, Double_t &pz, Double_t &E, Double_t &M, Double_t &vx, Double_t &vy, Double_t &vz, Double_t &vt )
{

  // Unit vectors
  static const TVector3 xhat(1,0,0), yhat(0,1,0), zhat(0,0,1);

  if ( mVdxdz == 0.0 && mVdydz == 0.0 ) return; // Nothing to do

  // Vertex position and momentum
  TLorentzVector vertex(vx,vy,vz,vt);
  TLorentzVector moment(px,py,pz,E);

  Double_t thetaX = TMath::ATan( mVdxdz );
  Double_t thetaY = TMath::ATan( mVdydz );

  vertex.Rotate( -thetaY, xhat ); // - comes about because of the cross product, order of rotations, ...
  vertex.Rotate( +thetaX, yhat );
  moment.Rotate( -thetaY, xhat );
  moment.Rotate( +thetaX, yhat );

  vx = vertex[0];
  vy = vertex[1];
  vz = vertex[2];
  px = moment[0];
  py = moment[1];
  pz = moment[2];

}

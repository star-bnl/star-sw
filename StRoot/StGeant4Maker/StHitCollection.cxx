#include <StHitCollection.h>

#include <TVirtualMC.h>
#include <TMCManager.h>
#include <TGeoManager.h>
#include <TGeoNavigator.h>

#include <StSensitiveDetector.h>
#include <TVirtualMCStack.h>
#include <StMCParticleStack.h>
#include <StMessMgr.h>

#include <StarVMC/StarAgmlLib/AgMLExtension.h>
#include <GeometryUtils.h>

#include <cassert>

//____________________________________________________________________________________________
ostream&  operator<<(ostream& os,  const TrackerHit& hit) {

  TString mypath("");

  int i=0;
  int volu=hit.volu[i];
  int copy=hit.copy[i];
  while ( volu>0 ) {
    mypath += "/"; mypath += gGeoManager->GetVolume(volu)->GetName(); mypath += "_"; mypath += copy;
    i++;
    volu=hit.volu[i];
    copy=hit.copy[i];
  }

  TString numbv;
  for ( unsigned int iv=0;iv<DetectorHit::maxdepth;iv++ ) {
    int nv = hit.numbv[iv];
    if ( 0==nv ) break;
    numbv += nv; numbv+="    ";
  }

   // Printout hit information
   os << Form( "Tracker Hit [i=%i t=%i v=%i] \t[%s]\n\t[%s]\n\tpos:(%f,%f,%f)(%f,%f,%f)\n\tmom:(%f,%f,%f),(%f,%f,%f) \n\tde=%f nstep=%i",
               hit.id, hit.idtruth,hit.volId,
               hit.path.Data(),
	       numbv.Data(),
	       hit.position_in[0],  
               hit.position_in[1],  
               hit.position_in[2],  
               hit.position_out[0],  
               hit.position_out[1],  
               hit.position_out[2],  
	       hit.momentum_in[0],  
               hit.momentum_in[1],  
               hit.momentum_in[2],  
               hit.momentum_out[0],  
               hit.momentum_out[1],  
               hit.momentum_out[2],  
               hit.de,
               hit.nsteps );
   return os; 
   
}
//____________________________________________________________________________________________
ostream&  operator<<(ostream& os,  const CalorimeterHit& hit) {
   // Printout hit information

  TString mypath("");
  
  int i=0;
  int volu=hit.volu[i];
  int copy=hit.copy[i];

  while ( volu>0 ) {
    
    mypath += "/"; mypath += gGeoManager->GetVolume(volu)->GetName(); mypath += "_"; mypath += copy;

    i++;

    volu=hit.volu[i];
    copy=hit.copy[i];

  }

  TString numbv;
  for ( unsigned int iv=0;iv<DetectorHit::maxdepth;iv++ ) {
    int nv = hit.numbv[iv];
    if ( 0==nv ) break;
    numbv += nv; numbv+="    ";
  }


   os << Form( "Calorimeter Hit [%i %i %i]\n\t[%s]\n\t[%s]\n\tpos:(%f,%f,%f)\n\tde=%f nstep=%i",
               hit.id, hit.idtruth,hit.volId,
               hit.path.Data(),
	       numbv.Data(),
	       hit.position_in[0],  
               hit.position_in[1],  
               hit.position_in[2],  
	       hit.de,
               hit.nsteps );
   return os; 
   
}



//_____________________________________________________________________________________________
StHitCollection::StHitCollection( const char* name, const char* title ) : TNamed(name,title){ }
//_____________________________________________________________________________________________
StTrackerHitCollection::StTrackerHitCollection( const char* name, const char* title ) : StHitCollection(name,title), mHits() { }
//_____________________________________________________________________________________________
StCalorimeterHitCollection::StCalorimeterHitCollection( const char* name, const char* title ) : StHitCollection(name,title), mHits(), mBirk{1.0,0.0130,9.6E-6},mEsum(0) { }
//_____________________________________________________________________________________________


//_____________________________________________________________________________________________
void StTrackerHitCollection::Initialize() {

}
//_____________________________________________________________________________________________
void StTrackerHitCollection::ProcessHits() {

  TGeoNavigator* navigator = gGeoManager->GetCurrentNavigator();
  TVirtualMC*    mc = (TMCManager::Instance()) ? 
    TMCManager::Instance()->GetCurrentEngine() :
    TVirtualMC::GetMC();

  TGeoVolume*    current = navigator->GetCurrentVolume();

  // Is this a charged particle?  If not, skip it...
  if ( 0 == mc->TrackCharge() ) return;

  // Energy deposited in this tracking step
  double Edep = mc->Edep();

  // Position and momentum of the hit 
  double x, y, z, px, py, pz, etot;
  mc->TrackPosition( x, y, z );
  mc->TrackMomentum( px, py, pz, etot ); 

  TVirtualMCStack* stack = (TVirtualMCStack *)mc->GetStack();
  
  // Get list of tracks from particle stack
  StMCParticleStack* userstack = (StMCParticleStack*)mUserStack;
  std::vector<StarMCParticle*>& truthTable    = userstack->GetTruthTable();
  std::vector<StarMCParticle*>& particleTable = userstack->GetParticleTable();

  // This should be the current particle truth 
  StarMCParticle* truth = userstack->GetCurrentPersistentTrack(); 
  if ( 0==truth ) {
    LOG_INFO << "There is no truth.  Keep the hit but do not register it to the truth particle." << endm;
  }

  LOG_DEBUG << "Process hits with track " << truth << " current tn=" << userstack->GetCurrentTrackNumber() << endm;

  bool isNewTrack      = mc->IsNewTrack();
  bool isTrackEntering = mc->IsTrackEntering();
  bool isTrackExiting  = mc->IsTrackExiting();
  bool isTrackInside   = mc->IsTrackInside();
  bool isTrackOut      = mc->IsTrackOut();
  bool isTrackStop     = mc->IsTrackStop();

  TrackerHit* hit = 0;
  
  // Track has entered this volume, create a new hit
  if ( isTrackEntering || isNewTrack ) {

    // Make sure that the level is not too deep
    if ( navigator->GetLevel() > int(DetectorHit::maxdepth) ) {
      LOG_INFO << "Cannot score hits with depth " <<   navigator->GetLevel() << endm;
      return; 
    }

    mHits.push_back( hit = new TrackerHit );
    if (truth) truth->addHit( hit );

    // Get the current path to the sensitive volume
    hit->path = mc->CurrentVolPath(); 

    // Get the current volume / copy numbers to the sensitive volume.  n.b. GetBranchNumbers 
    // only writes to the current level, so if hit is not new or cleared, need to clear by hand.
    gGeoManager->GetBranchNumbers( hit->copy, hit->volu );

    int inumbv = 0;
    AgMLExtension* agmlext = 0;
    for ( int ilvl=0; ilvl<navigator->GetLevel()+1;ilvl++ ) {
      TGeoVolume* volume = gGeoManager->GetVolume( hit->volu[ilvl] );
      agmlext = getExtension( volume ); 
      if ( 0 == agmlext )                  continue; // but probably an error
      if ( agmlext->GetBranchings() <= 1 ) continue; // skip unique volumes (and HALL)
      hit->numbv[ inumbv++ ] = hit->copy[ilvl];
    }

    // Set the volume unique ID
    assert(agmlext);
    hit->volId = agmlext->GetVolumeId( hit->numbv );

    // Assign the hit a unqiue ID (index + 1)
    hit->id = mHits.size();

    // Assign the hit the ID truth of the current track (index + 1)
    //    hit->idtruth = particleTable.size();
    //    hit->idtruth = truthTable.size();
    hit->idtruth = (truth)? userstack->GetIdTruth ( truth ) : -1;



    // Score entrance momentum and 
    mc->TrackMomentum( hit->momentum_in[0], hit->momentum_in[1],  hit->momentum_in[2],  hit->momentum_in[3] ); 
    mc->TrackPosition( hit->position_in[0], hit->position_in[1],  hit->position_in[2] );
    hit->position_in[3] = mc->TrackTime();
 
  } 
  
  if ( mHits.size() == 0 ) {
    LOG_INFO << "No available hits" << endm;
    return; 
  }

  // Update the final step in this hit
  hit = mHits.back();
  
  hit -> nsteps += 1; 

  // For all other tracking states, update the exit momentum and position 
  mc->TrackMomentum( hit->momentum_out[0], hit->momentum_out[1],  hit->momentum_out[2],  hit->momentum_out[3] ); 
  mc->TrackPosition( hit->position_out[0], hit->position_out[1],  hit->position_out[2] );
  hit->position_out[3] = mc->TrackTime();

  // Increment the energy loss and step sums
  hit -> de += mc->Edep();
  hit -> ds += mc->TrackStep();

  // Total length to this point
  hit -> length = mc->TrackLength();

  double mass = mc->TrackMass();
  double Etot = mc->Etot(); // total energy

  double Ekin = Etot - mass;
  if      ( mass <= 0 ) hit->lgam = -999;
  else if ( Ekin <= 0 ) hit->lgam = -998;
  else                  hit->lgam = TMath::Log10( Ekin/mass );
  

  // Grab the agml extension and evaluate user hits
  AgMLExtension* agmlext = getExtension( current ); 
  if ( 0==agmlext ) return;

  // Score user defined hit quantities
  for ( auto score : agmlext->GetUserHits() ) {
    hit->user.push_back( score->hit() );
  }
  
}
//_____________________________________________________________________________________________
void StTrackerHitCollection::EndOfEvent() {
  // Do nothing 
}
//_____________________________________________________________________________________________
void StTrackerHitCollection::Clear(Option_t*) {
  for ( auto h : mHits ) {
    if ( h ) delete h;
  }
  mHits.clear();
}


//_____________________________________________________________________________________________
void StCalorimeterHitCollection::Initialize() {

}
//_____________________________________________________________________________________________
void StCalorimeterHitCollection::ProcessHits() {

  TGeoNavigator* navigator = gGeoManager->GetCurrentNavigator();
  TVirtualMC*    mc = TVirtualMC::GetMC();
  TGeoVolume*    current = navigator->GetCurrentVolume();

  // Is this a charged particle?  If not, skip it...
  if ( 0 == mc->TrackCharge() ) return;

  // Energy deposited in this tracking step
  double Edep = mc->Edep();

  // Correct for Birk's law
  //  Edep = Edep * mBirk[0] / ( 1.0 + mBirk[1]*Edep + mBirk[2]*Edep*Edep );

  // Position and momentum of the hit 
  double x, y, z, px, py, pz, etot;
  mc->TrackPosition( x, y, z );
  mc->TrackMomentum( px, py, pz, etot ); 

  TVirtualMCStack* stack = (TVirtualMCStack *)mc->GetStack();
  
  // Get list of tracks from particle stack
  StMCParticleStack* userstack = (StMCParticleStack*)mUserStack;
  std::vector<StarMCParticle*>& truthTable = userstack->GetTruthTable();
  std::vector<StarMCParticle*>& particleTable = userstack->GetParticleTable();

  // This should be the current particle truth 
  StarMCParticle* truth = userstack->GetCurrentPersistentTrack(); 
  if ( 0==truth ) {
    LOG_INFO << "There is no truth.  Keep the hit but do not register it to the truth particle." << endm;
  }

  LOG_DEBUG << "Process hits with track " << truth << " current tn=" << userstack->GetCurrentTrackNumber() << endm;

  bool isNewTrack      = mc->IsNewTrack();
  bool isTrackEntering = mc->IsTrackEntering();
  bool isTrackExiting  = mc->IsTrackExiting();
  bool isTrackInside   = mc->IsTrackInside();
  bool isTrackOut      = mc->IsTrackOut();
  bool isTrackStop     = mc->IsTrackStop();

  CalorimeterHit* hit = 0;
  
  // Track has entered this volume, create a new hit
  if ( isTrackEntering || isNewTrack ) {
    
    // Zero out the energy sum
    mEsum = 0;

    // Make sure that the level is not too deep
    if ( navigator->GetLevel() > int(DetectorHit::maxdepth) ) {
      LOG_INFO << "Cannot score hits with depth " <<   navigator->GetLevel() << endm;
      return; 
    }

    mHits.push_back( hit = new CalorimeterHit );
    if (truth) truth->addHit( hit );

    // Get the current path to the sensitive volume
    hit->path = mc->CurrentVolPath(); 

    // Get the current volume / copy numbers to the sensitive volume.  n.b. GetBranchNumbers 
    // only writes to the current level, so if hit is not new or cleared, need to clear by hand.
    gGeoManager->GetBranchNumbers( hit->copy, hit->volu );

    // Set reduced volume path
    int inumbv = 0;
    AgMLExtension* agmlext = 0;
    for ( int ilvl=0; ilvl<navigator->GetLevel()+1;ilvl++ ) {
      TGeoVolume* volume = gGeoManager->GetVolume( hit->volu[ilvl] );
      agmlext = getExtension(volume);
      if ( 0 == agmlext )                  continue; // but probably an error
      if ( agmlext->GetBranchings() <= 1 ) continue; // skip unique volumes (and HALL)
      hit->numbv[ inumbv++ ] = hit->copy[ilvl];
    }

    // Set the volume unique ID
    assert(agmlext);
    hit->volId = agmlext->GetVolumeId( hit->numbv );

    // Assign the hit a unqiue ID (index + 1)
    hit->id = 1 + mHits.size();

    // Assign the hit the ID truth of the current track (index + 1)
    //    hit->idtruth = particleTable.size();
    //    hit->idtruth = truthTable.size();
    hit->idtruth = (truth)? userstack->GetIdTruth ( truth ) : -1;

    // Score entrance
    mc->TrackPosition( hit->position_in[0], hit->position_in[1],  hit->position_in[2] );
    hit->position_in[3] = mc->TrackTime();
 
  }
  
  if ( mHits.size() == 0 ) {
    LOG_INFO << "No available hits" << endm;
    return; 
  }

  // Update the final step in this hit
  hit = mHits.back();
  
  hit -> nsteps += 1; 

  // Add current energy deposit to energy sum
  mEsum += Edep;

  // Hit energy will be the total energy deposition corrected by Birk's law
  hit -> de =   mEsum * mBirk[0] / ( 1.0 + mBirk[1]*mEsum + mBirk[2]*mEsum*mEsum );

  // Grab the agml extension and evaluate user hits
  AgMLExtension* agmlext = getExtension( current ); 
  if ( 0==agmlext ) return;

  // Score user defined hit quantities
  for ( auto score : agmlext->GetUserHits() ) {
    hit->user.push_back( score->hit() );
  }


}
//_____________________________________________________________________________________________
void StCalorimeterHitCollection::EndOfEvent() {
  // Aggregate hits in each calorimeter sensitive volume
  int count=0;
  int idtruth=0;
  double demax=-9E9;
  std::map<int, CalorimeterHit*> hitsByVolume;

  for ( auto hit : mHits ) {
    int volumeId = hit->volId;
    auto myhit   = hitsByVolume[volumeId];

    if ( 0==myhit ) {
      myhit = hitsByVolume[volumeId] = new CalorimeterHit();
      myhit->id = ++count;
      myhit->idtruth=hit->idtruth;
      std::copy( hit->volu, hit->volu+DetectorHit::maxdepth, myhit->volu );
      std::copy( hit->copy, hit->copy+DetectorHit::maxdepth, myhit->copy );
      myhit->volId = volumeId;
      myhit->path  = hit->path;      
      myhit->user.resize(hit->user.size());
      std::copy(hit->position_in,hit->position_in+4,myhit->position_in);
      myhit->idtruth=hit->idtruth;      
    }

    if ( hit->idtruth>=0 ) // negative truth is ignored
      myhit->idtruth = TMath::Min( hit->idtruth, myhit->idtruth );

    myhit->nsteps += hit->nsteps;
    myhit->de     += hit->de;

    if ( hit->user.size() == myhit->user.size() ) 
      {
	for ( int i=0;i<myhit->user.size();i++ ) {
	  myhit->user[i]+=hit->user[i];
	}    	
      }
    else 
      {
	LOG_INFO << "Size mismatch in user hit vector, skip adding this hit. " << endm;
      }
  }

  for ( auto h : mHits ) {
    if (h) delete h;
  }
  mHits.clear();

  for ( auto kv : hitsByVolume ) {
    mHits.push_back( kv.second );
  }
  
}
//_____________________________________________________________________________________________

//_____________________________________________________________________________________________
void StCalorimeterHitCollection::Clear(Option_t*) {
  for ( auto h : mHits ) {
    if (h) delete h;
  }
  mHits.clear();
  mEsum=0;
}

#include "StGeant4Maker.h"
//________________________________________________________________________________________________
#include "StMessMgr.h"
//________________________________________________________________________________________________
#include "StarMagField.h"
#include "StMCParticleStack.h"
#include "TSystem.h"
#include "StBFChain.h"
#include "TInterpreter.h"
#include "TGeoManager.h"
#include "StarVMC/StarAgmlLib/StarAgmlStacker.h"
#include "StarGenerator/BASE/StarPrimaryMaker.h"
#include "StarGenerator/BASE/StarParticleStack.h"
#include "StarGenerator/UTIL/StarParticleData.h"
#include "StarGenerator/UTIL/StarRandom.h"

#include "TString.h"
#include "StSensitiveDetector.h"

#include <CLHEP/Random/Random.h>

#include "StarVMC/StarAgmlLib/AgMLExtension.h"
#include "GeometryUtils.h"
#include "TString.h"

#include "StChain/StEvtHddr.h"
#include "TH2F.h"

#include <functional>

//_______________________________________________________________________________________________
#include <AgMLVolumeIdFactory.h>
//_______________________________________________________________________________________________
#include "TMCManager.h"
//________________________________________________________________________________________________
#include "TGeant4.h"
#include "TG4RunManager.h"
#include "TG4RunConfiguration.h"
#include "TGeant3/TGeant3TGeo.h"
//________________________________________________________________________________________________
#include "tables/St_g2t_event_Table.h"
#include "tables/St_g2t_vertex_Table.h"
#include "tables/St_g2t_track_Table.h"
//________________________________________________________________________________________________
#include "g2t/St_g2t_tpc_Module.h"
#include "g2t/St_g2t_hca_Module.h"
#include "g2t/St_g2t_wca_Module.h"
#include "g2t/St_g2t_pre_Module.h"
#include "g2t/St_g2t_fts_Module.h"
#include "g2t/St_g2t_stg_Module.h"
#include "g2t/St_g2t_epd_Module.h"
#include "g2t/St_g2t_tfr_Module.h"
#include "g2t/St_g2t_mtd_Module.h"
#include "g2t/St_g2t_vpd_Module.h"
#include "g2t/St_g2t_fpd_Module.h"
//________________________________________________________________________________________________
#include <StHitCollection.h> 
#include <cstring>
//________________________________________________________________________________________________

// Functors used to copy the hits from the sensitive detector hit collections into the g2t tables.
// There's an explitive-load of boilerplate in these things

struct SD2Table_TPC {
  void operator()( StSensitiveDetector* sd, St_g2t_tpc_hit* table, St_g2t_track* track ) {
    // Retrieve the hit collection 
    StTrackerHitCollection* collection = (StTrackerHitCollection *)sd->hits();
    // Iterate over all hits
    for ( auto hit : collection->hits() ) {
      
      g2t_tpc_hit_st g2t_hit; memset(&g2t_hit,0,sizeof(g2t_tpc_hit_st)); 
      
      g2t_hit.id        = hit->id;

      g2t_hit.track_p   = hit->idtruth;
      g2t_hit.volume_id = hit->volId;
      g2t_hit.de        = hit->de;
      g2t_hit.ds        = hit->ds;
      for ( int i=0; i<3; i++ ) {
	g2t_hit.p[i]  = 0.5 * ( hit->momentum_in[i] + hit->momentum_out[i] );
	g2t_hit.x[i]  = 0.5 * ( hit->position_in[i] + hit->position_out[i] );
      }
      g2t_hit.tof       = 0.5 * ( hit->position_in[3] + hit->position_out[3] ); 
      g2t_hit.length    = hit->length;
      g2t_hit.lgam      = hit->lgam;

      

      /*
	these are used downstream by the slow simulator (and should not be filled here)

	g2t_hit.adc = ...;
	g2t_hit.pad = ...;
	g2t_hit.timebucket = ...;
	g2t_hit.np = ...; // number of primary electrons

      */

      int idtruth = hit->idtruth;
      g2t_track_st* g2t_track = (g2t_track_st*)track->At(idtruth-1);

      g2t_hit.next_tr_hit_p = g2t_track->hit_tpc_p; // store next hit on the linked list
      g2t_track->hit_tpc_p = hit->id;            // this hit becomes the head of the linked list
      
      g2t_track->n_tpc_hit++;     

      // Add hit to the table
      table -> AddAt( &g2t_hit );           

    }
    // TODO: increment hit count on track 
  } 
} sd2table_tpc; 
struct SD2Table_EPD {
  void operator()( StSensitiveDetector* sd, St_g2t_epd_hit* table, St_g2t_track* track ) {
    // Retrieve the hit collection 
    StTrackerHitCollection* collection = (StTrackerHitCollection *)sd->hits();
    // Iterate over all hits
    for ( auto hit : collection->hits() ) {
      
      g2t_epd_hit_st g2t_hit; memset(&g2t_hit,0,sizeof(g2t_epd_hit_st)); 
      
      g2t_hit.id        = hit->id;
      // TODO: add pointer to next hit on the track 
      g2t_hit.track_p   = hit->idtruth;
      g2t_hit.volume_id = hit->volId;
      g2t_hit.de        = hit->de;
      g2t_hit.ds        = hit->ds;
      for ( int i=0; i<3; i++ ) {
	g2t_hit.p[i]  = 0.5 * ( hit->momentum_in[i] + hit->momentum_out[i] );
	g2t_hit.x[i]  = 0.5 * ( hit->position_in[i] + hit->position_out[i] );
      }
      g2t_hit.tof       = 0.5 * ( hit->position_in[3] + hit->position_out[3] ); 
      
      table -> AddAt( &g2t_hit );     

      int idtruth = hit->idtruth;
      g2t_track_st* trk = (g2t_track_st*)track->At(idtruth-1);
      trk->n_epd_hit++;
      
    }

  } 
} sd2table_epd; 
// Copy to sTGC and FST structures
struct SD2Table_STGC {
  void operator()( StSensitiveDetector* sd, St_g2t_fts_hit* table, St_g2t_track* track ) {
    // Retrieve the hit collection 
    StTrackerHitCollection* collection = (StTrackerHitCollection *)sd->hits();
    // Iterate over all hits
    for ( auto hit : collection->hits() ) {
      
      g2t_fts_hit_st g2t_hit; memset(&g2t_hit,0,sizeof(g2t_fts_hit_st)); 
      
      g2t_hit.id        = hit->id;
      g2t_hit.track_p   = hit->idtruth;
      g2t_hit.volume_id = hit->volId;
      g2t_hit.de        = hit->de;
      g2t_hit.ds        = hit->ds;
      for ( int i=0; i<3; i++ ) {
	g2t_hit.p[i]  = 0.5 * ( hit->momentum_in[i] + hit->momentum_out[i] );
	g2t_hit.x[i]  = 0.5 * ( hit->position_in[i] + hit->position_out[i] );
      }
      g2t_hit.tof       = 0.5 * ( hit->position_in[3] + hit->position_out[3] );             

      int idtruth = hit->idtruth;
      g2t_track_st* g2t_track = (g2t_track_st*)track->At(idtruth-1);

      g2t_hit.next_tr_hit_p = g2t_track->hit_stg_p; // store next hit on the linked list
      g2t_track->hit_stg_p = hit->id;            // this hit becomes the head of the linked list
      g2t_track->n_stg_hit++;

      table -> AddAt( &g2t_hit );     
      
    }
  } 
} sd2table_stgc; 
struct SD2Table_FST {
  void operator()( StSensitiveDetector* sd, St_g2t_fts_hit* table, St_g2t_track* track ) {
    // Retrieve the hit collection 
    StTrackerHitCollection* collection = (StTrackerHitCollection *)sd->hits();
    // Iterate over all hits
    for ( auto hit : collection->hits() ) {
      
      g2t_fts_hit_st g2t_hit; memset(&g2t_hit,0,sizeof(g2t_fts_hit_st)); 
      
      g2t_hit.id        = hit->id;
      // TODO: add pointer to next hit on the track 
      g2t_hit.track_p   = hit->idtruth;
      g2t_hit.volume_id = hit->volId;
      g2t_hit.de        = hit->de;
      g2t_hit.ds        = hit->ds;
      for ( int i=0; i<3; i++ ) {
	g2t_hit.p[i]  = 0.5 * ( hit->momentum_in[i] + hit->momentum_out[i] );
	g2t_hit.x[i]  = 0.5 * ( hit->position_in[i] + hit->position_out[i] );
      }
      g2t_hit.tof       = 0.5 * ( hit->position_in[3] + hit->position_out[3] ); 
     
      int idtruth = hit->idtruth;
      g2t_track_st* g2t_track = (g2t_track_st*)track->At(idtruth-1);

      g2t_hit.next_tr_hit_p = g2t_track->hit_fts_p; // store next hit on the linked list
      g2t_track->hit_fts_p = hit->id;            // this hit becomes the head of the linked list
      g2t_track->n_fts_hit++;

      table -> AddAt( &g2t_hit );     

    }
  } 
} sd2table_fst; 

// Generic EMC copy (no increment on track hits)
struct SD2Table_EMC {
  void operator()( StSensitiveDetector* sd, St_g2t_emc_hit* table, St_g2t_track* track ) {
    
    TString sdname = sd->GetName();

    // Retrieve the hit collection 
    StCalorimeterHitCollection* collection = (StCalorimeterHitCollection *)sd->hits();
    // Iterate over all hits
    for ( auto hit : collection->hits() ) {

      g2t_emc_hit_st g2t_hit; memset(&g2t_hit,0,sizeof(g2t_emc_hit_st)); 
      
      g2t_hit.id        = hit->id;
      g2t_hit.track_p   = hit->idtruth;
      g2t_hit.volume_id = hit->volId;
      g2t_hit.de        = hit->de;
      g2t_hit.x         = hit->position_in[0];
      g2t_hit.y         = hit->position_in[1];
      g2t_hit.z         = hit->position_in[2];
      
      table -> AddAt( &g2t_hit );     

      int idtruth = hit->idtruth;
      g2t_track_st* trk = (g2t_track_st*)track->At(idtruth-1);
      
      if      ( sdname == "CSCI" ) 
	trk->n_emc_hit++;
      else if ( sdname == "ESCI" )
	trk->n_eem_hit++;
      else if ( sdname == "PSCI" )
	trk->n_pre_hit++;
      else if ( sdname == "WSCI" )
	trk->n_wca_hit++;
      else if ( sdname == "HSCI" )
	trk->n_hca_hit++;
    }
  } 
} sd2table_emc; 

struct SD2Table_HCA {
  void operator()( StSensitiveDetector* sd, St_g2t_hca_hit* table, St_g2t_track* track ) {
    
    TString sdname = sd->GetName();

    // Retrieve the hit collection 
    StCalorimeterHitCollection* collection = (StCalorimeterHitCollection *)sd->hits();
    // Iterate over all hits
    for ( auto hit : collection->hits() ) {

      g2t_hca_hit_st g2t_hit; memset(&g2t_hit,0,sizeof(g2t_hca_hit_st)); 
      
      g2t_hit.id        = hit->id;
      // TODO: add pointer to next hit on the track 
      g2t_hit.track_p   = hit->idtruth;
      g2t_hit.volume_id = hit->volId;
      g2t_hit.de        = hit->de;
      if ( hit->user.size()>=1 ) g2t_hit.deA       = hit->user[0];   else g2t_hit.deA = -1;
      if ( hit->user.size()>=2 ) g2t_hit.deB       = hit->user[1];   else g2t_hit.deB = -2;
      if ( hit->user.size()>=3 ) g2t_hit.deC       = hit->user[2];   else g2t_hit.deC = -3;
      if ( hit->user.size()>=4 ) g2t_hit.deD       = hit->user[3];   else g2t_hit.deD = -4;
      g2t_hit.x         = hit->position_in[0];
      g2t_hit.y         = hit->position_in[1];
      g2t_hit.z         = hit->position_in[2];
      
      table -> AddAt( &g2t_hit );     

      int idtruth = hit->idtruth;
      g2t_track_st* trk = (g2t_track_st*)track->At(idtruth-1);

      trk->n_hca_hit++;

    }
  } 
} sd2table_hca; 


struct SD2Table_CTF {
  void operator()( StSensitiveDetector* sd, St_g2t_ctf_hit* table, St_g2t_track* track ) {
    
    TString sdname = sd->GetName();

    // Retrieve the hit collection 
    StTrackerHitCollection* collection = (StTrackerHitCollection *)sd->hits();
    // Iterate over all hits
    for ( auto hit : collection->hits() ) {

      g2t_ctf_hit_st g2t_hit; memset(&g2t_hit,0,sizeof(g2t_ctf_hit_st)); 
      
      g2t_hit.id        = hit->id;
      // TODO: add pointer to next hit on the track 
      g2t_hit.track_p   = hit->idtruth;
      g2t_hit.volume_id = hit->volId;
      g2t_hit.de        = hit->de;
      g2t_hit.ds        = hit->ds;

      g2t_hit.x[0]      = (hit->position_in[0] + hit->position_out[0]) * 0.5;
      g2t_hit.x[1]      = (hit->position_in[1] + hit->position_out[1]) * 0.5;
      g2t_hit.x[2]      = (hit->position_in[2] + hit->position_out[2]) * 0.5;
      g2t_hit.tof       = (hit->position_in[3] + hit->position_out[3]) * 0.5;
      g2t_hit.p[0]      = (hit->momentum_in[0] + hit->momentum_out[0]) * 0.5;
      g2t_hit.p[1]      = (hit->momentum_in[1] + hit->momentum_out[1]) * 0.5;
      g2t_hit.p[2]      = (hit->momentum_in[2] + hit->momentum_out[2]) * 0.5;
      g2t_hit.s_track   = hit->length;
      
      table -> AddAt( &g2t_hit );     

      int idtruth = hit->idtruth;
      g2t_track_st* trk = (g2t_track_st*)track->At(idtruth-1);

      trk->n_tof_hit++;
      
    }
  } 
} sd2table_ctf; 
struct SD2Table_VPD {
  void operator()( StSensitiveDetector* sd, St_g2t_vpd_hit* table, St_g2t_track* track ) {
    
    TString sdname = sd->GetName();

    // Retrieve the hit collection 
    StTrackerHitCollection* collection = (StTrackerHitCollection *)sd->hits();
    // Iterate over all hits
    for ( auto hit : collection->hits() ) {

      g2t_vpd_hit_st g2t_hit; memset(&g2t_hit,0,sizeof(g2t_vpd_hit_st)); 
      
      g2t_hit.id        = hit->id;
      // TODO: add pointer to next hit on the track 
      g2t_hit.track_p   = hit->idtruth;
      g2t_hit.volume_id = hit->volId;
      g2t_hit.de        = hit->de;
      g2t_hit.ds        = hit->ds;

      g2t_hit.x[0]      = (hit->position_in[0] + hit->position_out[0]) * 0.5;
      g2t_hit.x[1]      = (hit->position_in[1] + hit->position_out[1]) * 0.5;
      g2t_hit.x[2]      = (hit->position_in[2] + hit->position_out[2]) * 0.5;
      g2t_hit.tof       = (hit->position_in[3] + hit->position_out[3]) * 0.5;
      g2t_hit.p[0]      = (hit->momentum_in[0] + hit->momentum_out[0]) * 0.5;
      g2t_hit.p[1]      = (hit->momentum_in[1] + hit->momentum_out[1]) * 0.5;
      g2t_hit.p[2]      = (hit->momentum_in[2] + hit->momentum_out[2]) * 0.5;
      g2t_hit.s_track   = hit->length;
      
      table -> AddAt( &g2t_hit );     

      int idtruth = hit->idtruth;
      g2t_track_st* trk = (g2t_track_st*)track->At(idtruth-1);

      trk->n_vpd_hit++;
      
    }
  } 
} sd2table_vpd; 
struct SD2Table_MTD {
  void operator()( StSensitiveDetector* sd, St_g2t_mtd_hit* table, St_g2t_track* track ) {
    
    TString sdname = sd->GetName();

    // Retrieve the hit collection 
    StTrackerHitCollection* collection = (StTrackerHitCollection *)sd->hits();
    // Iterate over all hits
    for ( auto hit : collection->hits() ) {

      g2t_mtd_hit_st g2t_hit; memset(&g2t_hit,0,sizeof(g2t_mtd_hit_st)); 
      
      g2t_hit.id        = hit->id;
      // TODO: add pointer to next hit on the track 
      g2t_hit.track_p   = hit->idtruth;
      g2t_hit.volume_id = hit->volId;
      g2t_hit.de        = hit->de;
      g2t_hit.ds        = hit->ds;

      g2t_hit.xglobal[0]      = (hit->position_in[0] + hit->position_out[0]) * 0.5;
      g2t_hit.xglobal[1]      = (hit->position_in[1] + hit->position_out[1]) * 0.5;
      g2t_hit.xglobal[2]      = (hit->position_in[2] + hit->position_out[2]) * 0.5;

      g2t_hit.tof       = (hit->position_in[3] + hit->position_out[3]) * 0.5;
      g2t_hit.p[0]      = (hit->momentum_in[0] + hit->momentum_out[0]) * 0.5;
      g2t_hit.p[1]      = (hit->momentum_in[1] + hit->momentum_out[1]) * 0.5;
      g2t_hit.p[2]      = (hit->momentum_in[2] + hit->momentum_out[2]) * 0.5;
      g2t_hit.s_track   = hit->length;
      
      table -> AddAt( &g2t_hit );     

      int idtruth = hit->idtruth;
      g2t_track_st* trk = (g2t_track_st*)track->At(idtruth-1);

      trk->n_mtd_hit++;
      
    }
  } 
} sd2table_mtd; 

//________________________________________________________________________________________________
TGeant4* gG4 = 0;
TGeant3TGeo* gG3 = 0;

// Function to process one event
std::function<void(void)> trigger;

//________________________________________________________________________________________________
// Pointer to the maker so we can forward VMC calls there
static StGeant4Maker* _g4maker = 0;
//________________________________________________________________________________________________
StarParticleData &particleData = StarParticleData::instance();
//________________________________________________________________________________________________
StarVMCApplication::StarVMCApplication( const Char_t *name, const Char_t *title, double zmax, double rmax, std::string multi, StMCParticleStack* stack ) : 
  TVirtualMCApplication(name,title),mZmax(zmax),mRmax(rmax),mMulti( multi=="multi" )
{

  mMCStack = stack;

  if ( mMulti && stack ) {

    LOG_INFO << "VMC Application Initialized for Multi-engine Run" << endm;

    RequestMCManager();
    fMCManager->SetUserStack(stack);

  }

}
//________________________________________________________________________________________________
void StGeant4Maker::PrintOptions( const char* opts ) {
  if ( 0==opts ) {
    
    std::cout << std::endl << std::endl;
    for ( auto o : mCmdOptions ) {

      std::cout << o.name 
		<< " value=" << SAttr(o.name.c_str()) << std::endl
		<< "   " << o.help << std::endl;


    }
    std::cout << std::endl << std::endl;

  }
}
//________________________________________________________________________________________________
StGeant4Maker::StGeant4Maker( const char* nm ) : 
  StMaker(nm),
  mCmdOptions(),
  mVmcApplication ( NULL ),
  mGeomPath       ( "./StarDb/AgMLGeometry:$STAR/StarDb/AgMLGeometry" ),
  mStarField      ( NULL ),
  mMCStack        ( new StMCParticleStack( "MCstack" ) ),
  mMagfield       ( NULL ),
  mRunConfig      ( NULL ),
  mCurrentNode    (0),
  mPreviousNode   (0),
  mCurrentVolume  (0),
  mPreviousVolume (0),
  mCurrentTrackingRegion(2),
  mPreviousTrackingRegion(2),
  acurr(0),aprev(0),
  mEventHeader(0),
  mDefaultEngine(0)
{ 


  LOG_INFO << "Geant4Maker configuration" << endm;
  LOG_INFO << "Each option can be overridden using --option=value" << endm;


  // Setup default attributes
  //  SetAttr( "G4Opt:Nav",   "geomRoot" );// Default uses only possibility for us... ROOT geom with ROOT nav.  No VGM to convert to G4 geometry.
  //  Possibilities geomVMCtoGeant4 geomVMCtoRoot geomRoot geomRootToGeant4 geomGeant4
  SetAttr( "G4VmcOpt:Nav",   "geomVMCtoRoot" );
  SetAttr( "G4VmcOpt:Name",  "Geant4"  );
  SetAttr( "G4VmcOpt:Title", "The Geant4 Monte Carlo" );
  AddOption( "G4VmcOpt:Phys",  "FTFP_BERT", "Geant 4 physics list" ); // default physics list
  //  SetAttr( "G4VmcOpt:Process", "stepLimiter+specialCuts" ); // special process
  SetAttr( "G4VmcOpt:Process", "stepLimiter+specialControls+specialCuts+stackPopper" ); // special process
  //  SetAttr( "G4VmcOpt:Process", "stepLimiter+stackPopper" ); // special process

  SetAttr("G3VmcOpt:Name", "GEANT3" );
  SetAttr("G3VmcOpt:nwgeant", 0 );

  AddOption( "AgMLOpt:TopVolume", "HALL", "Name of the top-level volume" );

  AddOption( "Stepping:Punchout:Stop", 1, "Punchout action: 0=no action, 1=track stopped, 2=track stopped and re-injected" );
  AddOption( "Stepping:Punchout:Rmin", 223.49, "Min radius applied to punchout logic" );
  AddOption( "Stepping:Punchout:Zmin", 268.75, "Min Z applied to punchout logic" );
  AddOption( "Random:G4", 12345,       "Sets the Random number seed"); 
  AddOption( "field", -5.0,            "Sets the STAR magnetic field [kG]" );

  AddOption( "Application:Zmax", DBL_MAX, "Universe maximum z" );
  AddOption( "Application:Rmax", DBL_MAX, "Universe maximum radius" );

  SetAttr("Scoring:Transit",0);
  AddOption("Scoring:Rmax",450.0,  "Maxium secondary production radius to enter truth tables" );
  AddOption("Scoring:Zmax",2000.0, "Maximum secondary production z to enter truth tables");
  AddOption("Scoring:Emin",0.01,   "Minimum secondary energy to enter truth tables");

  AddOption("vertex:x",0.0, "Primary vertex x [cm]");
  AddOption("vertex:y",0.0, "Primary vertex y [cm]");
  AddOption("vertex:z",0.0, "Primary vertex z [cm]");
  AddOption("vertex:sigmax",0.0, "Primary vertex width [cm]");
  AddOption("vertex:sigmay",0.0, "Primary vertex width [cm]");
  AddOption("vertex:sigmaz",0.0, "Primary vertex width [cm]");


  AddOption( "runnumber", 1, "Run number" );


  // Setup default cuts
  AddOption("CUTGAM", 0.001, "Minimum photon energy for track propagation");
  AddOption("CUTELE", 0.001, "Minimum electron energy for track propagation" );
  AddOption("CUTHAD", 0.01,  "Minimum charged hadron energy for track propagation");
  AddOption("CUTNEU", 0.01,  "Minimum neutral hadron energy for track propagation");
  AddOption("CUTMUO", 0.01,  "Minimum muon energy for track propagation");
  AddOption("BCUTE",  /*     R 'Cut for electron brems.'     D=*/     0.0001, "Cut for electron brem" );
  AddOption("BCUTM",  /*     R 'Cut for muon brems.'         D=-1.*/  0.0001, "Cut for muon brem" );
  AddOption("DCUTE",  /*     R 'Cut for electron delta-rays' D=*/     0.0001, "Cut for electron delta rays" );
  AddOption("DCUTM",  /*     R 'Cut for muon delta-rays'     D=-1.*/  0.0001, "Cut for muon delta rays" );
  AddOption("PPCUTM", /*     R 'Cut for e+e- pairs by muons' D=0.01*/ 0.01,   "Cut for e+e- production by muons" );
  AddOption("TOFMAX", /*     R 'Time of flight cut'          D=*/     1.E+10, "Maximum time-of-flight for tracks" );

  // Setup default physics
  AddOption("PAIR", 1, "Enable/disable pair production");
  AddOption("COMP", 1, "Enable/disable compton scattering" );
  AddOption("PHOT", 1, "Enable/diable photoelectric effect" );
  AddOption("PFIS", 1, "Enable/disable photo fission" );
  AddOption("DRAY", 1, "Enable/disable delta rays");
  AddOption("ANNI", 1, "Enable/disable positron annihilation");
  AddOption("BREM", 1, "Enable/disable bremstrahlung");
  AddOption("HADR", 1, "Enable/disable hadronic interactions");
  AddOption("MUNU", 1, "Enable/disable muon-nucleus interactions");
  AddOption("DCAY", 1, "Enable/disable decays");
  AddOption("LOSS", 2, "Energy loss mode" );
  AddOption("MULS", 1, "Multiple scattering");
  AddOption("CKOV", 1, "Cherenkov radiation");
  AddOption("RAYL", 1, "Rayleigh scattering");
  AddOption("LABS", 1, "Absorption of ckov photons");
  AddOption("SYNC", 1, "Synchrotron radiation");

  // Application defaults to single engine mode with Geant4
  AddOption("application:engine","multi","Application mode: G3=GEANT3, G4=Geant4, multi=mixed G3/G4 mode with defaults below"); 

  AddOption("all:engine",  "G3", "In multi-engine mode, selects the default engine for all subsystems" ); // default engine in multi-engine mode is G3
  //AddOption("NAME:engine", "XX", "Specifies the physics engine (XX=G3 or XX=G4) for all volumes defined in NAMEGeo");
  AddOption("wcal:engine", "G4", "Default engine for all volumes defined in WcalGeo" ); // Forward EMC defaults to G4
  AddOption("hcal:engine", "G4", "Default engine for all volumes defined in HcalGeo" ); // Forward hcal defaults to G4


    
  // Naughty
  _g4maker = this; // Provide a global pointer to the G4 maker  

  PrintOptions();

}
//________________________________________________________________________________________________
int StGeant4Maker::Init() {

  InitGeom();

  mVmcApplication = new StarVMCApplication("g4star","STAR G4/VMC",DAttr("Application:Zmax"),DAttr("Application:Rmax"), SAttr("application:engine"), mMCStack );

  const bool specialStacking = false;
  const bool multiThreaded   = false;
  mRunConfig = new TG4RunConfiguration( SAttr("G4VmcOpt:Nav"), SAttr("G4VmcOpt:Phys" ), SAttr("G4VmcOpt:Process"), specialStacking, multiThreaded );

  AddObj( mVmcApplication, ".const", 0 ); // Register VMC application  

  if ( 0==std::strcmp( SAttr("application:engine"), "G3") || 0==std::strcmp( SAttr("application:engine"), "multi") )  {
    gG3 = new TGeant3TGeo(SAttr("G3VmcOpt:Name"), IAttr("G3VmcOpt:nwgeant" ) ); // ID = 0 in multi engine
    LOG_INFO << "Created GEANT3  instance" << gG3->GetName() << endm;
  }

  if ( 0==std::strcmp( SAttr("application:engine"), "G4") || 0==std::strcmp( SAttr("application:engine"), "multi") )  {
    gG4 = new TGeant4(SAttr("G4VmcOpt:Name"), SAttr("G4VmcOpt:Title") ,mRunConfig); // ID = 1 in multi engine
    LOG_INFO << "Created Geant 4 instance " << gG4->GetName() << endm;
  }

  if ( gG4 ) AddObj( gG4, ".const", 0 );
  if ( gG3 ) AddObj( gG3, ".const", 0 );

  bool multimode = gG3 && gG4;

  if ( multimode ) {
    LOG_INFO << "Application will run both G3 and G4 physics engines, with default as "<< SAttr("all:physics") << endm;
    // Verify that G3 and G4 registered themselves with the manager
    auto* mgr = TMCManager::Instance();
    if ( mgr ) {
      std::vector<TVirtualMC*> engines;
      mgr->GetEngines(engines);
      for ( auto* mc : engines ) {
	mc->Print();
      }
    }
  }

  //
  // Create the function which processes a single event
  //

  trigger = [](){ std::cout << "StGeant4Maker::trigger warning. No trigger function defined.  No events produced." << std::endl; } ;
  if ( gG3 || gG4 ) { 
    if ( 0==std::strcmp( SAttr("application:engine"), "G4" ) ) {
      trigger = []() { gG4->ProcessRun(1); }   ;
    }

    else if ( 0==std::strcmp( SAttr("application:engine"), "G3" ) ) {
      trigger = []() { gG3->ProcessRun(1); }   ;
    }

    else if ( 0==std::strcmp( SAttr("application:engine"), "multi" ) ) {
      trigger = [](){ TMCManager::Instance()->Run(1); }   ;
    }
  }

  mMagfield = new StarMagFieldAdaptor(/*nada*/);  

  auto SetDefaultCuts      = [this](TVirtualMC* mc) {
    mc->SetCut( "CUTGAM", DAttr("cutgam") );
    mc->SetCut( "CUTELE", DAttr("cutele") );
    mc->SetCut( "CUTHAD", DAttr("cuthad") );
    mc->SetCut( "CUTNEU", DAttr("cutneu") );
    mc->SetCut( "CUTMUO", DAttr("cutmuo") );
    mc->SetCut( "BCUTE" , DAttr("bcute") );
    mc->SetCut( "DCUTE" , DAttr("dcute") );
    mc->SetCut( "BCUTM" , DAttr("bcutm") );
    mc->SetCut( "DCUTM" , DAttr("dcutm") );
  };  
  auto SetDefaultProcesses = [this](TVirtualMC* mc) {
    mc->SetProcess("PAIR",   IAttr("PAIR"));
    mc->SetProcess("COMP",   IAttr("COMP"));
    mc->SetProcess("PHOT",   IAttr("PHOT"));
    mc->SetProcess("PFIS",   IAttr("PFIS"));
    mc->SetProcess("DRAY",   IAttr("DRAY"));
    mc->SetProcess("ANNI",   IAttr("ANNI"));
    mc->SetProcess("BREM",   IAttr("BREM"));
    mc->SetProcess("HADR",   IAttr("HADR"));
    mc->SetProcess("MUNU",   IAttr("MUNU"));
    mc->SetProcess("DCAY",   IAttr("DCAY"));
    mc->SetProcess("LOSS",   IAttr("LOSS"));
    mc->SetProcess("MULS",   IAttr("MULS"));
    mc->SetProcess("CKOV",   IAttr("CKOV"));
    mc->SetProcess("RAYL",   IAttr("RAYL"));
    mc->SetProcess("LABS",   IAttr("LABS"));
    mc->SetProcess("SYNC",   IAttr("SYNC"));
  };
  auto SetFieldAndGeometry = [this](TVirtualMC* mc) {
    mc->SetMagField( mMagfield );
    mc->SetRootGeometry();
  };
  auto SetStack            = [this,multimode](TVirtualMC* mc) { 
    if ( 0==multimode ) mc->SetStack(mMCStack); 
  };
  auto InitializeMC        = [SetDefaultCuts,SetDefaultProcesses,SetFieldAndGeometry,SetStack](TVirtualMC* mc) {
    SetDefaultCuts(mc);
    SetDefaultProcesses(mc);
    SetFieldAndGeometry(mc);
    SetStack(mc);
    mc->Init();
    mc->BuildPhysics();
  };

  // Multi-engine initialization
  if ( 0==std::strcmp( SAttr("application:engine"), "multi") ) {

    LOG_INFO << "Initialize Geant 4 + GEANT3 multiengine run" << endm;    
    TMCManager::Instance()->Init( InitializeMC );

  }
  
  // Geant4 standalone initialization
  if ( 0==std::strcmp( SAttr("application:engine"), "G4") ) {  
  
    LOG_INFO << "Initialize Geant 4 standalone" << endm;

    InitializeMC( gG4 );
    
    TG4RunManager* runManager = TG4RunManager::Instance();
    runManager->UseRootRandom(false);

  }

  // GEANT3 standalone initialization
  if ( 0==std::strcmp( SAttr("application:engine"), "G3") ) {  

    LOG_INFO << "Initialize GEANT3 standalone" << endm;
    
    InitializeMC( gG3 );
 
  }

  return StMaker::Init();
}
//________________________________________________________________________________________________
int StGeant4Maker::InitRun( int /* run */ ){

  auto result = kStOK;

  // Get magnetic field scale
  double field = DAttr("field"); /* kG */ 

  if ( 0 == StarMagField::Instance() ) new StarMagField( StarMagField::kMapped, field / 5.0 );

  if ( 0.0 == field ) { // TODO: Not sure about the logic here... 
    // field = St_MagFactorC::instance()->ScaleFactor();
    // if ( TMath::Abs(field)<1E-3 ) field = 1E-3;  
    StarMagField::Instance()->SetFactor(field);
  }

  // Obtain a pointer to the event header
  mEventHeader = (StEvtHddr*) ( GetTopChain()->GetDataSet("EvtHddr") );

  // If it does not exist, create and register
  if ( 0 == mEventHeader ) {
    mEventHeader = new StEvtHddr(GetConst());                                                                                                                                                                
    mEventHeader->SetRunNumber(0);                
    SetOutput(mEventHeader);                      // Declare this event header for output
  }

  // Obtain pointer to the primary maker
  StarPrimaryMaker* primarymk   = dynamic_cast<StarPrimaryMaker*> (GetMaker("PrimaryMaker"));
  if (primarymk) { 
    primarymk->SetVertex( DAttr("vertex:x"), DAttr("vertex:y"), DAttr("vertex:z") );
    primarymk->SetSigma ( DAttr("vertex:sigmax"), DAttr("vertex:sigmay"), DAttr("vertex:sigmaz") );
  }
  else {
    LOG_FATAL << "Primary event generator not registered" << endm;
    result = kStFatal;
  }
  
  return result;
}
//________________________________________________________________________________________________
void StarVMCApplication::ConstructGeometry(){ 
  // Geometry construction is the responsability of the framework and should
  // have already occurred.  If the geometry is missing, it should be fatal.
  if ( 0==gGeoManager ) {
    LOG_FATAL << "Geometry manager is not available at StarVMCApplication::ConstructGeometry... this will not go well" << endm;
  }
  gGeoManager->CloseGeometry(); 
}
//________________________________________________________________________________________________
int  StGeant4Maker::InitGeom() {

  const DbAlias_t *DbAlias = GetDbAliases();
  if ( 0==gGeoManager ) {
  for (int i = 0; DbAlias[i].tag; i++) // iterate over DB aliases
    {
      StBFChain *bfc = (StBFChain *)(GetTopChain());
      if ( 0==bfc ) break; // nothing to do in this case...

      //
      // Look for BFC option of form y2019x or ry2019x
      //
      TString  rtag = "r"; rtag += DbAlias[i].tag;
      TString   tag =              DbAlias[i].tag;
           if ( 0==bfc->GetOption(rtag.Data()) && 0==bfc->GetOption(tag.Data()) ) continue;
      //
      // Get the geometry generation macro
      //
      TString   geo = "Geometry."; geo += DbAlias[i].tag; geo += ".C";
      Char_t   *mac = gSystem->Which( mGeomPath.Data(), geo, kReadPermission );
      if ( 0==mac ) continue;
      //
      // Load and execute
      //
      LOG_INFO << "Load geometry file " << mac << endm;
      gInterpreter->ProcessLine( Form( ".L %s", mac ) );
      gInterpreter->ProcessLine( "CreateTable();" );
      gInterpreter->ProcessLine( Form( ".U %s", mac ) );
      //
      // Cleanup file
      // 
      if (mac) delete [] mac;
    }
  }

  assert(gGeoManager);

  LOG_INFO << "Geometry constructed." << endm;
  TGeoVolume* top = gGeoManager->FindVolumeFast( SAttr("AgMLOpt:TopVolume") );
  gGeoManager->SetTopVolume( top );

  return kStOK;
}
//________________________________________________________________________________________________
double StarVMCApplication::TrackingRmax() const {
  return mRmax;
}
double StarVMCApplication::TrackingZmax() const {
  return mZmax;
}
//________________________________________________________________________________________________
int StGeant4Maker::InitHits() {
  return kStOK;
}
//________________________________________________________________________________________________
struct A { };
struct B { };
int StGeant4Maker::Make() {

  static int eventNumber  = 1;
  const  int runnumber   = IAttr("runnumber");
  
  // One time initialization
  if ( 1 == eventNumber ) {

    mMCStack->SetScoring( DAttr("Scoring:Rmax"), DAttr("Scoring:Zmax"), DAttr("Scoring:Emin") );

  }

  trigger();

  // Update event header.  Note that event header's SetRunNumber method sets the run number AND updates the previous run number.

  if ( runnumber != mEventHeader->GetRunNumber() ) mEventHeader -> SetRunNumber( runnumber );
  mEventHeader -> SetEventNumber( eventNumber );
  mEventHeader -> SetProdDateTime();

  // SetDateTime();
 
  // Increment event number
  eventNumber++;

  // Dump the  stack at the end of make
  //  mMCStack -> StackDump(); 

  return kStOK; 
}
//________________________________________________________________________________________________
void StGeant4Maker::Clear( const Option_t* opts ){

  mMCStack -> Clear(); // Clear the MC stack
  acurr = aprev = 0;   // zero out pointers to the current and previous agml extensions


  StMaker::Clear();
}
//________________________________________________________________________________________________
void StarVMCApplication::InitGeometry(){ 

  _g4maker -> ConfigureGeometry(); 

}
//________________________________________________________________________________________________
void StarVMCApplication::ConstructSensitiveDetectors() {

  //
  // This routine registers sensitive detectors to volumes.  Additionally,
  // in multi-engine mode, this routine is responsible for associating
  // a physics engine to each volume in the geometry.
  //
  // NOTE: physics engines are assigned at the granularity of geometry 
  // modules.  Users have the ability to specify the physics engine for
  // a given module (and all volumes defined within) by using the syntax
  // 
  // --syst:engine=G3 or --syst:engine=G4
  //
  // where "syst" denotes the first four letters of the name of the geometry
  // module.
  //

  assert(gGeoManager);

  //
  // Returns the engine enum (kGeant3 or kGeant4) from the command line option
  // of the form NAME:engine=G3 or NAME:engine=G4
  //
  // NAME = all signifies the default engine
  // NAME = modu is the first four letters (lower cased) of the module name.  All
  //        volumes within that module will be assigned to the engine.
  //
  auto engineFromOption = []( const std::string _option ) -> int {
    std::string option = _option;
    int result;
    // Get the default from the maker
    std::string v;
    const std::string default_ = _g4maker->SAttr("all:engine");

    if ( option == "" ) v = default_;
    else                v = _g4maker->SAttr(option.c_str());
    if ( v == "" )      v = default_;

    // Returns either geant3 or geant4
    if      ( v == "G3" ) result = AgMLExtension::Geant3;
    else if ( v == "G4" ) result = AgMLExtension::Geant4;        
    else { assert(0); } // should never fall through
    assert( result==0 || result==1 );
    return result;
  };
  auto engineFromModule = [engineFromOption]( const std::string _m ) -> int {
    // grab the first four characters of the module
    std::string m4 = _m.substr(0,4); 
    // is there an attribute set
    std::string option = m4 + ":engine";
    return engineFromOption( option );
  };

  _g4maker->SetDefaultEngine( engineFromOption("all:engine") );

  // First collect all AgML extensions with sensitive volumes
  // by the family name of the volume
  std::map<TString, StSensitiveDetector*> sdmap;

  // Get list of volumes
  TObjArray *volumes = gGeoManager->GetListOfVolumes();

  // Get list of extensions mapped to volume 
  for ( int i=0; i<volumes->GetEntries(); i++ ) {

    TGeoVolume* volume = (TGeoVolume *)volumes->At(i);
    AgMLExtension* ae = getExtension(volume);

    if ( 0==ae ) {
      LOG_INFO << "No agml extension on volume = " << volume->GetName() << endm;
      continue; // shouldn't happen
    }

    // Name of the volume
    TString vname=volume->GetName();
    TString fname=ae->GetFamilyName();
    TString mname=ae->GetModuleName();      

    ae->SetEngine( engineFromModule( mname.Data() ) );

    //    if ( 0==ae->GetSensitive() ) {
    //      LOG_INFO << "Not sensitive = " << volume->GetName() << endm;
    //      continue; 
    //    }

    AgMLVolumeId* identifier = AgMLVolumeIdFactory::Create( fname );
    if ( identifier ) {
      ae -> SetVolumeIdentifier( identifier );
    }

    //
    // Get the sensitive detector.  If we don't have one registered
    // to this family, create one and register it
    //
    StSensitiveDetector* sd = sdmap[fname];
    if ( 0==sd ) {
      // add sensitive detector to local map
      sd = sdmap[fname] = new StSensitiveDetector( fname, mname );
      sd->SetUserStack( mMCStack );
    }

    // Register this volume to the sensitive detector

    auto* mgr = TMCManager::Instance();
    if ( 0 == mgr ) {

      auto* mc =  TVirtualMC::GetMC();  // Question: Do we need to obtain pointer through TMCManager here?
      if ( nullptr == mc->GetSensitiveDetector( vname ) ) {
	mc->SetSensitiveDetector( vname, sd );
      }

    }
    else { // multi engine mode

      mgr->Apply( [sd,vname]( TVirtualMC* _mc ) {
	  _mc->SetSensitiveDetector( vname, sd );
	});

    }
    
    // Register this volume with the sensitive detector
    sd->addVolume( volume );

  }

}
//________________________________________________________________________________________________
void StGeant4Maker::SetEngineForModule( const char* module_, const int engine ) {

  LOG_INFO << "Remapping all volumes in module " << module_ << " to MC engine " << engine << endm;

  // Get list of volumes
  TObjArray *volumes = gGeoManager->GetListOfVolumes();

  // Get list of extensions mapped to volume 
  for ( int i=0; i<volumes->GetEntries(); i++ ) {

    TGeoVolume* volume = (TGeoVolume *)volumes->At(i);
    AgMLExtension* ae = getExtension(volume);
    if ( 0==ae ) {
      LOG_INFO << "No agml extension on volume = " << volume->GetName() << endm;
      continue; // shouldn't happen
    }

    // Name of the volume
    TString vname=volume->GetName();
    TString fname=ae->GetFamilyName();
    TString mname=ae->GetModuleName();      
    TString mname_ = module_;

    if ( mname == mname_ ) {

      ae->SetEngine( engine );

    }
  
  }

}
//________________________________________________________________________________________________
int  StGeant4Maker::ConfigureGeometry() {

  auto* mgr = TMCManager::Instance();

  // Iterate overall volumes and set volume specific tracking cuts
  std::map<int, int> media;
  TObjArray* objArray = gGeoManager->GetListOfVolumes();
  for ( int i=0; i<objArray->GetEntries();i++ ) {
    TGeoVolume* volume = (TGeoVolume *)objArray->At(i);
    if ( 0==volume ) continue;
    TGeoMedium* medium = volume->GetMedium();
    int id = medium->GetId();
    if ( media[id]>0 ) continue; // skip if medium already encountered
    AgMLExtension* agmlExt = getExtension(volume);
    if ( 0==agmlExt ) continue;
    media[id] = id;
    for ( auto kv : agmlExt->GetCuts() ) {
      if ( 0==mgr ) {
	TVirtualMC::GetMC()->Gstpar( id, kv.first, kv.second );
      }
      else {
	mgr->Apply( [id,kv]( TVirtualMC* mc ) {
	    mc->Gstpar( id, kv.first, kv.second );
	  });
      }
    }
  }


  return kStOK;
}
//________________________________________________________________________________________________
void StarVMCApplication::BeginEvent(){ _g4maker->BeginEvent(); }
void StGeant4Maker::BeginEvent(){


}
//________________________________________________________________________________________________
void StarVMCApplication::FinishEvent(){ _g4maker -> FinishEvent(); }
void StGeant4Maker::FinishEvent(){

  // Event information is (for the time being) zeroed out
  St_g2t_event*  g2t_event  = new St_g2t_event("g2t_event",1);          AddData(g2t_event);
  g2t_event_st event;
  event = {0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0, 0,0};
  g2t_event->AddAt( &event );

  // TODO: handle multi-engine 
  StMCParticleStack* stack    = mMCStack; // (StMCParticleStack *)TVirtualMC::GetMC()->GetStack();
  auto&              vertex   = stack->GetVertexTable();
  auto&              particle = stack->GetParticleTable();
  unsigned int nvertex = vertex.size();
  unsigned int ntrack  = particle.size();

  St_g2t_vertex* g2t_vertex = new St_g2t_vertex("g2t_vertex",nvertex);  AddData(g2t_vertex);
  St_g2t_track*  g2t_track  = new St_g2t_track ("g2t_track", ntrack);   AddData(g2t_track);

  // Add tracks and vertices to the data structures...

  std::map<const StarMCParticle*,int> truthTrack;
  std::map<const StarMCVertex*,int>  truthVertex;

  // Map vertex and track ID truth to their pointers

  int ivertex = 1; // vertex numbering starts from 1
  for ( auto v : vertex ) {
    truthVertex[v] = ivertex;
    ivertex++;
  }

  int itrack = 1; // track numbering starts from 1
  for ( auto t : particle ) {
    truthTrack[t] = itrack;
    itrack++;
  }

  ivertex = 1;
  for ( auto v : vertex ) {

    // partial fill of vertex table ________________________
    g2t_vertex_st myvertex;   memset(&myvertex, 0, sizeof(g2t_vertex_st));    
    myvertex.id = ivertex;
    std::string vname = v->volume();
    std::copy(vname.begin(), vname.end(), myvertex.ge_volume);
    myvertex.eg_x[0] = myvertex.ge_x[0] = v->vx();
    myvertex.eg_x[1] = myvertex.ge_x[1] = v->vy();
    myvertex.eg_x[2] = myvertex.ge_x[2] = v->vz();
    myvertex.eg_tof  = myvertex.ge_tof  = v->tof();
    myvertex.n_daughter = v->daughters().size();
    if ( v->daughters().size() ) {
      myvertex.daughter_p = truthTrack[ v->daughters()[0] ];
    }
    if ( v->parent() ) {
      myvertex.n_parent = 1; // almost by definition
      myvertex.parent_p = truthTrack[ v->parent() ];
    }
    myvertex.ge_medium = v->medium();
    myvertex.ge_proc   = v->process();
    myvertex.is_itrmd  = v->intermediate();

    // TODO: map ROOT mechanism to G3 names

    // An intermediate vertex with no daughters makes no
    // sense (daughters must have been ranged out) so
    // skip filling
    //    if ( myvertex.is_itrmd && 0==myvertex.n_daughter ) continue;

    
    //__________________________________________ next vertex
    g2t_vertex->AddAt( &myvertex );
    ivertex++;

  }

  itrack = 1; // track numbering starts from 1
  for ( auto t : particle ) {

    auto* pdgdata = particleData.GetParticle( t->GetPdg() );
    
    // partial fill of track table _______________________
    g2t_track_st mytrack;   memset(&mytrack, 0, sizeof(g2t_track_st));    
    mytrack.id       = itrack;
    mytrack.eg_pid   = t->GetPdg();
    if ( pdgdata ) {
      mytrack.ge_pid = pdgdata->TrackingCode();
      mytrack.charge = pdgdata->Charge()/3.0;
    }
    else {
      LOG_WARN << Form("Particle w/ pdgid = %i has no G3 ID (assign 0 to g2t_track::ge_pid)",t->GetPdg()) << endm;
    }
    mytrack.p[0]     = t->px();
    mytrack.p[1]     = t->py();
    mytrack.p[2]     = t->pz();
    mytrack.e        = t->particle()->Energy();
    mytrack.pt       = t->pt(); // NOTE: starsim secondaries have pt = -999
    mytrack.eta      = t->particle()->Eta();
    mytrack.rapidity = t->particle()->Y();
    // index of the start and stop vertices.
    // TODO: particle stop vertices need to be scored
    mytrack.start_vertex_p = truthVertex[ t->start() ];
    mytrack.stop_vertex_p  = truthVertex[ t->stop()  ];
    // next, track parent
    mytrack.next_parent_p = truthTrack[ t->start()->parent() ];    
    //__________________________________________ next track
    g2t_track->AddAt(&mytrack);
    itrack++;
  }
  
  // Copy hits to tables
  AddHits<St_g2t_tpc_hit>( "TPCH", {"TPAD"}, "g2t_tpc_hit", sd2table_tpc  );
  AddHits<St_g2t_emc_hit>( "CALH", {"CSCI"}, "g2t_emc_hit", sd2table_emc  );
  AddHits<St_g2t_emc_hit>( "ECAH", {"ESCI"}, "g2t_eem_hit", sd2table_emc  );

  AddHits<St_g2t_epd_hit>( "EPDH", {"EPDT"}, "g2t_epd_hit", sd2table_epd  );
  AddHits<St_g2t_fts_hit>( "FSTH", {"FTUS"}, "g2t_fsi_hit", sd2table_fst  );
  AddHits<St_g2t_fts_hit>( "STGH", {"STGP","STGL","STGS"}, "g2t_stg_hit", sd2table_stgc );
  AddHits<St_g2t_emc_hit>( "PREH", {"PSCI"}, "g2t_pre_hit", sd2table_emc  );
  AddHits<St_g2t_emc_hit>( "WCAH", {"WSCI"}, "g2t_wca_hit", sd2table_emc  );
  AddHits<St_g2t_hca_hit>( "HCAH", {"HSCI"}, "g2t_hca_hit", sd2table_hca  ); // HCA should have its own copier

  AddHits<St_g2t_emc_hit>( "FPDH", {"FLXF","FLGR","FPSC","FOSC"}, "g2t_fpd_hit", sd2table_emc ); // n.b. does not read out the flxf or flgr hit defined in the geom...

  AddHits<St_g2t_ctf_hit>( "BTOH", {"BRSG"}, "g2t_tfr_hit", sd2table_ctf  );
  AddHits<St_g2t_vpd_hit>( "VPDH", {"VRAD"}, "g2t_vpd_hit", sd2table_vpd  );
  AddHits<St_g2t_mtd_hit>( "MUTH", {"MIGG"}, "g2t_mtd_hit", sd2table_mtd  );

  //  g2t_track->Print(0,10);

}
//________________________________________________________________________________________________
void StarVMCApplication::BeginPrimary(){ _g4maker -> BeginPrimary(); }
void StGeant4Maker::BeginPrimary()
{

  std::vector<StarMCParticle*>& truthTable    = mMCStack->GetTruthTable();
  truthTable.clear();


}
//________________________________________________________________________________________________
void StarVMCApplication::FinishPrimary(){ _g4maker->FinishPrimary(); }
void StGeant4Maker::FinishPrimary()
{

}
//________________________________________________________________________________________________
void StarVMCApplication::PreTrack(){ _g4maker->PreTrack(); }
void StGeant4Maker::PreTrack()
{

  auto* navigator    = gGeoManager->GetCurrentNavigator();

  // Reset the history (tracks always born in full tracking region)
  mPreviousNode = mCurrentNode = 0;
  mPreviousVolume = mCurrentVolume = 0;
  mCurrentTrackingRegion=2;
  mPreviousTrackingRegion=2;

  mCurrentNode    = navigator->GetCurrentNode();
  mCurrentVolume  = navigator->GetCurrentVolume();
}
//________________________________________________________________________________________________
void StarVMCApplication::PostTrack(){ _g4maker->PostTrack(); }
void StGeant4Maker::PostTrack()
{

}
//________________________________________________________________________________________________
void StGeant4Maker::UpdateHistory() {

  static auto* navigator    = gGeoManager->GetCurrentNavigator();
  // TODO: handle multi-engine 
//static auto* mc           = TVirtualMC::GetMC();


  mPreviousNode   = mCurrentNode;
  mPreviousVolume = mCurrentVolume;
  mCurrentNode    = navigator->GetCurrentNode();
  mCurrentVolume  = navigator->GetCurrentVolume();

  // Obtain the agml extensions, giving priority to anything attached to
  // a node.

  aprev = (mPreviousNode ) ? dynamic_cast<AgMLExtension*>( mPreviousNode->GetUserExtension() ) : 0;
  acurr = (mCurrentNode  ) ? dynamic_cast<AgMLExtension*>( mCurrentNode->GetUserExtension() )  : 0;

  if ( 0==aprev ) {
    aprev = (mPreviousVolume) ? getExtension(mPreviousVolume)  : 0;
  }
  if ( 0==acurr ) {
    acurr = (mCurrentVolume) ? getExtension(mCurrentVolume) : 0;
  }

  // Possibly inherit from parent volume
  if ( 0==aprev && mPreviousNode ) {
    aprev = getExtension( mPreviousNode->GetMotherVolume() );
  }
  if ( 0==acurr && mCurrentNode ) {
    acurr = getExtension( mCurrentNode->GetMotherVolume() );
  }

  // If the previous or current extension is null, there is no change in the tracking state.

  if ( aprev ) {     
    mPreviousTrackingRegion = aprev->GetTracking(); 
    // HACK override for CAVE, SCON
    if ( aprev->GetVolumeName() == "CAVE" ) mPreviousTrackingRegion = 2;
  }
  if ( acurr ) { 
    mCurrentTrackingRegion  = acurr->GetTracking(); 
    // HACK override for CAVE
    if ( acurr->GetVolumeName() == "CAVE" ) mCurrentTrackingRegion = 2;
  }

}
//________________________________________________________________________________________________
int StGeant4Maker::regionTransition( int curr, int prev ) {
  TString previous = (mPreviousNode) ? mPreviousNode->GetName() : "";
  int result = 0;

  // TODO: handle multi-engine 
//static auto mc = TVirtualMC::GetMC();

//static double Rmin = DAttr("Stepping:Punchout:Rmin");
//static double Zmin = DAttr("Stepping:Punchout:Zmin");

  // TODO:  This is a hack.  We need to update the geometry and group these three
  //        detectors underneath a single integration volume / region.
  if ( previous == "PMOD" || 
       previous == "WMOD" || 
       previous == "HMOD" ) {
    result = 0;
  }
  else {
    result = curr - prev;
  }

  //     2      2       0     no transition
  //     2      1       1     into tracking from calorimeter
  //     1      2      -1     into calorimeter from tracking
  //     1      1       0     no transition    

  return result;

}
//________________________________________________________________________________________________
void StarVMCApplication::Stepping(){ _g4maker -> Stepping(); }
void StGeant4Maker::Stepping(){                                           

  // At start of user stepping, try to transfer the track between engine
  auto* mgr = TMCManager::Instance();
  auto* mc = TVirtualMC::GetMC(); 

  if ( mgr ) {
    
    mc = mgr->GetCurrentEngine();

    int target  = mDefaultEngine;
    int current = mc->GetId();

    auto* ext = getExtension( mCurrentVolume );
    if ( ext ) {     
      target = ext->GetEngine();      
    }


    if ( current != target ) {
      mgr->TransferTrack(target);
    }

  }

  auto* stack = mMCStack;

  // Get access to the current track
  TParticle* current = stack->GetCurrentTrack(); 

  // Get access to the current MC truth
  StarMCParticle* truth = stack->GetParticleTable().back(); 

  // Update the immediate track history
  UpdateHistory();

  // Check for region transitions
  const int transit = regionTransition( mCurrentTrackingRegion, mPreviousTrackingRegion );

  double vx, vy, vz, tof;
  mc->TrackPosition( vx,vy,vz );
  tof = mc->TrackTime(); // because consistent interface ala TrackMomentum is hard...

  bool stopped = false;

  static double Rmin = DAttr("Stepping:Punchout:Rmin");
  static double Zmin = DAttr("Stepping:Punchout:Zmin");

  // Defines a tracking region which overrides the geometry module region assignment
  auto trackingRegion = [=]()->bool {
       
//  bool result = false;
    
    double x,y,z,r;
    mc->TrackPosition( x, y, z );
    r = TMath::Sqrt( x*x + y*y );
    z = TMath::Abs(z);

    bool tpcFiducial = r < Rmin && z < Zmin;
    bool fwdFiducial = r < 90.0;             // 90cm is poletip donut hole

    bool fcsFiducial = z > 700 && z < 1000 && TMath::Abs(x) < 150 && TMath::Abs(y) < 101.0;


    return tpcFiducial || fwdFiducial || fcsFiducial;

  };

  if ( IAttr("Stepping:verbose") > 0 ) {
    //    truth->Print();
    current->Print();
  }

  // Check if option to stop punchout tracks is enabled
  if ( IAttr("Stepping:Punchout:Stop") && 1==transit && !trackingRegion() ) {
    
    if ( 2==IAttr("Stepping:Punchout:Stop") ) {

      assert(truth);

        // Parent track is the ID known to the particle stack
        int parent = truth->idStack(); 

      assert(current);

        // PDG of the track has not changed
        int pdg = current->GetPdgCode();
        // We will use the current momentum of the particle
        double px, py, pz, e;
        mc->TrackMomentum( px, py, pz, e );
        // ... and its current vertex and TOF from the point where it
        // emerges from the calorimeter

        // this is a user process (and I would dearly love to be able to extend the definitions here...)
        TMCProcess mech = kPUserDefined;
        int ntr;
        
        stack->PushTrack( 1, parent, pdg, px, py, pz, e, vx, vy, vz, tof, 0., 0., 0., mech, ntr, 1.0, 1 ); 

    } 

    mc->StopTrack();
    stopped = true;

    if ( IAttr("Stepping:verbose") > 0 ) { std::cout << "Track is stopped" << std::endl; }
    
  }

  // Score interaction vertices on entrance / exit of a tracking region
  bool transitCheck = (0!=transit)&&IAttr("Scoring:Transit");
  if ( 2==mCurrentTrackingRegion || transitCheck ) {

    int nsec  = mc->NSecondaries();

    // Track has decayed or otherwise been stopped 
    if ( mc->IsTrackDisappeared() || 
	 mc->IsTrackStop()        ||
	 mc->IsTrackOut()         ) {
      
      const StarMCVertex* vertex_ = truth->stop();
      if ( 0==vertex_ ) {
	
	auto* vertex = mMCStack->GetVertex( vx, vy, vz, tof, -1 );
	vertex->setParent( truth );
	vertex->setMedium( mc->CurrentMedium() );
	
//	int pdgid = 0;
	if ( mc->IsTrackDisappeared() ) {
	  
	  if ( nsec )                         vertex->setProcess( mc->ProdProcess(0) );
	  else if ( mc->IsTrackStop() )       vertex->setProcess( kPStop );
	  else if ( mc->IsTrackOut()  )       vertex->setProcess( kPNull );
	  
	}
	else if ( mc->IsTrackStop() )   vertex->setProcess( kPStop );
	else if ( mc->IsTrackOut()  )   vertex->setProcess( kPNull );
	
	truth->setStopVertex( vertex );         
      }
      
    }
    else if ( nsec > 0 ) {
      
      
      TMCProcess proc = mc->ProdProcess(0);
      {
	
	// interaction which throws off secondaries and track contiues...
	auto* vertex = mMCStack->GetVertex(vx,vy,vz,tof,proc);
	
	vertex->setParent( truth );
	vertex->setMedium( mc->CurrentMedium() );
	vertex->setProcess( mc->ProdProcess(0) );
	vertex->setIntermediate(true);
	
	// this is an intermediate vertex on the truth track
	truth->addIntermediateVertex( vertex );
	
      }
      
    }

  }

  if ( stopped || IAttr("Stepping:verbose") ) {
    LOG_DEBUG << Form("track stopped x=%f y=%f z=%f ds=%f transit=%d %d stopped=%s  %s",
		     vx,vy,vz,mc->TrackStep(), mCurrentTrackingRegion, mPreviousTrackingRegion, (stopped)?"T":"F", mc->CurrentVolPath() ) << endm;
  }

  // Perform any post stepping actions
  for ( auto f : mPostSteppingActions ) f();


}
//________________________________________________________________________________________________
void StarVMCApplication::GeneratePrimaries() { _g4maker -> PushPrimaries(); }
void StGeant4Maker::PushPrimaries() {

  StMaker           *mymaker   = GetMaker("PrimaryMaker");
  StarPrimaryMaker  *myprimary = (StarPrimaryMaker *)mymaker;
  StarParticleStack *mystack   = myprimary->stack();
  
  int ntrack = mystack -> GetNtrack();

  int ntr = 0; // set by reference below...
  for ( int itrack = 0; itrack < ntrack; itrack++ )
    {

      TParticle *track = mystack->GetParticle(itrack);
      int pdg = track -> GetPdgCode();
      TParticlePDG *pdgPart = particleData(pdg); 

      if ( 0 == pdgPart )
	{ // Protect against unknown particle codes
	  continue;
	}

      int parent = track->GetFirstMother();
//    int kid1   = track->GetFirstDaughter();
//    int kid2   = track->GetLastDaughter();
      double px = track->Px();
      double py = track->Py();
      double pz = track->Pz();

      double M = pdgPart->Mass();

      double E  = TMath::Sqrt( px * px + py * py + pz * pz + M * M );
      double vx = track->Vx();
      double vy = track->Vy();
      double vz = track->Vz();
      double tof= track->T();
      double weight = 1.0;
      TMCProcess proc = kPPrimary;
      int stat = track->GetStatusCode();
      
      if ( 1 == stat ) 
	{
	  
	  // Push all tracks with parent = -1 to flag as primary
	  mMCStack->PushTrack( 1, parent=-1, pdg, px, py, pz, E, vx, vy, vz, tof, 0, 0, 0, proc, ntr, weight, stat );

	}
            

    }

  LOG_INFO << "Pushed " << ntr << " tracks from primary event generator" << endm;

}
//________________________________________________________________________________________________
int StGeant4Maker::Finish() {

  LOG_INFO << "Energy sums per sensitive volume" << endm;
  for ( auto kv : mHitSum ) {

    std::string name = kv.first;
    double      esum = kv.second;

    LOG_INFO << name.c_str() << " dE sum = " << esum << endm;

  }

  return kStOK;
}



//________________________________________________________________________________________________


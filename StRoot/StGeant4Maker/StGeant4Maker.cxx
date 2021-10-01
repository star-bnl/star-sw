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
//#include "StarVMC/StarAgmlLib/AgModule.h"
#include "StarVMC/StarAgmlLib/StarAgmlStacker.h"
//#include "StDetectorDbMaker/St_MagFactorC.h"  
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

//_______________________________________________________________________________________________
#include <AgMLVolumeIdFactory.h>
//_______________________________________________________________________________________________



//#include "StVMCStepManager.h"
//________________________________________________________________________________________________
//#include <G4SDManager.hh>
//#include <G4RunManager.hh>
//#include <G4LogicalVolume.hh>
//#include <G4LogicalVolumeStore.hh>
//#include <SystemOfUnits.h>
//#include <StGeant4Maker/SCORING/GenTrackerSensitiveDetector.h>
//#include <StGeant4Maker/StSensitiveDetectorFactory.h>
//________________________________________________________________________________________________
#include "TGeant4.h"
#include "TG4RunManager.h"
#include "TG4RunConfiguration.h"
//#include "TG4TrackManager.h"
//#include "TG4SDManager.h"
//#include "TG4SDConstruction.h"
//#include "TG4StepStatus.h"
//#include "G4Step.hh"
//#include "G4Track.hh"
//#include "G4VPhysicalVolume.hh"
//#include "G4StepPoint.hh"
//________________________________________________________________________________________________
//#include <StGeant4Maker/SCORING/StTrackerSD.h>
//________________________________________________________________________________________________
//#include "StMcHitFiller.h"
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
//________________________________________________________________________________________________
#include <StHitCollection.h> 
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
      g2t_hit.length    = hit->length;
      g2t_hit.lgam      = hit->lgam;
      /*
	these are used downstream by the slow simulator (and should not be filled here)

	g2t_hit.adc = ...;
	g2t_hit.pad = ...;
	g2t_hit.timebucket = ...;
	g2t_hit.np = ...; // number of primary electrons

      */
      
      table -> AddAt( &g2t_hit );     

      int idtruth = hit->idtruth;
      g2t_track_st* trk = (g2t_track_st*)track->At(idtruth-1);
      trk->n_tpc_hit++;     

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
      trk->n_stg_hit++;
      
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
      
      table -> AddAt( &g2t_hit );     

      int idtruth = hit->idtruth;
      g2t_track_st* trk = (g2t_track_st*)track->At(idtruth-1);
      trk->n_fts_hit++;

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
      // TODO: add pointer to next hit on the track 
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
//________________________________________________________________________________________________
// Pointer to the maker so we can forward VMC calls there
static StGeant4Maker* _g4maker = 0;
//________________________________________________________________________________________________
StarParticleData &particleData = StarParticleData::instance();
//________________________________________________________________________________________________
StarVMCApplication::StarVMCApplication( const Char_t *name, const Char_t *title, double zmax, double rmax ) : 
  TVirtualMCApplication(name,title),mZmax(zmax),mRmax(rmax)  {
}
//________________________________________________________________________________________________
StGeant4Maker::StGeant4Maker( const char* nm ) : 
  StMaker(nm),
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
  mEventHeader(0)
{ 

  // Setup default attributes
  //  SetAttr( "G4Opt:Nav",   "geomRoot" );// Default uses only possibility for us... ROOT geom with ROOT nav.  No VGM to convert to G4 geometry.
  //  Possibilities geomVMCtoGeant4 geomVMCtoRoot geomRoot geomRootToGeant4 geomGeant4
  SetAttr( "G4VmcOpt:Nav",   "geomVMCtoRoot" );
  SetAttr( "G4VmcOpt:Name",  "Geant4"  );
  SetAttr( "G4VmcOpt:Title", "The Geant4 Monte Carlo" );
  SetAttr( "G4VmcOpt:Phys",  "FTFP_BERT" ); // default physics list
  //  SetAttr( "G4VmcOpt:Process", "stepLimiter+specialCuts" ); // special process
  SetAttr( "G4VmcOpt:Process", "stepLimiter+specialControls+specialCuts+stackPopper" ); // special process
  //  SetAttr( "G4VmcOpt:Process", "stepLimiter+stackPopper" ); // special process
  SetAttr( "AgMLOpt:TopVolume", "HALL" );
  SetAttr( "Stepping:Punchout:Stop", 1 ); // 0=no action, 1=track stopped, 2=track stopped and re-injected            
  SetAttr( "Stepping:Punchout:Rmin", 223.49 ); // min radius applied to punchout logic
  SetAttr( "Stepping:Punchout:Zmin", 268.75 ); // min radius applied to punchout logic
  SetAttr( "Random:G4", 12345); 
  SetAttr( "field", -5.0 );

  SetAttr( "Application:Zmax", DBL_MAX );
  SetAttr( "Application:Rmax", DBL_MAX );

  SetAttr("Scoring:Transit",0);
  SetAttr("Scoring:Rmax",450.0);
  SetAttr("Scoring:Zmax",2000.0);
  SetAttr("Scoring:Emin",0.01);

  SetAttr("vertex:x",0.0);
  SetAttr("vertex:y",0.0);
  SetAttr("vertex:z",0.0);
  SetAttr("vertex:sigmax",0.0);
  SetAttr("vertex:sigmay",0.0);
  SetAttr("vertex:sigmaz",0.0);


  SetAttr( "runnumber", 1 );


  // Setup default cuts
  SetAttr("CUTGAM", 0.001);
  SetAttr("CUTELE", 0.001);
  SetAttr("CUTHAD", 0.01);
  SetAttr("CUTNEU", 0.01);
  SetAttr("CUTMUO", 0.01);
  SetAttr("BCUTE",  /*     R 'Cut for electron brems.'     D=*/     0.0001);
  SetAttr("BCUTM",  /*     R 'Cut for muon brems.'         D=-1.*/  0.0001);
  SetAttr("DCUTE",  /*     R 'Cut for electron delta-rays' D=*/     0.0001);
  SetAttr("DCUTM",  /*     R 'Cut for muon delta-rays'     D=-1.*/  0.0001);
  SetAttr("PPCUTM", /*     R 'Cut for e+e- pairs by muons' D=0.01*/ 0.01);
  SetAttr("TOFMAX", /*     R 'Time of flight cut'          D=*/     1.E+10);

  // Setup default physics
  SetAttr("PAIR", 1);
  SetAttr("COMP", 1);
  SetAttr("PHOT", 1);
  SetAttr("PFIS", 1);
  SetAttr("DRAY", 1);
  SetAttr("ANNI", 1);
  SetAttr("BREM", 1);
  SetAttr("HADR", 1);
  SetAttr("MUNU", 1);
  SetAttr("DCAY", 1);
  SetAttr("LOSS", 2);
  SetAttr("MULS", 1);
  SetAttr("CKOV", 1);
  SetAttr("RAYL", 1);
  SetAttr("LABS", 1);
  SetAttr("SYNC", 1);
    
  // TODO-- 
  //  SetAttr( "AgMLOpt:Hits:Deactivate", "ECAL:*,TPCE:*,*" );
  //  SetAttr( "AgMLOpt:Hits:Activate", "TPAD,CSUP,ESCI" );

  _g4maker = this; // Provide a global pointer to the G4 maker  

}
//________________________________________________________________________________________________
int StGeant4Maker::Init() {

  LOG_INFO << "Initialize geometry" << endm;
  InitGeom();

  LOG_INFO << "Create VMC application" << endm;
  mVmcApplication = new StarVMCApplication("g4star","STAR G4/VMC",DAttr("Application:Zmax"),DAttr("Application:Rmax"));

  LOG_INFO << "Create VMC run configuration" << endm;
  mRunConfig = new TG4RunConfiguration( SAttr("G4VmcOpt:Nav"), SAttr("G4VmcOpt:Phys" ), SAttr("G4VmcOpt:Process") );

  AddObj( mVmcApplication, ".const", 0 ); // Register VMC application  

  LOG_INFO << "Create GEANT4 instance" << endm;
  AddObj( gG4 = new TGeant4(SAttr("G4VmcOpt:Name"), SAttr("G4VmcOpt:Title") ,mRunConfig) , ".const", 0 );
  //gMC = gG4;
  
  // Set default track propation cuts
  gG4->SetCut( "CUTGAM", DAttr("cutgam") );
  gG4->SetCut( "CUTELE", DAttr("cutele") );
  gG4->SetCut( "CUTHAD", DAttr("cuthad") );
  gG4->SetCut( "CUTNEU", DAttr("cutneu") );
  gG4->SetCut( "CUTMUO", DAttr("cutmuo") );
  gG4->SetCut( "BCUTE" , DAttr("bcute") );
  gG4->SetCut( "DCUTE" , DAttr("dcute") );
  gG4->SetCut( "BCUTM" , DAttr("bcutm") );
  gG4->SetCut( "DCUTM" , DAttr("dcutm") );
  
  // Set default physics flags
  gG4->SetProcess("PAIR",   IAttr("PAIR"));
  gG4->SetProcess("COMP",   IAttr("COMP"));
  gG4->SetProcess("PHOT",   IAttr("PHOT"));
  gG4->SetProcess("PFIS",   IAttr("PFIS"));
  gG4->SetProcess("DRAY",   IAttr("DRAY"));
  gG4->SetProcess("ANNI",   IAttr("ANNI"));
  gG4->SetProcess("BREM",   IAttr("BREM"));
  gG4->SetProcess("HADR",   IAttr("HADR"));
  gG4->SetProcess("MUNU",   IAttr("MUNU"));
  gG4->SetProcess("DCAY",   IAttr("DCAY"));
  gG4->SetProcess("LOSS",   IAttr("LOSS"));
  gG4->SetProcess("MULS",   IAttr("MULS"));
  gG4->SetProcess("CKOV",   IAttr("CKOV"));
  gG4->SetProcess("RAYL",   IAttr("RAYL"));
  gG4->SetProcess("LABS",   IAttr("LABS"));
  gG4->SetProcess("SYNC",   IAttr("SYNC"));


  LOG_INFO << "Create StarMagFieldAdaprtor" << endm;
  mMagfield = new StarMagFieldAdaptor(/*nada*/);

  LOG_INFO << "Pass stack and magnetic field to G4, flag ROOT geometry" << endm;
  if ( gG4 ) {
    gG4->SetStack( mMCStack );  
    gG4->SetMagField( mMagfield );
    gG4 -> SetRootGeometry();
    //    gG4->ProcessGeantCommand( "/mcControl/g3Defaults" );
  } else {
    LOG_FATAL << "Could not instantiate concrete MC.  WTF?" << endm;
    return kStFATAL;
  };

  // Obtain the G4 run manager
  TG4RunManager* runManager = TG4RunManager::Instance();
  runManager->UseRootRandom(false);


  LOG_INFO << "Initialize GEANT4" << endm;
  gG4 -> Init(); // FinishGeometry is called here...

  // VMC SD manager appears to be the last thing initialized when gMC->Init()
  // is called... so we should initialize our hits here...

  //
  // Some geant4 configurations
  //
  const char* g4cmd[] = {
    "/mcPhysics/printGlobalCuts",
    "/mcPhysics/printGlobalControls",
  };

  for ( auto cmd : g4cmd )   gG4->ProcessGeantCommand( cmd );

  LOG_INFO << "Initialize GEANT4 Physics" << endm;
  gG4 -> BuildPhysics();

  // Create histograms
  TH1* h;
  AddHist( h = new TH2F("MC:vertex:RvsZ","MC vertex;z [cm];R [cm]",1801,-900.5,900.5,501,-0.5,500.5) );

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
  //  assert(gGeoManager);
}
int  StGeant4Maker::InitGeom() {
  // if ( gGeoManager ) {
  //   LOG_INFO << "Running with existing geometry manager" << endm;
  // }

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
      // AgML should have initalized the geometry data structure.  Add it as a const.
      //
      //      AddConst( AgModule::Geom() );
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

  // Process one single event.  Control handed off to VMC application.
  gG4 -> ProcessRun( 1 );

  // Update event header.  Note that event header's SetRunNumber method sets the run number AND updates the previous run number.

  if ( runnumber != mEventHeader->GetRunNumber() ) mEventHeader -> SetRunNumber( runnumber );
  mEventHeader -> SetEventNumber( eventNumber );
  mEventHeader -> SetProdDateTime();

  // SetDateTime();
 
  // Increment event number
  eventNumber++;

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
  //  _g4maker -> InitHits();

}
//________________________________________________________________________________________________
void StarVMCApplication::ConstructSensitiveDetectors() {

  assert(gGeoManager);
  LOG_INFO << "Create and register sensitive detectors" << endm;

  // First collect all AgML extensions with sensitive volumes
  // by the family name of the volume
  std::map<TString, StSensitiveDetector*> sdmap;

  // Get list of volumes
  TObjArray *volumes = gGeoManager->GetListOfVolumes();

  // Get list of extensions mapped to volume 
  for ( int i=0; i<volumes->GetEntries(); i++ ) {

    TGeoVolume* volume = (TGeoVolume *)volumes->At(i);
    AgMLExtension* ae = (AgMLExtension *)volume->GetUserExtension();
    if ( 0==ae ) {
      LOG_DEBUG << "No agml extension on volume = " << volume->GetName() << endm;
      continue; // shouldn't happen
    }
    if ( 0==ae->GetSensitive() ) {
      LOG_DEBUG << "Not sensitive = " << volume->GetName() << endm;
      continue; 
    }

    // Name of the volume
    TString vname=volume->GetName();
    TString fname=ae->GetFamilyName();
    TString mname=ae->GetModuleName();

    
    AgMLVolumeId* identifier = AgMLVolumeIdFactory::Create( fname );
    if ( identifier ) {
      LOG_INFO << "Setting volume identifier for " << fname.Data() << " " << vname.Data() << endm;
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
    }

    // Register this volume to the sensitive detector
    LOG_INFO << vname.Data() << "/" << fname.Data() << " --> " << sd->GetName() << endm;
    TVirtualMC::GetMC()->SetSensitiveDetector( vname, sd );
    
    // Register this volume with the sensitive detector
    sd->addVolume( volume );

  }




}
//________________________________________________________________________________________________
int  StGeant4Maker::ConfigureGeometry() {

  // Iterate overall volumes and set volume specific tracking cuts
  std::map<int, int> media;
  TObjArray* objArray = gGeoManager->GetListOfVolumes();
  for ( int i=0; i<objArray->GetEntries();i++ ) {
    TGeoVolume* volume = (TGeoVolume *)objArray->At(i);
    if ( 0==volume ) continue;
    TGeoMedium* medium = volume->GetMedium();
    int id = medium->GetId();
    if ( media[id]>0 ) continue; // skip if medium already encountered
    AgMLExtension* agmlExt = (AgMLExtension*)( volume->GetUserExtension() );
    if ( 0==agmlExt ) continue;
    for ( auto kv : agmlExt->GetCuts() ) {
      gG4->Gstpar( media[id]=id, kv.first, kv.second );
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


  LOG_INFO << "End of Event" << endm;

  // Event information is (for the time being) zeroed out
  St_g2t_event*  g2t_event  = new St_g2t_event("g2t_event",1);          AddData(g2t_event);
  g2t_event_st event = {0};
  g2t_event->AddAt( &event );

  StMCParticleStack* stack    = (StMCParticleStack *)TVirtualMC::GetMC()->GetStack();
  auto&              vertex   = stack->GetVertexTable();
  auto&              particle = stack->GetParticleTable();
  unsigned int nvertex = vertex.size();
  unsigned int ntrack  = particle.size();

  LOG_INFO << " nvertex=" << nvertex << endm;
  LOG_INFO << " ntrack =" << ntrack << endm;

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

    // Fill histograms
    {
      float& x = myvertex.ge_x[0];
      float& y = myvertex.ge_x[1];
      float& z = myvertex.ge_x[2];
      float  r2 = x*x + y*y;
      float  r = sqrt(r2);
      GetHist("MC:vertex:RvsZ")->Fill(z,r);
    }

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
    mytrack.e        = t->E();
    mytrack.pt       = t->pt(); // NOTE: starsim secondaries have pt = -999
    mytrack.eta      = t->particle()->Eta();
    mytrack.rapidity = t->particle()->Y();
    // index of the start and stop vertices.
    // TODO: particle stop vertices need to be scored
    mytrack.start_vertex_p = truthVertex[ t->start() ];
    mytrack.stop_vertex_p  = truthVertex[ t->stop()  ];
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
  AddHits<St_g2t_fts_hit>( "STGH", {"TGCG"}, "g2t_stg_hit", sd2table_stgc );
  AddHits<St_g2t_emc_hit>( "PREH", {"PSCI"}, "g2t_pre_hit", sd2table_emc  );
  AddHits<St_g2t_emc_hit>( "WCAH", {"WSCI"}, "g2t_wca_hit", sd2table_emc  );
  AddHits<St_g2t_hca_hit>( "HCAH", {"HSCI"}, "g2t_hca_hit", sd2table_hca  ); // HCA should have its own copier

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

  int current = mMCStack->GetCurrentTrackNumber();
  truthTable.push_back( mMCStack->GetPersistentTrack( current ) );

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
  // Reset the history (tracks always born in full tracking region)
  mPreviousNode = mCurrentNode = 0;
  mPreviousVolume = mCurrentVolume = 0;
  mCurrentTrackingRegion=2;
  mPreviousTrackingRegion=2;
}
//________________________________________________________________________________________________
void StarVMCApplication::PostTrack(){ _g4maker->PostTrack(); }
void StGeant4Maker::PostTrack()
{

}
//________________________________________________________________________________________________
void StGeant4Maker::UpdateHistory() {

  static auto* navigator    = gGeoManager->GetCurrentNavigator();
  static auto* mc           = TVirtualMC::GetMC();


  mPreviousNode   = mCurrentNode;
  mPreviousVolume = mCurrentVolume;
  mCurrentNode    = navigator->GetCurrentNode();
  mCurrentVolume  = navigator->GetCurrentVolume();

  // Obtain the agml extensions, giving priority to anything attached to
  // a node.

  aprev = (mPreviousNode ) ? dynamic_cast<AgMLExtension*>( mPreviousNode->GetUserExtension() ) : 0;
  acurr = (mCurrentNode  ) ? dynamic_cast<AgMLExtension*>( mCurrentNode->GetUserExtension() )  : 0;

  if ( 0==aprev ) {
    aprev = (mPreviousVolume) ? dynamic_cast<AgMLExtension*>( mPreviousVolume->GetUserExtension() ) : 0;
  }
  if ( 0==acurr ) {
    acurr = (mCurrentVolume) ? dynamic_cast<AgMLExtension*>( mCurrentVolume->GetUserExtension() ) : 0;
  }

  // Possibly inherit from parent volume
  if ( 0==aprev && mPreviousNode ) {
    aprev = dynamic_cast<AgMLExtension*>( mPreviousNode->GetMotherVolume()->GetUserExtension() );
  }
  if ( 0==acurr && mCurrentNode ) {
    acurr = dynamic_cast<AgMLExtension*>( mCurrentNode->GetMotherVolume()->GetUserExtension() );
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

  static auto mc = TVirtualMC::GetMC();
  static double Rmin = DAttr("Stepping:Punchout:Rmin");
  static double Zmin = DAttr("Stepping:Punchout:Zmin");



  // TODO:  This is a hack.  We need to update the geometry and group these three
  //        detectors underneath a single integration volume / region.
  if ( previous == "PMOD" || 
       previous == "WMOD" || 
       previous == "HMOD" ) {
    result = 0;
  }
  else
    result = curr - prev;

  //     2      2       0     no transition
  //     2      1       1     into tracking from calorimeter
  //     1      2      -1     into calorimeter from tracking
  //     1      1       0     no transition


 
 
 
    

 

  return result;

}
//________________________________________________________________________________________________
void StarVMCApplication::Stepping(){ _g4maker -> Stepping(); }
void StGeant4Maker::Stepping(){                                           

  //  static auto* navigator    = gGeoManager->GetCurrentNavigator();
  //  static auto* trackManager = TG4TrackManager::Instance();
  static auto* mc = TVirtualMC::GetMC(); 
  static auto* stack = (StMCParticleStack* )mc->GetStack();

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
       
    bool result = false;
    
    double x,y,z,r;
    mc->TrackPosition( x, y, z );
    r = TMath::Sqrt( x*x + y*y );
    z = TMath::Abs(z);

    bool tpcFiducial = r < Rmin && z < Zmin;
    bool fwdFiducial = r < 90.0;             // 90cm is poletip donut hole

    bool fcsFiducial = z > 700 && z < 1000 && TMath::Abs(x) < 150 && TMath::Abs(y) < 101.0;


    return tpcFiducial || fwdFiducial || fcsFiducial;

  };

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

    //    LOG_INFO << "Stopping track at z=" << vz << endm;
    //    mPreviousVolume->Print();
    //    mCurrentVolume->Print();
    mc->StopTrack();
    stopped = true;
    
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
	
	int pdgid = 0;
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

  if ( stopped ) {
    LOG_INFO << Form("track stopped x=%f y=%f z=%f ds=%f transit=%d %d stopped=%s  %s",
		     vx,vy,vz,mc->TrackStep(), mCurrentTrackingRegion, mPreviousTrackingRegion, (stopped)?"T":"F", mc->CurrentVolPath() ) << endm;
  }


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
      int kid1   = track->GetFirstDaughter();
      int kid2   = track->GetLastDaughter();
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

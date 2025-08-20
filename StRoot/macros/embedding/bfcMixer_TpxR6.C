class StBFChain;        
class StMessMgr;

#include <string>

const int debuglevel = 1;

std::string   chain1opts_ = "in,magF,tpcDb,NoDefault,TpxRaw,-ittf,usexgeom,xgeometry ";
std::string   chain2opts_ = "gen_T,emc_T,geomT,sim_T,TpcRS,-ittf,-tpc_daq,nodefault,noinput prepembed ry2021a ";
std::string   chain3opts_ = "DbV20230818 P2021a StiCA BEmcChkStat EbyET0 ODistoSmear VFMCE TpxClu -VFMinuit -hitfilt TpcMixer,GeantOut,MiniMcMk,McAna ,useInTracker,emcSim,bemcMixer,eefs,eemcmixer";

std::string   chain0opts = ( chain1opts_ + " " + chain2opts_ + " " + chain3opts_ + " " );

std::string   chain1opts = chain1opts_ + "nooutput ";
std::string   chain2opts = chain2opts_;
std::string   chain3opts = chain3opts_ + " -in noinput ";

const bool runchains[] = { false, true, true, true };

// Load sufficient libraries to bootstrap the StBFChain framework
#pragma cling load("libTree.so")
#pragma cling load("StarRoot")
#pragma cling load("St_base")
#pragma cling load("StChain")
#pragma cling load("StUtilities")
#pragma cling load("StBFChain")
#pragma cling load("liblog4cxx")
#pragma cling load("StStarLogger.so")
#pragma cling load("StarClassLibrary.so")

#if !(defined(__CINT__) || defined(__CLING__)) || defined(__MAKECINT__)

#include "Stiostream.h"
#include "TSystem.h"
#include "TClassTable.h"
#include "TApplication.h"
#include "TInterpreter.h"
#include "StBFChain.h"
#include "StMessMgr.h"
#include "TROOT.h"
#include "TAttr.h"
#include "Rtypes.h"

#endif

// Defined w/in the StBFChain library
extern StBFChain* chain;

StChain*    top    = 0;
StBFChain*  chain1 = 0;
StBFChain*  chain2 = 0;
StBFChain*  chain3 = 0;

int          nevents=10;
std::string  daqfile="/gpfs01/star/embed/daq/2021/auau17_phys_chop/st_physics_adc_22158015_raw_5000016.daq";
std::string  tagfile="/gpfs01/star/embed/tags/2021/auau17_phys/st_physics_adc_22158015_raw_5000016.tags.root";

double pt_low=0.1;
double pt_high=5.0;
double eta_low=-1.5;
double eta_high=1.5;
double vzlow = -150.0;
double vzhigh = 150.0;
double vr = 100.0;
int pid=9;
double mult=100;
std::vector<int> triggers = {};

std::string prodName = "P23idAuAu17";
std::string type = "FlatPt";



void process( const char* line ){
  gMessMgr->Info(line);
  gInterpreter->ProcessLine( Form("%s", line) );
};

void SetTagFile( const char* tags ) {
  process( "#pragma cling add_include_path(\"StRoot\")               ");
  process( "#include \"St_geant_Maker/Embed/StPrepEmbedMaker.h\"     ");
  process( "auto* embmk = dynamic_cast<StPrepEmbedMaker*>( StMaker::GetTopChain()->Maker(\"PrepEmbed\") );");
  process( "assert(embmk);                                           ");
  process( Form( "embmk->SetTagFile(\"%s\");                         ", tags ) );
}
void SetOpt( double ptmn, double ptmx, double etamn, double etamx, double phimn, double phimx, const char* type_ ) {
  process( Form( "embmk->SetOpt( %f, %f, %f, %f, %f, %f, \"%s\" );   ", ptmn, ptmx, etamn, etamx, phimn, phimx, type_ ) );
}
void SetPartOpt( int pid, int mult ) {
  process( Form( "embmk->SetPartOpt(%i,%i);", pid, mult ) );
  process( "embmk->SetSkipMode(true);" );
  process( "embmk->SetTemp(0.35);");
}
void SetTriggers( std::vector<int> triggers ) {
  for ( int t : triggers ) {
    process( Form( "embmk->SetTriOpt(%i);",t ) );
  }
}
void SetZVertexCut( double vzmn, double vzmx, double vr=-1.0 ) {
  process( Form( "embmk->SetZVertexCut(%f, %f);", vzmn, vzmx ) );
  if ( vr>0.0 )   
    process( Form( "embmk->SetVrCut(%f);", vr ) );
};


void bfcMixer_TpxR6() 
{

  // Create the top level chain
  top    = new StChain("Embedding");

  //
  // Create the three embedding chains.  Do not load or instantiate yet.
  //
  top->cd();

  auto* chain0 = new StBFChain();
  chain0->cd();
  chain0->SetDebug( debuglevel+1 );
  chain0->SetFlags( chain0opts.c_str() );
  chain0->SetName( "Zero" );
  

  if ( runchains[1] ) {
    chain1 = new StBFChain("One"); 
    chain1->cd(); 
    chain1->SetDebug( debuglevel );
    chain1 -> SetFlags( chain1opts.c_str() );
    chain1 -> SetName("One");
    chain1->Set_IO_Files(daqfile.c_str(), nullptr);
  }

  top->cd();

  if ( runchains[2] ) {
    chain2 = new StBFChain("Two");
    chain2 -> cd();
    chain2 ->SetDebug( debuglevel );
    chain2 -> SetFlags( chain2opts.c_str() );
    chain2 -> SetName("Two");
  }
  
  top->cd();

  if ( runchains[3] ) {
    chain3 = new StBFChain("Three");
    chain3 -> cd();
    chain3 -> SetDebug( debuglevel );
    chain3 -> SetFlags( chain3opts.c_str() );
    chain3 -> SetName("Three");
    TString outfile = gSystem->BaseName(daqfile.c_str());    
    outfile.ReplaceAll(".daq",".root");
    chain3->Set_IO_Files(nullptr, outfile);    
  }

  chain0->cd();
  chain0->Load();
  
  if ( chain1 ) { chain1->cd();  chain1->Instantiate(); }
  if ( chain2 ) { chain2->cd();  chain2->Instantiate(); }
  if ( chain3 ) { chain3->cd();  chain3->Instantiate(); }

  top->cd();

  //
  // Set inputs on TPC mixer
  //

  if ( chain3 ) {
    chain3->cd();
    auto* tpxmixer = chain3->Maker("TpcMixer"); assert(tpcmixer);
    tpxmixer -> SetInput( "Input1", "TpxRaw/.data/Event" );
    tpxmixer -> SetInput( "Input2", "TpcRS/Event" );
  }


#if 0
  if ( chain3 ) { 
    process("StMaker* eefs = StMaker::GetChain()->Maker(\"eefs\");  eefs=(eefs)?eefs:StMaker::GetChain()->Maker(\"EEmcFastSim\");");
    process("StMaker* eess = StMaker::GetChain()->Maker(\"eess\");");
    process("StMaker* eemx = StMaker::GetChain()->Maker(\"EEmcMixer\");");  
    process("if ( eemx && eefs ) { eefs->SetEmbeddingMode(); StMaker::GetChain()->AddBefore( eemx->GetName(), eefs ); }");
    process("if ( eemx && eess ) { eess->SetEmbeddingMode(); StMaker::GetChain()->AddBefore( eemx->GetName(), eess ); }");


    //
    // Make sure mcevent runs after the eemc mixer
    //
    //  auto* mcevent = chain3->Maker("StMcEventMaker");  assert(mcevent);
    //  chain3->AddAfter( eemx->GetName(), mcevent );
    process("StMaker* mcevent=StMaker::GetChain()->Maker(\"StMcEventMaker\");");
    process("StMaker::GetChain()->AddAfter(eemx->GetName(), mcevent );");

  }

    //
    // Configure prep embedding maker
    //
#endif

  process("StiDetectorBuilder::setDebug(10);");
  process("StiVMCToolkit::SetDebug(10);");

  top->cd();

  SetTagFile( tagfile.c_str() );
  SetOpt( pt_low, pt_high, eta_low, eta_high, 0.0, TMath::TwoPi(), type.c_str() );
  SetPartOpt( pid, mult );
  SetZVertexCut( vzlow, vzhigh, vr );
  SetTriggers( triggers );

  
  TAttr::SetDebug(0);
  
  //
  // Set privileges on makers
  //
  top->SetAttr(".Privilege",0,"*"                ); 	//All  makers are NOT priviliged
  top->SetAttr(".Privilege",1,"StBFtop::*" ); 	//StBFtop is priviliged
  top->SetAttr(".Privilege",1,"StIOInterFace::*" ); 	//All IO makers are priviliged
  top->SetAttr(".Privilege",1,"St_geant_Maker::*"); 	//It is also IO maker


  top->Init();

  top -> ls(10);

  gSystem->SetFPEMask( kNoneMask );
  top->EventLoop(nevents, top->Maker("outputStream"));

  top->Finish();
 
}



void bfcMixer_TpxR6( 
		    const int   nevents_ , 
		    const char* daqfile_   = "/gpfs01/star/embed/daq/2021/auau17_phys_chop/st_physics_adc_22158015_raw_5000016.daq"   , 
		    const char* tagfile_   = "/gpfs01/star/embed/tags/2021/auau17_phys/st_physics_adc_22158015_raw_5000016.tags.root" , 
		    double ptmn_           = 0.0           , 
		    double ptmx_           = 6.0           ,
		    double etamn_          = -1.75         , 
		    double etamx_          =  1.55         , 
		    double vzmn_           =  -145.0       , 
		    double vzmx_           = 145.0         , 
		    double vr_             = 2.0           , 
		    int pid_               = 14            ,
		    double mult_           = 0.1           , 
		    std::vector<int> triggers_  = {870010} , 
		    const char* prodname  = "P23idAuAu17" , 
		    const char* kintype   = "FlatPT"      ) {


  nevents  = nevents_;
  pt_low   = ptmn_;
  pt_high  = ptmx_;
  eta_low  = etamn_;
  eta_high = etamx_;
  vzlow    = vzmn_;
  vzhigh   = vzmx_;
  vr       = vr_;
  pid      = pid_;
  mult     = mult_;
  triggers = triggers_;
  // TODO: prodName, type ...
  prodName = prodname;
  type     = kintype;

  bfcMixer_TpxR6();

};

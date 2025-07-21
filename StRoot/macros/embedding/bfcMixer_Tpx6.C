class StBFChain;        
class StMessMgr;

const int debuglevel = 0;

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

const int    nevents=10;
const char*  daqfile="/gpfs01/star/embed/daq/2021/auau17_phys_chop/st_physics_adc_22158015_raw_5000016.daq";
const char*  tagfile="/gpfs01/star/embed/tags/2021/auau17_phys/st_physics_adc_22158015_raw_5000016.tags.root";
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
const char* prodName = "P23idAuAu17";
const char* type = "FlatPt";

std::string   chain1opts = "in,magF,tpcDb,NoDefault,TpxRaw,-ittf,NoOutput,usexgeom,xgeometry ";
std::string   chain2opts = "gen_T,emc_T,geomT,sim_T,TpcRS,-ittf,-tpc_daq,nodefault,noinput,prepembed";
std::string   chain3opts = "DbV20230818 P2021a StiCA BEmcChkStat EbyET0 ODistoSmear VFMCE TpxClu -VFMinuit -hitfilt "
   "TpcMixer,GeantOut,MiniMcMk,McAna,-in,NoInput,useInTracker,emcSim,bemcMixer,eefs,eemcmixer"
  //"btofSim,vpdSim,btofMixer,TpcMixer,GeantOut,MiniMcMk,McAna,-in,NoInput,useInTracker,emcSim,BEmcMixer,EEfs,EEmcMixer"
;

void process( const char* line ){
  gMessMgr->Info(line);
  gInterpreter->ProcessLine( Form("%s", line) );
};

void SetTagFile( const char* tags ) {
  process( "#pragma cling add_include_path(\"StRoot\")               ");
  process( "#include \"St_geant_Maker/Embed/StPrepEmbedMaker.h\"     ");
  process( "auto* embmk = dynamic_cast<StPrepEmbedMaker*>( chain3->Maker(\"PrepEmbed\") );");
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



void bfcMixer_Tpx6() 
{

  
  // Create the top level chain
  top    = new StChain("Embedding");

  //
  // Create the three embedding chains.  Do not load or instantiate yet.
  //
  top->cd();
  chain1 = new StBFChain("One"); 
  chain1->cd(); 
  chain1->SetDebug( debuglevel );
  chain1 -> SetFlags( chain1opts.c_str() );
  chain1 -> SetName("One");
  chain1->Set_IO_Files(daqfile, nullptr);

  top->cd();

  chain2 = new StBFChain("Two");
  chain2 -> cd();
  chain2 ->SetDebug( debuglevel );
  chain2 -> SetFlags( chain2opts.c_str() );
  chain2 -> SetName("Two");



  top->cd();
  chain3 = new StBFChain("Three");
  chain3 -> cd();
  chain3 -> SetDebug( debuglevel );
  chain3 -> SetFlags( chain3opts.c_str() );
  chain3 -> SetName("Three");
  chain3->Set_IO_Files(nullptr, "embedding_output.root");

  chain1->cd();
  chain1->Load();
  chain2->cd();
  chain2->Load();
  chain3->cd();
  chain3->Load();

  chain1->cd();
  chain1->Instantiate();
  chain2->cd();
  chain2->Instantiate();
  chain3->cd();
  chain3->Instantiate();

  top->cd();

  // Configure prep embedding maker
  SetTagFile( tagfile );
  SetOpt( pt_low, pt_high, eta_low, eta_high, 0.0, TMath::TwoPi(), "FlatPT" );
  SetPartOpt( pid, mult );
  SetZVertexCut( vzlow, vzhigh, vr );
  SetTriggers( triggers );


  top->Init();

  top->EventLoop(nevents);

  top->Finish();
 
}


class StBFChain;        
class StMessMgr;

#include <string>
#include <TString.h>

#include "EmbeddingChainOptions.h"

// Functor for the embedding chain options
EmbeddingChains getChainOptions;
const int debuglevel = 1;

std::string   chain1opts_ = "in,magF,tpcDb,NoDefault,TpxRaw,-ittf,usexgeom,xgeometry stargen:stubs "; // agml??
std::string   chain2opts_ = "gen_T,emc_T,geomT,sim_T,tpcrs     -ittf,-tpc_daq,nodefault stargen:embed kinematics:embed ry2021a g4star:mk ";
//std::string   chain2opts_ = "gen_T,emc_T,geomT,sim_T,     -ittf,-tpc_daq,nodefault stargen:embed kinematics:embed ry2021a g4star:mk ";
//std::string   chain2opts_ = "gen_T,emc_T,geomT,sim_T,TpcRS,-ittf,-tpc_daq,nodefault  ry2021a  ";
std::string   chain3opts_ = "DbV20230818 P2021a StiCA BEmcChkStat EbyET0 ODistoSmear VFMCE TpxClu -VFMinuit -hitfilt TpcMixer MiniMcMk McAna useInTracker emcSim bemcMixer eefs eemcmixer nodefault";
//std::string   chain3opts_ = "DbV20230818 P2021a StiCA BEmcChkStat EbyET0 ODistoSmear VFMCE TpxClu -VFMinuit -hitfilt TpcMixer " " MiniMcMk McAna useInTracker " " emcSim bemcMixer eefs eemcmixer nodefault";


std::string   prepend = "";

std::string   chain0opts = ( prepend + " " + chain1opts_ + chain2opts_ + " " + chain3opts_ + " " );

std::string   chain1opts = chain1opts_ + " nooutput " ;
std::string   chain2opts = chain2opts_ + " in ";// + " noinput nooutput " ;
std::string   chain3opts = chain3opts_ + " in " ;


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
#pragma cling load("libmysqlclient.so")
// #pragma cling load("libStIOMaker.so")
//#pragma cling load("libStarMiniCern.so")

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

int    nevents=2;
std::string  daqfile="/gpfs01/star/embed/daq/2021/auau17_phys_chop/st_physics_adc_22158015_raw_5000016.daq";
std::string  tagfile="/gpfs01/star/embed/tags/2021/auau17_phys/st_physics_adc_22158015_raw_5000016.tags.root";
std::string  simfile="/gpfs01/star/pwg/yelfeky/g4_hft/geant4out.geant.root";
double vzlow = -150.0;
double vzhigh = 150.0;
double vr = 2.0;
std::vector<int> triggers = {870010};
std::string prodName = "P23idAuAu17";
std::string type = "FlatPT";


void process( const char* line ){
  gMessMgr->Info(line);
  gInterpreter->ProcessLine( Form("%s", line) );
};
void SetTagFile( const char* tags ) {
  process(
    Form(
      "{\n auto* stembed = dynamic_cast<StarEmbedMaker*>( StMaker::GetTopChain()->Maker(\"StarEmbed\") );\n"
      "if ( stembed ) stembed->SetAttr(\"tags\", \"%s\");\n }"
      ,
      tags
    )
  );

}
void SetTriggers( std::vector<int> triggers ) {
  std::string triglist = "";
  for ( int t : triggers ) {
    triglist += Form( "%i ", t );
  }
  process(
    Form(
      "{\n auto* stembed = dynamic_cast<StarEmbedMaker*>( StMaker::GetTopChain()->Maker(\"StarEmbed\") );\n"
      "if ( stembed ) stembed->SetAttr(\"triggers\", \"%s\");\n }"
      ,
      triglist.c_str()
    )
  );

}
void SetZVertexCut( double vzmn, double vzmx, double vr=-1.0 ) {
  process(
    Form(
      "{\n auto* stembed = dynamic_cast<StarEmbedMaker*>( StMaker::GetTopChain()->Maker(\"StarEmbed\") );\n"
        "if ( stembed ) {"
        "  stembed->SetAttr(\"vzmin\", %f);\n"
        "  stembed->SetAttr(\"vzmax\", %f);\n"
        "  stembed->SetAttr(\"ZMIN\", %f);\n"
        "  stembed->SetAttr(\"ZMAX\", %f);\n"
        "  stembed->SetAttr(\"vrmax\", %f);\n"
        "  stembed->SetAttr(\"output\", \"genevents.root\");\n"
        "}\n}"
      ,
      vzmn, vzmx, vzmn, vzmx, vr
    )
  );
};


void bfcMixer_HftG4()
{
  top = new StChain("Embedding");
  top->cd();

  auto* chain0 = new StBFChain();
  chain0->cd();
  chain0->SetDebug( debuglevel + 1 );
  chain0->SetFlags( chain0opts.c_str() );
  chain0->SetName("Zero");

  if ( runchains[1] ) {
    chain1 = new StBFChain("One");
    chain1->cd();
    chain1->SetDebug( debuglevel );
    chain1 -> SetFlags( chain1opts.c_str() );
    chain1->SetName("One");
    chain1->Set_IO_Files( daqfile.c_str(), nullptr );
  }

  top->cd();

  if ( runchains[2] ) {
    chain2 = new StBFChain("Two");
    chain2->cd();
    chain2->SetDebug( debuglevel );
    chain2 -> SetFlags( chain2opts.c_str() );
    chain2->SetName("Two");
    chain2->Set_IO_Files( simfile.c_str(), nullptr );
  }

  top->cd();

  if ( runchains[3] ) {
    chain3 = new StBFChain("Three");
    chain3->cd();
    chain3->SetDebug( debuglevel );
    chain3 -> SetFlags( chain3opts.c_str() );
    chain3->SetName("Three");
    TString outfile = gSystem->BaseName( daqfile.c_str() );
    outfile.ReplaceAll(".daq", "_HftG4.root");
    chain3->Set_IO_Files( nullptr , outfile );
  }

  chain0->cd();
  chain0->Load();

  if ( chain1 ) { chain1->cd(); chain1->Instantiate(); }
  if ( chain2 ) { chain2->cd(); chain2->Instantiate(); }
  if ( chain3 ) { chain3->cd(); chain3->Instantiate(); }

  top->cd();

  if ( chain2 ) {
    

    auto* tpcrs = chain2->Maker("TpcRS");
    if ( tpcrs ) {
      tpcrs->SetAttr("inputds", "geantBranch");
    }

    // StarEmbedMaker configuration
    SetTagFile( tagfile.c_str() );
    SetZVertexCut( vzlow, vzhigh, vr );
    SetTriggers( triggers );

    
  }
  
  top->cd();

  if ( chain3 ) {
    chain3->cd();


    auto* tpxmixer = chain3->Maker("TpcMixer");
    if (tpxmixer) {
      tpxmixer->SetInput("Input1", "TpxRaw/.data/Event");
      tpxmixer->SetInput("Input2", "TpcRS/Event");
    }

  
    auto* eefs_ = chain3->Maker("eefs");
    auto* eess_ = chain3->Maker("eess");
    if ( eefs_ ) eefs_->SetAttr("embedding", 1);
    if ( eess_ ) eess_->SetAttr("embedding", 1);

    if (chain3->GetMaker("ist_raw_hit")) { // not to crash for non-hft productions
      process("auto* ist = dynamic_cast<StIstRawHitMaker*>( StMaker::GetTopChain()->GetMaker(\"ist_raw_hit\") );");
      process("if ( ist ) { ist->setDataType(1); ist->setDoEmbedding(kTRUE); }");
    }
    // we are settitng an alais to chain2, we do not want to run many input 
    // streams because we have to deal with skiping the events in them, a job
    // that is done by the embed maker not us :)
    // ok i do not need to do this anymore, i am using the embed maker
    // in the simulation step which by definition fixes this problem
    // but it is 5 am....
    chain3->SetInput("geant", "Two/inputStream/geantBranch");
    chain3->SetInput("geant4star", "Two/inputStream/geantBranch");
  }

  top->cd();

  TAttr::SetDebug(0);
  top->SetAttr(".Privilege", 0, "*");
  top->SetAttr(".Privilege", 1, "StBFChain::*");
  top->SetAttr(".Privilege", 1, "StIOInterFace::*");
  top->SetAttr(".Privilege", 1, "StGeant4Maker::*"); // ??
  top->SetAttr(".Privilege", 1, "StarEmbedMaker::*"); 

  gInterpreter->ProcessLine("{Geometry* __hack = new Geometry(); delete __hack;}");

  top->Init();
  top->ls(10);

  gSystem->SetFPEMask( kNoneMask );
  chain2->ls(3);
  top->EventLoop( nevents, top->Maker("outputStream") );
  top->Finish();
}

void bfcMixer_HftG4( 
  int nevents_,
  const char* daqfile_,
  const char* tagfile_,
  const char* simfile_,
  double vzlow_,
  double vzhigh_,
  double vr_,
  std::vector<int> triggers_,
  const char* prodName_
 )
{
  nevents  = nevents_;
  daqfile = daqfile_;
  tagfile = tagfile_;
  simfile = simfile_;
  vzlow = vzlow_;
  vzhigh = vzhigh_;
  vr = vr_;
  triggers = triggers_;
  prodName = prodName_;

  // TODO: impelent a chainoption like fzin for geant4, maybe g4:read?
  auto opts = getChainOptions( geant4star, prodName, false ); 

  if ( opts.isValid ) {
    
    chain0opts = opts.loadopts;
    chain1opts = opts.chain1 + " nooutput ";
    chain2opts = opts.chain2;
    chain3opts = opts.chain3;
    

    while ( replace( chain2opts, "g4star:mk", "" ) );
    while ( replace( chain2opts, "kinematics:embed", "" ) );
    while ( replace( chain2opts, "noinput", "" ) );
    while ( replace( chain2opts, "-in", "" ) );
    // while ( replace( chain0opts, "geantout", "" ) );
    // while ( replace( chain3opts, "geantout", "" ) );

    chain2opts += " in "; // ensure that the last chain has an input

    bfcMixer_HftG4();

  }

  else {

    std::cout << "Chain options are not setup properly for this production and/or simulation engine" << std::endl;

  }


  return;
}
void bfcMixer_HftG4( const char* dbg ) {

  std::string dbg_ = dbg;

  if ( dbg_ == "test2" ) {

    // https://drupal.star.bnl.gov/STAR/starsimrequests/2018/sep/06/pi-pi-k-k-p-p-dau-run16-hft
    const int   nevents_     = 10; 
    const char* mydaqfile_   = "/gpfs/mnt/gpfs01/star/pwg/yelfeky/g4_hft/hft_files/st_physics_adc_17137017_raw_4000057.daq"   ;
    const char* mytagfile_   = "/gpfs/mnt/gpfs01/star/pwg/yelfeky/g4_hft/hft_files/st_physics_adc_17137017_raw_4000057.tags.root"   ;
    const char* mysimfile_   = "/gpfs/mnt/gpfs01/star/pwg/yelfeky/g4_hft/st_physics_adc_17137017_raw_4000057_HftG4_sim.geant.root" ;
    double myvzmn_           = -6.0        ; 
    double myvzmx_           = 6.0         ; 
    double myvr_             = 9999.0           ; 
    std::vector<int> mytriggers_  = {530003} ; 
    const char* myprodname  = "P17iddAu200hft" ; 
    bfcMixer_HftG4( nevents_, mydaqfile_, mytagfile_, mysimfile_, myvzmn_, myvzmx_, myvr_, mytriggers_, myprodname );

  };

}
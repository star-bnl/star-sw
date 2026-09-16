class StBFChain;
class StMessMgr;

#include <string>
#include <TString.h>
#include "TFile.h"
#include "TTree.h"
#include "TLeaf.h"
#include "TSystem.h"

#include <iostream>

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

StBFChain* top = new StBFChain("physicssim");

StMaker* g4star = 0;

float   mult = 0.05;  // fraction of track multiplicity to embed
float   pt_low  = 5.0 - 0.0001  ;  // min pT to simulate [GeV]
float   pt_high  = 5.0 + 0.0001  ; // max pT to simulate [GeV]
float   eta_low =  0.25 - 0.0001;   // min eta to simulate
float   eta_high =  0.25 + 0.0001;   // max eta to simulate
float   vzlow = -150.0; // min z vertex to simulate
float   vzhigh = 150.0; // max z vertex to simulate
float   vr = 2.0; // max r vertex to simulate
std::vector<int> triggers{530003}; // triggers to simulate
int     pid   = 14; // default to proton
int     npart  = 0;
std::string dbv="DbV20230818";
std::string geom="y2021a";
std::string tagfile = "/gpfs01/star/embed/tags/2021/auau17_phys/st_physics_adc_22158015_raw_5000016.tags.root";
std::string daqfile = "/gpfs01/star/pwg/yelfeky/g4_hft/hft_files/st_physics_adc_17137017_raw_4000057.daq";
int         nevents = 2;
//______________________________________________________________________________________
void process( const char* line ){
  gMessMgr->Info(line);
  gInterpreter->ProcessLine( Form("%s", line) );
};
void SetTagFile( const char* tags ) {
  //  process( "#pragma cling add_include_path(\"StRoot\")               ");
  //  process( "#include \"St_geant_Maker/Embed/StPrepEmbedMaker.h\"     ");
  //  process( "auto* embmk = dynamic_cast<StPrepEmbedMaker*>( StMaker::GetTopChain()->Maker(\"PrepEmbed\") );");
  //  process( "assert(embmk);                                           ");
  //  process( Form( "embmk->SetTagFile(\"%s\");                         ", tags ) );
  process( "auto* stembed = dynamic_cast<StarEmbedMaker*>( StMaker::GetTopChain()->Maker(\"StarEmbed\") );");
  process( Form("stembed->SetAttr(\"tags\",\"%s\");",tags) );
  //  process( Form("stembed->SetInputFile(\"%s\");", tags ) );

}
void SetOpt( double ptmn, double ptmx, double etamn, double etamx, double phimn, double phimx ) {
  //  process( Form( "embmk->SetOpt( %f, %f, %f, %f, %f, %f, \"%s\" );   ", ptmn, ptmx, etamn, etamx, phimn, phimx, type_ ) );
  auto* kine = top->Maker("StarKine");
  kine->SetAttr("ptlow", ptmn);
  kine->SetAttr("pthigh", ptmx);
  kine->SetAttr("etalow", etamn);
  kine->SetAttr("etahigh", etamx);
  kine->SetAttr("philow", phimn);
  kine->SetAttr("phihigh", phimx);
  kine->SetAttr("mode", "FlatPT" );
}
void SetPartOpt( int pid, double mult ) {
  // Map PID onto particle name
  auto* kine = top->Maker("StarKine");
  kine->SetAttr("pid",int(pid));
  kine->SetAttr("ntrack", double(mult));
  if ( mult < 1.0 ) {
    auto* embed = top->Maker("StarEmbed");
    LOG_INFO << "Setting eventmult=" << mult << endm;
    embed->SetAttr("eventmult",double(mult));
  }
}
void SetTriggers( std::vector<int> triggers ) {
  auto* kine = top->Maker("StarEmbed");
  std::string triglist = "";
  for ( int t : triggers ) {
    //    process( Form( "embmk->SetTriOpt(%i);",t ) );
    triglist += Form( "%i ", t );
  }
  kine->SetAttr("triggers", triglist.c_str());
}
void SetZVertexCut( double vzmn, double vzmx, double vr=-1.0 ) {
  //  process( Form( "embmk->SetZVertexCut(%f, %f);", vzmn, vzmx ) );
  //  if ( vr>0.0 )   
  //    process( Form( "embmk->SetVrCut(%f);", vr ) );

  auto* embed = top->Maker("StarEmbed");  
  embed->SetAttr("vzmin", vzmn);
  embed->SetAttr("vzmax", vzmx);
  embed->SetAttr("vrmax", vr );
  embed->SetAttr("minMult", 1.);

};
void runEmbeddingSimulationHftG4()
{

  std::string chainopts =  "in nodefault " + geom + " " + dbv + " agml misalign simu sim_T gen_T stargen:embed kinematics:embed g4star:mk geant4out daq ";
  top->SetDebug(1);
  top->SetFlags(chainopts.c_str());
  TString outfile = gSystem->BaseName( daqfile.c_str() );
  outfile.ReplaceAll(".daq", "_HftG4_sim.root");
  top->Set_IO_Files(daqfile.c_str(), outfile);
  top->Load();
  top->Instantiate();

  process( "auto* primary_ = dynamic_cast<StarPrimaryMaker*>( StMaker::GetTopChain()->Maker(\"StarEmbed\") );" );
  process( "auto* kine_    = dynamic_cast<StarKinematics*>( StMaker::GetTopChain()->Maker(\"StarKine\") );" );
  process( "kine_->SetAttr(\"rapidity\",1);" );
  process( "primary_ -> AddGenerator( kine_ );");
  SetTagFile( tagfile.c_str() );
  SetOpt( pt_low, pt_high, eta_low, eta_high, 0.0, TMath::TwoPi() );
  SetPartOpt( pid, 1.0 );
  SetZVertexCut( vzlow, vzhigh, vr );
  SetTriggers( triggers );



  g4star = top->Maker("geant4star");
  g4star->SetAttr("G4VmcOpt","FTFP_BERT");

  g4star->SetAttr(
      "G4UI:PREINIT",
      "/process/eLoss/maxKinEnergy 250.0 GeV"   ";"
      "/mcCrossSection/setMaxKinE  250.0 GeV"   ";"
  );
  g4star->SetAttr(
      "G4UI:INIT",
      "/mcCrossSection/setMinKinE 1 keV "       ";"
      "/mcCrossSection/setMaxKinE 250 GeV "     ";"
      "/mcCrossSection/setMinMomentum 10 keV "  ";"
      "/mcCrossSection/setMaxMomentum 250 GeV " ";"
  );

  g4star->SetAttr( "g4:initAtInitRun" , 1 ); // Defer geometry and VMC initialization until InitRun
  g4star->SetAttr( "embedding:mode", 1 ); 
  // g4star->SetAttr( "embedding:mode:hft", 1 ); // don't want to explain, go read code.

  std::cout << "========================================================" << std::endl;
  auto* input = top->Maker("inputStream");
  top->AddBefore("StarEmbed", input);
  top->ls(5);

  gInterpreter->ProcessLine("{Geometry* __hack = new Geometry(); delete __hack;}");
  
  top->Init();
  
  gSystem->SetFPEMask( kNoneMask );
  
  TAttr::SetDebug(0);
  top->SetAttr(".Privilege", 0, "*");
  top->SetAttr(".Privilege", 1, "StBFChain::*");
  top->SetAttr(".Privilege", 1, "StIOInterFace::*");
  top->SetAttr(".Privilege", 1, "StGeant4Maker::*"); // ??
  top->SetAttr(".Privilege", 1, "StarEmbedMaker::*"); 

  top->EventLoop( nevents, top->Maker("outputStream") );

  top->Finish();


}

//______________________________________________________________________________________


void runEmbeddingSimulationHftG4(
		    const int   nevents_ , 
		    const char* daqfile_, 
		    const char* tagfile_, 
		    double ptmn_, 
		    double ptmx_,
		    double etamn_, 
		    double etamx_, 
		    double vzmn_, 
		    double vzmx_, 
		    double vr_, 
		    int pid_,
		    double mult_, 
		    std::vector<int> triggers_, 
        const char* dbv_,
        const char* geom_)
{

  nevents = nevents_;
  pt_low  = ptmn_;
  pt_high  = ptmx_;
  eta_low = etamn_;
  eta_high = etamx_;
  vzlow = vzmn_;
  vzhigh = vzmx_;
  vr = vr_;
  pid   = pid_;
  mult = mult_;
  triggers = triggers_;
  daqfile = daqfile_;
  tagfile = tagfile_;
  dbv   = dbv_;
  geom  = geom_;
  
  runEmbeddingSimulationHftG4();

}
//______________________________________________________________________________________

void runEmbeddingSimulationHftG4(const char* dbg)
{
  std::string dbg_ = dbg;

  if ( dbg_ == "test1" ) {
   nevents = 10;
    tagfile = "/gpfs01/star/embed/tags/2019/auau19_phys/st_physics_adc_20057049_raw_2000003.tags.root";
    mult = 0.05;
    pt_low  = 0.;
    pt_high  = 6.;
    eta_low = -1.;
    eta_high =  1.;
    pid   = 45;
    dbv   = "DbV20210827";
    geom  = "y2019a";
    runEmbeddingSimulationHftG4();
  }
  if ( dbg_ == "test2" ) {
    nevents = 10;
    tagfile = "/gpfs/mnt/gpfs01/star/pwg/yelfeky/g4_hft/hft_files/st_physics_adc_17137017_raw_4000057.tags.root";
    daqfile = "/gpfs/mnt/gpfs01/star/pwg/yelfeky/g4_hft/hft_files/st_physics_adc_17137017_raw_4000057.daq";
    mult = 3;
    pid   = 8;
    pt_low  = 0.;
    pt_high  = 5.;
    eta_low = -1.;
    eta_high =  1.;
    vzlow = -6.;
    vzhigh = 6.;
    vr = 9999.0 ;
    triggers = {530003};
    dbv   = "DbV20161216";
    geom  = "y2016x";
    runEmbeddingSimulationHftG4();
  }
  if ( dbg_ == "test3" ) {
    nevents = 10;
    tagfile = "/gpfs/mnt/gpfs01/star/pwg/yelfeky/g4_hft/hft_files/st_physics_adc_17137017_raw_4000057.tags.root";
    daqfile = "/gpfs/mnt/gpfs01/star/pwg/yelfeky/g4_hft/hft_files/st_physics_adc_17137017_raw_4000057.daq";
    mult = 10;
    pt_low  = 5.-0.0001;
    pt_high  = 5.+0.0001;
    eta_low = 0.-0.0001;
    eta_high = 0.+0.0001;
    pid   = 8;
    dbv   = "DbV20161216";
    geom  = " y2016x";
    triggers = {530003};
    runEmbeddingSimulationHftG4();
  }
}
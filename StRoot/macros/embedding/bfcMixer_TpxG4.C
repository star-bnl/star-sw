
class StBFChain;        
class StMessMgr;

#include <string>
#include <TString.h>
// #include <TVirtualMC.h>

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
std::string   chain2opts = chain2opts_ + " noinput nooutput " ;
std::string   chain3opts = chain3opts_ + " -in noinput " ;

const bool runchains[] = { false, true, true, true };

// Load sufficient libraries to bootstrap the StBFChain framework
#pragma cling load("libTree.so")
#pragma cling load("libEG.so")
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

StChain*    top    = 0;
StBFChain*  chain1 = 0;
StBFChain*  chain2 = 0;
StBFChain*  chain3 = 0;

int    nevents=1;
std::string  daqfile="/gpfs01/star/embed/daq/2021/auau17_phys_chop/st_physics_adc_22158015_raw_5000016.daq";
std::string  tagfile="/gpfs01/star/embed/tags/2021/auau17_phys/st_physics_adc_22158015_raw_5000016.tags.root";
double pt_low=0.1;
double pt_high=5.0;
double eta_low=-1.5;
double eta_high=1.5;
double vzlow = -150.0;
double vzhigh = 150.0;
double vr = 2.0;
int pid=9;
double mult=100;
std::vector<int> triggers = {870010};
std::string prodName = "P23idAuAu17";
std::string type = "FlatPT";
std::string engine = "G4";
int simcore = geant4star;
std::string pidtype = "pid";

struct DecayMode {
  int    pdg;
  float bratio[6];
  int mode[6][3];
};

struct SingleDecay {
  double br;
  int   daughter1, daughter2, daughter3;
};

DecayMode decayMode( int pdg, SingleDecay d, SingleDecay d2={}, SingleDecay d3={}, SingleDecay d4={}, SingleDecay d5={}, SingleDecay d6={} ) {
  DecayMode result={
    pdg,    
    {float(d.br), float(d2.br), float(d3.br), float(d4.br), float(d5.br), float(d6.br) },
    {
      {d.daughter1, d.daughter2, d.daughter3},
      {d2.daughter1, d2.daughter2, d2.daughter3},
      {d3.daughter1, d3.daughter2, d3.daughter3},
      {d4.daughter1, d4.daughter2, d4.daughter3},
      {d5.daughter1, d5.daughter2, d5.daughter3},
      {d6.daughter1, d6.daughter2, d6.daughter3}      
    }
  };
  return result;
};

ostream& operator<<(ostream& os, const DecayMode& m) {
  os << "Set decay mode pdg=" << m.pdg << " ";
  for ( int i=0;i<6;i++ ) {
    if ( m.bratio[i] <= 0 ) break;
    os << m.bratio[i] << " ";
    for ( int j=0;j<3;j++ ) {
      if ( m.mode[i][j] ) os << m.mode[i][j] << " ";
    }
  }
  return os;
};

std::vector< DecayMode > decayModes;

void process( const char* line ){
  std::cout << "]" << line << std::endl;
  gMessMgr->Info(line);
  gInterpreter->ProcessLine( Form("%s", line) );
};

void SetTagFile( const char* tags ) {
  if ( simcore == geant4star ) {
    process( "auto* stembed = dynamic_cast<StarEmbedMaker*>( StMaker::GetTopChain()->Maker(\"StarEmbed\") );");
    process( Form("stembed->SetAttr(\"tags\",\"%s\");",tags) );
  } else if ( simcore == starsimR6 ) {
    process( "#pragma cling add_include_path(\"StRoot\")               ");
    process( "#include \"St_geant_Maker/Embed/StPrepEmbedMaker.h\"     ");
    process( "auto* embmk = dynamic_cast<StPrepEmbedMaker*>( StMaker::GetTopChain()->Maker(\"PrepEmbed\") );" );
    process( "assert(embmk);" );
    process( Form( "embmk->SetTagFile(\"%s\");", tags ) );
  }
}
void SetOpt( double ptmn, double ptmx, double etamn, double etamx, double phimn, double phimx, const char* type_ ) {
  if ( simcore == geant4star ) {
    auto* kine = chain2->Maker("StarKine");
    kine->SetAttr("ptlow", ptmn);
    kine->SetAttr("pthigh", ptmx);
    kine->SetAttr("etalow", etamn);
    kine->SetAttr("etahigh", etamx);
    kine->SetAttr("philow", phimn);
    kine->SetAttr("phihigh", phimx);
    kine->SetAttr("mode", "FlatPT" );
  } else if ( simcore == starsimR6 ) {
    process( Form( "embmk->SetOpt( %f, %f, %f, %f, %f, %f, \"%s\" );   ", ptmn, ptmx, etamn, etamx, phimn, phimx, type_ ) );
  }
}
void SetPartOpt( int pid, double mult, const char* pidtype="pid" ) {
  if ( simcore == geant4star ) {
    // Map PID onto particle name
    auto* kine = chain2->Maker("StarKine");
    gMessMgr->Info() << "SetPartOpt " << pidtype << " " << pid << " " << mult << endm;
    kine->SetAttr(pidtype,int(pid));
    kine->SetAttr("ntrack", double(mult));
    if ( mult < 1.0 ) {
      auto* embed = chain2->Maker("StarEmbed");
      LOG_INFO << "Setting eventmult=" << mult << endm;
      embed->SetAttr("eventmult",double(mult));
    }
  } else if ( simcore == starsimR6 ) {
    process( Form( "embmk->SetPartOpt(%i,%f,\"%s\");", pid, mult, pidtype) );
    process( "embmk->SetSkipMode(true);" );
    process( "embmk->SetTemp(0.35);");
  }
}
void SetTriggers( std::vector<int> triggers ) {
  if ( simcore == geant4star ) {
    auto* kine = chain2->Maker("StarEmbed");
    std::string triglist = "";
    for ( int t : triggers ) {
      triglist += Form( "%i ", t );
    }
    kine->SetAttr("triggers", triglist.c_str());
  } else if ( simcore == starsimR6 ) {
    for ( int t : triggers ) {
      process( Form( "embmk->SetTrgOpt(%i);",t ) );
    }
  }
}
void SetZVertexCut( double vzmn, double vzmx, double vr=-1.0 ) {
  if ( simcore == geant4star ) {
    auto* embed = chain2->Maker("StarEmbed");  
    embed->SetAttr("vzmin", vzmn);
    embed->SetAttr("vzmax", vzmx);
    embed->SetAttr("vrmax", vr );
  } else if ( simcore == starsimR6 ) {
    process( Form( "embmk->SetZVertexCut(%f, %f);", vzmn, vzmx ) );
    if ( vr>0.0 )   
      process( Form( "embmk->SetVrCut(%f);", vr ) );
  }
};


void bfcMixer_TpxG4() 
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
  };

  top->cd();

  if ( runchains[2] ) { 
    chain2 = new StBFChain("Two");
    chain2 -> cd();
    chain2 ->SetDebug( debuglevel );
    chain2 -> SetFlags( chain2opts.c_str() );
    chain2 -> SetName("Two");
  };

  top->cd();


  if ( runchains[3] ) {
    chain3 = new StBFChain("Three");
    chain3 -> cd();
    chain3 -> SetDebug( debuglevel );
    chain3 -> SetFlags( chain3opts.c_str() );
    chain3 -> SetName("Three");
    TString outfile = gSystem->BaseName(daqfile.c_str());    
    outfile.ReplaceAll(".daq",Form("_%s.root", engine.c_str()));
    chain3->Set_IO_Files(nullptr, outfile);
  };

  chain0->cd();
  chain0->Load();

  if ( chain1 ) { chain1->cd();  chain1->Instantiate(); }
  if ( chain2 ) { chain2->cd();  chain2->Instantiate();
    if ( simcore == geant4star ) {
      auto* prim=chain2->Maker("StarEmbed");
      prim->SetAttr("output","genevents.root");
    }
  }
  if ( chain3 ) { chain3->cd();  chain3->Instantiate(); }

  top->cd();


  //
  // Set input DS for TpcRS
  //
  if ( chain2 ) {
    auto* tpcrs = chain2->Maker("TpcRS");
    if ( tpcrs ) {
  if ( simcore == geant4star ) tpcrs->SetAttr("inputds","geant4star");
    }
  }


  //
  // Set inputs on TPC mixer
  //



  if ( chain3 ) { 
    chain3->cd();
    auto* tpxmixer = chain3->Maker("TpcMixer"); assert(tpxmixer);
    tpxmixer -> SetInput( "Input1", "TpxRaw/.data/Event" );
    tpxmixer -> SetInput( "Input2", "TpcRS/Event" );

    auto*  eefs_ = chain3->Maker("eefs"); 
    auto*  eess_ = chain3->Maker("eess"); 
    if (eefs_) eefs_->SetAttr("embedding",1);
    if (eess_) eess_->SetAttr("embedding",1);

  }

  top->cd();



  auto* g4star = chain2->Maker("geant4star");
  if ( g4star ) {

    process( "auto* primary_ = dynamic_cast<StarPrimaryMaker*>( StMaker::GetTopChain()->Maker(\"StarEmbed\") );" );
    process( "auto* kine_    = dynamic_cast<StarKinematics*>( StMaker::GetTopChain()->Maker(\"StarKine\") );" );
    process( "kine_->SetAttr(\"rapidity\",1);" );
    process( "primary_ -> AddGenerator( kine_ );");
    
    //
    // Configure G4 maker
    //
    g4star->SetAttr( "embedding:mode",1);
    g4star->SetAttr( "G4VmcOpt:Nav",   "geomRoot");

    g4star->SetAttr( "application:engine", engine.c_str() );

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
		    "/mcVerbose/physicsExtDecayer 2"     ";"
		    "/mcVerbose/particlesManager 2"      ";"
		    );
		    

  // After initialization, set any decay modes on the virtual MC
  // NOTE: This implementation is not compatible with multi engine
  // simulation mode.
  for ( auto d : decayModes ) {
    LOG_INFO << "Setting custom decay modes for " << d.pdg << endm;
    if ( d.pdg ) {
      g4star->SetAttr( "setdecay", d.pdg );
      g4star->SetAttr( "setdecay", d.pdg );
      g4star->SetAttr( "setdecay", d.pdg );
      g4star->SetAttr( "setdecay", d.pdg );
      g4star->SetAttr( Form("setdecay:%i:bratio", d.pdg), Form("{%f,%f,%f,%f,%f,%f}", d.bratio[0], d.bratio[1], d.bratio[2], d.bratio[3], d.bratio[4], d.bratio[5] ));
      g4star->SetAttr( Form("setdecay:%i:mode", d.pdg),  
		       Form("{{%i,%i,%i},{%i,%i,%i},{%i,%i,%i},{%i,%i,%i},{%i,%i,%i},{%i,%i,%i}}",
			    d.mode[0][0],	d.mode[0][1], d.mode[0][2],
			    d.mode[1][0],	d.mode[1][1], d.mode[1][2],
			    d.mode[2][0],	d.mode[2][1], d.mode[2][2],
			    d.mode[3][0],	d.mode[3][1], d.mode[3][2],
			    d.mode[4][0],	d.mode[4][1], d.mode[4][2],
			    d.mode[5][0],	d.mode[5][1], d.mode[5][2]) );
    }
							    
  }

    

  }
  
  SetTagFile( tagfile.c_str() );
  SetOpt( pt_low, pt_high, eta_low, eta_high, 0.0, TMath::TwoPi(), type.c_str() );
  SetPartOpt( pid, mult, pidtype.c_str() );
  SetZVertexCut( vzlow, vzhigh, vr );
  SetTriggers( triggers );


  TAttr::SetDebug(0);
  
  //
  // Set privileges on makers
  //
  top->SetAttr(".Privilege",0,"*"                ); 	//All  makers are NOT priviliged
  top->SetAttr(".Privilege",1,"StBFChain::*" ); 	//StBFChain is priviliged
  top->SetAttr(".Privilege",1,"StIOInterFace::*" ); 	//All IO makers are priviliged
  top->SetAttr(".Privilege",1,"St_geant_Maker::*"); 	//It is also IO maker
  top->SetAttr(".Privilege",1,"StPrepEmbedMaker::*"); 	//It is also IO maker
  top->SetAttr(".Privilege",1,"StGeant4Maker::*"); 	//It is also IO maker
  top->SetAttr(".Privilege",1,"StarEmbedMaker::*"); 	//It is also IO maker


  gInterpreter->ProcessLine("{Geometry* __hack = new Geometry(); delete __hack;}");


  top->Init();
  top->ls(10);




  //  top->Maker("TpcRS")->SetDebug(999);
  //  top->Maker("Sti")->SetDebug(999);

  // Disable FPE detection enabled by geant4
  gSystem->SetFPEMask( kNoneMask );

  top->EventLoop(nevents, top->Maker("outputStream"));

  top->Finish();
 
}



void bfcMixer_TpxG4( 
		    const int   nevents_ , 
		    const char* daqfile_   = "/gpfs01/star/pwg/yelfeky/PicoThirdMaker/hpss_restore_with_tags/st_hf_adc_19101008_raw_3500011.daq", 
		    const char* tagfile_   = "/gpfs01/star/pwg/yelfeky/PicoThirdMaker/hpss_restore_with_tags/st_hf_adc_19101008_raw_3500011.tags.root" , 
		    double ptmn_           = 0.0           , 
		    double ptmx_           = 20.0          ,
		    double etamn_          = -1.0          , 
		    double etamx_          =  1.0          , 
		    double vzmn_           = -55.0         , 
		    double vzmx_           = 55.0          , 
		    double vr_             = 100.0         , 
		    int pid_               = 7             ,
		    double mult_           = 0.05          , 
		    std::vector<int> triggers_  = {600213,600214,600231,600232} , 
		    const char* prodname  = "P21idIsobar200"  , 
		    const char* kintype   = "FlatPT"       ,
		    bool simIn = false                     ,
		    const char* pidtype_ = "pid"           ,  // PID (aka geant 3 id) or PDG
		    const std::vector< DecayMode > decays_ = {}, 
		    const char* engine_ = "G4"
		     ) {




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
  engine   = engine_;
  pidtype  = pidtype_;

  daqfile=daqfile_;
  tagfile=tagfile_;

  decayModes = decays_;

  if ( engine == "G4" || engine == "G3" ) simcore = geant4star;
  else if ( engine == "starsimR6" ) simcore = starsimR6;
  else {
    std::cout << "Unknown simulation engine: " << engine << std::endl;
    std::cout << "Valid options are: G4, G3, starsimR6" << std::endl;
    return;
  }

  auto opts = getChainOptions(simcore, prodName, simIn );

  if ( opts.isValid ) {
    
    chain0opts = opts.loadopts;
    chain1opts = opts.chain1 + " nooutput ";
    chain2opts = opts.chain2 + " noinput nooutput ";
    chain3opts = opts.chain3 + " -in noinput ";    
    bfcMixer_TpxG4();

  }

  else {

    std::cout << "Chain options are not setup properly for this production and/or simulation engine" << std::endl;

  }


  return;

};

void bfcMixer_TpxG4( const char* dbg ) {

  std::string dbg_ = dbg;

  if ( dbg_ == "crash1" ) {

    const int   nevents_ = 50; 
    const char* mydaqfile_   = "/gpfs01/star/embed/daq/2021/auau17_phys_chop/st_physics_adc_22155034_raw_5500004.daq"   ;
    const char* mytagfile_   = "/gpfs01/star/embed/tags/2021/auau17_phys/st_physics_adc_22155034_raw_5500004.tags.root" ;
    bfcMixer_TpxG4( 50, mydaqfile_, mytagfile_ );

  };

  if ( dbg_ == "test1" ) {

    const int   nevents_     = 10; 
    const char* mydaqfile_   = "/gpfs01/star/embed/daq/2021/auau17_phys_chop/st_physics_adc_22155034_raw_5500004.daq"   ;
    const char* mytagfile_   = "/gpfs01/star/embed/tags/2021/auau17_phys/st_physics_adc_22155034_raw_5500004.tags.root" ;
    double myptmn_           = 5.0 - 0.0001  ; 
    double myptmx_           = 5.0 + 0.0001  ;
    double myetamn_          =  0.25 - 0.0001; 
    double myetamx_          =  0.25 + 0.0001; 
    double myvzmn_           =  -145.0       ; 
    double myvzmx_           = 145.0         ; 
    double myvr_             = 2.0           ; 
    int mypid_               = 14            ;
    double mymult_           = 0.1           ; 
    std::vector<int> mytriggers_  = {870010} ; 
    const char* myprodname  = "P23idAuAu17" ; 
    const char* mykintype   = "FlatPT"      ;
    bfcMixer_TpxG4( nevents_, mydaqfile_, mytagfile_, myptmn_, myptmx_, myetamn_, myetamx_, myvzmn_, myvzmx_, myvr_, mypid_, mymult_, mytriggers_, myprodname, mykintype );

  };



  if ( dbg_ == "P21icAuAu19" ) {
    const int   nevents_     = 100; 
    const char* mydaqfile_   = "/star/data03/daq/2019/057/20057049/st_physics_adc_20057049_raw_2000003.daq";
    const char* mytagfile_   = "/gpfs01/star/embed/tags/2019/auau19_phys/st_physics_adc_20057049_raw_2000003.tags.root" ;
    double myptmn_           = 0.0;
    double myptmx_           = 6.0;
    double myetamn_          = -1.0;
    double myetamx_          = +1.0;
    double myvzmn_           = -145.0       ; 
    double myvzmx_           =  145.0         ; 
    double myvr_             = 2.0           ; 
    int mypid_               = 45            ;
    double mymult_           = 0.05           ; 
    std::vector<int> mytriggers_  = {640001,640011,640021,640031,640041,640051} ; 
    const char* myprodname  = "P21icAuAu19" ; 
    const char* mykintype   = "FlatPT"      ;
    bfcMixer_TpxG4( nevents_, mydaqfile_, mytagfile_, myptmn_, myptmx_, myetamn_, myetamx_, myvzmn_, myvzmx_, myvr_, mypid_, mymult_, mytriggers_, myprodname, mykintype );
  };

  /*
/star/data101/EMBED/daq/2019/079/20079022/st_physics_adc_20079022_raw_1000005.daq
/star/data20/tags/production_19GeV_2019/ReversedFullField/P21ic/2019/079/20079022/st_physics_adc_20079022_raw_1000005.tags.root
   */
  if ( dbg_ == "P21icAuAu19:phi" ) {
    const int   nevents_     = 100; 
    const char* mydaqfile_   = "/star/data101/EMBED/daq/2019/079/20079022/st_physics_adc_20079022_raw_1000005.daq";
    const char* mytagfile_   = "/star/data20/tags/production_19GeV_2019/ReversedFullField/P21ic/2019/079/20079022/st_physics_adc_20079022_raw_1000005.tags.root";
    double myptmn_           = 0.0;
    double myptmx_           = 6.0;
    double myetamn_          = -1.0;
    double myetamx_          = +1.0;
    double myvzmn_           = -145.0       ; 
    double myvzmx_           =  145.0         ; 
    double myvr_             = 2.0           ; 
    int mypid_               = 333        ;
    double mymult_           = 0.05           ; 
    std::vector<int> mytriggers_  = {640001,640011,640021,640031,640041,640051} ; // TODO:  check triggers
    const char* myprodname  = "P21icAuAu19" ; 
    const char* mykintype   = "FlatPT"      ;
    const char* mypidtype   = "pdg";
    // 50% phi --> K+ K-
    // 50% phi --> K- K+ 
    //    DecayMode phiKK = decayMode( 333, {50.0, 321, -321, 0 }, {50.0, -321, 321, 0 } );
    //    decayModes.push_back( phiKK );			 

    // bfcMixer_TpxG4( 100, "/star/data101/EMBED/daq/2019/079/20079022/st_physics_adc_20079022_raw_1000005.daq", "/star/data20/tags/production_19GeV_2019/ReversedFullField/P21ic/2019/079/20079022/st_physics_adc_20079022_raw_1000005.tags.root", 0., 6., -1., 1., -145., 145., 2.0, 333, 0.05, {640001,640011,640021,640031,640041,640051}, "P21icAuAu19" , "FlatPT", "pdg",        {    decayMode(  333, { 100.0,  321, -321,    0 } ),           decayMode(  321, { 100.0,  211,  211, -211 } ),         decayMode( -321, { 100.0, -211, -211,  211 } )       }


    bfcMixer_TpxG4
      ( 
       nevents_, 
       mydaqfile_, 
       mytagfile_,
       myptmn_, 
       myptmx_, 
       myetamn_, 
       myetamx_, 
       myvzmn_, 
       myvzmx_, 
       myvr_, 
       mypid_, 
       mymult_, 
       mytriggers_, 
       myprodname, 
       mykintype, 
       false, 
       mypidtype,
       {
	 decayMode(  333, { 100.0,  321, -321,    0 } ),     // phi -- K+ K- 100%
	 decayMode(  321, { 100.0,  211,  211, -211 } ),     //    K+ --> pi+ pi+ pi- 100%
	 decayMode( -321, { 100.0, -211, -211,  211 } )      //    K- --> pi+ pi- pi- 100%	   
       }
	);
  };




  if ( dbg_ == "P21idIsobar200" ) {
    const int   nevents_ = 100; 
    const char* mydaqfile_   = "/gpfs01/star/pwg/yelfeky/PicoThirdMaker/hpss_restore_with_tags/st_hf_adc_19101008_raw_3500011.daq"; 
    const char* mytagfile_   = "/gpfs01/star/pwg/yelfeky/PicoThirdMaker/hpss_restore_with_tags/st_hf_adc_19101008_raw_3500011.tags.root" ; 
    double myptmn_           = 0.0           ; 
    double myptmx_           = 20.0          ;
    double myetamn_          = -1.0          ; 
    double myetamx_          =  1.0          ; 
    double myvzmn_           = -55.0         ; 
    double myvzmx_           = 55.0          ; 
    double myvr_             = 100.0         ; 
    int mypid_               = 7             ;
    double mymult_           = 0.05          ; 
    std::vector<int> mytriggers_  = {600213,600214,600231,600232} ; 
    const char* myprodname  = "P21idIsobar200"  ; 
    const char* mykintype   = "FlatPT"       ;
    bool mysimIn = false                     ;
    bfcMixer_TpxG4( nevents_, mydaqfile_, mytagfile_, myptmn_, myptmx_, myetamn_, myetamx_, myvzmn_, myvzmx_, myvr_, mypid_, mymult_, mytriggers_, myprodname, mykintype );
     
  }
  /*
/star/data101/EMBED/daq/2019/193/20193026/st_physics_adc_20193026_raw_4000002.daq
/star/data20/tags/production_AuAu200_2019/ReversedFullField/P23ie/2019/193/20193026/st_physics_adc_20193026_raw_4000002.tags.root

   */
  if ( dbg_ == "P23ieAuAu200:pion" ) {
    const int   nevents_     = 100; 
    const char* mydaqfile_   = "/star/data101/EMBED/daq/2019/193/20193026/st_physics_adc_20193026_raw_4000002.daq"; 
    const char* mytagfile_   = "/star/data20/tags/production_AuAu200_2019/ReversedFullField/P23ie/2019/193/20193026/st_physics_adc_20193026_raw_4000002.tags.root" ; 
    double myptmn_           = 0.0           ; 
    double myptmx_           = 20.0          ;
    double myetamn_          = -1.0          ; 
    double myetamx_          =  1.0          ; 
    double myvzmn_           = -55.0         ; 
    double myvzmx_           = 55.0          ; 
    double myvr_             = 100.0         ; 
    int mypid_               = 7             ;
    double mymult_           = 0.05          ; 
    std::vector<int> mytriggers_  = {700001} ; 
    const char* myprodname  = "P23ieAuAu200"  ; 
    const char* mykintype   = "FlatPT"       ;
    bool mysimIn = false                     ;
    bfcMixer_TpxG4( nevents_, mydaqfile_, mytagfile_, myptmn_, myptmx_, myetamn_, myetamx_, myvzmn_, myvzmx_, myvr_, mypid_, mymult_, mytriggers_, myprodname, mykintype );     
  }

  /*
/star/data101/EMBED/daq/2019/062/20062042/st_physics_adc_20062042_raw_6000002.daq
/star/data20/tags/production_19GeV_2019/ReversedFullField/P23id/2019/062/20062042/st_physics_adc_20062042_raw_6000002.tags.root

   */
  if ( dbg_ == "P23idAuAu19:deuteron" ) {
    const int   nevents_     = 100; 
    const char* mydaqfile_   = "/star/data101/EMBED/daq/2019/062/20062042/st_physics_adc_20062042_raw_6000002.daq";
    const char* mytagfile_   = "/star/data20/tags/production_19GeV_2019/ReversedFullField/P23id/2019/062/20062042/st_physics_adc_20062042_raw_6000002.tags.root";
    double myptmn_           = 0.0;
    double myptmx_           = 6.0;
    double myetamn_          = -1.0;
    double myetamx_          = +1.0;
    double myvzmn_           = -145.0       ; 
    double myvzmx_           =  145.0         ; 
    double myvr_             = 2.0           ; 
    int mypid_               = 45            ;
    double mymult_           = 0.05           ; 
    std::vector<int> mytriggers_  = {640001,640011,640021,640031,640041,640051} ; 
    const char* myprodname  = "P23idAuAu19" ; 
    const char* mykintype   = "FlatPT"      ;
    bfcMixer_TpxG4( nevents_, mydaqfile_, mytagfile_, myptmn_, myptmx_, myetamn_, myetamx_, myvzmn_, myvzmx_, myvr_, mypid_, mymult_, mytriggers_, myprodname, mykintype );
  };

    
};


//________________________________________________________________________________________
void bfcMixer_Tpx( 
		    const int   nevents_        , 
		    const char* daqfile_        ,
		    const char* tagfile_        ,
		    double ptmn_                ,
		    double ptmx_                ,
		    double etamn_               ,
		    double etamx_               ,
		    double vzmn_                ,
		    double vzmx_                ,
		    double vr_                  ,
		    int pid_                    ,
		    double mult_                ,
		    std::vector<int> triggers_  ,
		    const char* prodname        ,
		    const char* kintype         ,
		    bool simIn                  ,
		    const char* engine_ 
		   ) {

  bfcMixer_TpxG4( nevents_, daqfile_, tagfile_, ptmn_, ptmx_, etamn_, etamx_, vzmn_, vzmx_, vr_, pid_, mult_, triggers_, prodname, kintype, simIn, engine_ );

};

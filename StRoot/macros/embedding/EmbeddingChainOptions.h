#ifndef __EMBEDDING_CHAIN_OPTIONS_H__
#define __EMBEDDING_CHAIN_OPTIONS_H__

#include <string>
#include <vector>
#include <map>
#include <utility>
#include <algorithm>

enum SimulationCore {
  starsimR5   = 0,
  starsimR6   = 1,
  geant4star  = 2
};

// Utility to replace a portion of the specified string
bool replace(std::string& str, const std::string& from, const std::string& to) {
    size_t start_pos = str.find(from);
    if(start_pos == std::string::npos)
        return false;
    str.replace(start_pos, from.length(), to);
    return true;
}

struct EmbeddingChainOptions_t {
  bool isValid ;          // flag indicating that the chain options are not valid for the given simulation engine
  std::string chain1;     // list of options in chain 1
  std::string chain2;     // list of options in chain 2
  std::string chain3;     // list of options in chain 3
  std::string loadopts;   // concatination of all options, stripping out options which prevent loading libs needed in other chains
};

template< int simEngine > 
struct EmbeddingChains {

  EmbeddingChainOptions_t operator() ( const std::string& prodName, const bool readSim=false ) {

    EmbeddingChainOptions_t result = { false, "", "", "", "" };
    
    // Exclude generating a production for the list of engines 
    std::map<std::string, std::vector<int>> skip = {
      {"P23idAuAu17", {starsimR5}}
    };
    
    
    if ( std::find(skip[prodName].begin(), skip[prodName].end(), simEngine) != skip[prodName].end() ) {return result;}
    
    
    struct ChainConfig {
      std::string prod;
      std::string geom;
      std::string geomMode;
    };
    
    struct ChainMods {
      std::string append;
      std::string remove;
      std::vector< std::pair< std::string,std::string>> replace;
    };
    
    std::map< int, std::string > engineMap = {
      { starsimR5, "starsimr5" },
      { starsimR6, "starsimr6" },
      { geant4star,"geant4star"}
    };
    
    std::string chain1Opt = "in,magF,tpcDb,NoDefault,TpxRaw,-ittf,NoOutput ";
    std::string chain2Opt = "gen_T,geomT,sim_T,TpcRS,-ittf,-tpc_daq,nodefault ";
    
    if (readSim) { chain2Opt += ",fzin ";	}
    else         { chain2Opt += ",NoInput,PrepEmbed "; }
    
    std::string chain3Opt = "";
    
    static const std::map<std::string, ChainConfig> chainMap = {
      {"P08icpp",      {"DEPRECATED DbV20080712,pp2008,ITTF,OSpaceZ2,OGridLeak3D,beamLine,VFMCE,TpxClu -VFPPV -hitfilt",       "ry2008e",       ""}},
      {"P08iepp",      {"DbV20081117 B2008a ITTF IAna ppOpt l3onl emcDY2 fpd ftpc trgd ZDCvtx NosvtIT NossdIT Corr4 OSpaceZ2 OGridLeak3D VFMCE -hitfilt",       "ry2008e",       ""}},
      {"P08icAuAu9",   {"DEPRECATED DbV20080709 P2008 ITTF VFMCE -hitfilt",    "ry2008e",       ""}},
      {"P08icdAu",     {"DEPRECATED DbV20080712 P2008 ITTF OSpaceZ2 OGridLeak3D beamLine, VFMCE TpxClu -VFMinuit -hitfilt",      "ry2008e",       ""}},
      {"P08iedAu",     {"DbV20090213 P2008 ITTF OSpaceZ2 OGridLeak3D beamLine VFMCE TpxClu -VFMinuit -hitfilt",      "ry2008e",       ""}},
      {"P08icAuAu200", {"DEPRECATED DbV20070101 P2008 ITTF VFMCE -hitfilt",  "ry2008e",       ""}},
      {"P09igpp500",   {"DbV20091225 pp2009c ITTF BEmcChkStat btof Corr4 OSpaceZ2 OGridLeak3D VFMCE TpxClu -hitfilt",    "ry2009d",       ""}},
      {"P11ibpp500",   {"DbV20110310 OGGVoltErr pp2009c ITTF VFPPVnoCTB BEmcChkStat beamLine Corr4 OSpaceZ2 OGridLeak3D VFPPVnoCTB beamLine TpxClu -VFMinuit -hitfilt",    "ry2009d",       ""}},
      {"P10iapp",      {"DbV20091001 pp2009c TpcRS ITTF OSpaceZ2 OGridLeak3D beamLine, VFMCE TpcRS -VFMinuit -hitfilt",       "ry2010c",       ""}},
      {"P10icpp200",   {"DbV20100301 pp2009c ITTF BEmcChkStat btof Corr4 OSpaceZ2 OGridLeak3D VFMCE TpxClu -hitfilt",    "ry2009d",       ""}},
      {"P11idpp200",   {"DbV20120908,pp2009d,ITTF,BEmcChkStat,btof,fmsdat,Corr4,OSpaceZ2,OGridLeak3D,VFMCE,TpxClu,-hitfilt",    "ry2009d",       ""}},
      {"P10ikAuAu62",  {"DbV20110413 P2010a,btof,BEmcChkStat,Corr4,OSpaceZ2,OGridLeak3D,VFMCE TpxClu -VFMinuit -hitfilt",   "ry2010c",       ""}},
      {"P10ihAuAu39",  {"DbV20100909 P2010a,btof,BEmcChkStat,Corr4,OSpaceZ2,OGridLeak3D,VFMCE TpxClu -VFMinuit -hitfilt",   "ry2010c",       ""}},
      {"P10ikAuAu39",  {"DbV20100909 P2010a,btof,BEmcChkStat,Corr4,OSpaceZ2,OGridLeak3D,VFMCE TpxClu -VFMinuit -hitfilt",   "ry2010c",       ""}},
      {"P10ihAuAu11",  {"DbV20100821 P2010a,btof,BEmcChkStat,Corr4,OSpaceZ2,OGridLeak3D,VFMCE TpxClu -VFMinuit -hitfilt",   "ry2010c",       ""}},
      {"P10ihAuAu7",   {"DbV20100821 P2010a,btof,BEmcChkStat,Corr4,OSpaceZ2,OGridLeak3D,VFMCE TpxClu -VFMinuit -hitfilt",    "ry2010c",       ""}},
      {"P10ikAuAu200", {"DbV20101213 P2010a pmdReco btof BEmcChkStat Corr4 OSpaceZ2 OGridLeak3D VFMCE TpxClu -VFMinuit -hitfilt",  "ry2010c",       ""}},
      {"P11idAuAu200", {"DbV20111124 P2011a pmdReco btof mtdDat BEmcChkStat Corr4 OSpaceZ2 OGridLeak3D VFMCE TpxClu -VFMinuit -hitfilt",  "ry2011",       ""}},
      {"P11idAuAu27",  {"DbV20110911 P2011a btof mtddat pmdReco BEmcChkStat Corr4 OSpaceZ2 OGridLeak3D VFMCE TpxClu -VFMinuit -hitfilt",   "ry2011",       ""}},
      {"P11idAuAu19",  {"DbV20110820 P2011a btof mtddat pmdReco BEmcChkStat Corr4 OSpaceZ2 OGridLeak3D VFMCE TpxClu -VFMinuit -hitfilt",   "ry2011",       ""}},
      {"P11idpp500",   {"DbV20110923 pp2011a btof mtddat fmsdat BEmcChkStat Corr4 OSpaceZ2 OGridLeak3D VFMCE TpxClu -hitfilt",    "ry2011",       ""}},
      {"P12idUU193",   {"DbV20120921,P2012b,AgML,mtdDat,btof,fmsDat,BEmcChkStat,Corr4,OSpaceZ2,OGridLeak3D,VFMCE,TpxClu -VFMinuit -hitfilt",    "ry2012a",       ""}},
      {"P12idpp200",   {"DbV20130212,pp2012b,AgML,mtdDat,btof,fmsDat,BEmcChkStat,Corr4,OSpaceZ2,OGridLeak3D,VFMCE,TpxClu,-hitfilt",    "ry2012a",       ""}},
      {"P14igpp500",   {"DbV20140905,pp2013b,StiHftP,mtd,btof,fmsDat,fgt,fgtPoint,BEmcChkStat,Corr4,OSpaceZ2,OGridLeak3D,VFMCE,TpxClu,-hitfilt,mtdsim",    "ry2013_1c",       " useXgeom "}},
      {"P14iiAuAu15",  {"DbV20150110,P2014a,btof,mtd,BEmcChkStat,Corr4,OSpaceZ2,OGridLeak3D,VFMCE,TpxClu,-VFMinuit,-hitfilt,mtdsim",   "ry2014a",       " useXgeom "}},
      {"P15icAuAu200", {"DbV20150316,P2014a,btof,mtd,mtdCalib,BEmcChkStat,CorrX,OSpaceZ2,OGridLeak3D,VFMCE,TpxClu,-VFMinuit,-hitfilt,mtdsim",  "ry2014a",       " useXgeom "}},
      {"P15ieAuAu200", {"DbV20150504,P2014a,btof,mtd,mtdCalib,pxlHit,istHit,BEmcChkStat,CorrX,OSpaceZ2,OGridLeak3D,VFMCE,TpxClu,-VFMinuit,-hitfilt,mtdsim",  "ry2014a",       " useXgeom "}},
      {"P15ieCuAu200", {"DbV20150529,P2012b,AgML,mtd,btof,fmsDat,BEmcChkStat,Corr4,OSpaceZ2,OGridLeak3D,VFMCE,TpxClu,-VFMinuit,-hitfilt",  "ry2012a",       ""}},
      {"P16idAuAu200", {"DbV20160418,P2014a,btof,mtd,mtdCalib,BEmcChkStat,CorrX,OSpaceZ2,OGridLeak3D,VFMCE,TpxClu,-VFMinuit,-hitfilt,mtdsim",  "ry2014a",       " useXgeom "}},
      {"P16iaAuAu5",   {"DbV20160126,DbV20150920_tpc_Calibrations,P2015b,mtd,btof,BEmcChkStat,CorrX,VFMCE,TpxClu,-VFMinuit,-hitfilt,mtdsim",    "ry2015b",       " useXgeom "}},
      {"P16idpp200",   {"DbV20160418,DbV20161117_TPC_Calibrations,pp2015c,btof,mtd,mtdCalib,pp2pp,fmsDat,fmsPoint,fpsDat,BEmcChkStat,CorrX,OSpaceZ2,OGridLeak3D,VFMCE,TpxClu,-VFPPVnoCTB,-VFPPV,-beamline,-hitfilt,mtdsim",    "ry2015c", " useXgeom "}},
      {"P16idpAu200",  {"DbV20160710,DbV20161117_TPC_Calibrations,pp2015c,btof,mtd,mtdCalib,pp2pp,fmsDat,fmsPoint,fpsDat,BEmcChkStat,CorrX,OSpaceZ2,OGridLeak3D,VFMCE,TpxClu,-VFPPVnoCTB,-VFPPV,-beamline,-hitfilt,mtdsim",   "ry2015c", " useXgeom "}},
      {"P16ibpp500",   {"DbV20110311,OGGVoltErr,pp2009c,ITTF,BEmcChkStat,btof,Corr4,OSpaceZ2,OGridLeak3D,DbV20151021_TOF_Calibrations,DbV20161021_tpc_Calibrations,VFMCE,TpxClu,-hitfilt",    "ry2009d",       ""}},
      {"P16igAuAu200", {"DbV20160406,P2014a,StiCA,btof,mtd,mtdCalib,BEmcChkStat,CorrX,OSpaceZ2,OGridLeak3D,VFMCE,TpxClu,-VFMinuit,-hitfilt,mtdsim",  "ry2014a",       " useXgeom "}},
      {"P16ijAuAu200", {"DbV20161018,P2016a,StiCA,mtd,mtdCalib,btof,BEmcChkStat,CorrX,OSpaceZ2,OGridLeak3D,VFMCE,TpxClu,-VFMinuit,-hitfilt,mtdsim",  "ry2016a",       " useXgeom "}},
      {"P17idAuAu200", {"DbV20150316 DbV20170712_trg_Calibrations,FiltTrg_JetCorrTrgs,P2014a,btof,mtd,mtdCalib,BEmcChkStat,CorrX,OSpaceZ2,OGridLeak3D,VFMCE,TpxClu,-VFMinuit,-hitfilt,mtdsim",  "ry2014a",       " useXgeom "}},
      {"P17iddAu200",  {"DbV20161216,P2016a,StiCA,mtd,mtdCalib,btof,BEmcChkStat,CorrX,OSpaceZ2,OGridLeak3D,VFMCE,TpxClu,-VFMinuit,-hitfilt,mtdsim",   "ry2016a", " useXgeom "}},
      {"P17iddAu39",   {"DbV20170426,P2016a,StiCA,mtd,mtdCalib,btof,BEmcChkStat,CorrX,OSpaceZ2,OGridLeak3D,VFMCE,TpxClu,-VFMinuit,-hitfilt,mtdsim",    "ry2016a", " useXgeom "}},
      {"P17iddAu20",   {"DbV20170426,P2016a,StiCA,mtd,mtdCalib,btof,BEmcChkStat,CorrX,OSpaceZ2,OGridLeak3D,VFMCE,TpxClu,-VFMinuit,-hitfilt,mtdsim",    "ry2016a", " useXgeom "}},

      {"P23idAuAu17", {"DbV20230818 P2021a StiCA BEmcChkStat EbyET0 ODistoSmear VFMCE TpxClu -VFMinuit -hitfilt ", "ry2021a", "useXgeom "}}
    };

    // Return if no such production exists
    auto it = chainMap.find(prodName);
    if (it == chainMap.end()) {
      cout << "Choice prodName " << prodName << " does not correspond to known chain. Processing impossible. " << endl;
      return result;
    }


    // Now we will massage the chains based on the simulation engine involved

    std::map< int, ChainMods> chain1Mods = {
      //    { starsimR5,  { "", "", {} }},
      //    { starsimR6,  { "", "", {} }},
      { geant4star, { " stargen:stubs ", "", {} }}
    };
    
    std::map<int, ChainMods> chain2Mods = {
      //              append  remove  replace    
      //    { starsimR5,  { "", "", {} }},
      //    { starsimR6,  { "", "", {} }},
      //{ geant4star, { " stargen:embed kinematics:embed g4star:mk ", "", {} }}
      { geant4star, { "", "", {{",fzin"," CONFIGG4MK "},{"PrepEmbed"," stargen:embed kinematics:embed g4star:mk "}} }}    
    };
    // n.b. CONFIGG4MK is a placeholder.  We need to setup the G4 simulation rather than running a separate input event generator.
    
    std::map<int, ChainMods> chain3Mods = {
      //              append  remove  replace
      //    { starsimR5,  { "", "", {} }},
      { starsimR6,  { " -IN NoInput ", "", {} }},
      //    { geant4star, { " nodefault ", "", {} }}
      { geant4star, { " nodefault ", "", {} }}
    };
    
    
    
    const auto& config = it->second;
    chain3Opt =  config.prod;
    chain2Opt += config.geom;
    chain1Opt += config.geomMode;
    chain3Opt += ",TpcMixer,GeantOut,MiniMcMk,McAna,-in,NoInput,useInTracker,emcSim,BEmcMixer,EEfs,EEmcMixer,EEss";
    
    // Append first
    chain1Opt += chain1Mods[ simEngine ].append;
    chain2Opt += chain2Mods[ simEngine ].append;
    chain3Opt += chain3Mods[ simEngine ].append;

    // Replace next
    for ( auto p : chain1Mods[ simEngine ].replace ) replace( chain1Opt, p.first, p.second );
    for ( auto p : chain2Mods[ simEngine ].replace ) replace( chain2Opt, p.first, p.second );
    for ( auto p : chain3Mods[ simEngine ].replace ) replace( chain3Opt, p.first, p.second );

    // ... and we can probably dispence with remove, just replace with an empty string.
    
    std::cout << "CHAIN 1: " << chain1Opt.c_str() << std::endl;
    std::cout << "CHAIN 2: " << chain2Opt.c_str() << std::endl;
    std::cout << "CHAIN 3: " << chain3Opt.c_str() << std::endl;

    std::transform(chain1Opt.begin(), chain1Opt.end(), chain1Opt.begin(),  [](unsigned char c){ return std::tolower(c); });
    std::transform(chain2Opt.begin(), chain2Opt.end(), chain2Opt.begin(),  [](unsigned char c){ return std::tolower(c); });
    std::transform(chain3Opt.begin(), chain3Opt.end(), chain3Opt.begin(),  [](unsigned char c){ return std::tolower(c); });

    // replace all commas with whitespace
    while ( replace( chain1Opt, ",", " " ) );
    while ( replace( chain2Opt, ",", " " ) );
    while ( replace( chain3Opt, ",", " " ) );

    // replace all double whitespace with single whitespace
    while ( replace( chain1Opt, "  ", " " ) );
    while ( replace( chain2Opt, "  ", " " ) );
    while ( replace( chain3Opt, "  ", " " ) );

    std::string chain0Opt = " " + chain1Opt + " " + chain2Opt + " " + chain3Opt + " ";

    // replace in, -in, noinput and noutput with nothing.  Careful b/c "in" appears in e.g. vfminuit
    while ( replace( chain0Opt, "noinput", "" ) );
    while ( replace( chain0Opt, "nooutput", "" ) );
    while ( replace( chain0Opt, "-in", "" ) );
    while ( replace( chain0Opt,  " in ", " " ) );

    return { true, chain1Opt, chain2Opt, chain3Opt, chain0Opt };

  }

};

#endif

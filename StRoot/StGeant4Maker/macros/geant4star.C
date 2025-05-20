R__LOAD_LIBRARY(libGeom.so);
R__LOAD_LIBRARY(libVMCLibrary.so);
R__LOAD_LIBRARY(libgeant321.so);

R__LOAD_LIBRARY(libTable.so);
R__LOAD_LIBRARY(StarRoot.so);
R__LOAD_LIBRARY(St_base.so);
R__LOAD_LIBRARY(StStarLogger.so);
R__LOAD_LIBRARY(StChain.so);
R__LOAD_LIBRARY(StBFChain.so);

struct _dummyinit_ {
  _dummyinit_(){
    gROOT->ProcessLine("StLoggerManager::StarLoggerInit();");
  };
} __1;

//class StBFChain;
StBFChain* chain = 0;

#if !defined(__CINT__) && !defined(__CLING__)
#include <TString.h>
#include <iostream>
#endif
#include <StMessMgr.h>

//extern "C" {
//  void Load( const char* lib );
//}

// Random number generator seed
int __rngSeed = 12345;
bool __export = false;
TString __rngName = ""; 
TString __geometry_tag="dev2021"; 

// Add a new maker to the chain
void addMaker( const char* name, const char* maker ) {
  LOG_INFO << "// Adding maker " << maker << endm;
  LOG_INFO << "auto* _" << name << " = new " << maker << endm;
  gROOT->ProcessLine( Form( "auto* _%s = new %s;", name, maker ) );
}

// Add a new event generator to the primary event generator
void addGenerator( const char* name, const char* maker ) {
  LOG_INFO << "auto* _" << name << " = new " << maker << endm;
  gROOT->ProcessLine( Form( "auto* _%s = new %s;", name, maker ) );
  gROOT->ProcessLine( Form( "_primary->AddGenerator( _%s );", name ) );
}

// Add a filter to the primary event generator
void addFilter( const char* name ) {

  TString myname = name; myname.ToLower();
  LOG_INFO << "auto* _" << myname.Data() << " = new " << name << endm;
  gROOT->ProcessLine( Form( "auto* _%s = new %s;", myname.Data(), name ) );
  gROOT->ProcessLine( Form( "_primary->AddFilter( _%s );", myname.Data() ) );

}

std::map<TString,TString> _generatorMap = {
  { "genreader", "StarGenEventReader" }
};

// Hack to make sure finish is called on the chain
struct __Fini {
  ~__Fini() {
    gROOT->ProcessLine("chain->Finish();");
  }
} __fini__;

bool hasRuntimeArg( const char* arg_ ) {
  bool result = false;
  for ( int i=0; i<gApplication->Argc();i++ ) {
    TString arg = gApplication->Argv(i);
    arg.ReplaceAll("--","");
    if ( arg.Contains(arg_) ) {
      result = true;
      break;
    }
  }
  return result;
}

void loadStar(TString mytag="dev2021", Bool_t agml = true  )
{

  gROOT->ProcessLine("chain = new StBFChain();");
  gROOT->ProcessLine("chain->cd();");
  gROOT->ProcessLine("chain->SetDebug(1);");  

//TString chainOpts = "agml geant4 geant3vmc geant4vmc stargen geant4mk kinematics -emc_t -ftpcT mysql nodefault ";
  TString chainOpts = "agml geant4           geant4vmc stargen geant4mk kinematics -emc_t -ftpcT mysql nodefault ";


  // pickup command line options ala "--" and add them as a chain option

  for ( int i=0; i<gApplication->Argc();i++ ) {
    TString arg = gApplication->Argv(i);
    if ( arg.Contains("--web") || arg.Contains("notebook") ) continue;
    if ( arg=="--export" ) { __export = true; continue; } // exports geometry
    // Parse "--" style options for ourselves
    if ( arg.Contains("--") ) {
      arg.ReplaceAll("--"," "); // n.b. the space pads out the chain options
      // If the option matches key=value, treat this as an attribute to be
      // set on the G4 maker...
      if ( arg.Contains("=") ) {
	// Generally skip, but output triggers geantout chain option
	if ( arg.Contains("output") ) {
	  chainOpts += " geant4out ";
	}
	// By specifying a filter, load the stargeneratorfilt package
	if ( arg.Contains("filter") ) {
	  chainOpts += " stargen.filt";	  
	}
      }
      else {
	chainOpts += arg;
      }
    }
  }

  // Set the chain options
  gROOT->ProcessLine(Form("chain->SetFlags(\"%s\");",chainOpts.Data()));

  TString output = "";
  std::vector< std::string > filters;
  for ( int i=0; i<gApplication->Argc();i++ ) {
    TString arg = gApplication->Argv(i);  
    if ( arg.Contains("--") ) {
      gMessMgr->Info() << arg.Data() << endm;
      arg.ReplaceAll("--","");
      // If the option matches key=value, treat this as an attribute to be
      // set on the G4 maker...
      if ( arg.Contains("=") ) {
	TString key = arg.Tokenize("=")->At(0)->GetName();
	TString val = arg.Tokenize("=")->At(1)->GetName();
	//     std::cout << " key = [" << key.Data() << "] value = " << val.Data() << std::endl;
	// Find the output filename, if given, and set as the output
	if ( key=="output" ){
	  output = val;
	  gROOT->ProcessLine(Form("chain->Set_IO_Files(\"\",\"%s\");",output.Data()));
         //break;
	}
        if ( key=="geometry" ) {
	    mytag = val;
	}
	if ( key=="filter" ) {
	  filters.push_back( val.Data() );
	}
      }
    }
  }


  // Load shared libraries
  gROOT->ProcessLine("chain->Load();");

  // Add in star mag field
  gSystem->Load("libStarMagFieldNoDict.so");

  gROOT->ProcessLine( "int __result = chain->Instantiate();" );

  // Now add makers...
  addMaker( "primary", "StarPrimaryMaker()" );
  addMaker( "geant4",  "StGeant4Maker()" );

  // Attach filters to the primary maker...
  for ( auto s : filters ) {
    addFilter( s.c_str() );
  }


  //  addMaker( "pythia8", "StarPythia8()" );
  //  gROOT->ProcessLine("_primary->AddGenerator( _pythia8 );");

  // Always add the kinematic generator
  addMaker( "kine",        "StarKinematics()" );
  gROOT->ProcessLine("_primary->AddGenerator( _kine );");


  // Loop on the chain options and add in other generators which have been called for
  for ( auto _s : *chainOpts.Tokenize(" ") ) {
    auto s = ( dynamic_cast<TObjString*>( _s ) ) -> String() ; // annoying
    if ( _generatorMap[s] != "" ) {
      addGenerator( s, _generatorMap[s] );
    }
  }

  



  // Move outputStream after the geant maker
  gROOT->ProcessLine("StMaker* __outputStream = chain->GetMaker(\"outputStream\");");
  // gROOT->ProcessLine("LOG_INFO << "
  // 		     "\"outputStream = \" << __outputStream << endm;")

  gROOT->ProcessLine("chain->AddAfter( _geant4->GetName(), __outputStream ); ");




  //  gROOT->ProcessLine("auto* __outputStream = chain->GetMaker(\"outputStream\"\);


  gROOT->ProcessLine("StMaker::lsMakers(chain);");


  // set attributes for arguements matching --x=y
  for ( int i=0; i<gApplication->Argc();i++ ) {

    TString arg = gApplication->Argv(i);
    if ( arg.Contains("--web") || arg.Contains("notebook") ) continue;

    // Parse "--" style options for ourselves
    if ( arg.Contains("--") ) {
      arg.ReplaceAll("--","");

      // If the option matches key=value, treat this as an attribute to be
      // set on the G4 maker...                                        
      if ( arg.Contains("=") ) {

	TString key = arg.Tokenize("=")->At(0)->GetName();
	TString val = arg.Tokenize("=")->At(1)->GetName();

	if ( key=="geometry" ) continue; // action already taken
	if ( key=="output"   ) continue; // ... ditto
	if ( key=="filter"   ) continue; // ... ditto

	//	std::cout << "geant4star commandline option " << key.Data() << " = " << val.Data() << std::endl;

	// Process RNG seed
	if ( key=="seed" ) {
	  __rngSeed = val.Atoi();
	  gMessMgr->Info() << "Setting RNG seed --seed=" << __rngSeed << endm;	  
	  continue;
	}

	if ( key=="rng" ) {
	  __rngName = val;
	  gMessMgr->Info() << "Setting RNG --rng=" << __rngName.Data() << endm;	  
	  continue;
	}

	// All other variables pass through to G4 maker
	if ( val.IsFloat() ) 	  gROOT->ProcessLine(Form("_geant4->SetAttr(\"%s\",%s);",     key.Data(), val.Data() ));
	else                   	  gROOT->ProcessLine(Form("_geant4->SetAttr(\"%s\",\"%s\");", key.Data(), val.Data() ));
						     

      }                                                               
    } 
  } 


  //
  // ROOT6 command line processing
  //
  const char* cmds[] = { 

    // Geometry instantiation
    //"TString __geometry_tag = \"dev2021\";"
    "AgModule::SetStacker( new StarTGeoStacker() );"
    "StarGeometry::Construct(__geometry_tag);"
    // "gGeoManager->Export(\"y2014x.root\");"
    // "gGeoManager->Export(\"y2014x.C\");"

  };

  gROOT->ProcessLine( Form("TString __geometry_tag = \"%s\";", mytag.Data() ) );

  for ( auto cmd : cmds ) {
    gROOT->ProcessLine( cmd );
  }

  if ( __export ) {
    gROOT->ProcessLine(Form( "gGeoManager->Export(\"%s.root\");", mytag.Data() ));
  }

  


}

bool __initialized = false;

bool initChain( std::vector<std::string> _cmds={ "std::cout << \"Chain has been initialized.\" << std::endl;" } ) {
  std::cout << "initChain is called seed = " << __rngSeed << std::endl;
  if ( !__initialized ) { 

    if ( __rngSeed > -1 ) {
      gROOT->ProcessLine(Form("chain->SetAttr(\"Random:G4\",%i)",__rngSeed));
      // Setup RNG seed and map all ROOT TRandom here
      gROOT->ProcessLine(Form("StarRandom::seed( %i );",__rngSeed));
      gROOT->ProcessLine("StarRandom::capture();"); 
      gMessMgr->Info() << "RNG seed set to " << __rngSeed << endm;
    }

    gROOT->ProcessLine("chain->Init();"); 
    for ( auto cmd : _cmds ) {
      gROOT->ProcessLine( cmd.c_str() );
    }
    __initialized = true; 
  }
  return true;
}

void particleGun( const int ntrack=1, const char* particles="pi+,pi-", double ptmn=1.0,double ptmx=10.0, double etamn=-1, double etamx=2 ) {

  initChain();

  const char* _cmds[] = {

    // Clear the chain from the previous event
    "chain->Clear();",
    Form("_kine->Kine(%i,\"%s\",%f,%f,%f,%f);",ntrack,particles,ptmn,ptmx,etamn,etamx),
    "chain->Make();"
  };

  for ( auto cmd : _cmds ) {
    gROOT->ProcessLine( cmd );
  }

};


void particleGun( const char* particle="mu+", double px=1.0/sqrt(2), double py=1.0/sqrt(2), double pz=0.0, double vx=0., double vy=0., double vz=0. ) {

  initChain();

  const char* _cmds[] = {
    "chain->Clear();",
    "{",
    Form("double _px=%f",px),
    Form("double _py=%f",py),
    Form("double _pz=%f",pz),
    Form("auto   _part=_kine->AddParticle(\"%s\");",particle),
    "double _mass = _part->GetMass();",
    "double _energy = sqrt( _px*_px+_py*_py+_pz*_pz+_mass*_mass );",
    "_part->SetPx(_px);",
    "_part->SetPy(_py);",
    "_part->SetPz(_pz);",
    "_part->SetEnergy(_energy);",
    "_part->SetVx(_vx);",
    "_part->SetVy(_vy);",
    "_part->SetVz(_vz);",
    "_part->SetTof(0);",
    "chain->Make();",    
    "}"
  };

  for ( auto cmd : _cmds ) {
    gROOT->ProcessLine( cmd );
  }

};




void geant4star(){ 
  TString cmdline="geant4star:";
  for ( int i=0;i<gApplication->Argc();i++ ) {
    cmdline+=" ";
    cmdline+=gApplication->Argv(i); 
  }
  std::cout << cmdline.Data() << std::endl;
  loadStar(); 

  gROOT->ProcessLine(".I");
  gROOT->ProcessLine("gSystem->GetLibraries()");
  gROOT->ProcessLine("gSystem->GetLinkedLibs()");

}

// Use StBFChain to load STAR framework plus geant4 toolkit

#include <TString.h>
#include <TSystem.h>
#include <TGeoManager.h>

void Load( const char* lib ) {
  std::cout << "Loading shared library " << lib << std::endl;
  gSystem->Load( lib );  
}

void initStar() {

  for (int sig = 0; sig < kMAXSIGNALS; sig++) gSystem->ResetSignal((ESignals)sig);

  // Add a few things to the include path
  gSystem->AddIncludePath(" -I$STAR/StRoot -I$STAR/StarVMC ");
  gSystem->AddIncludePath(" -IStRoot -Igeom -IStarVMC -IStarVMC/Geometry/macros -I.${STAR_HOST_SYS}/include ");
  gEnv->SetValue("Logger.Colors","YES");   

  gSystem->SetAclicMode(TSystem::kDebug);

  // ADdress sanitizer 
  //  Load("/usr/lib64/libasan.so.0"); 
  
  // Preload a few libraries
  Load("libGeom.so");

  Load("libPhysics.so");
  Load("libTable.so");

  Load("StarRoot.so"); 
  Load("St_base.so"); 
  Load("StStarLogger.so"); 

  gROOT->ProcessLine("StLoggerManager::StarLoggerInit();");

  Load("StChain.so"); 
  Load("StBFChain.so"); 

  // And preload the system's mysql library
  //TString mysqldir = gSystem->Getenv("__G4STAR_MYSQL_DIR__");
  TString mysqldir = "/usr/lib64/mysql";
  Load( mysqldir + "/libmysqlclient.so" );
  

};

void loadGeant4Vmc() {

  // Initialize STAR framework
  initStar();

  // Create new chain
  gROOT->ProcessLine("chain = new StBFChain();");
  gROOT->ProcessLine("chain->cd();");
  gROOT->ProcessLine("chain->SetDebug(1);");  
  
  TString chainOpts = "agml geant4 geant4vmc nodefault ";

  // Set the chain options
  gROOT->ProcessLine(Form("chain->SetFlags(\"%s\");",chainOpts.Data()));

  // Load shared libraries
  gROOT->ProcessLine("chain->Load();");

  // Add in star mag field
  Load("libStarMagFieldNoDict.so");

  gROOT->ProcessLine( "int __result = chain->Instantiate();" );


};

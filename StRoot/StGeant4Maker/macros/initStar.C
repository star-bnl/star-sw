#if !defined(__CINT__) && !defined(__CLING__)
#include <TString.h>
#include <TSystem.h>
#include <TGeoManager.h>
#endif

void Load( const char* lib ) {
  std::cout << "Loading shared library " << lib << std::endl;
  gSystem->Load( lib );  
}

//
// Add include paths and preload the shared libraries which are required for the geant4star.C
// macro to instantiate StBFChain.
//
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

  //  Load("libPhysics.so");
  Load("libTable.so");

  Load("StarRoot.so"); 
  Load("St_base.so"); 
  Load("StStarLogger.so"); 

  gROOT->ProcessLine("StLoggerManager::StarLoggerInit();");

  Load("StChain.so"); 
  Load("StBFChain.so"); 
  
};


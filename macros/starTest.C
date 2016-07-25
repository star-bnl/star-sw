// $Id: starTest.C,v 1.1 2006/01/21 17:51:40 fisyak Exp $
//
// Macro for running Example02 with Geant3 
// Before running this macro, the libexampl02.so library
// must have been built. To build it, go to your geant4_vmc/examples directory
// and run make.
// Note that this macro is a simplified version of the equivalent macro
// in the geant4_vmc/examples/E02 directory

const char *geoFile=0; // default
void starTest(const char *geofil=0){
  char config;
  geoFile = geofil;
  if (!geoFile) {
//  geoFile = "/star/u/potekhin/gstardata/max_y2003x-svt.root";
//  geoFile = "/star/u/potekhin/gstardata/yura_y2003x-svt.root";
//  geoFile = "/star/u/potekhin/gstardata/y2003x_tpc_only.root";
//  geoFile = "tpc_only_g2r.root";
//    geoFile = "../VMC/y2003x-svt.root";
//    geoFile = "../VMC/y2003x_complete.root";
    geoFile = "y2003x_complete.root";
}
    cout<<"Reading the geometry from the file: "<< geoFile <<endl;

//  cout<<"Enter f for reading the geometry from a file, or any other letter otherwise: ";
//  cin>>config;

  cout<<"Loading components..."<<endl;

  
  // gSystem->Load("libGpad");
  gSystem->Load("libGeom");
  gSystem->Load("libVMC");
  gSystem->Load("libPhysics");
  gSystem->Load("libEG"); 
  gSystem->Load("libEGPythia6");
  gSystem->Load("libPythia6");  
  gSystem->Load("libGui");  
  
//  if(config!='f') {
    cout<<"Creating Default Geometry"<<endl;
//  }
  
  cout<<"Loading Geant3 libraries..."<<endl;

  gSystem->Load("minicern.so");
  gSystem->Load("StVmcTools.so");
//VP  gSystem->Load("StG3Dummy.so");
  gSystem->Load("StGeant321.so");
  gSystem->Load("StTGeant3.so");
  gSystem->Load("StMCAppl.so");
  
  // Load the application-specific infrastructure lib
  //VP gSystem->Load("../../geant4_vmc/lib/tgt_Linux/libvmcApp.so");

  // MC application
  StMCApplication* appl = new StMCApplication("StExample01", "The STAR example01 VMC application");


  cout <<"================================= Before InitMC ============================="<<endl;

  // The "Init" method in the gMC object causes the geometry to be cosntructed
  appl->InitMC("starTestConfig.C");

  cout <<"================================= Past   InitMC ============================="<<endl;
  // Conduct the simulation:
  StTGeant3 *geant3 = (StTGeant3*)gMC;
  appl->SetField(new StMCField);
  appl->SetGeneratePrimaries(new StMCGeneratePrimariesTest(1000));
  appl->SetStepping(new StMCSteppingHist("tgeom"));
//  appl->SetStepping(new StMCStepping);

  geant3->SetDEBU(1,0,100);
  geant3->SetSWIT(4,0);
  geant3->SetSWIT(1,2);
  geant3->SetSWIT(2,2);

  TStopwatch sw;;
  appl->RunMC(1);
  sw.Print();
  StMCSteppingHist::Instance()->Finish();
  
  }  

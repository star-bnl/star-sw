// $Id: starTest.C,v 1.4 2004/07/16 22:50:34 potekhin Exp $
// $Log: starTest.C,v $
// Revision 1.4  2004/07/16 22:50:34  potekhin
// Added a log tag
//


{
  TString config;


  //  cout<<"Configuration file: (star.config is default) ";
  //  config.ReadToken(cin);

  if(config.Length()==0) config="star.config";

  cout<<"Loading components..."<<endl;

  gSystem->Load("libGeom");
  gSystem->Load("libVMC");
  gSystem->Load("libPhysics");
  gSystem->Load("libEG"); 
  gSystem->Load("libEGPythia6");
  gSystem->Load("libPythia6");  
  
  
  cout<<"Loading Geant3 libraries..."<<endl;

  gSystem->Load("~/geant3/lib/tgt_linux/libdummies.so");
  gSystem->Load("~/geant3/lib/tgt_linux/libgeant321.so");
  
  // Load the application-specific infrastructure lib
  TString star_lib = getenv("STAR_LIB");
  TString StMcEvent = star_lib;
  TString St_base   = star_lib;
  TString StarClassLibrary = star_lib;

  StMcEvent+="/StMcEvent.so";
  St_base  +="/St_base.so";
  StarClassLibrary+="/StarClassLibrary.so";

  gSystem->Load("../lib/.rh80_gcc32/libvmcApp.so");
  gSystem->Load("../lib/.rh80_gcc32/libtpc.so");
  gSystem->Load("../lib/.rh80_gcc32/libecal.so");
  gSystem->Load("../lib/.rh80_gcc32/libinterfaces.so");

  //  gSystem->Load(St_base);
  //  gSystem->Load(StarClassLibrary);
  //  gSystem->Load(StMcEvent);


  // Configure various parameters
  StarConfiguration::setConFile(config);
  StarConfiguration::parse();
  StarConfiguration::print();

  // MC application
  cout<<"Constructing the VMC application"<<endl;
  StarMCApplication* appl =
    new StarMCApplication("StarMCApplication", "Testing the STAR VMC application");

  StarTpc* tpc = new StarTpc();
  cout<<"tpc "<<tpc<<endl;
  appl->AddModule(tpc);

  StarEcal* ecal = new StarEcal();
  cout<<"ecal "<<ecal<<endl;
  appl->AddModule(ecal);

  TGeant3* geant3 = new TGeant3("TGeant3"); 
  cout << "TGeant3 object has been created." << endl;

  if(StarConfiguration::isExternal())  geant3->SetRootGeometry();

  geant3->SetDRAY(1);
  geant3->SetHADR(1);
  geant3->SetLOSS(1);
  geant3->SetMULS(1);
  geant3->SetPHOT(1);
  geant3->SetCOMP(1);
  geant3->SetBREM(1);
  geant3->SetANNI(1);
  geant3->SetDCAY(1);
  geant3->SetPAIR(1);
  geant3->SetRAYL(1);

  //  geant3->SetProcess("PAIR",0.000000001);
  //  geant3->SetProcess("DRAY",0.000000001);
  //  geant3->SetProcess("BREM",0.000000001);

  //  geant3->SetDEBU(1,2,1);
  //  geant3->SetSWIT(2,2);

  appl->InitMC();
  appl->InitDisplay();
  appl->SetFinishEventCB(StMcEventInterface::FinishEventCB);


  system("date");
  appl->RunMC(StarConfiguration::getTriggers());
  system("date");
}  

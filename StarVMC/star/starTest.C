// $Id: starTest.C,v 1.1 2004/07/12 20:16:31 potekhin Exp $
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
  gSystem->Load("../lib/.rh80_gcc32/libvmcApp.so");
  gSystem->Load("../lib/.rh80_gcc32/libtpc.so");

  // MC application
  cout<<"Constructing the VMC application"<<endl;
  StarMCApplication* appl =
    new StarMCApplication("StarMCApplication", "Testing the STAR VMC application");

  StarConfiguration::setConFile(config);
  StarConfiguration::parse();
  StarConfiguration::print();

  StarTpc* tpc = new StarTpc();
  cout<<"tpc "<<tpc<<endl;
  appl->AddModule(tpc);

  TGeant3* geant3 = new TGeant3("TGeant3"); 
  cout << "TGeant3 object has been created." << endl;

  if(StarConfiguration::isExternal())  geant3->SetRootGeometry();

  geant3->SetHADR(0);
  appl->InitMC();



  system("date");
  appl->RunMC(StarConfiguration::getTriggers());
  system("date");
}  

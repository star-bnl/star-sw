// $Id: maxim.C,v 1.1 2005/01/11 15:07:53 fisyak Exp $
//
// Macro for running Example02 with Geant3 
// Before running this macro, the libexampl02.so library
// must have been built. To build it, go to your geant4_vmc/examples directory
// and run make.
// Note that this macro is a simplified version of the equivalent macro
// in the geant4_vmc/examples/E02 directory

void maxim() {
  // Load basic libraries
  gSystem->Load("libGeom");
  gSystem->Load("libVMC");
  gSystem->Load("libPhysics");
  gSystem->Load("libEG"); 
  gSystem->Load("libEGPythia6");
  gSystem->Load("libPythia6");  
  //  new TGeoManager("E02","test"); 
  // Load Geant3 libraries
  
  gSystem->Load("/afs/rhic.bnl.gov/star/ROOT/4.99.99/.rh80_gcc32/rootdeb/geant3/lib/tgt_Linux/libdummies.so");
  
 
  gSystem->Load("/afs/rhic.bnl.gov/star/ROOT/4.99.99/.rh80_gcc32/rootdeb/geant3/lib/tgt_Linux/libgeant321.so");
  
  // Load this example library
  gSystem->Load("~potekhin/geant4_vmc/lib/tgt_Linux/libexample02.so");

  // MC application
  Ex02MCApplication* appl = new Ex02MCApplication("Example02", "The example02 VMCapplication");

  appl->InitMC("E02_g3Config.C");
  TGeant3 *geant3 = (TGeant3*)gMC;
  geant3->SetDEBU(1,2,1);
  geant3->SetSWIT(2,2);
  appl->RunMC(5);
}  

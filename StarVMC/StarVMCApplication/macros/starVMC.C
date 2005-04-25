class StarVMCApplication;
StarVMCApplication* appl = 0;
class TGeant3TGeo;
TGeant3TGeo* geant3 = 0;
//________________________________________________________________________________
TGeoManager  *GetVMC() {
#ifndef __NOVMC__
  /*! Load Geometry
   */
  if (gGeoManager) return gGeoManager;
#ifdef __ROOT__
  cout << "StiVMCToolKit::GetVMC() -I- Get VMC geometry" <<endl;
  if (! StMaker::GetChain()) {
    cout << "StiVMCToolKit::GetVMC() -I- There is no chain. Get geometry for y2005x" <<endl;
    gInterpreter->ProcessLine(".L $STAR/StarDb/VmcGeometry/Geometry.y2005x.C");
    gInterpreter->Calc("CreateTable()");
    //    TFile::Open("$STAR/StarDb/VmcGeometry/Geometry.y2005x.root");
  } else {
    StMaker::GetChain()->GetDataBase("VmcGeometry");
  }
#else
  if (TString(gDirectory->GetName()) == "Rint") {
    cout << "StiVMCToolKit::GetVMC() -I- There is no chain. Get geometry for y2005x" <<endl;
    gROOT->LoadMacro("$STAR/StarDb/VmcGeometry/Geometry.y2005x.C");
    gInterpreter->Calc("CreateTable()");
    //    TFile::Open("$STAR/StarDb/VmcGeometry/Geometry.y2005x.root");
    //    if (! gGeoManager) gDirectory->Get("Geometry");
  }
#endif
  if (! gGeoManager) 
    cout << "StiVMCToolKit::GetVMC() -E- Can't get VMC geometry" <<endl;
  return gGeoManager;
#else
  return 0;
#endif
}
//________________________________________________________________________________
void Load() {
  //  gSystem->Load("libGeom");
  gSystem->Load("libVMC");
  //  gSystem->Load("libPhysics");
  //  gSystem->Load("libEG"); 
  gSystem->Load("libEGPythia6");
  gSystem->Load("libPythia6");  
  //  gSystem->Load("libGui");  
  //  gSystem->Load("minicern");
  //  gSystem->Load("StVmcTools");
  //VP  gSystem->Load("StG3Dummy");
  //  gSystem->Load("StGeant321");
  //  gSystem->Load("StTGeant3");
  //  gSystem->Load("TGeant3");
  gSystem->Load("libdummies");
  gSystem->Load("libgeant3");
  gSystem->Load("StarMagField");
  gSystem->Load("StarVMCApplication");
  gSystem->Load("libgeometry_Tables");
  gSystem->Load("libsim_Tables");
}
//________________________________________________________________________________
void starVMC(){
  Load();
  if (! GetVMC()) return;
  appl = new StarVMCApplication("StarVMC", "The STAR VMC application");
  geant3 = new TGeant3TGeo("C++ Interface to Geant3");//, 1, 200000); 
  
  cout << "Geant3 has been created." << endl;
  geant3->SetProcess("DCAY", 0);
  geant3->SetProcess("ANNI", 0);
  geant3->SetProcess("BREM", 0);
  geant3->SetProcess("COMP", 0);
  geant3->SetProcess("HADR", 0);
  geant3->SetProcess("MUNU", 0);
  geant3->SetProcess("PAIR", 0);
  geant3->SetProcess("PFIS", 0);
  geant3->SetProcess("PHOT", 0);
  geant3->SetProcess("RAYL", 0);
  geant3->SetProcess("LOSS", 4); // no fluctuations 
  //  geant3->SetProcess("LOSS 1"); // with delta electron above dcute
  geant3->SetProcess("DRAY", 0);
  geant3->SetProcess("MULS", 0);
  geant3->SetProcess("STRA", 0);
  geant3->SetCut("CUTGAM",	1e-3  );
  geant3->SetCut("CUTELE", 	1e-3  );
  geant3->SetCut("CUTHAD", 	.001  );
  geant3->SetCut("CUTNEU", 	.001  );
  geant3->SetCut("CUTMUO", 	.001  );
  geant3->SetCut("BCUTE", 	.001  );
  geant3->SetCut("BCUTM", 	.001  );
  geant3->SetCut("DCUTE", 	1e-3  );
  geant3->SetCut("DCUTM", 	.001  );
  geant3->SetCut("PPCUTM", 	.001  );
  geant3->SetCut("TOFMAX", 	50.e-6);
  cout <<"================================= Before InitMC ============================="<<endl;
#if 1
  // The "Init" method in the gMC object causes the geometry to be cosntructed
  appl->InitMC();
  
  cout <<"================================= Past   InitMC ============================="<<endl;
  // Conduct the simulation:
  TGeant3TGeo *geant3 = (TGeant3TGeo*)gMC;
  //  geant3->SetRootGeometry();
  if (! StarMagField::Instance()) new StarMagField();
  appl->SetMagField(StarMagField::Instance());
  //                                              Ntrack Id Ptmin Ptmax Ymin Ymax Phimin Phimax Zmin Zmax
  appl->SetPrimaryGenerator(new StarMCPrimaryGenerator(1, 5,   1.,     1.,0.1, 0.1, 0.57,  0.57,  0.,   0.));
  appl->SetStepping(StarMCHits::instance());
  //  appl->SetStepping(new StMCSteppingHist("tgeom"));
  //  appl->SetStepping(new StMCStepping);

  geant3->SetDEBU(1,1,100);
  geant3->SetSWIT(4,0);
  geant3->SetSWIT(1,2);
  geant3->SetSWIT(2,2);
  TStopwatch sw;;
  appl->InitGeometry();
#if 1
  appl->RunMC(1);
  sw.Print();
#endif
  //  StMCSteppingHist::Instance()->Finish();
#endif
}  

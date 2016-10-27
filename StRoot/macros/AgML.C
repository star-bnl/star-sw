void AgML(const Char_t *tag="y2013_2", const Char_t *geom="") {
  TString Tag(tag);
  TString Geom(geom);
  if (Tag == "") {
    cout << "Tag has not defined" << endl;
    exit(1);
  }
  if (Geom == "") Geom = Tag;
#if 1
  gSystem->Load("St_base");
  if (gSystem->Load("liblog4cxx.so") >=  0) {             //  StMemStat::PrintMem("load log4cxx");
    cout << " + liblog4cxx.so";
    if(gSystem->Load("libStStarLogger.so") >= 0) {              //  StMemStat::PrintMem("load log4cxx");
      cout << " + libStStarLogger.so";
    }
  }
  gSystem->Load("StarAgmlUtil");
  gSystem->Load("StarAgmlLib");
  //  gSystem->Load("StarGeometry");
  gSystem->Load("Geometry");
  gSystem->AddIncludePath(" -IStRoot -Igeom -IStarVMC -IStarVMC/Geometry/macros -I$STAR/StRoot -Igeom -I$STAR/StarVMC -I$STAR/StarVMC/Geometry/macros ");
  gErrorIgnoreLevel=9999;                        // Silence ROOT warnings for now
  gGeoManager = new TGeoManager(Geom.Data(),Form("%s/AgML",Geom.Data()));
  AgBlock::SetStacker( new StarTGeoStacker() );  // Creates TGeo geometry
  Geometry *build = new Geometry();                        // Instantiate the geometry
  build -> ConstructGeometry ( Geom.Data() );            

#else
  gROOT->LoadMacro("./StarVMC/Geometry/macros/loadStarGeometry.C");
  loadStarGeometry(geom);
#endif
  gGeoManager->CloseGeometry();
  //  gGeoManager->Export(Form("%s.root",Geom.Data()));
  TObjectSet *geomOS = new TObjectSet("Geometry",gGeoManager,kFALSE);
  TFile *fOut = new TFile(Form("Geometry.%s.root",Tag.Data()),"recreate");
  geomOS->Write();
  fOut->Close();
  TCollection::StartGarbageCollection();
  delete fOut;
  delete gGeoManager;
}

void AgML(const Char_t *tag="y2013_2", const Char_t *geom="") {
  TString Tag(tag);
  TString Geom(geom);
  if (Tag == "") {
    cout << "Tag has not defined" << endl;
    exit(1);
  }
  if (Geom == "") Geom = Tag;
  gSystem->Load("StarAgmlLib");
  //  gSystem->Load("StarGeometry");
  gSystem->Load("Geometry");
  gSystem->AddIncludePath(" -IStRoot -Igeom -IStarVMC -IStarVMC/Geometry/macros -I$STAR/StRoot -Igeom -I$STAR/StarVMC -I$STAR/StarVMC/Geometry/macros ");
  gErrorIgnoreLevel=9999;                        // Silence ROOT warnings for now
  gGeoManager = new TGeoManager(Geom.Data(),Form("%s/AgML",Geom.Data()));
  AgBlock::SetStacker( new StarTGeoStacker() );  // Creates TGeo geometry
  Geometry *build = new Geometry();                        // Instantiate the geometry
  build -> ConstructGeometry ( Geom.Data() );            

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

void lAgML(const Char_t *tag="y2013_2") {
  gSystem->Load("StarAgmlLib");
  //  gSystem->Load("StarGeometry");
  gSystem->Load("Geometry");
  gSystem->AddIncludePath(" -IStRoot -Igeom -IStarVMC -IStarVMC/Geometry/macros -I$STAR/StRoot -Igeom -I$STAR/StarVMC -I$STAR/StarVMC/Geometry/macros ");
  gErrorIgnoreLevel=9999;                        // Silence ROOT warnings for now
  gGeoManager = new TGeoManager(tag,Form("%s/AgML",tag));
  AgBlock::SetStacker( new StarTGeoStacker() );  // Creates TGeo geometry
  Geometry *build = new Geometry();                        // Instantiate the geometry
  build -> ConstructGeometry ( tag );            

  gGeoManager->CloseGeometry();
  gGeoManager->Export(Form("%s.root",tag));
  
}

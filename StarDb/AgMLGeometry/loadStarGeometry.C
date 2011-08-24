class Geometry;
Geometry *build = 0;

void loadStarGeometry( const Char_t *mytag="y2009a" )
{

  TString tag = mytag;
  gSystem->AddIncludePath(" -IStRoot -Igeom -IStarVMC -IStarVMC/Geometry/macros ");

  gROOT   -> LoadMacro("Load.C");

  Load("StarAgmlLib.so");                        // AgML support library
  Load("libGeometry.so");                        // Geometry Steering
  Load("libStarGeometry.so");                    // Geometry Modules

  gErrorIgnoreLevel=9999;                        // Silence ROOT warnings for now

  AgBlock::SetStacker( new StarTGeoStacker() );  // Creates TGeo geometry

  build = new Geometry();                        // Instantiate the geometry
  build -> ConstructGeometry ( tag );            

  gGeoManager->CloseGeometry();

  gErrorIgnoreLevel=0;                           // Enable ROOT warnings

  return;

}

class Geometry;
Geometry *build = 0;

void loadStarGeometry( const Char_t *mytag="y2009a" )
{

  TString tag = mytag;
  gSystem->AddIncludePath(" -IStRoot -Igeom -IStarVMC -IStarVMC/Geometry/macros ");
  gErrorIgnoreLevel=9999;                        // Silence ROOT warnings for now

  if ( gGeoManager==NULL ) 
    {
      gGeoManager = new TGeoManager(mytag,Form("%s/AgML",mytag));
    }

  AgBlock::SetStacker( new StarTGeoStacker() );  // Creates TGeo geometry

  ///////////////////////////////////////////////////////////
  //
  // This is the name of the file in which we cache the AgML
  // geometry
  //
  TString filename = ((StBFChain*)StMaker::GetTopChain())->GetFileOut();
  filename.ReplaceAll(".root",".geom.root");

  ///////////////////////////////////////////////////////////
  //
  // If there is an existing AgML geometry, load from a cached file
  //
  if ( AgModule::Find("HALL") )
    {
      std::cout << Form(">>> AgML geometry detected.  Loading from %s <<<",filename.Data()) << std::endl;      
      gGeoManager = 0;// Prevent ROOT deleting existing geometry
      TGeoManager::Import( filename );
      assert(gGeoManager);
      return;
    }

  build = new Geometry();                        // Instantiate the geometry
  build -> ConstructGeometry ( tag );            

  /**
  gGeoManager->CloseGeometry();
  gGeoManager->Export(Form("%s.C",mytag));
  **/

  gGeoManager->CloseGeometry();


  TFile *file = new TFile( filename, "recreate" );
  file->cd();
  gGeoManager->Write();
  file->Close();
  delete file;

  gErrorIgnoreLevel=0;                           // Enable ROOT warnings

  return;

}

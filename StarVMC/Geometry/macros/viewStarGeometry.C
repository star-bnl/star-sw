void viewStarGeometry( const Char_t *tag="y2011", const Char_t *addons="PipeGeo;" )
{
 
  cacheGeometry(tag, addons);

  //
  // Load using TEveManager
  //
  TEveManager::Create();
  gEve -> RegisterGeometryAlias("Default", Form("%s.root",tag));
  gGeoManager = gEve->GetDefaultGeometry();

  TGeoNode *CAVE = gGeoManager->FindVolumeFast("HALL")->FindNode("CAVE_1");
  TEveGeoTopNode *cave = new TEveGeoTopNode( gGeoManager, CAVE );
  gEve -> AddGlobalElement(cave);

  gEve->Redraw3D(kTRUE);
  

}

void cacheGeometry( const Char_t *tag, const Char_t *addons )
{  
  TFile *file = new TFile(Form("%s.root",tag));
  if ( file->IsZombie() )
    {
      delete file;
      gROOT -> ProcessLine(".L StarVMC/Geometry/macros/loadStarGeometry.C");

      // Load development geometry
      loadDevStarGeometry(tag);

      TString    addOns = addons;
      TObjArray *array  = addOns.Tokenize(" ,;");
      
      for ( Int_t i = 0; i<array->GetLast(); i++ )
	{
	  TObjString *str = (TObjString *)array->At(i);
	  addModule ( str->String() );
	}
      

      // Close the geometry
      gGeoManager->CloseGeometry();


      ColorScheme();
      gGeoManager->Export(Form("%s.root",tag));
    }
  delete file;
  gROOT -> Reset();
}

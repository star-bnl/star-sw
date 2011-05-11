void viewStarGeometry( const Char_t *tag="y2012dev" )
{
  
  // 
  // Create a ROOT file containing our geometry if we can't
  // find it in the local directory
  //
  {
    TFile *file = new TFile(Form("%s.root",tag));
    if ( file->IsZombie() )
      {
	delete file;
	gROOT -> ProcessLine(".L StarVMC/Geometry/macros/loadStarGeometry.C");
	loadStarGeometry(tag);
	ColorScheme();
	gGeoManager->Export(Form("%s.root",tag));
	gROOT -> Reset();	
      }
    delete file;
  }



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

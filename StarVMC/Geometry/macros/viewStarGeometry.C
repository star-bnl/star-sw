TGLViewer* viewer;

Bool_t nocache = false;
Bool_t viewall = false;

void viewStarGeometry( const Char_t *tag="y2011", const Char_t *addons="",
		       const Char_t *TOP="HALL",
		       const Char_t *NODE="CAVE_1"
		       )
{
 
  cacheGeometry(tag, addons);

  // Load using TEveManager
  TEveManager::Create();

  // Get the default viewe
  viewer = gEve->GetDefaultGLViewer(); // Default

  // Register geometry
  gEve -> RegisterGeometryAlias("Default", Form("%s.root",tag));
  gGeoManager = gEve->GetDefaultGeometry();
  
  
  // Get the specified top volume and node to draw
  TGeoNode *CAVE = gGeoManager->FindVolumeFast(TOP)->FindNode(NODE);
  TEveGeoTopNode *cave = new TEveGeoTopNode( gGeoManager, CAVE );
  gEve -> AddGlobalElement(cave);

  
  // Draw the scene
  gEve->Redraw3D(kTRUE);
  
  if ( viewall ) {
  addDetectorTab( "TPCE_1", "TPC"  );

  addDetectorTab( "CALB_1", "BEMC" );
  {
    addDetectorTab( "CPHI_1", "BEMC module", "CHLV" );
  }
  addDetectorTab( "ECAL_1", "EEMC" );
  addDetectorTab( "FBOX_1", "FPD" );
  addDetectorTab( "FBO1_3", "FMS N" );
  addDetectorTab( "FBO2_4", "FMS S" );
  addDetectorTab( "IDSM_1", "IDSM" );
  {
    addDetectorTab( "FGTM_1", "FGT", "IDSM" );
  }

  addDetectorTab( "BBCM_1", "BBC" );
  addDetectorTab( "MUTD_1", "MTD" );
  addDetectorTab( "BTOF_1", "TOF", "CAVE", 10  );
  {
    addDetectorTab("BTRA_1","TOF tray", "BSEC", 10);
  }

  addDetectorTab( "FTPC_1", "FTPC" );
  addDetectorTab( "FTPC_1", "FTPC", "SVTT" );
  addDetectorTab( "FTRO_1", "FTPC readout" );
  addDetectorTab( "SVTT_1", "SVTT" );
  addDetectorTab( "SCON_1", "SCON" );
  addDetectorTab( "SCON_1", "SCON", "SVTT" );

  addDetectorTab( "FSCE_1", "FSCE" );
  addDetectorTab( "ETTV_1", "EIDD" );

  addDetectorTab( "PIPE_1", "pipe" );
  addDetectorTab( "MAGP_1", "magnet" );



  }//viewall

  const Char_t *path  = ".:./StarVMC/Geometry/macros/:$STAR/StarVMC/Geometry/macros/";
  Char_t *file = gSystem->Which(path,"applyColorScheme.C",kReadPermission);  
  //  gROOT->ProcessLine(".L applyColorScheme.C+");
  //  gROOT->ProcessLine(Form(".L %s+",file));
  gSystem->CompileMacro( file, "k-", "libApplyColorScheme", "/tmp" );
  applyColorScheme("CAVE");

}


void addDetectorTab( const Char_t *name, 
		     const Char_t *title, 
		     const Char_t *_top="CAVE",
		     const Int_t   vis = 5 )
{

  TGeoVolume *top = gGeoManager->FindVolumeFast(_top);
  if (!top)
    {
      cout << Form("Top volume %s not found",_top) << endl;
      return;
    }

  TGeoNode       *node = top -> FindNode(name);
  if ( !node )
    {
      cout << Form("Node %s not found",name) << endl;
      return;
    }

  TEveViewer *viewer = gEve -> SpawnNewViewer( title );
  TEveScene  *scene  = gEve -> SpawnNewScene ( title );
  viewer -> AddScene( scene );

  TGLViewer *v3 = viewer->GetGLViewer();
  v3->SetClearColor(33);
  
  TEveGeoTopNode *edon = new TEveGeoTopNode( gGeoManager, node );
  {
    edon -> SetVisLevel( vis );
  }
  
  gEve -> AddGlobalElement( edon, scene );

}






void cacheGeometry( const Char_t *tag, const Char_t *addons )
{  
  TFile *file = new TFile(Form("%s.root",tag));
  if ( file->IsZombie() || nocache )
    {

      delete file;

      const Char_t *path  = ".:./StarVMC/Geometry/macros/:$STAR/StarVMC/Geometry/macros/";
      Char_t *file = gSystem->Which(path,"loadStarGeometry.C",kReadPermission);
      cout << "Loading macro: " << file << endl;
      gROOT -> ProcessLine(Form(".L %s",file));

      // Load development geometry
      //      loadDevStarGeometry(tag);
      loadStarGeometry(tag);

      TString    addOns = addons;
      TObjArray *array  = addOns.Tokenize(" ,;");
      
      for ( Int_t i = 0; i<array->GetEntries(); i++ )
	{
	  TObjString *str = (TObjString *)array->At(i);
	  cout << "Adding module " << str->String().Data() << endl;
	  addModule ( str->String() );
	}
      

      // Close the geometry
      gGeoManager->CloseGeometry();


      ColorScheme();
      gGeoManager->Export(Form("%s.root",tag));
    }
  delete file;
  // gROOT -> Reset();

}

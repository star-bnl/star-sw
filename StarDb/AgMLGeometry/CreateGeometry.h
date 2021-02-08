/**
 * Adapted from StarDb/VmcGeometry/CreateGeometry.h
 */
TDataSet *CreateGeometry(const Char_t *name="y2011") {
  TObjectSet *geom = 0;
  if ( gGeoManager ) { 
    cout << "AgML geometry:  Existing TGeoManager " << gGeoManager->GetName() 
	 << " detected, ignoring request for " 
	 << name << endl;
    return geom;
  }

  // Cache geometry to a TFile.  Geometry will be restored from TFile on subsequent calls.
  TString filename = "";  
  if  (chain)  { filename = chain->GetFileOut(); if ( filename=="" ) filename = chain->GetFileIn();  }
  else { filename = name;  }

  // Strip out @ symbol
  filename = filename.ReplaceAll("@",""); 
  // Strip off the last extention in the filename
  filename = filename( 0, filename.Last('.') );
  // Append geom.root to the extentionless filename
  filename+=".geom.root";

  // Detect second call to the system
  if ( AgModule::Find("HALL") ) {
    if ( chain->GetOption("Sti")    ||
	 chain->GetOption("StiCA")  ||
	 chain->GetOption("StiVMC") ){
      cout << "AgML geometry:  HALL exists.  Restore from cache file " 
	   << filename.Data() << endl;
      gGeoManager = 0;
      assert(0);
      TGeoManager::Import( filename );
      assert(gGeoManager);
    }
    return geom;
  }

  cout << "AgML: Building geometry " << name << " " << endl;

  // Create the geometry using TGeo
  AgBlock::SetStacker( new StarTGeoStacker() ); 

  Geometry *build = new Geometry();  

  // Suppress copious ROOT warnings 
  Long_t save = gErrorIgnoreLevel; gErrorIgnoreLevel = 9999;
  build->ConstructGeometry(name);
  gErrorIgnoreLevel = save;

  if ( gGeoManager ) 
    {
      gGeoManager->CloseGeometry();
      geom = new TObjectSet("Geometry",gGeoManager, false );
      geom -> SetTitle( Form("AgML Geometry: %s",name) );

      TFile *file = new TFile( filename, "recreate" );
      file->cd();
      gGeoManager->Write();
      file->Close();
      delete file;
    }

  return (TDataSet *)geom;  
}



#if 0 

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
#if 0  
  TString filename = ((StBFChain*)StMaker::GetTopChain())->GetFileOut();
#else /* NOTE: this assumes that "chain" is in scope in CINT */
  TString filename = "";  
  if ( chain ) {
      filename = chain -> GetFileOut();
      filename.ReplaceAll(".root","");
  }
#endif
  if ( filename == "" ) 
    {
      filename = chain -> GetFileIn();
      filename.ReplaceAll("@","");      // special case for filelist
      filename.ReplaceAll(".list","");
    }
  if ( filename == "" )
    {
      filename = mytag;
    }

  filename += ".geom.root";

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

  Geometry *build = new Geometry();                        // Instantiate the geometry
  build -> ConstructGeometry ( tag );            
  gGeoManager->CloseGeometry();


  TFile *file = new TFile( filename, "recreate" );
  file->cd();
  gGeoManager->Write();
  file->Close();
  delete file;

  gErrorIgnoreLevel=0;                           // Enable ROOT warnings

  return;

}



TDataSet *CreateGeometry(const Char_t *name="y2011") {

  std::cout << "=====================================>>> CreateGeometry " << name << " <<<=====================================" << std::endl;

  TObjectSet *geom = NULL;

  // Check for existing geometries and return (NULL) 
  // if we find one.
  if (gGeoManager) {
    cout << "-- Geometry " << gGeoManager->GetName() << " -- " 
	 << " has beed created.  Ignoring request for AgML geometry " 
	 << name << "." << endl;
    return geom;
  }

  const Char_t *path  = ".:./StarDb/AgMLGeometry/:$STAR/StarDb/AgMLGeometry/";
  Char_t *file = gSystem->Which(path,"loadStarGeometry.Cxx",kReadPermission);

  // Load the geometry macro
  //  gROOT -> ProcessLine( Form(".L %s",file ) );

  // Instantiate the geometry
  loadStarGeometry( name );

  // Unload the macro
  //  gROOT -> ProcessLine( Form(".U %s",file ) );


  // Make damn sure we have a navigator... 
  assert(gGeoManager);
  //  if ( !gGeoManager->GetCurrentNavigator() )
  //    {
  //      gGeoManager->AddNavigator( new TGeoNavigator() );
  //    }

  // Wrap TGeoManager in a TDataSet and return it
  if ( gGeoManager ) 
    {
      geom = new TObjectSet("Geometry",gGeoManager, false );
      geom -> SetTitle( Form("AgML Geometry: %s",name) );
    }

  return (TDataSet *)geom;

}
#endif

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
  TString filename = "agmlcache.root";  

  static int count = 0;

  // Detect second call to the system (I believe this never gets called?)
  if ( AgModule::Find("HALL") ) {

    count++;
    // Warn that we have reloaded the geometry from cache
    std::cout << "Info: [AgML geometry] HALL exists.  Restore from cache file [x" << count << "] " << filename.Data() << std::endl;

    assert(count<100); // should never be done in ::Make
  
    gGeoManager = 0;
    TGeoManager::Import( filename );
    assert(gGeoManager);
    
      
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


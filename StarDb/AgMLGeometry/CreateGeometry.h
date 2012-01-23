/**
 * Adapted from StarDb/VmcGeometry/CreateGeometry.h
 */
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
  gROOT -> ProcessLine( Form(".L %s",file ) );

  // Instantiate the geometry
  loadStarGeometry( name );

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

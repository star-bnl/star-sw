/**
 * Adapted from StarDb/VmcGeometry/CreateGeometry.h
 */
TDataSet *CreateGeometry(const Char_t *name="y2011") {

  TObjectSet *geom = 0;
  if (gGeoManager) {
    cout << "VMC geometry " << gGeoManager->GetName() << " has beed created. Ignore request for " 
	 << name << " ! " << endl;
    return geom;
  }

  const Char_t *path  = ".:./StarVMC/Geometry/macros:$STAR/StarVMC/Geometry/macros";
  Char_t *file = gSystem->Which(path,"loadStarGeometry.C",kReadPermission);

  // Load the geometry macro
  gROOT -> ProcessLine( Form(".L %s",file ) );

  // Instantiate the geometry
  loadStarGeometry( name );

  // Now drop the geometry macro
  gROOT -> ProcessLine( Form(".U %s",file ) );

  // Wrap TGeoManager in a TDataSet and return it
  if ( gGeoManager ) 
    {
      geom = new TObjectSet("Geometry",gGeoManager);
      geom -> SetTitle( Form("AgML Geometry: %s",name) );
    }

  return (TDataSet *)geom;

}

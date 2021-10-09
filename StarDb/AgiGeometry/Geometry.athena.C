TDataSet *CreateTable() {
  TDataSet *geometry = 0;
  TObjectSet *geom = 0;
  if (gGeoManager) {
    cout << "VMC geometry " << gGeoManager->GetName() << " has beed created. Ignore request for " 
	 << name << " ! " << endl;
    return geometry;
  }
  TString name = gSystem->DirName(__FILE__);
  name += "/athena.root";
  cout << "File: " << name.Data() << endl;
  TFile *f = new TFile(name);
  f->Get("default");
  if (gGeoManager) {
    geometry = new TDataSet("Geometry");
    geom = new TObjectSet("GeoManager",gGeoManager,kFALSE);
    geometry->Add(geom);
  }
  return geometry;
}

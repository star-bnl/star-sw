TDataSet *CreateGeometry(const Char_t *name="y2005") {
  TObjectSet *geom = 0;
  if (gGeoManager) {
    cout << "VMC geometry " << gGeoManager->GetName() << " has beed created. Ignore request for " 
	 << name << " ! " << endl;
    return geom;
  }
  Char_t *path  = ".:./StarDb/AgiGeometry:$STAR/StarDb/AgiGeometry";
  TString geomF(name); geomF += ".h";
  Char_t *file = gSystem->Which(path,geomF,kReadPermission);
  if (! file) Fatal("CreateGeometry","File %s has not found in path %s",geomF.Data(),path);
  else        Warning("CreateGeometry","File %s has been found as %s",geomF.Data(),file);
  TString command = ".L "; command += file;
  gInterpreter->ProcessLine(command);
  TString cmd(name); cmd += "()";
  gInterpreter->Calc(cmd);
  command.ReplaceAll(".L ",".U "); 
  gInterpreter->ProcessLine(command);
  if (gGeoManager) {
    geom = new TObjectSet("Geometry",gGeoManager,kFALSE);
    geom->SetTitle(name);
  }
  return (TDataSet *) geom;
}

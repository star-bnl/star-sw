void SaveTGeoVolume(const Char_t *volN, const Option_t *opt = "old"){
  if (! gGeoManager) {
    cout << "There is no geometry" << endl;
    return;
  }
  TGeoVolume *vol = gGeoManager->GetVolume(volN);
  if (! vol) {
    cout << "Cant' find volume " << volN << endl;
    return;
  }
  TString path(vol->GetName());
  path += "."; path += opt; path += ".C";
  ofstream *out = new ofstream(path.Data());
  cout << "Open " << path.Data() << endl;
  //  *out << "-------------" << endl;
  //  vol->SavePrimitive(*out,"smxd");
  vol->SavePrimitive(*out,"");
  delete out;
}

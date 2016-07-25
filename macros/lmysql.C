void lmysql() {
  Char_t *libs[]= {
    "$OPTSTAR/lib/",
    "$OPTSTAR/lib/mysql/",
    "/usr/lib/", 
    "/usr/lib/mysql/", 
    "/usr/mysql/lib/",
    "/sw/lib/",
    NULL}; 
  TString Arch( gSystem->GetBuildArch() );
  Char_t *mysql = "libmysqlclient";
  Bool_t i64 = kFALSE;
  if ( gSystem->Getenv("USE_64BITS")==1 || Arch.Contains("x8664")) i64 = kTRUE;
  Int_t i = 0;
  while ((libs[i])) {
    TString lib(libs[i]);
    //    cout << "Try " << lib << endl;
    if (i64) lib.ReplaceAll("/lib","/lib64");
    lib += mysql;
    lib = gSystem->ExpandPathName(lib.Data());
    if (gSystem->DynamicPathName(lib,kTRUE)) {
      gSystem->Load(lib.Data()); 
      cout << " + " << mysql << " from " << lib.Data();
      break;
    }
    i++;
  }
  cout << endl;
  gSystem->Load("libStDb_Tables");
}

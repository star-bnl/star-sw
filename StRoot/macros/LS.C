void LS(const Char_t *path=0, const Char_t *pattern = "*") {
  if (!path || !path[0]) path = gSystem->WorkingDirectory();
  TString mypath(path);
  TPRegexp reg(pattern);
  
  void *dir = gSystem->OpenDirectory(mypath);
  const char *name;
  int n = 0;
  while ((name = gSystem->GetDirEntry(dir))) {
    if (TString(name).Contains(reg)) {
      TString full(mypath);
      full+="/"; full+=name;
      Long_t id,flags,modtime;
      Long64_t size;
      gSystem->GetPathInfo(full, &id,&size,&flags,&modtime);    
      cout  << size << "\t - " << name << endl;
      //      printf("%20i - %s\n",size,name);
      n++;
    }
  }
}

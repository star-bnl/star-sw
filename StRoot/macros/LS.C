void LS(const char *path=0)
{
if (!path || !path[0]) path = gSystem->WorkingDirectory();
TString mypath(path);


void *dir = gSystem->OpenDirectory(mypath);
const char *name;
int n = 0;
  while ((name = gSystem->GetDirEntry(dir))) {
    TString full(mypath);
    full+="/"; full+=name;
    Long_t id,size,flags,modtime;
    gSystem->GetPathInfo(full, &id,&size,&flags,&modtime);    
    printf("%10d - %s\n",size,name);
    n++;
  }
}

// Author V.Fine 06/12/2001 BNL mailto:fine@bnl.gov
// This macro load the share library one needs to run Pythia and create ROOT file
void LoadPythia()
{
 printf(" Loading Pythia/Athena share libraries\n");
 const char *fileList[] = {  "libEG"
                            , "/afs/usatlas.bnl.gov/offline/external/ROOT/XNew/i386_redhat71/root/lib/libPythia6"
                            , "libEGPythia6"
                            , "libRootKernel"  
                          };
 int nFiles = sizeof(fileList)/sizeof(char *);
 int i;
 for (i=0;i<nFiles;i++) 
 {
   printf(" Loading %s ",fileList[i]);
   if ( gSystem->Load(fileList[i]) ) printf(" \t... failed :-(\n");
   else printf(" \t... Ok! :-)\n");
 }
}


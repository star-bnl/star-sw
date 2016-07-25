// Author V.Fine 06/12/2001 BNL mailto:fine@bnl.gov
// This macro load the share library one needs to run Pythia and create ROOT file
void LoadPythia()
{
 printf(" Loading Pythia/Athena share libraries\n");
 const char *fileList[] = {  "libEG", "libPythia6", "libEGPythia6", "libStar"  };
//  ,"AtlfastAthenaRoot" };  
 int nFiles = sizeof(fileList)/sizeof(char *);
 int i;
 for (i=0;i<nFiles;i++) 
 {
   printf(" Loading %s ",fileList[i]);
   if ( gSystem->Load(fileList[i]) ) printf(" \t... failed :-(\n");
   else printf(" \t... Ok! :-)\n");
 }
}


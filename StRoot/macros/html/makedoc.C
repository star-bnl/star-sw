{
  gROOT.Reset();
  Char_t *libs[] = {"libasu","libdsl","St_base","St_Tables", 
  "libmsg","libtls","tpc","St_tpc","svt","St_svt","StChain"};
  Char_t *suffix=0;
  Int_t nlist = 11;
  Bool_t NT=kFALSE;
  if (strcmp(gSystem.GetName(),"WinNT") == 0 ) {
     NT=kTRUE;
     gSystem.Load("Root_html.dll");
     suffix = ".dll";
  }
  else
     suffix =".so";

   Char_t buffer[256];
   for(Int_t i=0;i<nlist;i++) {
      strcpy(buffer,libs[i]);strcat(buffer,suffix);
      if (gSystem.Load(buffer))  printf(" Loading DLL \"%s\" failed \n",buffer);
   }
  //Create the object of the THml class
  THtml *html = new THtml();

  char *sourcedir = 0;
  if (NT) 
    sourcedir = "//sol/afs_rhic/star/packages/dev/StRoot/base";
  else
    sourcedir = "/afs/rhic/star/packages/dev/StRoot/base:/afs/rhic/star/packages/dev/.share/tables";

  char *lookup = 0;
  if (NT) 
    lookup = "//sol/afs_rhic/star/packages/dev/.share/tables;../base;//sol/afs_rhic/star/packages/dev/.share/base";
  else
    lookup = "/afs/rhic/star/packages/dev/.share/tables:/afs/rhic/star/packages/dev/StRoot/base";

  html->SetSourceDir(lookup);

  if (NT) 
     html->SetOutputDir("J:/Public/STAF/draft/base/html");
  else
     html->SetOutputDir("/afs/rhic/star/packages/dev/StRoot/html");

  // Create the list of the classes defined with the loaded DLL's to be documented

  Char_t *classes[] = {"St_XDFFile",  "St_Module",   "St_Table"
                       ,"St_DataSet", 
                       ,"St_particle", "particle_st"
                       ,"StParticleView",
                       ,"StMaker",     "StChain"
                       ,"table_head_st"};
  Int_t nclass = 10;
  // Creat the definitions of the classes not derived from TObjects
  TClass header1("table_head_st",1,"table_header.h","table_header.h");
  // Make class descriptions
  Int_t i=0;
  for (i=0;i<nclass;i++) 
                   html.MakeClass(classes[i]);

  // Make HTML docs for the "plain" text files those are not in the dictionaries

  html.Convert("./../Chain/xdf.C","STAR chain example");
  html.Convert("./XDFCopy.c","How to read/write XDF file");
  html.Convert("./par_anal.cxx","How to create several histrograms from XDF file");
  html.Convert("./test10.c","How to use the dataset iterator class");
  html.Convert("./test9.c","How to read the event from XDF file and build some histograms with ROOT");
  html.Convert("./makedoc.c","How to create the HTML documentation");
  if (NT) {
    html.Convert("//hepburn/common/p32/root/star/macros/CallMevSaveXDF.cxx","How to call STAF module");
    html.Convert("//hepburn/common/p32/root/star/macros/par_anal.cxx","How to pick the XDF file up with ROOT");
    html.Convert("//hepburn/common/p32/root/star/macros/MakeHists.cxx","How to read the event from XDF file and build some histograms with ROOT");
  }
  else {
    html.Convert("./CallMevSaveXDF.cxx","How to call STAF module");
    html.Convert("./par_anal.cxx","How to pick the XDF file up with ROOT");
    html.Convert("./MakeHists.cxx","How to read the event from XDF file and build some histograms with ROOT");
  }



//  html.MakeClass("EModuleTypes");
//  html.MakeIndex();
}

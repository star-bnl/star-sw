{
  gROOT.Reset();
  Char_t *libs[] = {"St_base","xdf2root","St_Tables", 
  "libmsg","libtls","tpc.sl","St_tpc","svt.sl","St_svt","StChain"};

 Char_t *suffix=0;
  Int_t nlist = 10;
  Bool_t NT=kFALSE;
  if (strcmp(gSystem.GetName(),"WinNT") == 0 ) {
     NT=kTRUE;
     gSystem.Load("Root_html.dll");
     suffix = ".dll";
  }
  else 
     suffix =".so";

   Char_t buffer[256];
   if (NT) {
    for(Int_t i=0;i<nlist;i++) {
       strcpy(buffer,libs[i]);
       if (!strchr(libs[i],'.')) strcat(buffer,suffix);
       if (gSystem.Load(buffer))  printf(" Loading DLL \"%s\" failed \n",buffer);
    }
    
   }
   else {
     gSystem->Load("St_base.so");
     gSystem->Load("xdf2root.so");
     gSystem->Load("St_Tables.so");
  
     gSystem->Load("libmsg.so");
     gSystem->Load("libtls.so");
     gSystem->Load("tpc.sl");
     gSystem->Load("St_tpc.so");
     gSystem->Load("svt.sl");
     gSystem->Load("St_svt.so");
     gSystem->Load("global.sl");
     gSystem->Load("St_global.so");
     gSystem->Load("ftpc.sl");
     gSystem->Load("St_ftpc.so");
     gSystem->Load("StChain.so");

   //  gSystem->Load("St_srs_Maker.so");
   //  gSystem->Load("St_tpt_Maker.so");
   //  gSystem->Load("St_xdfin_Maker.so");
   //  gSystem->Load("St_evg_Maker.so");
   //  gSystem->Load("St_tcl_Maker.so");
   //  gSystem->Load("St_tss_Maker.so");
  //   gSystem->Load("St_ebye_Maker.so");
  //   gSystem->Load("St_laser_Maker.so");
  //   gSystem->Load("St_run_Maker.so");
  //   gSystem->Load("St_tpctest_Maker.so");

     gSystem->Load("St_calib_Maker.so");
   }
   
   
  //Create the object of the THtml class
  THtml *html = new THtml();

  char *sourcedir = 0;
  if (NT) 
    sourcedir = "//sol/afs_rhic/star/packages/dev/StRoot/base";
  else
    sourcedir = "/afs/rhic/star/packages/dev/StRoot/base:/afs/rhic/star/packages/dev/StRoot/xdf2root:/afs/rhic/star/packages/dev/.share/tables:/afs/rhic/star/packages/dev/inc";

  char *lookup = 0;
  if (NT) 
    lookup = "//sol/afs_rhic/star/packages/dev/.share/tables;../base;//sol/afs_rhic/star/packages/dev/.share/base";
  else
    lookup = "/afs/rhic/star/packages/dev/StRoot/StChain:/afs/rhic/star/packages/dev/StRoot/xdf2root:/afs/rhic/star/packages/dev/.share/tables:/afs/rhic/star/packages/dev/StRoot/base";

  html->SetSourceDir(lookup);

  if (NT) 
     html->SetOutputDir("J:/Public/STAF/draft/base/html");
  else
     html->SetOutputDir("/afs/rhic/star/packages/dev/StRoot/html");

  // Create the list of the classes defined with the loaded DLL's to be documented

  Char_t *classes[] = {"St_XDFFile",  "St_Module",   "St_Table"
                       ,"St_DataSet", "St_DataSetIter","St_FileSet"
                       ,"StParticleView",
                       ,"StMaker",     "StChain"
                       ,"table_head_st"
                       ,"St_srs_Maker","St_tpt_Maker","St_xdfin_Maker"
                       ,"St_evg_Maker","St_tcl_Maker","St_tss_Maker"
                       ,"St_ebye_Maker","St_laser_Maker","St_run_Maker"
                       ,"St_tpctest_Maker","St_calib_Maker"
                        };
  Int_t nclass = 21;
  // Creat the definitions of the classes not derived from TObjects
  if (NT) {
     gROOT->LoadMacro("//sol/afs_rhic/star/packages/dev/inc/table_header.h");
  }
  else
     gROOT->LoadMacro("/afs/rhic/star/packages/dev/inc/table_header.h");

  TClass header1("table_head_st",1,"table_header.h","table_header.h");
  // Make class descriptions
  Int_t i=0;

  for (i=0;i<nclass;i++) 
                   html.MakeClass(classes[i]);

  // Make HTML docs for the "plain" text files those are not in the dictionaries

  html.Convert("./../Chain/xdf.C","STAR chain example");
  html.Convert("./../test/XDFcopy.C","How to read/write XDF file");
  html.Convert("./../test/XDFtest.C","How to read/write XDF and ROOT files");
  html.Convert("./par_anal.cxx","How to create several histrograms from XDF file");
  html.Convert("./../test/test10.C","How to use the dataset iterator class");
  html.Convert("./../test/test9.C","How to read the event from XDF file and build some histograms with ROOT");
  html.Convert("./makedoc.C","How to create the HTML documentation");
  html.Convert("./tss.C","\"TPC slow simulator\" chain");
  html.Convert("./bfc.C","An example of the \"Big Full Chain\" production chain");
  html.Convert("./bfcx.C","An example of the \"Big Full Chain\" production chain");
  html.Convert("./ebye.C","An example of\"Event by Event\" production chain");
  html.Convert("./STAR_Demos.C","The source of the STAR_demos macro");
  html.Convert("./StarView.C","How to Draw the local STAR 3D geometry");
  html.Convert("./StarWebView.C","How to Draw the remote STAR 3D geometry");
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

  html.Convert("./laser.C","An example of the analysis of laser events equivalent to Iwona's old tst.kumac");
  html.Convert("./tst.kumac","An example of  Iwona's old tst.kumac of the analysis of laser events");
  html.Convert("./tpctest.C","ROOT based TPC test analysis");

//  html.MakeClass("EModuleTypes");
//  html.MakeIndex();
}

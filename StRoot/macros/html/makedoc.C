{
  gROOT.Reset();
  Char_t *libs[] = {"St_base","xdf2root","St_Tables", 
  "libmsg","libtls","tpc.sl","St_tpc","svt.sl","St_svt","StChain"};
  
 TString AFS; // STAR root directory

 Char_t *suffix=0;
  Int_t nlist = 10;
  Bool_t NT=kFALSE;
  if (strcmp(gSystem.GetName(),"WinNT") == 0 ) {
     NT=kTRUE;
     gSystem.Load("Root_html");
     AFS = "//sol/afs_rhic";
  }
  else
     AFS = "/afs/rhic";

   Char_t buffer[256];
   if (NT) {
    for(Int_t i=0;i<nlist;i++) {
       strcpy(buffer,libs[i]);
       if (gSystem.Load(buffer))  printf(" Loading DLL \"%s\" failed \n",buffer);
    }
    
   }
   else {
     gSystem->Load("St_base");
     gSystem->Load("xdf2root");
     gSystem->Load("St_Tables");
  
     gSystem->Load("libmsg.so");
     gSystem->Load("libtls.so");
     gSystem->Load("tpc.sl");
     gSystem->Load("St_tpc");
     gSystem->Load("svt.sl");
     gSystem->Load("St_svt");
     gSystem->Load("global.sl");
     gSystem->Load("St_global");
     gSystem->Load("ftpc.sl");
     gSystem->Load("St_ftpc");
     gSystem->Load("StChain");

     gSystem->Load("St_srs_Maker");
     gSystem->Load("St_tpt_Maker");
     gSystem->Load("St_xdfin_Maker");
     gSystem->Load("St_evg_Maker");
     gSystem->Load("St_tcl_Maker");
     gSystem->Load("St_tss_Maker");
     gSystem->Load("St_ebye_Maker");
     gSystem->Load("St_laser_Maker");
     gSystem->Load("St_run_Maker");
     gSystem->Load("St_tpctest_Maker");

     gSystem->Load("St_calib_Maker");
   }
   
   
  //Create the object of the THtml class
  THtml *html = new THtml();

  TString STAR = AFS;  
  STAR += "/star/packages/dev";
  TString sourcedir;
  sourcedir = STAR;
  sourcedir += "/StRoot/base";
  if (!NT) { 
    sourcedir += ":";
    sourcedir += STAR;
    sourcedir += "/.share/tables:";
    sourcedir += STAR;
    sourcedir += "/inc";
  }

  TString lookup ;
  if (NT) {
    lookup = STAR;
    lookup += "/.share/tables;../base;";
    lookup += STAR;
    lookup += ".share/base";
  }
  else {
    lookup = STAR;
    lookup += "/StRoot/StChain:";
    lookup += STAR;
    lookup += "/StRoot/xdf2root:";
    lookup += STAR;
    lookup += "/.share/tables:";
    lookup += STAR;
    lookup += "/StRoot/base";
  }

  
  TString giffile = STAR;
  giffile += "/StRoot/html/src/gif";
  
  if (gSystem->AccessPathName(giffile.Data()) && !NT) {
    // Create "gif" subdirectory for the first time
    gSystem->MakeDirectory(giffile);
    // Create links 
    gSystem->Symlink("../src/gif","/afs/rhic/star/packages/dev/StRoot/html/examples/gif");
    gSystem->Symlink("src/gif","/afs/rhic/star/packages/dev/StRoot/html/gif");
    // Copy the old images into a new directrory
    gSystem->Exec("cp /afs/rhic/star/packages/new/StRoot/html/src/gif/*.* /afs/rhic/star/packages/dev/StRoot/html/src/gif");
  }
  
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

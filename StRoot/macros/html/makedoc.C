// $Id: makedoc.C,v 1.45 1999/10/29 16:45:06 fine Exp $
// $Log: makedoc.C,v $
// Revision 1.45  1999/10/29 16:45:06  fine
// new macro videoStyle and bolStyle have been introduced
//
// Revision 1.44  1999/10/03 00:09:53  fine
// New classes docs StMicky and StCL have been introduced
//
// Revision 1.43  1999/09/26 02:59:54  fine
// html for RMath has been introduced
//
// Revision 1.42  1999/09/14 15:30:40  fine
// makedocs adjusted to the new source tree
//
// Revision 1.41  1999/09/12 01:09:57  fine
// Adjusted to the new source tree
//
// Revision 1.40  1999/08/07 18:36:33  fine
// StDisplayMaker has been included into makedoc
//
// Revision 1.39  1999/07/10 22:59:55  fine
//   St_TLA_Maker docs was introduced
//
// Revision 1.38  1999/06/30 16:29:04  fine
// New scheme to create gif directories. St_geom_Maker introduced
//
// Revision 1.37  1999/06/11 21:41:19  fine
// StarClassLibary share lib is to be loaded
//
// Revision 1.36  1999/06/07 22:56:45  fisyak
// Add EventRead
//
// Revision 1.35  1999/06/04 01:46:09  fine
// New examples have been included
//
// Revision 1.34  1999/06/02 16:32:46  fine
// PadControlPanel macro html has been introduced
//
// Revision 1.33  1999/06/01 01:46:41  fine
// New classes have been added StTrack, StHit, StHelixD StObjArray
//
// Revision 1.32  1999/05/23 21:36:40  fine
// New class have been introduce for html
//
// Revision 1.31  1999/05/21 15:33:59  kathy
// made sure Log & Id are in each file and also put in standard comment line with name of owner
//
//=======================================================================
// owner: Valery Fine
// what it does: 
//=======================================================================
  {
 //*-- Author :    Valery Fine   25/12/98
  gROOT.Reset();
  Char_t *libs[] = { "St_base","xdf2root","St_Tables"
                    ,"libmsg", 
                    , "StChain"};
  
 TString AFS; // STAR root directory

 Char_t *suffix=0;
  Int_t nlist = 8;
  Bool_t NT=kFALSE;
  if (strcmp(gSystem.GetName(),"WinNT") == 0 )
  {
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
     gSystem->Load("St_baseTest");
     gSystem->Load("xdf2root");
     gSystem->Load("St_Tables");
   
//     gSystem->Load("global.sl");
//     gSystem->Load("St_global");
     gSystem->Load("StChain");
   
     gSystem->Load("St_TLA_Maker.so");
     gSystem->Load("St_io_Maker.so");
     gSystem->Load("St_xdfin_Maker");
     gSystem->Load("St_evg_Maker");
     gSystem->Load("StarClassLibrary");
     gSystem->Load("StEvent");
     gSystem->Load("StPadDisplayMaker");
     gSystem->Load("St_geom_Maker");
     gSystem->Load("StEventDisplayMaker");

//     gSystem->Load("St_ebye_Maker");
//     gSystem->Load("St_laser_Maker");
//     gSystem->Load("St_run_Maker");
//     gSystem->Load("St_tpctest_Maker");

//     gSystem->Load("St_calib_Maker");
   }
   
   
  //Create the object of the THtml class
  THtml *html = new THtml();

  TString STAR = "$STAR";  
  TString sourcedir;
  sourcedir = STAR;
  if (!NT) { 
    sourcedir += ":";
    sourcedir = STAR;
    sourcedir += "/StRoot/St_base:";
    sourcedir = STAR;
    sourcedir += "/StRoot/St_baseTest:";
    sourcedir += STAR;
    sourcedir += "/.share/tables:";
    sourcedir += STAR;
    sourcedir += "/include:";
    sourcedir += STAR;
    sourcedir += "/StRoot/StEvent:";
    sourcedir += STAR;
    sourcedir += "/StRoot/St_TLA_Maker:";
    sourcedir += STAR;
    sourcedir += "/StRoot/StPadDisplay:";
    sourcedir += STAR;
    sourcedir += "/StRoot/StarClassLibrary:";
    sourcedir += STAR;
    sourcedir += "/StRoot/St_geom_Maker:";
    sourcedir += STAR;
    sourcedir += "/StRoot/StEventDisplayMaker";  }

  TString lookup ;
  if (NT) {
    lookup = STAR;
    lookup += "/.share/tables;../St_base;";
    lookup += STAR;
    lookup += ".share/St_base";
  }
  else {
    lookup = STAR;
    lookup += ":";
    lookup += STAR;
    lookup += "/StRoot/StChain:";
    lookup += STAR;
    lookup += "/include:";
    lookup += STAR;
    lookup += "/include/tables:";
    lookup += STAR;
    lookup += "/StRoot/xdf2root:";
    lookup += STAR;
    lookup += "/.share/tables:";
    lookup += STAR;
    lookup += "/StRoot/St_base:";
    lookup += STAR;
    lookup += "/StRoot/St_baseTest:";
    lookup += STAR;
    lookup += "/StRoot/St_TLA_Maker:";
    lookup += STAR;
    lookup += "/StRoot/St_io_Maker:";
    lookup += STAR;
    lookup += "/StRoot/St_geom_Maker:";
    lookup += STAR;
    lookup += "/StRoot/StPadDisplayMaker:";
    lookup += STAR;
    lookup += "/StRoot/StEventDisplayMaker:";
    lookup += STAR;
    lookup += "/StRoot/StEvent:";
    lookup += STAR;
    lookup += "/StRoot/StarClassLibrary";
  }

    
  html->SetSourceDir(lookup.Data());

  if (NT) 
     html->SetOutputDir("J:/Public/STAF/draft/base/html");
  else
     html->SetOutputDir("$STAR/StRoot/html");

  // Create the list of the classes defined with the loaded DLL's to be documented

  Char_t *classes[] = { "St_TableSorter","StCL", "StMicky", "St_tableDescriptor"
                       ,"St_XDFFile",    "St_Module",       "St_Table"
                       ,"St_DataSet",    "St_DataSetIter", "St_FileSet"
                       ,"StParticleView","St_ObjectSet",    "St_Node",     "St_NodePosition"
                       ,"StMaker",       "StChain",         "St_NodeView"
                       ,"table_head_st", "St_NodeViewIter", "St_PolyLineShape"
                       ,"St_Points3D",   "St_PolyLine3D",   "St_PointsArray3D"
                       ,"St_AttributesABC", "St_Table3Points","St_TablePoints"
                       ,"St_io_Maker",   "StHelix3DPoints", "StHits3DPoints"
                       ,"StObjArray",    "StHit",            "StHelixD"
                       ,"StTrack",       "St_TableElementDescriptor"
                       ,"St_geom_Maker", "StPadDisplayMaker", "St_TLA_Maker"
                       ,"StEventDisplayMaker"
                       ,"St_srs_Maker",  "St_xdfin_Maker"
                      };
  Int_t nclass = 38;
  // Creat the definitions of the classes not derived from TObjects
  if (NT) {
     gROOT->LoadMacro("$STAF/inc/table_header.h");
  }
  else
     gROOT->LoadMacro("$STAF/inc/table_header.h");

  TClass header1("table_head_st",1,"table_header.h","table_header.h");
  // Create HTML subdirectory tree
   const Char_t* htmlTree = "$STAR/StRoot/html";
  if (gSystem->AccessPathName(htmlTree) && !NT) {
     gSystem->MakeDirectory(htmlTree);
  }

  // Make class descriptions
  Int_t i=0;
  for (i=0;i<nclass;i++)  html.MakeClass(classes[i],kTRUE);

  TString giffile = STAR;
  giffile += "/StRoot/html/src/gif";
  if (!NT) {
    if (gSystem->AccessPathName(giffile.Data()) && !NT) {
    // Create "gif" subdirectory for the first time
      printf(" Image directiry is created: <%s>\n", giffile.Data());
      gSystem->MakeDirectory(giffile.Data());
      gSystem->Exec("cp /afs/rhic/star/packages/pro/StRoot/html/src/gif/*.* /afs/rhic/star/packages/dev/StRoot/html/src/gif");
    }
      // Create links 
    gSystem->Symlink("../src/gif","/afs/rhic/star/packages/dev/StRoot/html/examples/gif");
    gSystem->Symlink("src/gif","/afs/rhic/star/packages/dev/StRoot/html/gif");
      // Copy the old images into a new directrory
//      gSystem->Exec("cp /afs/rhic/star/packages/new/StRoot/html/src/gif/*.* /afs/rhic/star/packages/dev/StRoot/html/src/gif")
  }

  // Make HTML docs for the "plain" text files those are not in the dictionaries
  cout << " Making HTML's for macros" << endl;
  html.Convert("../test/micky.C","\"Micky\" to test the <matrix> and <triangilar matrix> methods");
  html.Convert("../graphics/boldStyle.C","Style file for making presentation histograms.");
  html.Convert("../graphics/videoStyle.C","Style file for video presentation histograms.");
  html.Convert("../graphics/PadBrowser.C","How to use St_geom_Maker and StPadDisplayMaker");
  html.Convert("../graphics/basic3dPrimitives.C","An example of the basic 3D STAR object");
  html.Convert("../graphics/EventPanel.C","An example of the ToolBar to control an applications");
  html.Convert("../graphics/DrawTpcHits.C","How to draw 3D view for hits and");
  html.Convert("../graphics/DrawTrackTpcHits.C","How to draw 3D view for hits and tracks");
  html.Convert("../graphics/PadControlPanel.C","How to manipulate with 3D pad images");
  html.Convert("../graphics/HitsDraw.C","3D drawing of the STAR Geometry and the hits from the STAF table");
  html.Convert("../graphics/SubDetectorView.C","How to create sub-detector view");
  html.Convert("../graphics/StarFromWeb.C","Access to ROOT/GEANT geometry database");
  html.Convert("./GetEvent.C","An example of the reading MDC2 dst events");
  html.Convert("./QA_Hist_Draw.C","An example of the plotting postscript file of the MDC2 histograms");
  html.Convert("../graphics/StarGeom.C","An example of the ROOT/STAR/GEANT interface");
  html.Convert("../examples/TestSorter.C","An example of the STAF table sort utility");
  html.Convert("../examples/XDFBrowser.C","XDF file interactive ROOT browser");
  html.Convert("../../Chain/xdf.C","STAR chain example");
  html.Convert("../../test/XDFcopy.C","How to read/write XDF file");
  html.Convert("../../test/XDFtest.C","How to read/write XDF and ROOT files");
  html.Convert("./par_anal.cxx","How to create several histrograms from XDF file");
  html.Convert("../../test/test10.C","How to use the dataset iterator class");
  html.Convert("../../test/test9.C","How to read the event from XDF file and build some histograms with ROOT");
  html.Convert("./makedoc.C","How to create the HTML documentation");
  html.Convert("../tss.C","\"TPC slow simulator\" chain");
  html.Convert("../bfc.C","An example of the \"Big Full Chain\" production chain");
  html.Convert("./bfcx.C","An example of the \"Big Full Chain\" production chain");
  html.Convert("./ebye.C","An example of\"Event by Event\" production chain");
  html.Convert("./../STAR_Demos.C","The source of the STAR_demos macro");
  html.Convert("../graphics/STAR_shapes.C","Test for the basic STAR GEOMETRY classes");
  html.Convert("../graphics//StarView.C","How to Draw the local STAR 3D geometry");
  html.Convert("../graphics/StarWebView.C","How to Draw the remote STAR 3D geometry");
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

 html.MakeClass("EModuleTypes");
 html.MakeIndex();
}

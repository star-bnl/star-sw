// $Id: makedoc.C,v 1.60 2006/08/15 21:43:19 jeromel Exp $
//=======================================================================
// owner: Valery Fine
// what it does: 
//=======================================================================
  {
 //*-- Author :    Valery Fine   25/12/98
 gROOT.Reset();
  Char_t *libs[] = { "libTable", "Star2Root","St_base","xdf2root","St_Tables"                    
                    , "StChain","StUtilities","StBFChain"};

 TString AFS; // STAR root directory

  Char_t *suffix=0;
  Int_t nlist = sizeof(libs)/sizeof(Char_t *);;
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
     gSystem->Load("libTable");
     gSystem->Load("Star2Root");
     gSystem->Load("St_base");
     gSystem->Load("St_baseTest");
     gSystem->Load("xdf2root");
     gSystem->Load("St_Tables");
   
//     gSystem->Load("global.sl");
//     gSystem->Load("St_global");
     gSystem->Load("StChain");
     gSystem->Load("StUtilities");
     gSystem->Load("StBFChain");
   
     gSystem->Load("St_TLA_Maker.so");
     gSystem->Load("St_io_Maker.so");
     gSystem->Load("St_xdfin_Maker");
     gSystem->Load("StarClassLibrary");
     gSystem->Load("StEvent");
     //-- gSystem->Load("StPadDisplayMaker");
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

  TString STAR = "$STAR/.$STAR_SYS/obj";  
  TString ROOTSYS = "$ROOTSYS/.$STAR_SYS/obj";

  TString sourcedir;
  sourcedir =  "$STAR";
  if (!NT) { 
    sourcedir += ":";
    sourcedir += "$STAR/include:";
    sourcedir += "$STAR/include/tables:";
    sourcedir += "$ROOTSYS/ROOT/root/STAR:";
    sourcedir = STAR;
    sourcedir += "/StRoot/St_base:";
    sourcedir += STAR;
    sourcedir += "/StRoot/St_baseTest:";
    sourcedir += "$STAR";
    sourcedir += "/.share/tables:";
    sourcedir += "$STAR";
    sourcedir += "/include:";
    sourcedir += "$STAR";
    sourcedir += "/include/tables:";
    sourcedir += STAR;
    sourcedir += "/StRoot/StEvent:";
    sourcedir += STAR;
    sourcedir += "/StRoot/StBFChain:";
    sourcedir += STAR;
    sourcedir += "/StRoot/StChain:";
    sourcedir += STAR;
    sourcedir += "/StRoot/St_TLA_Maker:";
    sourcedir += STAR;
    sourcedir += "/StRoot/StPadDisplay:";
    sourcedir += STAR;
    sourcedir += "/StRoot/StarClassLibrary:";
    sourcedir += "$ROOTSYS";
    sourcedir += "/src:";
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
    lookup = "$STAR";
    lookup += ":";
    lookup += "$ROOTSYS/ROOT/root/STAR:";
    lookup += "$ROOTSYS/include:";
    lookup += "$STAR";
    lookup += "/StRoot/StChain:";
    lookup += "$STAR";
    lookup += "/include:";
    lookup += "$STAR";
    lookup += "/include/tables:";
    lookup += STAR;
    lookup += "/StRoot/xdf2root:";
    lookup += STAR;
    lookup += "/.share/tables:";
    lookup += STAR;
    lookup += "/StRoot/St_base:";
    lookup += STAR;
    lookup += "/StRoot/StChain:";
    lookup += STAR;
    lookup += "/StRoot/StBFChain:";
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
    lookup += "$ROOTSYS";
    lookup += "/src:";
    lookup += STAR;
    lookup += "/StRoot/StarClassLibrary";
  }

    
  html->SetSourceDir(lookup.Data());

  if (NT) 
     html->SetOutputDir("J:/Public/STAF/draft/base/html");
  else
     html->SetOutputDir("$STAR/StRoot/html");

  // Create the list of the classes defined with the loaded DLL's to be documented

  Char_t *classes[] = { "TTableSorter",  "TCL", "StMicky",  "TTableDescriptor"
                       ,"St_XDFFile",    "St_Module",       "TTable"
                       ,"TDataSet",      "TDataSetIter",    "TFileSet"
                       ,"StParticleView","TObjectSet",      "TVolume",      "TVolumePosition"
                       ,"StMaker",       "StChain",         "TVolumeView"
                       ,"table_head_st", "TVolumeViewIter", "TPolyLineShape"
                       ,"TPoints3D",     "St_PolyLine3D",   "TPointsArray3D"
                       ,"TTable3Points", "TTablePoints",    "StDefaultFilter"
                       ,"St_io_Maker",   "StHelix3DPoints", "StHits3DPoints"
                       ,"StObjArray",    "StHit",            "StHelixD"
                       ,"StTrack",       
                       ,"St_geom_Maker", "StPadDisplayMaker", "St_TLA_Maker"
                       ,"StBFChain",     "StEventDisplayMaker",
                       ,"StVirtualEventFilter", "TTableIter"
                       ,"St_srs_Maker",  "St_xdfin_Maker"
                      };
  Int_t nclass = 40;
  html.MakeIndex();

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

  // Make tables descriptions
  St_FileSet tableDir("$STAR/include/tables");
  St_DataSetIter nextTable(&tableDir);
  St_DataSet *table = 0;
  while (table = nextTable()) {
    TString tabName = table.GetName();
    tabName.ReplaceAll("St_","");
    tabName.ReplaceAll("_Table.h","");
    tabName += "_st";
    html.MakeClass(tabName.Data());

    tabName = table.GetName();    
    tabName.ReplaceAll("_Table.h","");
    html.MakeClass(tabName.Data());
  } 
   
  TString giffile = STAR;
  giffile += "/StRoot/html/src/gif";
  if (!NT) {
    if (gSystem->AccessPathName(giffile.Data()) && !NT) {
    // Create "gif" subdirectory for the first time
      printf(" Image directiry is created: <%s>\n", giffile.Data());
      gSystem->MakeDirectory(giffile.Data());
      gSystem->Exec("cp /afs/rhic.bnl.gov/star/packages/pro/StRoot/html/src/gif/*.* /afs/rhic.bnl.gov/star/packages/adev/StRoot/html/src/gif");
    }
      // Create links 
    gSystem->Symlink("../src/gif","/afs/rhic.bnl.gov/star/packages/adev/StRoot/html/examples/gif");
    gSystem->Symlink("src/gif","/afs/rhic.bnl.gov/star/packages/adev/StRoot/html/gif");
      // Copy the old images into a new directrory
//      gSystem->Exec("cp /afs/rhic.bnl.gov/star/packages/new/StRoot/html/src/gif/*.* /afs/rhic.bnl.gov/star/packages/adev/StRoot/html/src/gif")
  }

  // Make HTML docs for the "plain" text files those are not in the dictionaries
  cout << " Making HTML's for macros" << endl;
  const Char_t *macros[] = {
       "/test/micky.C" ,               "\"Micky\" to test the <matrix> and <triangilar matrix> methods"
     , "/test/test10.C",               "How to use the dataset iterator class"
     , "/test/test9.C",                "How to read the event from XDF file and build some histograms with ROOT"
     , "/test/XDFcopy.C",              "How to read/write XDF file"
     , "/test/XDFtest.C",              "How to read/write XDF and ROOT files"
     , "/graphics/TurnDisplay.C",      "Macro to plug StEventDisplayMaker into chain and turn it on."
     , "/graphics/SetObjectFilter.C",  "Macro to adjust the parameters of the current \"event filter\"."
     , "/graphics/TrackFilters/StTrackFilter.cxx", "an example of the implementation of the \"advanced\"  event filter."
     , "/graphics/TrackFilters/StTrackFilter.h",   "an example of the definition of the \"advanced\"  event filter."
     , "/graphics/boldStyle.C",        "Style file for making presentation histograms."
     , "/graphics/videoStyle.C",       "Style file for video presentation histograms."
     , "/graphics/PadBrowser.C",       "How to use St_geom_Maker and StPadDisplayMaker"
     , "/graphics/basic3dPrimitives.C","An example of the basic 3D STAR object"
     , "/graphics/EventPanel.C",       "An example of the ToolBar to control an applications"
     , "/graphics/DrawTpcHits.C",      "How to draw 3D view for hits and"
     , "/graphics/DrawTrackTpcHits.C", "How to draw 3D view for hits and tracks"
     , "/graphics/PadControlPanel.C",  "How to manipulate with 3D pad images"
     , "/graphics/HitsDraw.C",         "3D drawing of the STAR Geometry and the hits from the STAF table"
     , "/graphics/SubDetectorView.C",  "How to create sub-detector view"
     , "/graphics/StarFromWeb.C",      "Access to ROOT/GEANT geometry database"
     , "/graphics/STAR_shapes.C",      "Test for the basic STAR GEOMETRY classes"
     , "/graphics/StarView.C",         "How to Draw the local STAR 3D geometry"
     , "/graphics/StarWebView.C",      "How to Draw the remote STAR 3D geometry"
     , "/graphics/StarGeom.C",         "An example of the ROOT/STAR/GEANT interface"

     , "/html/GetEvent.C",             "An example of the reading MDC2 dst events"
     , "/html/makedoc.C",              "How to create the HTML documentation"

     , "/QA_Hist_Draw.C",              "An example of the plotting postscript file of the MDC2 histograms"

     , "/examples/TestSorter.C",       "An example of the STAF table sort utility"
     , "/examples/XDFBrowser.C",       "XDF file interactive ROOT browser"

     , "/tss.C" ,                      "\"TPC slow simulator\" chain"
     , "/bfc.C",                       "An example of the \"Big Full Chain\" production chain"
     , "/bfcx.C",                      "An example of the \"Big Full Chain\" production chain"
     , "/ebye.C",                      "An example of\"Event by Event\" production chain"
     , "/STAR_Demos.C",                "The source of the STAR_demos macro"
};

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

//  html.Convert("./par_anal.cxx","How to create several histrograms from XDF file");

//  html.Convert("./laser.C","An example of the analysis of laser events equivalent to Iwona's old tst.kumac");
//  html.Convert("./tst.kumac","An example of  Iwona's old tst.kumac of the analysis of laser events");
//  html.Convert("./tpctest.C","ROOT based TPC test analysis");

 Int_t lmacros = sizeof(macros)/sizeof(Char_t *);
 for (int i = 0; i < lmacros; i+=2) {
      TString starRoot = "$STAR/StRoot/macros";
      starRoot += macros[i];
      html.Convert(starRoot.Data(),macros[i+1]); 
      //  cout << starRoot.Data() << macros[i+1] << ";" << endl;
 }

 html.MakeClass("EModuleTypes");
 html.MakeIndex();
}

//___________________________________________________________________________
//___________________________________________________________________________
// $Log: makedoc.C,v $
// Revision 1.60  2006/08/15 21:43:19  jeromel
// Fix rhic -> rhic.bnl.gov
//
// Revision 1.59  2003/04/30 20:40:17  perev
// Warnings cleanup. Modified lines marked VP
//
// Revision 1.58  2001/09/10 13:48:00  jeromel
// Modif by Valeri but not commited
//
// Revision 1.57  2001/09/10 01:37:55  jeromel
// dev -> adev
//
// Revision 1.56  2000/08/27 19:01:19  fine
// StDefaultFilter docs added
//
// Revision 1.55  2000/05/30 00:00:37  fine
// Correction to create STAR/ROOT classes html docs
//
// Revision 1.54  2000/04/18 23:31:16  fine
// adjusted to ROOT 2.24
//
// Revision 1.53  2000/02/24 02:23:47  fine
// St_Table html docs generationn added
//
// Revision 1.52  1999/12/15 23:55:14  fine
// St_TableIter doc included
//
// Revision 1.51  1999/12/15 22:41:24  fine
// clean up
//
// Revision 1.50  1999/12/15 22:33:30  fine
// new class StVirtualEventFilter doc  added
//
// Revision 1.49  1999/12/15 22:29:55  fine
// new macros added
//
// Revision 1.48  1999/12/13 17:05:42  fine
// new marcos TurnDisplay.C SetObjectFilter included
//
// Revision 1.47  1999/12/11 00:27:14  fine
// StBFChain has been added
//
// Revision 1.46  1999/11/18 13:21:45  fine
// Adjusted to the new source tree
//
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
//___________________________________________________________________________

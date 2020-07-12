// $Id: rootlogon.C,v 1.48 2006/10/10 14:00:02 jeromel Exp $
//
//=======================================================================
// owner:  Yuri Fisyak
// what it does: opens the ROOT session
//=======================================================================
{
  //  cout << gSystem->GetDynamicPath() << endl;
  gSystem->ResetSignal(kSigChild,kTRUE);
  int rootlogon_fpe=0;const char *rootlogon_env=0;
  const Char_t *rootlogon_path  = ".:~";
  Int_t rootlogon_error = 0;
  // eliminate the need to provide the custom ".rootrc" to activate Qt-layer 
  if (gSystem->Getenv("WITH_QT") && gSystem->Getenv("QTDIR")) 
  {
      // this allows to eliminate (or overwrite) the custom ".rootrc" file
      gEnv->SetValue("Gui.Backend","qt");
      gEnv->SetValue("Gui.Factory","qtgui");     
  }
  TString StarVersion(gSystem->Getenv("STAR_VERSION"));
  rootlogon_fpe =StarVersion == ".DEV" ||StarVersion == "DEV2" ||StarVersion == "DEV6";
#if 1
  if (! rootlogon_fpe && gClassTable->GetID("TGiant3") < 0) { // ! root4star
    rootlogon_fpe = StarVersion == ".DEV2";
  }
#endif
  rootlogon_env = gSystem->Getenv("STARFPE");
  if (rootlogon_env) {
    if (strcmp(rootlogon_env,"YES")==0) rootlogon_fpe=1;
    if (strcmp(rootlogon_env,"NO" )==0) rootlogon_fpe=0;
  }
  if (rootlogon_fpe) {
    gSystem->SetFPEMask(kInvalid | kDivByZero | kOverflow );
    printf("*** Float Point Exception is ON ***\n");
  } else {
    printf("*** Float Point Exception is OFF ***\n");
  }
#if 0
  if (gROOT->IsBatch()) {
    printf("*** Change Abort Level from kFatal to kError ***\n");
    gErrorAbortLevel = kError;
  }
#endif
  gROOT->SetStyle("Plain");// Default white background for all plots
  // The modes below are provided by Nick van Eijndhoven <Nick@phys.uu.nl>
  // from Alice.
  gStyle->SetCanvasColor(10);
  gStyle->SetStatColor(10);
  gStyle->SetTitleFillColor(10);
  gStyle->SetPadColor(10);
#if 0
  gStyle->SetFillStyle(4010);
  gStyle->SetFillColor(kBlue);
  gStyle->SetStatStyle(4010);
  gStyle->SetStatColor(kBlue);
#endif
  // Settings for statistics information
  gStyle->SetOptFit(1);
  gStyle->SetOptStat(1);
  // SetPaperSize wants width & height in cm: A4 is 20,26 & US is 20,24
  gStyle->SetPaperSize(20,24); 
   
  // Positioning of axes labels
  gStyle->SetTitleOffset(1.2);
  // grid
  gStyle->SetPadGridX(1);
  gStyle->SetPadGridY(1);
  //  Set date/time for plot
  gStyle->SetOptDate(1);
  //  gStyle->SetPalette(1);
  //  gStyle->SetPalette(55);
  // Maxim's pallete
  const Int_t NRGBs = 5;
  const Int_t NCont = 100; // 255;
  Double_t stops[NRGBs] = { 0.00, 0.34, 0.61, 0.84, 1.00 };
  Double_t red[NRGBs] = { 0.00, 0.00, 0.87, 1.00, 0.51 };
  Double_t green[NRGBs] = { 0.00, 0.81, 1.00, 0.20, 0.00 };
  Double_t blue[NRGBs] = { 0.51, 1.00, 0.12, 0.00, 0.00 };
  TColor::CreateGradientColorTable(NRGBs, stops, red, green, blue, NCont);
  gStyle->SetNumberContours(NCont);
  gStyle->SetMarkerStyle(20);
  
  // Redefine prompt
  TString gPrompt =  gSystem->BaseName(gROOT->GetApplication()->Argv(0));
  gPrompt += " [%d] ";
  ((TRint*)gROOT->GetApplication())->SetPrompt( gPrompt.Data()); 
  //  if (gSystem->DynamicPathName("libNet",kTRUE))
  //    gSystem->Load("libNet");
  
  // 	Load StarRoot lib.
  //  if (gPrompt.Index("root4star")>=0 && !strstr(gSystem->GetLibraries(),"libTable")) {
  gSystem->Load("libMatrix");
  gSystem->Load("libPhysics");
  gSystem->Load("libGraf3d");
  if (!strstr(gSystem->GetLibraries(),"libTable")) {
    gSystem->Load("libGeom"); 
    gSystem->Load("libTable");
#ifdef __CLING__
    //    gSystem->Load("libStUtilities"); 
    gSystem->Load("libSt_base"); 
    gSystem->Load("libStStarLogger"); 
#endif
  }
  gSystem->Load("libEG");
  gSystem->Load("libVMC");
  if (gSystem->DynamicPathName("StarClassLibrary",kTRUE)) gSystem->Load("StarClassLibrary");
  else if (gSystem->DynamicPathName("libStarClassLibrary",kTRUE)) gSystem->Load("libStarClassLibrary");
  //#ifndef __CLING__
  if (gSystem->DynamicPathName("StarRoot",kTRUE)) {    
    gSystem->Load("StarRoot");
    gROOT->ProcessLine("StCloseFileOnTerminate::Instantiate();",&rootlogon_error);
  } else if (gSystem->DynamicPathName("libStarRoot",kTRUE)) { 
    gSystem->Load("libStarRoot");
    gROOT->ProcessLine("StCloseFileOnTerminate::Instantiate();",&rootlogon_error);
  }
  //#endif
#if 1
  if (gSystem->DynamicPathName("KFParticle",kTRUE)) {
    gSystem->Load("KFParticle");
  } else if (gSystem->DynamicPathName("libKFParticle",kTRUE)) {
    gSystem->Load("libKFParticle");
  }
#endif
  if (TString(gSystem->GetLibraries()).Contains("libTable")) {
    gROOT->ProcessLine("typedef TCL              StCL;");              
    gROOT->ProcessLine("typedef TDataSet         St_DataSet ;");       
    gROOT->ProcessLine("typedef TDataSetIter     St_DataSetIter;");    
    gROOT->ProcessLine("typedef TFileSet         St_FileSet;");        
    gROOT->ProcessLine("typedef TVolume          St_Node;");           
    gROOT->ProcessLine("typedef TVolumePosition  St_NodePosition;");   
    gROOT->ProcessLine("typedef TVolumeView      St_NodeView;");       
    gROOT->ProcessLine("typedef TVolumeViewIter  St_NodeViewIter;");   
    gROOT->ProcessLine("typedef TObjectSet       St_ObjectSet;");      
    //  gROOT->ProcessLine("typedef TPointPosition   St_PointPosition;");  
    gROOT->ProcessLine("typedef TPoints3D        St_Points3D;");       
    gROOT->ProcessLine("typedef TPointsArray3D   St_PointsArray3D;");  
    gROOT->ProcessLine("typedef TPolyLineShape   St_PolyLineShape;");  
    gROOT->ProcessLine("typedef TTable           St_Table;");          
    gROOT->ProcessLine("typedef TTable3Points    St_Table3Points;");   
    gROOT->ProcessLine("typedef TTableIter       St_TableIter;");      
    gROOT->ProcessLine("typedef TTablePoints     St_TablePoints;");    
    gROOT->ProcessLine("typedef TTableSorter     St_TableSorter;");    
    gROOT->ProcessLine("typedef TTableDescriptor St_tableDescriptor;");
  }
  printf(" *** Start at Date : %s\n",TDatime().AsString());
  
   // 	Assign bif size of hashtable for STAR I/O
   TBufferFile::SetGlobalWriteParam(2003);




  // This is already implied in system.rootrc although one could use
  // this to switch to a beta version of the client library.
  //ROOT->GetPluginManager()->AddHandler("TFile","^root:","TXNetFile", "Netx", "TXNetFile(const char*,Option_t*,const char*,Int_t,Int_t)"); 

  // This will help tracing failure on XrdOpen() if any
  gEnv->SetValue("XNet.DebugTimestamp","1"); 
  gEnv->SetValue("XNet.ConnectDomainAllowRE","rcf.bnl.gov|usatlas.bnl.gov");
  gEnv->SetValue("XNet.RedirDomainAllowRE","rcf.bnl.gov"); 
  gEnv->SetValue("XNet.ReconnectTimeout","5"); 
  //  Print version
  {
    TString STAR_GIT("$STAR/.git");            gSystem->ExpandPathName(STAR_GIT);
    TString STAR_LEVEL("$STAR_LEVEL");         gSystem->ExpandPathName(STAR_LEVEL);
    TString STAR_GIT_VERSION; 
    if (! gSystem->AccessPathName(STAR_GIT,kReadPermission)) {
      STAR_GIT_VERSION = ", git = ";
      STAR_GIT_VERSION += gSystem->GetFromPipe("git --git-dir=$STAR/.git describe --all"); 
      STAR_GIT_VERSION += ",tag:";
      STAR_GIT_VERSION += gSystem->GetFromPipe("git --git-dir=$STAR/.git describe --always"); 
      gEnv->SetValue("STAR_GIT_VERSION", STAR_GIT_VERSION.Data());
    }
    TString ROOT_LEVEL("$ROOT_LEVEL");         gSystem->ExpandPathName(ROOT_LEVEL);
    TString GARFIELD_HOME("$GARFIELD_HOME");   gSystem->ExpandPathName(GARFIELD_HOME);
    TString OPTSTAR("$OPTSTAR");               gSystem->ExpandPathName(OPTSTAR);
    TString XOPTSTAR("$XOPTSTAR");             gSystem->ExpandPathName(XOPTSTAR);
    TString QTDIR("$QTDIR");                   gSystem->ExpandPathName(QTDIR);
    cout <<  Form("QAInfo:You are using STAR_LEVEL : %s%s, ROOT_LEVEL : %s and node : %s ",  
		  STAR_LEVEL.Data(),STAR_GIT_VERSION.Data(),ROOT_LEVEL.Data(),gSystem->HostName());
#ifndef __CLING__1
    // ROOT and XROOTD
    // some rootd default dummy stuff
    TAuthenticate::SetGlobalUser("starlib");
    TAuthenticate::SetGlobalPasswd("ROOT4STAR");
#endif
    SysInfo_t info;
    if (gSystem->GetSysInfo(&info) >= 0) {
      cout << Form("with %i %s",info.fCpus,info.fModel.Data());
    }
    cout << endl;
    // note that the above bacward support the old mode for include whenever
    // it was not in .$STAR_HOST_SYS but one level up. The backward compatibility
    // can be removed only at the net root release ... 
    gSystem->SetIncludePath(" -D__ROOT__ -I.");
    gSystem->AddIncludePath(" -I./.$STAR_HOST_SYS/include -I./StRoot -I$STAR/.$STAR_HOST_SYS/include -I$STAR/StRoot -I$STAR -I/usr/include/mysql");
    if (GARFIELD_HOME != "$GARFIELD_HOME") gSystem->AddIncludePath(" -I$GARFIELD_HOME/Include -I$GARFIELD_HOME/Heed");
    if (XOPTSTAR != "$XOPTSTAR")           gSystem->AddIncludePath(" -I$XOPTSTAR/include");
    if (OPTSTAR != "$OPTSTAR" && XOPTSTAR != OPTSTAR)             
                                           gSystem->AddIncludePath(" -I$OPTSTAR/include");
    if (QTDIR != "$QTDIR")                 gSystem->AddIncludePath(" -I$QTDIR/include -I$QTDIR/include/Qt -I$QTDIR/include/QtCore -I$QTDIR/include/QtGui");
    //    gSystem->AddIncludePath(" -I$ROOTSYS/include");
    gSystem->SetBuildDir(".$STAR_HOST_SYS",kTRUE);
  }
  Char_t *file = gSystem->Which(rootlogon_path,"rootlogon.C",kReadPermission);
  if (file) {
    cout << "Found local " << file  << endl;
    gROOT->Macro(file);
  }
  delete [] file;
}

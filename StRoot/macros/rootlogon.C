// $Id: rootlogon.C,v 1.48 2006/10/10 14:00:02 jeromel Exp $
//
//=======================================================================
// owner:  Yuri Fisyak
// what it does: opens the ROOT session
//=======================================================================

{
  gSystem->ResetSignal(kSigChild,kTRUE);
  int rootlogon_fpe=0;const char *rootlogon_env=0;
  const Char_t *rootlogon_path  = ".:~";
  // eliminate the need to provide the custom ".rootrc" to activate Qt-layer 
  if (gSystem->Getenv("WITH_QT") && gSystem->Getenv("QTDIR")) 
  {
      // this allows to eliminate (or overwrite) the custom ".rootrc" file
      gEnv->SetValue("Gui.Backend","qt");
      gEnv->SetValue("Gui.Factory","qtgui");     
  }
  rootlogon_fpe = TString(gSystem->Getenv("STAR_VERSION")) == ".DEV";
#if 1
  if (! rootlogon_fpe && gClassTable->GetID("TGiant3") < 0) { // ! root4star
    rootlogon_fpe = TString(gSystem->Getenv("STAR_VERSION")) == ".DEV2";
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
  gStyle->SetPalette(55);
  gStyle->SetMarkerStyle(20);

  
  // Redefine prompt
  TString gPrompt =  gSystem->BaseName(gROOT->GetApplication()->Argv(0));
  gPrompt += " [%d] ";
  ((TRint*)gROOT->GetApplication())->SetPrompt( gPrompt.Data()); 
    
  //  if (gSystem->DynamicPathName("libNet.so",kTRUE))
  //    gSystem->Load("libNet.so");
  
  // 	Load StarRoot lib.
  if (gSystem->DynamicPathName("StarClassLibrary",kTRUE))
    gSystem->Load("StarClassLibrary");
  //  if (gPrompt.Index("root4star")>=0 && !strstr(gSystem->GetLibraries(),"libTable")) {
  gSystem->Load("libMatrix.so");
  gSystem->Load("libPhysics.so");
#if 1
  gSystem->Load("libGraf3d.so");
  if (!strstr(gSystem->GetLibraries(),"libTable")) {
    gSystem->Load("libGeom"); 
    gSystem->Load("libTable");
  }
  gSystem->Load("libEG.so");
  if (gSystem->DynamicPathName("StarRoot",kTRUE)) {
    gSystem->Load("StarRoot");
#if 1
    if (gSystem->DynamicPathName("KFParticle",kTRUE)) {
      gSystem->Load("KFParticle");
    }
#endif
    StCloseFileOnTerminate::Instantiate();
  }
  if (strstr(gSystem->GetLibraries(),"libTable")) {
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
#endif
   
   // 	Assign bif size of hashtable for STAR I/O
   TBufferFile::SetGlobalWriteParam(2003);



  // ROOT and XROOTD
  // some rootd default dummy stuff
  TAuthenticate::SetGlobalUser("starlib");
  TAuthenticate::SetGlobalPasswd("ROOT4STAR");

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
    TString STAR_LEVEL("$STAR_LEVEL");         gSystem->ExpandPathName(STAR_LEVEL);
    TString ROOT_LEVEL("$ROOT_LEVEL");         gSystem->ExpandPathName(ROOT_LEVEL);
    TString GARFIELD_HOME("$GARFIELD_HOME");   gSystem->ExpandPathName(GARFIELD_HOME);
    TString OPTSTAR("$OPTSTAR");               gSystem->ExpandPathName(OPTSTAR);
    TString XOPTSTAR("$XOPTSTAR");             gSystem->ExpandPathName(XOPTSTAR);
    TString QTDIR("$QTDIR");                   gSystem->ExpandPathName(QTDIR);
    cout <<  Form("QAInfo:You are using STAR_LEVEL : %s, ROOT_LEVEL : %s and node : %s ",  
		  STAR_LEVEL.Data(),ROOT_LEVEL.Data(),gSystem->HostName());
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

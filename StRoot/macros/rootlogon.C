// $Id: rootlogon.C,v 1.46 2006/04/30 21:55:02 jeromel Exp $
//
//=======================================================================
// owner:  Yuri Fisyak
// what it does: opens the ROOT session
//=======================================================================

{

  // #pragma optimize 0 <-- removed BuTracking 247
  //  set FloatPointException trap
  namespace rootlogon {
    int fpe=0;const char *env=0;
  }

  rootlogon::fpe = TString(gSystem->Getenv("STAR_VERSION")) == ".DEV";
  rootlogon::env = gSystem->Getenv("STARFPE");
  if (rootlogon::env) {
    if (strcmp(rootlogon::env,"YES")==0) rootlogon::fpe=1;
    if (strcmp(rootlogon::env,"NO" )==0) rootlogon::fpe=0;
  }
  if (rootlogon::fpe) {
    gSystem->SetFPEMask(kInvalid | kDivByZero | kOverflow );
    printf("*** Float Point Exception is ON ***\n");
  } else {
    printf("*** Float Point Exception is OFF ***\n");
  }

  
  // Redefine prompt
  TString gPrompt =  gSystem->BaseName(gROOT->GetApplication()->Argv(0));
  gPrompt += " [%d] ";
  ((TRint*)gROOT->GetApplication())->SetPrompt( gPrompt.Data()); 
    

  // 	Load StarRoot lib.
  gSystem->Load("StarClassLibrary");
  //  if (gPrompt.Index("root4star")>=0 && !strstr(gSystem->GetLibraries(),"libTable")) {
  if (!strstr(gSystem->GetLibraries(),"libTable")) {
    gSystem->Load("libGeom"); gSystem->Load("libTable");
  }
  gSystem->Load("StarRoot");
  if (strstr(gSystem->GetLibraries(),"libTable")) {
    gInterpreter->ProcessLine("typedef TCL              StCL;");              
    gInterpreter->ProcessLine("typedef TDataSet         St_DataSet ;");       
    gInterpreter->ProcessLine("typedef TDataSetIter     St_DataSetIter;");    
    gInterpreter->ProcessLine("typedef TFileSet         St_FileSet;");        
    gInterpreter->ProcessLine("typedef TVolume          St_Node;");           
    gInterpreter->ProcessLine("typedef TVolumePosition  St_NodePosition;");   
    gInterpreter->ProcessLine("typedef TVolumeView      St_NodeView;");       
    gInterpreter->ProcessLine("typedef TVolumeViewIter  St_NodeViewIter;");   
    gInterpreter->ProcessLine("typedef TObjectSet       St_ObjectSet;");      
    //  gInterpreter->ProcessLine("typedef TPointPosition   St_PointPosition;");  
    gInterpreter->ProcessLine("typedef TPoints3D        St_Points3D;");       
    gInterpreter->ProcessLine("typedef TPointsArray3D   St_PointsArray3D;");  
    gInterpreter->ProcessLine("typedef TPolyLineShape   St_PolyLineShape;");  
    gInterpreter->ProcessLine("typedef TTable           St_Table;");          
    gInterpreter->ProcessLine("typedef TTable3Points    St_Table3Points;");   
    gInterpreter->ProcessLine("typedef TTableIter       St_TableIter;");      
    gInterpreter->ProcessLine("typedef TTablePoints     St_TablePoints;");    
    gInterpreter->ProcessLine("typedef TTableSorter     St_TableSorter;");    
    gInterpreter->ProcessLine("typedef TTableDescriptor St_tableDescriptor;");
  }
  printf(" *** Start at Date : %s\n",TDatime().AsString());


  if (gROOT->IsBatch()==0 && gSystem->Getenv("OPENGL")) gROOT->Macro("GL.C");
   
  
  gROOT->SetStyle("Plain");// Default white background for all plots
   
  // The modes below are provided by Nick van Eijndhoven <Nick@phys.uu.nl>
  // from Alice.
  gStyle->SetCanvasColor(10);
  gStyle->SetStatColor(10);
  gStyle->SetTitleFillColor(10);
  gStyle->SetPadColor(10);
   
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
  // 	Assign bif size of hashtable for STAR I/O
  TBuffer::SetGlobalWriteParam(2003);



  // ROOT and XROOTD
  // some rootd default dummy stuff
  TAuthenticate::SetGlobalUser("starlib");
  TAuthenticate::SetGlobalPasswd("ROOT4STAR");

  // This is already implied in system.rootrc although one could use
  // this to switch to a beta version of the client library.
  //ROOT->GetPluginManager()->AddHandler("TFile","^root:","TXNetFile", "Netx", "TXNetFile(const char*,Option_t*,const char*,Int_t,Int_t)"); 



  //  Print version
  namespace _rootlogon_ {
    TString STAR_LEVEL("$STAR_LEVEL");
    TString ROOT_LEVEL("$ROOT_LEVEL");
    int dumy = gSystem->ExpandPathName(STAR_LEVEL);
    int dumy = gSystem->ExpandPathName(ROOT_LEVEL);
    int dumy = printf("QAInfo:You are using STAR_LEVEL : %s, ROOT_LEVEL : %s and node : %s \n",  
		      STAR_LEVEL.Data(),ROOT_LEVEL.Data(),gSystem->HostName());
  }
  // note that the above bacward support the old mode for include whenever
  // it was not in .$STAR_HOST_SYS but one level up. The backward compatibility
  // can be removed only at the net root release ...
  gSystem->SetIncludePath("-I. -I./.$STAR_HOST_SYS/include -I./StRoot -I$STAR/.$STAR_HOST_SYS/include -I$STAR/StRoot -I/usr/include/mysql");
   
}

// $Id: rootlogon.C,v 1.37 2004/01/27 18:43:26 perev Exp $
//
//=======================================================================
// owner:  Yuri Fisyak
// what it does: opens the ROOT session
//=======================================================================

{

#pragma optimize 0
  //  set FloatPointException trap
  if (strstr(gSystem->Getenv("STAR"),".DEV")
  ||         gSystem->Getenv("STARFPE")) {
    gSystem->SetFPEMask(kInvalid | kDivByZero | kOverflow );
    printf("*** Float Point Exception is ON ***");
  }

  // 	Load StarRoot lib.
  gSystem->Load("StarRoot");
  if (!strstr(gSystem->GetLibraries(),"libTable")) gSystem->Load("libTable");

  //    G__loadfile("iostream.h");
  TString gPrompt =  gSystem->BaseName(gROOT->GetApplication()->Argv(0));
  gPrompt += " [%d] ";
  ((TRint*)gROOT->GetApplication())->SetPrompt( gPrompt.Data()); // Redefine prompt
    
  printf("\nWelcome to the ROOT tutorials\n\n");
  printf("\nType \".x demos.C\" to get a toolbar from which to execute the demos\n");
  printf("\nType \".x demoshelp.C\" to see the help window\n\n");
  printf(" *** Start at Date : %s\n",TDatime().AsString());


  if (gROOT->IsBatch()==0 && gSystem->Getenv("OPENGL")) gROOT->Macro("GL.C");
   
  
  gROOT->SetStyle("Plain");// Default white background for all plots
   
  // 	The modes below are provided by Nick van Eijndhoven <Nick@phys.uu.nl>
  // 	from Alice.
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
   
  // 	Assign bif size of hashtable for STAR I/O
  TBuffer::SetGlobalWriteParam(2003);


  // some rootd default dummy stuff
  TAuthenticate::SetGlobalUser("starlib");
  TAuthenticate::SetGlobalPasswd("ROOT4STAR");



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
  // it was not in .$STAR_HOST_SYS but one level up. The bacward compatibility
  // can be removed only at the net root release ...
  gSystem->SetIncludePath("-I. -I./.$STAR_HOST_SYS/include -I./include -I./StRoot -I$STAR/.$STAR_HOST_SYS/include -I$STAR/include -I$STAR/StRoot -I$STAF/inc -I$CERN_ROOT/include -I$ROOTSYS/include");
  
}

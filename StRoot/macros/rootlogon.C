// $Id: rootlogon.C,v 1.12 1999/08/06 15:00:41 fisyak Exp $
// $Log: rootlogon.C,v $
// Revision 1.12  1999/08/06 15:00:41  fisyak
// Keep formwer bfc.C as BFC.C
//
// Revision 1.11  1999/07/22 15:31:41  fine
// US letter paper size has been set as default one
//
// Revision 1.10  1999/07/17 23:28:09  fisyak
// Add QAInfo tag
//
// Revision 1.9  1999/07/09 20:55:00  didenko
// set O0 (VP)
//
// Revision 1.8  1999/07/09 01:22:03  fisyak
// CleanUp, set sequantial processing of Chain flags, flags are not truncated to 3 characters any more
//
// Revision 1.7  1999/06/27 22:45:33  fisyak
// Merge StRootEvent and StEvent
//
// Revision 1.6  1999/06/15 16:19:11  fine
// New logon script
//
// Revision 1.5  1999/06/11 23:01:56  perev
// cleanup
//
// Revision 1.4  1999/05/21 15:34:00  kathy
// made sure Log & Id are in each file and also put in standard comment line with name of owner
//
//=======================================================================
// owner:  Yuri Fisyak
// what it does: opens the ROOT session
//=======================================================================

{
#pragma optimize 0
  //    gInterpreter->ProcessLine(".O0");
    G__loadfile("iostream.h");
    TString gPrompt =  gSystem->BaseName(gROOT->GetApplication()->Argv(0));
    gPrompt += " [%d] ";

   ((TRint*)gROOT->GetApplication())->SetPrompt( gPrompt.Data()); // Redefine prompt

   printf("\nWelcome to the ROOT tutorials\n\n");
   printf("\nType \".x STAR_Demos.C\" to get a toolbar from which to execute the STAR demos\n");
   printf("\nType \".x demos.C\" to get a toolbar from which to execute the demos\n");
   printf("\nType \".x demoshelp.C\" to see the help window\n\n");
   {
 TDatime start;
 
 int idate=start.GetDate();
 int itime=start.GetTime();

 int year=idate/10000;
 int month=(idate%10000)/100;
 int day=idate%100;
 int hh=itime/10000;
 int mm=(itime%10000)/100;
 int ss=itime%100;

 char* c[12]={"jan","feb","mar","apr","may","jun",
              "jul","aug","sep","oct","nov","dec"};

 cout << " *** Start at Date : " << day << "-" << c[month-1] << "-" << year
      << " Time : " << hh << ":" << mm << ":" << ss << " ***" << endl;
 cout << endl;

   }


//gROOT->SetStyle("Plain");// Default white background for all plots

 // The modes below are provided by Nick van Eijndhoven <Nick@phys.uu.nl>
 // from Alice.

 gStyle->SetCanvasColor(10);
 gStyle->SetStatColor(10);
 gStyle->SetTitleColor(10);
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
//      Print version
 TString STAR_LEVEL("$STAR_LEVEL");
 TString ROOT_LEVEL("$ROOT_LEVEL");
 gSystem->ExpandPathName(STAR_LEVEL);
 gSystem->ExpandPathName(ROOT_LEVEL);
 printf("QAInfo:You are using STAR_LEVEL : %s and ROOT_LEVEL : %s \n",  STAR_LEVEL.Data(),ROOT_LEVEL.Data());
}
 

// $Id: rootlogon.C,v 1.7 1999/06/27 22:45:33 fisyak Exp $
// $Log: rootlogon.C,v $
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
    TString gPrompt =  gROOT->GetApplication()->Argv(0);
    gPrompt += " [%d] ";

   ((TRint*)gROOT->GetApplication())->SetPrompt( gPrompt.Data()); // Redefine prompt

   printf("\nWelcome to the ROOT tutorials\n\n");
   printf("\nType \".x STAR_Demos.C\" to get a toolbar from which to execute the STAR demos\n");
   printf("\nType \".x demos.C\" to get a toolbar from which to execute the demos\n");
   printf("\nType \".x demoshelp.C\" to see the help window\n\n");
#pragma includepath "./StRoot"
#pragma includepath "/afs/rhic/star/packages/dev/StRoot"
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

 // Positioning of axes labels
 gStyle->SetTitleOffset(1.2);

// 	Assign bif size of hashtable for STAR I/O
TBuffer::SetGlobalWriteParam(2003);
printf("fgMapSize=%d\n",TBuffer::GetGlobalWriteParam());
//      Print version
 TString STAR_LEVEL("$STAR_LEVEL");
 TString ROOT_LEVEL("$ROOT_LEVEL");
 gSystem->ExpandPathName(STAR_LEVEL);
 gSystem->ExpandPathName(ROOT_LEVEL);
 printf("You are using STAR_LEVEL : %s and ROOT_LEVEL : %s \n",  STAR_LEVEL.Data(),ROOT_LEVEL.Data());
}
 

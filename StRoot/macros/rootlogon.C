// $Id: rootlogon.C,v 1.26 2000/06/23 23:49:02 fisyak Exp $
//
//=======================================================================
// owner:  Yuri Fisyak
// what it does: opens the ROOT session
//=======================================================================

{
#pragma optimize 0
  //    gInterpreter->ProcessLine(".O0");
    gROOT->ProcessLine(".O0"); 
    //    G__loadfile("iostream.h");
    TString gPrompt =  gSystem->BaseName(gROOT->GetApplication()->Argv(0));
    gPrompt += " [%d] ";
    ((TRint*)gROOT->GetApplication())->SetPrompt( gPrompt.Data()); // Redefine prompt
    
    printf("\nWelcome to the ROOT tutorials\n\n");
    //  printf("\nType \".x STAR_Demos.C\" to get a toolbar from which to execute the STAR demos\n");
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
     
     char* c[12]={"Jan","Feb","Mar","Apr","May","Jun",
		  "Jul","Aug","Sep","Oct","Nov","Dec"};
     
     // cout << " *** Start at Date : " << day << "-" << c[month-1] << "-" << year
     //      << " Time : " << hh << ":" << mm << ":" << ss << " ***" << endl;
     // cout << endl;
     printf(" *** Start at Date : %i-%s-%i Time : %i:%i:%i ***\n",day, c[month-1], year, hh, mm, ss);
   }
   
   
   gROOT->SetStyle("Plain");// Default white background for all plots
   
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
   printf("QAInfo:You are using STAR_LEVEL : %s, ROOT_LEVEL : %s and node : %s \n",  
	  STAR_LEVEL.Data(),
	  ROOT_LEVEL.Data(),
	  gSystem->HostName());
   gSystem->Exec("echo $USER from $HOST in STAR_LEVEL=$STAR_LEVEL / STAR_VERSION=$STAR_VERSION  `date` >>  $GROUP_DIR/statistics/root4star${STAR_VERSION}");
   gSystem->SetIncludePath("-I./include -I./StRoot -I$STAR/include -I$STAR/StRoot -I$STAF/inc -I$CERN_ROOT/include -I$ROOTSYS/src");
#ifdef RFIO    
   {
     // Load rfio map
     
     TNamed *tn=0;
     TList *rfiomap = new TList();
     rfiomap->SetName(".rfiomap");
     gROOT->GetListOfSpecials()->Add(rfiomap);
     char *map[] = {
       ":rfio:rmine02.rcf.bnl.gov:", "/star/data01",
       ":rfio:rmine02.rcf.bnl.gov:", "/star/data02",
       ":rfio:rmine02.rcf.bnl.gov:", "/star/data07",
       ":rfio:rmine02.rcf.bnl.gov:", "/star/data08",
       
       ":rfio:rmine02.rcf.bnl.gov:", "/star/rcf/GC",
       ":rfio:rmine02.rcf.bnl.gov:", "/star/rcf/daq",
       ":rfio:rmine02.rcf.bnl.gov:", "/star/rcf/daq/1999   ",  
       ":rfio:rmine02.rcf.bnl.gov:", "/star/rcf/daq/l3",
       
       ":rfio:rmine02.rcf.bnl.gov:", "/star/rcf/daq/2000/06",
       ":rfio:rmine02.rcf.bnl.gov:", "/star/rcf/disk0",
       ":rfio:rmine02.rcf.bnl.gov:", "/star/rcf/disk1",
       ":rfio:rmine02.rcf.bnl.gov:", "/star/rcf/dst",
       ":rfio:rmine02.rcf.bnl.gov:", "/star/rcf/pwg",
       ":rfio:rmine02.rcf.bnl.gov:", "/star/rcf/qa",
       ":rfio:rmine02.rcf.bnl.gov:", "/star/rcf/scratch",
       ":rfio:rmine02.rcf.bnl.gov:", "/star/rcf/simu",
       ":rfio:rmine02.rcf.bnl.gov:", "/star/rcf/star",
       ":rfio:rmine02.rcf.bnl.gov:", "/star/rcf/test",
      
       ":rfio:rmine03.rcf.bnl.gov:", "/star/data03",
       ":rfio:rmine03.rcf.bnl.gov:", "/star/data04",
       ":rfio:rmine03.rcf.bnl.gov:", "/star/data05",
       ":rfio:rmine03.rcf.bnl.gov:", "/star/data06",
       ":rfio:rmine03.rcf.bnl.gov:", "/star/data09",
       ":rfio:rmine03.rcf.bnl.gov:", "/star/data10",
      
       ":rfio:rmine03.rcf.bnl.gov:", "/star/rcf/daq/2000/01",
       ":rfio:rmine03.rcf.bnl.gov:", "/star/rcf/daq/2000/02",
       ":rfio:rmine03.rcf.bnl.gov:", "/star/rcf/daq/2000/03",
      
       ":rfio:rmine03.rcf.bnl.gov:", "/star/rcf/reco",
       
       ":rfio:rmine02.rcf.bnl.gov:", "/star/rcf/daq/2000",
       0
     };
     
     for (int i=0;map[i];i+=2) {
       tn = new TNamed();
       tn->SetName (map[i+1]);
       tn->SetTitle(map[i+0]);
       rfiomap->Add(tn);
     }
   }
#endif
}

// $Id: DstStruct.C,v 3.7 2000/08/17 01:31:26 fine Exp $

#include <iomanip.h>
class StChain;
StChain *chain;

//=======================================================================
// owner: Valery Fine
// what it does: Prints the structure of the selected events from the DST file
//=======================================================================

//__________________________________________________________________________
void DstStruct(Int_t firstEvent, Int_t numberOfEvents, const char *MainFile)

{
  cout << endl
       << "Usage: root.exe -b -q DstStruct.C(\"rootfile name.dst.root\",numberOfEvents)" << endl
       << "-----" << endl
       << "  or   root.exe -b -q DstStruct.C(firstEvent, numberOfEvents,\"rootfile name.dst.root\")" << endl
       << "  or   root.exe -b -q DstStruct.C(firstEvent, \"rootfile name.dst.root\")" << endl
       << "  or   root.exe -b -q DstStruct.C(\"rootfile name.dst.root\",numberOfEvents)" << endl
       << endl
       << "    It prints the structure of the selected events from the DST file."  << endl;

  if (numberOfEvents < 0) return;

  gSystem->Load("libStar");
  gSystem->Load("St_base");
  gSystem->Load("StChain");
    
  gSystem->Load("libgen_Tables");
  gSystem->Load("libsim_Tables");
  gSystem->Load("libglobal_Tables");

  gSystem->Load("StIOMaker");
  gSystem->Load("xdf2root");

//  Setup top part of chain
  chain = new StChain("bfc");
  chain->SetDebug();

// setup chain with IOMaker - can read in .dst.root, .dst.xdf files
  StIOMaker *IOMk = new StIOMaker("IO","r",MainFile,"bfcTree");
  IOMk->SetIOMode("r");
  IOMk->SetBranch("*",0,"0");                 //deactivate all branches
  if (strstr(MainFile,".dst.")) {
    IOMk->SetBranch("dstBranch",0,"r");    //activate dst Branch      
  } else {  
    IOMk->SetBranch("eventBranch",0,"r"); //activate event Branch  
  }
// --- now execute chain member functions
  chain->Init();

  if (firstEvent > 1) IOMk->Skip(firstEvent-1);

  TDataSet *ds=0;

  int iret=0,iev=0;
  // Loop over events
  if (!numberOfEvents) numberOfEvents = 9999;
  int counter = numberOfEvents;
  Float_t total = 0;
  Float_t totalPoints = 0;
  TStopwatch readTimer;
  for (iev = 0; iev < counter; iev++) {         // goto loop code
     readTimer.Start(kFALSE);
     if (iret = chain->MakeEvent()) break;
     readTimer.Stop();
     ds=chain->GetDataSet("dst");
     if (ds) {
         TDataSetIter next(ds);
         cout << endl << ds->Path().Data() << " datasets found" << endl;
         TDataSet *nextDs = 0;
          cout << endl 
               << setw(15) << "Table Name  " 
               << setw(25) << "Table type     " 
               << setw(10) << "# rows"
               << setw(14) << "Size (KBytes)"
               << endl;

          cout << setfill('-')
               << setw(15) << " " 
               << setw(25) << " " 
               << setw(10) << " "
               << setw(14) << " "
               << setw(3)  << ""
               << endl;
          cout << setfill(' ');

         while (nextDs = (TDataSet *)next()){
	   const char *thisName = nextDs->GetName();
          if (!strcmp(thisName,"RunEvent")) {continue;}
          TTable &t = *(TTable *)nextDs;
	  Float_t thisSize = t.GetTableSize()*t.GetRowSize();
	  total += thisSize;
	  if (!strcmp(thisName,"point")) totalPoints += thisSize;
          cout << setw(15) << t.GetName() 
               << setw(25) << t.GetType() 
               << setw(10) << t.GetNRows() 
               << setw(14) << setprecision(3) << thisSize/1024.
               << setw(3)  << "Kb"
               << endl;
         }
          cout << setfill('-')
               << setw(15) << " " 
               << setw(25) << " " 
               << setw(10) << " "
               << setw(14) << " "
               << endl;
          cout << setfill(' ')
               << endl;
         // next.Reset();
         // while (nextDs = (TDataSet *)next()){
         //   if (nextDs->HasData()) {
         //      cout << endl << "----------------- next table " 
         //           << nextDs->GetName() 
         //           << " size = " << ((TTable *)nextDs)->GetNRows() 
         //           << " row(s) ---------------------" 
         //           << endl;
         //  }
	 //  nextDs->Print();
         // }  
     }
     cout << " =================  END OF EVENT " << iev+1 << "  ================= " << endl << endl;
  }
  if (iev) { 
     Float_t kBytes = total/1024;
     readTimer.Stop();
     Double_t thisTime = readTimer.RealTime();
     cout << endl 
         << " *** DstStruct.C "
	 << endl 
	 << " \t-- Total: " 
	 << iev 
	 << " events ("  
	 << iev/thisTime
	 << " events/sec);  "
	 << setprecision(5) << total/1024. 
	 << " Kbytes ("
	 << kBytes/thisTime 
	 << " Kbytes/sec); "
	 << endl
	 << "----------- \tstarting from " 
	 << firstEvent 
	 << " have been printed out" 
	 << endl << endl;
}
  else {
    cout << "DstStruct.C - no events were printed" << endl;
  }
  cout << "See: \"http://www.rhic.bnl.gov/STAR/html/comp_l/ofl/dst_table_model.html\""
       << endl 
       << "      Web site for further information" 
       << endl << endl;
  if (total>0) {
    cout << "Total:" << total/1024. << " KBytes,  total hits: " 
         << totalPoints/1024. << "  Kbytes, hits/total: "<< 100*totalPoints/total << " %"
         << endl;
  }
  chain->Finish();   
}

//__________________________________________________________________________
void DstStruct()
{   cout << endl << "*** Error *** " 
         << "           Please, provide the input file name at least" << endl;
     DstStruct(0,-1,0);
}
//__________________________________________________________________________
void DstStruct(Int_t numbertOfEvents)
{   DstStruct(); }

//__________________________________________________________________________
void DstStruct(const char *MainFile, Int_t numbertOfEvents)
{   DstStruct(1,numbertOfEvents,MainFile);  }

//__________________________________________________________________________
void DstStruct(Int_t firstEvent, const char *MainFile)
{   DstStruct(firstEvent,1,MainFile);  }

//__________________________________________________________________________
void DstStruct(const char *MainFile="/afs/rhic/star/data/samples/gstar.dst.root")
{   DstStruct(1,1,MainFile); }

//__________________________________________________________________________
//__________________________________________________________________________
// $Log: DstStruct.C,v $
// Revision 3.7  2000/08/17 01:31:26  fine
// Estimatine time for event branches if event file is provided
//
// Revision 3.6  2000/08/17 00:54:16  fine
// more accurate timing
//
// Revision 3.4  2000/08/15 15:24:48  fine
// new output format
//
//__________________________________________________________________________
// Revision 3.3  2000/08/02 16:39:50  fine
// Change order of Skip and Init methods
//
// Revision 3.2  2000/08/02 16:36:39  fine
// Fixed wrong counter
//
// Revision 3.1  2000/08/02 01:32:02  fine
// New macro to print the struture of the selected event from the DST file
//__________________________________________________________________________


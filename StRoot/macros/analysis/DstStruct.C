// $Id: DstStruct.C,v 3.4 2000/08/15 15:24:48 fine Exp $

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
  IOMk->SetBranch("dstBranch",0,"r"); //activate dst Branch  
// --- now execute chain member functions
  chain->Init();

  if (firstEvent > 1) IOMk->Skip(firstEvent-1);

  TDataSet *ds=0;

  int iret=0,iev=0;
  // Loop over events
  if (!numberOfEvents) numberOfEvents = 9999;
  int counter = numberOfEvents;
  for (iev = 0; iev < counter; iev++) {         // goto loop code
     chain->Clear();
     iret = chain->Make();
     if (iret) break;
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
          if (!strcmp(nextDs->GetName(),"RunEvent")) {continue;}
          TTable &t = *(TTable *)nextDs;
          cout << setw(15) << t.GetName() 
               << setw(25) << t.GetType() 
               << setw(10) << t.GetNRows() 
               << setw(14) << setprecision(3) << t.GetTableSize()*t.GetRowSize()/1024.
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
    cout << endl << 
   " *** DstStruct.C -- Total: " << iev << 
   " events starting from " << firstEvent << " have been printed out" << endl << endl; 
  }
  else {
    cout << "DstStruct.C - no events were printed" << endl;
  }
  cout << "See: \"http://www.rhic.bnl.gov/STAR/html/comp_l/ofl/dst_table_model.html\""
       << endl 
       << "      Web site for further information" 
       << endl << endl;

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
{   DstStruct(firstEvent,numbertOfEvents,MainFile);  }

//__________________________________________________________________________
void DstStruct(Int_t firstEvent, const char *MainFile)
{   DstStruct(firstEvent,1,MainFile);  }

//__________________________________________________________________________
void DstStruct(const char *MainFile="/afs/rhic/star/data/samples/gstar.dst.root")
{   DstStruct(1,1,MainFile); }

//__________________________________________________________________________
//__________________________________________________________________________
// $Log: DstStruct.C,v $
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


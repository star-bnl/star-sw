#include <iostream.h>

class    St_geant_Maker;
class    EEmcMCData;

#define WRITE_FILE

TBrowser       *b    = 0;
St_geant_Maker *geant= 0;
// reads .fzd file and writes EEevents to a TTree

// ______________________________________________
void fzd2TTree(const Int_t Nevents=100, TString inFile ="mc_eveID1") {
  Int_t  i=0;
  // inFile="mc_one_pid3eta30.pt1.75";

  TString  inPath,outPath;
 
  inPath=outPath="";

  TString outFile=inFile;

  //  inFile.ReplaceAll("balewski","zolnie");  inFile.ReplaceAll("sim2002/d","sim2002");

  gROOT->LoadMacro("bfc.C"); 
  bfc(0,"fzin sim_T gen_T",inPath+inFile+".fzd");

  gSystem->Load("EEmc.so"); 
  TFile f(outPath+outFile+".root","RECREATE");   //create a Tree file tree4.root
  //old TFile f("tree.root","RECREATE");   //create a Tree file tree4.root
  TTree t4("t4","A Tree with Events");   // Create a ROOT Tree

  EEevent eveR;// Raw MC event, local, not saved  
  EEevent *eve=new EEevent();   // 'Fast sim'  Event stored in TTRee 
  t4.Branch("EEDst", "EEevent", &eve,16000,99);
  EEmcMCData      *evIN     = new EEmcMCData;
  
  for (i=1; i<=Nevents; i++ ) {
    printf("%2d ====================================\n",i);
   chain->Clear();
    if (chain->Make(i)>=kStEOF) break;

    int nh;
     if ( (nh = evIN->readEventFromChain(chain)) >0) {
      cerr << "actual hits " << nh << endl;
      if(i<3)evIN->print();
    } else {
      cerr << "EEMC/CTF hits not found" << endl;
      continue;
    }
     
    // generation of TTree
    evIN->write(&eveR); // Clear & Store EEevent
    //   printf("XXXXXXXXX  raw event.print():\n");eveR.print();
       
    eveR.sumRawMC(eve); //sum hits with any detector
    //    printf("XXXXXXXXX  summed event.print():\n");eve->print();
    
    t4.Fill();  // Fill the tree   
  }
  Int_t nevent = (Int_t)t4->GetEntries();
  printf("Total events in TTree=%d\n",nevent);
  
  
  f.Write() ;  // Write the TTree file header
  
}







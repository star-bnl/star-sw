#include <iostream.h>

class    St_geant_Maker;
class    EEmcMCData;


TBrowser       *b    = 0;
St_geant_Maker *geant= 0;
// reads .fzd file and prints on the screen

// ______________________________________________
void fzd2EEevent(const Int_t Nevents=10000, TString fzFile ="../sim2003/mc_eve2.fzd") {
  fzFile ="pid6.fzd";
  fzFile ="dAu83.fzd";
  //fzFile ="../sim2003/mc_pi0-1.5.fzd";
  //fzFile ="../sim2003/mc_eve3.fzd";
  Int_t  i=0;
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("StarClassLibrary");

  gROOT->LoadMacro("$STAR/StRoot/macros/bfc.C");
  gSystem->Load("StEEmcUtil.so"); 
  bfc(0,"fzin sim_T gen_T",fzFile);
  
  TString outFile=fzFile.ReplaceAll(".fzd",".root");
  TFile f(outFile,"RECREATE");   //create a Tree file
  TTree t4("EEtree","A Tree with Events");   // Create a ROOT Tree
  
  EEeventDst eveR;// Raw MC event, local, not saved  
  EEeventDst *eve=new EEeventDst();   // 'Fast sim'  Event stored in TTRee 
  t4.Branch("EEdst", "EEeventDst", &eve,16000,99);
  EEmcMCData      *evIN     = new EEmcMCData;

  for (i=1; i<=Nevents; i++ ) {
    chain->Clear();
    if (chain->Make(i)>=kStEOF) break;
    printf("%2d ====================================\n",i);
    int nh=-1;
    nh = evIN->readEventFromChain(chain);

    if(nh<=0) {
      printf("EEMC/CTF hits not found");
      continue;
    }
    
    if(i==1) {
      printf("  actual RAW geant EEMC hits =%d nh\n",nh);
     evIN->print();
    }

    // generation of TTree
    evIN->write(&eveR); // Clear & Store EEeventDst
    //printf("XXXXXXXXX  raw event.print():\n");eveR.print();

    eveR.sumRawMC(eve); //sum hits with any detector
    //printf("XXXXXXXXX  summed event.print():\n");eve->print();
   
    t4.Fill();  // Fill the tree   
  }

  Int_t nevent = (Int_t)t4->GetEntries();
  printf("Total events in TTree=%d file=%s\n",nevent,outFile.Data());
  t4->Print();

  f.Write() ;  // Write the TTree file header


}


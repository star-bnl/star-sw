TCanvas* c1;
static const int NX=2;
TH1F* HF[NX*2+1];
TH1F* HB[NX*2+1];

void bitcheck(int run=16043074, int file=1, int nEvents=10000, int pp=0){
  char name[200]; sprintf(name,"trg/run%d.%d.dat",run,file);
  cout << "Reading up to "<<nEvents<<" events from "<<name<<endl;

  LoadLibs();   
   
  StChain* chain = new StChain;
  StIOMaker* iomaker = new StIOMaker("IO","r", name);
  StTriggerDataMaker* trgmaker = new StTriggerDataMaker(); 
  //trgmaker->setDebug(1);
  StFmsTriggerMaker* fmstrg = new StFmsTriggerMaker(); 
  fmstrg->useTrgData();
  fmstrg->forceRunNumber(run);
  StFmsBitCheckMaker* bitcheck = new StFmsBitCheckMaker();

  chain->Init();

  c1=new TCanvas("FMS","FMS",700,800);
  gStyle->SetStatW(0.4); gStyle->SetStatH(0.4);
  for(int i=0; i<=NX*2; i++){
    int x=i-NX;  
    char tt[100];
    sprintf(tt,"FMS xing=%d",x);
    HF[i]=new TH1F(tt,tt,50,0,10000); HF[i]->SetFillColor(kRed);
    sprintf(tt,"BBC xing=%d",x);      
    HB[i]=new TH1F(tt,tt,50,0,10000); HB[i]->SetFillColor(kRed);
  }

  unsigned long long bxkeep=0;
  for(int iev=0; iev<nEvents; iev++){
    if(iev%100==0) cout << "****event="<<iev<<endl;
    chain->Clear();
    int ierr=chain->Make();
    if(ierr>1) break;

    printf("ierr=%d\n",ierr);
    StTriggerData *td = (StTriggerData*)chain->GetDataSet("StTriggerData")->GetObject();
    for(int i=0; i<=2*pp; i++){
      int x=i-pp;  
      int sumF=0, sumB=0;
      for(int crt=1; crt<=4; crt++){
	for(int adr=0; adr<16; adr++){
	  for(int ch=0; ch<32; ch++){
	    sumF+=td->fmsADC(crt,adr,ch,x);
	  }
	}
      }
      for(int ch=0; ch<16; ch++){
	sumB+=td->bbcADC(0,ch,x);
	sumB+=td->bbcADC(1,ch,x);
      }
      HF[i]->Fill(sumF);
      HB[i]->Fill(sumB);
    }	    
  }
  chain->Finish();  

  c1->Divide(2,NX*2+1);
  for(int i=0; i<=2*NX; i++){
    TVirtualPad *pad1 = c1->cd(i*2+1); pad1->SetLogy(); HF[i]->Draw();
    TVirtualPad *pad2 = c1->cd(i*2+2); pad2->SetLogy(); HB[i]->Draw();
  }
  c1->Update();
  char fname[100];
  sprintf(fname,"fms_%d.png",run);
  c1->SaveAs(fname);

  delete chain;
}

void LoadLibs() {
  gROOT->Macro("loadMuDst.C");
  gSystem->Load("libPhysics");
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("StUtilities");
  gSystem->Load("StEvent");
  gSystem->Load("libStDb_Tables.so");
  gSystem->Load("StDbLib.so");
  gSystem->Load("St_db_Maker");
  gSystem->Load("StDbBroker");
  //gSystem->Load("StFmsDbMaker");
  gSystem->Load("StFmsTriggerMaker");
};

class EEfeeDataBlock;
class EEfeeRawEvent;
class EEfeeRunDescr;

TFile *f;

void rdFeeTTreeUser1(int max=10000) {
  gSystem->Load("StRoot/StEEmcUtil/EEfeeRaw/libEEfeeRaw.so");
  gStyle->SetPalette(1,0);

  TH2F *h2[8];
  initHisto(h2); 
 
  TH2F *hd[8];
  initHistoD(hd); 
 
  TString fname="/star/u/eemcdb/miniDaq/Mar1/run01001.root";

  TFile   *f  = new TFile(fname);
  TTree   *t  = (TTree *)f->Get("fee");
  TBranch *bd = t->GetBranch("desc");
  TBranch *be = t->GetBranch("evt");
 
  EEfeeRawEvent  *eve = new  EEfeeRawEvent();
  EEfeeRunDescr  *des = new  EEfeeRunDescr();

  be->SetAddress(&eve);
  bd->SetAddress(&des);

  Int_t nentries = (Int_t)t->GetEntries();
  cout << nentries << endl;
  Int_t nbe=0;
  Int_t nbd=0;
  int nAcc=0;
  for(Int_t ieve=0; ieve<nentries && ieve<max; ieve++) {
    nbe += be->GetEntry(ieve);
    nbd += bd->GetEntry(ieve);
    TClonesArray  *block=eve->block;
    if(ieve%20==0)printf("EfeeRawEvent ID=%d with DataBlock entered=%d of %d, accepted %d events of %d\n", eve->getID(),block->GetEntries(),block->GetSize(), nAcc,ieve);
    int j;
    //    eve->print();
    

    //===========================================================
    // ===========  verify consistency ==========================

    int token=-1;
    int n256=0;    
    for(j=0;j<block->GetEntries();j++) {// loop over data blocks
      EEfeeDataBlock *blk= (EEfeeDataBlock *)block->At(j);
      if(j==0) token=blk->getToken();
      if(token!=blk->getToken()){ token=-2; break;}
      if(blk->getCrateID()<3 || blk->getCrateID()>5) { token =-3; break;}

      if(blk->getCrateID()!=j+3) {// expected cartes 3,4,5
	//printf("\n\nStarnge crateID=%d, STOP\n",blk->getCrateID());
	token=-4; break;
      }      
     
      
      // eliminate n*256 events
      for(int k=0;k<128;k++) {
	int adc=blk->getData()[k];
	if(adc==256)n256++;
      }
      if(n256>5){ token =-5;  ;break;}
    }

    if(token<0) {
      //printf("skip eveID=%d corrupted token or crate ID\n",eve->getID());
      continue;
    }

    nAcc++;
    //===========================================================
    //===============  sort data ================================

    int x3_71=-1;
    int x3_61=-1;
    int x4_10=-1;
    int x4_1=-1;
    int x5_95=-1;
    int x5_99=-1;

    for(j=0;j<block->GetEntries();j++) {// loop over data blocks
      EEfeeDataBlock *blk= (EEfeeDataBlock *)block->At(j);

      for(int k=0;k<128;k++) {
       int adc=blk->getData()[k];
       h2[j]->Fill(adc,k);  // increment histos
      }

      switch( blk->getCrateID()) {
      case 3: x3_71=blk->getData()[71];x3_61=blk->getData()[61]; break;
      case 4: x4_10=blk->getData()[10];x4_1=blk->getData()[1]; break;
      case 5: x5_95=blk->getData()[95];x5_99=blk->getData()[99]; break;
      }
    }// end of loop over blocks
    
    hd[0]->Fill(x4_10,x3_71);
    hd[1]->Fill(x4_10,x5_95);
    hd[2]->Fill(x4_10,x4_1);

    hd[3]->Fill(x3_61,x3_71);
    hd[4]->Fill(x3_61,x4_1);
    hd[5]->Fill(x5_99,x4_1);
    hd[6]->Fill(x5_99,x5_95);
    if(x4_10>40 && x4_1 >40  && x3_71>60  && x3_61>80  && x5_99>60 && x5_95>80  )
      eve->print();
  }

    //===========================================================
    //===============  draw plots  ================================
    
  c=new TCanvas();
  c->Divide(1,3);
  int i;
  for (i=0;i<3;i++){
    c->cd(i+1);
    h2[i]->Draw("colz");
    gPad->SetLogz();
  }

  c=new TCanvas();
  c->Divide(3,3);
  for (i=0;i<7;i++){
    c->cd(i+1);
    hd[i]->Draw("colz");
    gPad->SetLogz();
  }
}

//----------------------------------
void initHisto(TH2F **h2) {
 
    int i;
    for(i=0;i<3;i++) { //
      int crate=i+3;
      char tt1[100], tt2[100];
      sprintf(tt1,"cr%dspeR",crate);
      sprintf(tt2,"chan vs. raw ADC , carte=%d",crate);
      // h2[i]= new TH2F(tt1,tt2, 4048,-0.5,4097.5,129,-0.5,128.5);
      h2[i]= new TH2F(tt1,tt2, 200,0.,400,129,-0.5,128.5);
    }
}

//----------------------------------
void initHistoD(TH2F **h2) {

  h2[0]= new TH2F("dd0"," cr3/ch71 vs. cr4/ch10", 100,0.,200,100,0.,200);
  h2[1]= new TH2F("dd1"," cr5/ch95 vs. cr4/ch10", 100,0.,200,100,0.,200);
  h2[2]= new TH2F("dd2"," cr4/ch1 vs. cr4/ch10", 100,0.,200,100,0.,200);
  h2[3]= new TH2F("dd3"," cr3/ch71 vs. cr3/ch61", 100,0.,200,100,0.,200);
  h2[4]= new TH2F("dd4"," cr4/ch1 vs. cr3/ch61", 100,0.,200,100,0.,200);
  h2[5]= new TH2F("dd5"," cr4/ch1 vs. cr5/ch99", 100,0.,200,100,0.,200);
  h2[6]= new TH2F("dd6"," cr5/ch95 vs. cr5/ch99", 100,0.,200,100,0.,200);
}


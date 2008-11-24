TH2F *h2Ped=0, *h2Sig=0, *h2Peak;
TH1F *h1Stat=0, *h2CapStat;
TH1F *h1p7=0, *h1p77=0, *h1p777=0;
TH2S *h2PedS=0;

void mergePed(){
  int cap1=0, cap2=127;
  gStyle->SetPalette(1,0);

  int run=9067013;
  char *pathIn="outX/";
  char *pathOut="iter3/";
  char txt[1000], txt2[1000];
  int nCap=128;

  sprintf(txt,"%s/pedBprsR%dallCap.hist.root",pathOut,run);
  fd2=new TFile(txt,"recreate"); 
  int nb=800, xx1=100,xx2=300;
  h1p7=new TH1F("pedSoftId7","pedestals(cap) , softID=7, crate=0; pedestal(ADC)", nb,xx1,xx2);
  h1p607=new TH1F("pedSoftId607","pedestals(cap) , softID=607, crate=1; pedestal(ADC)",  nb,xx1,xx2);
  h1p2407=new TH1F("pedSoftId2407","pedestals(cap) , softID=2407, crate=2; pedestal(ADC)",  nb,xx1,xx2);
  h1p3007=new TH1F("pedSoftId3007","pedestals(cap) , softID=3007, crate=2; pedestal(ADC)",  nb,xx1,xx2);

  for(int cap=cap1;cap<=cap2;cap++) {
    //  if(cap==108 || cap==109 ||cap==118 || cap==119 ) continue;
    sprintf(txt,"%s/pedBprsR%d-cap%d.hist.root",pathIn,run,cap);
    fd1=new TFile(txt);  assert(fd1->IsOpen());
    TH1F * ih1ped= (TH1F *)fd1->Get("pedBPRS"); assert(ih1ped);
    TH1F * ih1stat= (TH1F *)fd1->Get("statBPRS"); assert(ih1stat);
    TH1F * ih1sig= (TH1F *)fd1->Get("sigPedBPRS"); assert(ih1sig);
    TH1F * ih1peak= (TH1F *)fd1->Get("pedPeakBPRS"); assert(ih1peak);
    printf("----- merge:  %s  entries=%d\n",txt,ih1ped->GetEntries());
    printf("              %s  entries=%d\n",txt,ih1stat->GetEntries());
    axX=ih1ped->GetXaxis();
    float x1=axX->GetXmin();
    float x2=axX->GetXmax();
    int nbX=axX->GetNbins();
    printf("X-axis range  --> [%.1f, %.1f], nb=%d %s\n",x1,x2,nbX,axX->GetTitle());
    
    if(h2Ped==0) {  
      sprintf(txt,"pedBPRScap");
      sprintf(txt2,"BPRS pedestal (ADC), R%d ; BPRS soft ID; capID",run);
      h2Ped=new TH2F(txt,txt2,nbX,x1,x2,nCap,-0.5,nCap-0.5);

      sprintf(txt,"sigPedBPRScap");
      sprintf(txt2,"BPRS sig(ped) (Z=ADC), R%d ; BPRS soft ID; capID",run);
      h2Sig=new TH2F(txt,txt2,nbX,x1,x2,nCap,-0.5,nCap-0.5);

      sprintf(txt,"peakPedBPRScap");
      sprintf(txt2,"integral of pedestal peak, R%d ; BPRS soft ID; capID",run);
      h2Peak=new TH2F(txt,txt2,nbX,x1,x2,nCap,-0.5,nCap-0.5);

      sprintf(txt,"statBPRSallCap");
      sprintf(txt2,"BPRS status from all caps, R%d ; BPRS soft ID; # of bad caps",run);
      h1Stat=new TH1F(txt,txt2,nbX,x1,x2);

      sprintf(txt,"capStatBPRSallSoft");
      sprintf(txt2,"BPRS status from all softID, R%d ; BPRS capID; # of bad softID",run);
      h1CapStat=new TH1F(txt,txt2,128,-0.5,127.5);

      printf("H2 created\n");
    }

    for(int i=1;i<=nbX;i++) {// i=softID
      h2Ped->SetBinContent(i,cap+1,ih1ped->GetBinContent(i));    
      h2Sig->SetBinContent(i,cap+1,ih1sig->GetBinContent(i));    
      h2Peak->SetBinContent(i,cap+1,ih1peak->GetBinContent(i));    
      if(ih1stat->GetBinContent(i)>0) {
	h1Stat->Fill(i,1);	
	h1CapStat->Fill(cap,1);	
      }
    }

    h1p7->Fill(ih1ped->GetBinContent(7));
    h1p607->Fill(ih1ped->GetBinContent(607));
    h1p2407->Fill(ih1ped->GetBinContent(2407));
    h1p3007->Fill(ih1ped->GetBinContent(3007));

  }// end of loop over caps    

  // count dead tiles
  int nDead=0;
  for(int i=1;i<=nbX;i++) {// i=softID
    if(h1Stat->GetBinContent(i)) nDead++;    
  }
  h1Stat->SetEntries(nDead);
  printf(" accumulated nDead=%d\n",nDead);

  fd2->cd();
  h2Ped->Write();
  h2Sig->Write();
  h2Peak->Write();
  h1Stat->Write();
  h1CapStat->Write();
  h1p7->Write();
  h1p607->Write();
  h1p2407->Write();
  h1p3007->Write();

  printf("total entries=%d\n",h2Ped->GetEntries());
 
  return;
  // dump 3D histo ....
  for(int k=120;k<=128;k++) //capID
    for(int i=1;i<=nbX;i++) //chan
      for(int j=1;j<=nbY;j++) //ADC
	{
	  float val=h3->GetBinContent(i,j,k);    
	  if(val<1.) continue;
	  printf("chan(i)=%d,  adc(j)=%d,  cap(k)=%d val=%f\n", i-1,j+100,k-1,val);
	}
}

void pl7(){
  c=new TCanvas();
  c->Divide(3,1);
  c->cd(1);pedSoft7->Draw();
  c->cd(2);pedSoft77->Draw();
  c->cd(3);pedSoft777->Draw();

   
}

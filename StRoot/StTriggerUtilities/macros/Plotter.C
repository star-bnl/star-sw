#define kNPatches 300
void Plotter(int p=4,int s=1) {

  if (s==0)  TFile *f=new TFile("BEMC_7098001_Offline.hist.root");
  if (s==1)  TFile *f=new TFile("BEMC_7098001_Online.hist.root");
   
  TH2F *detHT[10],*emuHT[10],*HT_2D[10];
  TH2F *detTP[10],*emuTP[10],*TP_2D[10];
  TH2F *HpedOff,*HpedOn,*HTowTP,*Hadc;
  TH1F *HT_1D[10],*TP_1D[10];
  
  switch(p){
    
  case 1:{
    
    detHT[0]=new TH2F();
    emuHT[0]=new TH2F();
    detTP[0]=new TH2F();
    emuTP[0]=new TH2F();
    HT_2D[1]=new TH2F();
    TP_2D[1]=new TH2F();

    detHT[0]=(TH2F*)f->Get("detHT_LO_ADC");
    detTP[0]=(TH2F*)f->Get("detTP_L0_ADC");
    emuHT[0]=(TH2F*)f->Get("emuHT_LO_ADC");
    emuTP[0]=(TH2F*)f->Get("emuTP_L0_ADC");
    HT_2D[1]=(TH2F*)f->Get("HT_det-emu_TP");
    TP_2D[1]=(TH2F*)f->Get("TP_det-emu_TP");

    gStyle->SetPalette(1);
    TCanvas *c1=new TCanvas("c1","c1",900,1200);
    c1->Divide(2,3);

    c1->cd(1);
    gPad->SetLogz();
    detHT[0]->Draw("colz");
    TAxis *x=detHT[0]->GetXaxis();
    TAxis *y=detHT[0]->GetYaxis();
    x->SetTitle("Trigger Patch #");
    y->SetTitle("6 bit ADC");

    c1->cd(2);
    gPad->SetLogz();
    detTP[0]->Draw("colz");
    TAxis *x=detTP[0]->GetXaxis();
    TAxis *y=detTP[0]->GetYaxis();
    x->SetTitle("Trigger Patch #");
    y->SetTitle("6 bit ADC");

    c1->cd(3);
    gPad->SetLogz();
    emuHT[0]->Draw("colz");
    x=emuHT[0]->GetXaxis();
    y=emuHT[0]->GetYaxis();
    x->SetTitle("Trigger Patch #");
    y->SetTitle("6 bit ADC");
    
    c1->cd(4);
    gPad->SetLogz();
    emuTP[0]->Draw("colz");
    x=emuTP[0]->GetXaxis();
    y=emuTP[0]->GetYaxis();
    x->SetTitle("Trigger Patch #");
    y->SetTitle("6 bit ADC");

    c1->cd(5);
    gPad->SetLogz();
    HT_2D[1]->Draw("colz");

    c1->cd(6);
    gPad->SetLogz();
    TP_2D[1]->Draw("colz");

    break;
  }
   
  case 2: {
    
    gStyle->SetPalette(1);
    HT_2D[0]=new TH2F();
    HT_1D[0]=new TH1F();
    TP_2D[0]=new TH2F();
    TP_1D[0]=new TH1F();
    HT_2D[0]=(TH2F*)f->Get("emu_vs_det_L0_HT_ADC");
    TP_2D[0]=(TH2F*)f->Get("emu_vs_det_L0_TP_ADC");
    HT_1D[0]=(TH1F*)f->Get("det_emu_diff_L0_HT_ADC");
    TP_1D[0]=(TH1F*)f->Get("det_emu_diff_L0_TP_ADC");

    TCanvas *c2=new TCanvas("c2","c2",1000,1000);
    c2->Divide(2,2);
    c2->cd(1);
    gPad->SetLogz();
    HT_2D[0]->Draw("colz");
    TAxis *x=HT_2D[0]->GetXaxis();
    x->SetTitle("Detector L0 HT ADC");
    TAxis *y=HT_2D[0]->GetYaxis();
    y->SetTitle("Emulated L0 HT ADC");
    c2->cd(2);
    gPad->SetLogy();
    HT_1D[0]->Draw();
    x=HT_1D[0]->GetXaxis();
    x->SetTitle("Detector - Emulated ADC");


    c2->cd(3);
    gPad->SetLogz();
    TP_2D[0]->Draw("colz");
    TAxis *x=TP_2D[0]->GetXaxis();
    x->SetTitle("Detector L0 HT ADC");
    TAxis *y=TP_2D[0]->GetYaxis();
    y->SetTitle("Emulated L0 HT ADC");
    c2->cd(4);
    gPad->SetLogy();
    TP_1D[0]->Draw();
    x=TP_1D[0]->GetXaxis();
    x->SetTitle("Detector - Emulated ADC");


    break;
  }

  case 3:{

    HpedOff=(TH2F*)f->Get("pedOff");
    HpedOn=(TH2F*)f->Get("pedOn");
    Hadc=(TH2F*)f->Get("adc");
    HTowTP=(TH2F*)f->Get("TowTP");

    gStyle->SetPalette(1);
    TCanvas *c3=new TCanvas("c3","c3",1300,500);
    c3->Divide(3,1);
    c3->cd(1);
    gPad->SetLogz();
    Hadc->Draw("colz");

    c3->cd(2);
    HpedOff->Draw("");

    c3->cd(3);
    HTowTP->Draw();

    break;
  }

  case 4:{

    HpedOff=(TH2F*)f->Get("pedOff");
    Hadc=(TH2F*)f->Get("adc");

    char tadc[100],padc[100];
    char toff[100],poff[100];
    TH1D *h[4800],*pedOff[4800];
    for (int i=0;i<4800;i++){
      sprintf(tadc,"h%d",i);
      sprintf(toff,"p%d",i);
      sprintf(padc,"pro_adc_%d",i);
      sprintf(poff,"pro_off_%d",i);
      h[i]=new TH1D(tadc,tadc,200,0,200);
      pedOff[i]=new TH1D(toff,toff,200,0,200);
      h[i]=Hadc->ProjectionX(padc,i+1,i+1);
      pedOff[i]=HpedOff->ProjectionX(poff,i+1,i+1);
      TAxis *x=h[i]->GetXaxis();
      x->SetRangeUser(10,60);
			      
    }

    TPostScript *ps=new TPostScript("TowerPed.eps");
    TCanvas *c4=new TCanvas("c4","c4",100,100,600,800);
    int pad=17;
    for (int i=0;i<4800;i++){
      if (pad%16==1){
	cout<<"tower #="<<i<<endl
	c4->Update();
	ps->NewPage();
	c4->Clear();
	c4->Divide(4,4);
	pad=1;
      }
      c4->cd(pad);
      gPad->SetLogy();
      h[i]->Draw("");
      pedOff[i]->Draw("same");
      pedOff[i]->SetLineColor(2);
      pedOff[i]->SetLineWidth(2);
      pad++;
    }

    if (s==1) ps->Close();
    
    break;
  }

    return;
  }
}




TCanvas *can=0;

//=================================================
plPubJS(  int page=1,int pl=0, char *core0="R10103042", char *iPath="", char *oPath=""){ //1=gif, 2=ps, 3=both
  //char *iPath="/star/data05/scratch/stevens4/wAnalysisOut/data/";
  //char *iPath="/star/u/stevens4/wAnalysis/out/verB4.3/useEtow2allLT20/";
  //char *iPath="/star/u/stevens4/wAnalysis/out/verB4.3/useEtow1/";
  char *iPath="/star/u/stevens4/wAnalysis/out/verB4.3/enhanceQCD/";
  //char *iPath="/star/u/stevens4/wAnalysis/out/";
  //core0="R10097000";
  core0="run9setABCD";
  //core0="mcSetD1_ppWprod";
  //core0="mcSetD2_ppQCD10_inf_filter";
  //core0="mcSetD1_ppZprod";
  if(page==0) {
    doAll(pl);
    return;
  }
  
  /*
    
  cat run9setABCD*ps | ps2pdf - all.pdf
  mv all.pdf ~/stevens4/tmp/all-run9setABCD-pub.pdf
  
  */
  
  
  char *nameA[]={"JSetowEneZ1","JSetowEneZ2","JSetowEneZ3","JSetowEneZ4"};
  char *nameB[]={"JSetowHighPostTr","JSetowTotPostTr","JSetowHighPreNear","JSetowTotPreNear"};
  char *nameC[]={"JSetowHighPreAway","JSetowTotPreAway"};
  char *nameD[]={"JSawayNTow","JSawayNTr"};
  //char *nameD[]={"JSawayNTow","JSawayNTr","JSnearNTow","JSnearNTr"};
  char *nameE[]={"JSawayCond1","JSawayCond2"};

  gStyle->SetOptFit(1);
  TString fullInpName=iPath;  fullInpName+=core0;
  fullInpName+=".wana.hist.root";  
  fd=new TFile(fullInpName);
  if(! fd->IsOpen()) {
    printf("EROR: input histo file not found, quit\n",fullInpName.Data());
    return;
  } else {
    printf("Opened: %s\n",fullInpName.Data());
  }

  TLine *ln; TList *Lx;

  gStyle->SetPalette(1,0);
  //gStyle->SetOptStat(0);
  char padTit[1000];
  sprintf(padTit,"%s",core0);
  
  switch (page) {
    
  case 1:{ sprintf(padTit,"ETOW Gain Study I, %s",core0);
  can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,padTit,page);
  c->Divide(2,2);gStyle->SetOptStat(1);
  char **nameX=nameA;
  for(int i=0;i<4;i++) {
    char txt[100]; int xInt=0;
    printf("->%s<\n",nameX[i]);
    h=(TH1*)fd->Get(nameX[i]);  assert(h);
    TAxis* axX=h->GetXaxis();  int nbX=axX->GetNbins();
    float entries= h->GetEntries();
    for(int j=nbX; j>0; j--){
      xInt += h->GetBinContent(j);
      //if(i==0) cout<<j<<" "<<xInt<<endl;
      if(xInt > 0.0005*entries) 
	{float bin=j; break;}
    }
    cout<<i+1<<" 0.01% of counts in energy = "<<bin/10<<" bin"<<endl;
    Lx=h->GetListOfFunctions();
    ln=new TLine(bin/10,0,bin/10,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);

    c->cd(i+1); h->Draw();
    c->GetPad(i+1)->SetLogy();
  }
  
  } break;//--------------------------------------
  
  case 2:{ sprintf(padTit,"ETOW Gain Study II, %s",core0);  
  can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,padTit,page);
  gStyle->SetOptStat(0); int entries=0;
  leg = new TLegend(0.6,0.7,.9,.9);
  leg->SetHeader("Z vertex range (cm)");
  char **nameX=nameA;
  for(int i=0;i<4;i++) {
    char txt[100]; string entry;
    printf("->%s<\n",nameX[i]);
    h=(TH1*)fd->Get(nameX[i]);  assert(h);
    h->Rebin(10);
    if(i==0) entry="[-100,-50]"; if(i==1) entry="[-50,0]";
    if(i==2) {entry="[0,50]"; h->SetLineStyle(2);} 
    if(i==3) {entry="[50,100]"; h->SetLineStyle(2);}
    
    if(i==0) {h->Draw(); entries=h->GetEntries(); gPad->SetLogy(); h->SetTitle("Normalized Endcap Tower E");}
    else { h->SetLineColor(i+1); h->DrawNormalized("same",entries);}
    leg->AddEntry(h,Form("%s",entry),"l");
  }
  leg->Draw();
  } break;//--------------------------------------

  case 3:{ sprintf(padTit,"Beam Background I, %s",core0);  
  can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,padTit,page);
  c->Divide(2,2); gStyle->SetOptStat(10);
    char **nameX=nameB;
    for(int i=0;i<4;i++) {
      char txt[100];
      printf("->%s<\n",nameX[i]);
      h=(TH1*)fd->Get(nameX[i]);  assert(h);
      c->cd(i+1); h->Draw("colz");
    }
  } break;//--------------------------------------
  
  case 4:{ sprintf(padTit,"Beam Background II, %s",core0);   
    can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,padTit,page);
    c->Divide(2,2); gStyle->SetOptStat(10);
    char **nameX=nameC;
    for(int i=0;i<2;i++) {
      char txt[100];
      printf("->%s<\n",nameX[i]);
      h=(TH1*)fd->Get(nameX[i]);  assert(h);
      c->cd(i+1); h->Draw("colz");
    }  
  } break;//--------------------------------------

  case 5:{ sprintf(padTit,"Away Side Counters, %s",core0);   
    can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,padTit,page);
    c->Divide(2,1);//gStyle->SetOptStat(0);
    char **nameX=nameD;
    for(int i=0;i<2;i++) {
      char txt[100];
      printf("->%s<\n",nameX[i]);
      h=(TH1*)fd->Get(nameX[i]);  assert(h);
      c->cd(i+1); h->Draw();
      c->GetPad(i+1)->SetLogy();
    }  
  } break;//--------------------------------------

  case 6:{ sprintf(padTit,"Away Side Conditions, %s",core0);   
    can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,padTit,page);
    c->Divide(2,2);//gStyle->SetOptStat(0);
    char **nameX=nameE;
    for(int i=0;i<2;i++) {
      char txt[100];
      printf("->%s<\n",nameX[i]);
      h=(TH1*)fd->Get(nameX[i]);  assert(h);
      c->cd(i+1); h->Draw();
    }  
  } break;//--------------------------------------
    
    
    
  default:
    printf("page=%d NOT defined\n",page);
    return;
  }
  
 char text[100];
 sprintf(text,"%s%s_page%03d",oPath,core0,page);
 TString tit=text;
 can->SetTitle(tit);
 can->SetName(tit);

 
 if(pl&1) can->Print(tit+".gif");
 if(pl&2) can->Print(tit+".ps");
 
}

//------------------------
TPad *makeTitle(TCanvas *c,char *core, int page) {

  c->Range(0,0,1,1);
  TPad *pad0 = new TPad("pad0", "apd0",0.0,0.95,1.,1.);
  pad0->Draw();
  pad0->cd();

  TPaveText *pt = new TPaveText(0,0.,1,1,"br");
  pt->Draw();
  TDatime dt;
  TString txt2=core;
  txt2+=", page=";
  txt2+=page;
  txt2+=",  ";
  txt2+=dt.AsString();
  pt->AddText(txt2);
  txt2="--";
  pt->AddText(txt2);

  c->cd();
  pad = new TPad("pad1", "apd1",0.0,0.0,1,.95);
  pad->Draw();
  return pad;
}

//============================
void doAll(int pl){
  for(int i=1;i<=4;i++)  {
    //if(i==10) continue;
    plPubJS(i,pl);
  }
}



// $Log: plPubJS.C,v $
// Revision 1.1  2009/11/23 23:00:20  balewski
// code moved spin-pool
//

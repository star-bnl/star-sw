const int mxGr=3;
TGraphErrors * grL[mxGr];

const int  nObs=3;
char *obsL[nObs]={ "A_n", "A_s", "A_d"};


instrAsy() {
  gStyle->SetOptStat(0);
  gStyle->SetOptFit(0);
  
  TGraphErrors *gr =new  TGraphErrors;
  gr->SetName("insyAsy");
  gr->SetTitle("Instrumental Asymmetries");
  //gr->SetMarkerColor(ampCol[iam]);
  gr->SetMarkerSize(0.8);
  gr->SetMarkerStyle(21);
  TGraphErrors *gr1=gr;
  

  TString  wrkDir="final/";
  TString  fname=wrkDir+"defaultB-H/asyVer1.hist.root";
  TFile *inpH=new TFile(fname);
  assert(inpH->IsOpen());
  //inpH->ls();
  const int  nSel=5;
  char *selL[nSel]={ "a0*All","b0*All","a2*All","b2*All","c1*All"};
  int isel;
  for (isel=0;isel<nSel;isel++) {
    TGraphErrors *gr =(TGraphErrors *) inpH->Get(selL[isel]);
    assert(gr);
    //gr->Print();
    //gr->Draw("AP");
    float x1=0;
    if(isel<2) x1=2189;
    gr->Fit("pol0","","",x1,3000);
    TF1 *ff=gr->GetFunction("pol0");
    if(ff==0) continue; // no fit was made
    float val=ff->GetParameter(0);
    float err=ff->GetParError(0);
    printf("pol0= %f +/- %f\n",val,err);
        int n=gr1->GetN();
      float x=isel+1;
      gr1->SetPoint(n,x,val);
      gr1->SetPointError(n,0,err); 
    }


  gr1->Print();

  // return;
  gr1->Draw("AP");

  // save Tgraph
  TString fname=wrkDir+"instAsy.hist.root";
  TFile *outH=new TFile(fname,"RECREATE");
  assert(outH->IsOpen());
  printf("save outH -->%s\n", fname.Data());
  gr1->Write();
  outH->ls();
  outH->Write();


    return;
}
#if 0

  createTgr();

  int isel;
  for (isel=0;isel<nSel;isel++) {
    TString  fname=wrkDir+selL[isel]+"endVer1.hist.root";
    // printf("ii=%d %s %s\n",isel, selL[isel], fname.Data());
    TFile *inpH=new TFile(fname);
    assert(inpH->IsOpen());
    inpH->ls();
    int io;
    for (io=0;io<nObs;io++) { 
      char name[100];
      sprintf(name,"%s*EtaBF",obsL[io]);
      TGraphErrors *gr =(TGraphErrors *) inpH->Get(name);
      assert(gr);
      gr->Print();
      sprintf(name,"avr%s*EtaBF",obsL[io]);
      TF1 *ff=gr->GetFunction(name);
      if(ff==0) continue; // no fit was made
      float val=ff->GetParameter(0);
      float err=ff->GetParError(0);
      printf("pol0= %f +/- %f\n",val,err);
      TGraphErrors * grOut=grL[io];
      int n=grOut->GetN();
      float x=isel+1;
      grOut->SetPoint(n,x,val);
      grOut->SetPointError(n,0,err); 
    }
  }

  grL[0]->Print();
    

  c=new TCanvas("aa","aa"   ,800,800);
  c->Divide(3,1);
  TLine *ln0=new TLine(0,0.,12,0.);
  
  int it; 
  for (it=0;it<mxGr;it++) {
    TGraphErrors * gr=grL[it];
    int n=gr->GetN();
    printf("\nit=%d  name='%s', N=%d\n",it,gr->GetName(),n);
    // gr->Print();
    if(n<=0) continue;
    c->cd(1+it);
    gr->Draw("AP");
    // gr->Fit("pol0");

    TAxis *ax=gr->GetXaxis();
    ax->SetTitle("choice of event selection");
    ln0->Draw();
    ln0->SetLineStyle(3);
  }

  // save Tgraph
  TString fname=wrkDir+"compCuts.hist.root";
  TFile *outH=new TFile(fname,"RECREATE");
  assert(outH->IsOpen());
  printf("save outH -->%s\n", fname.Data());
  outH->cd();
  for (it=0;it<mxGr;it++) {
    grL[it]->Write();
  }
  
  outH->ls();
  outH->Write();
}


//==========================================
//==========================================
void createTgr() {
  char *obsT[nObs]={ "An", "A#Sigma", "A#Delta"};
  // int  ampCol[mxAmp]={kBlue, kYellow, kGreen, kMagenta};

  int io;
  for (io=0;io<nObs;io++) { 
    char name[100];
    sprintf(name,"%s",obsL[io]);
    //printf("ic=%d iam=%d name=%s=\n",ic,iam,name);
    TGraphErrors *gr =new  TGraphErrors;
    gr->SetName(name);
    gr->SetTitle(obsT[io]);
    //gr->SetMarkerColor(ampCol[iam]);
    gr->SetMarkerSize(0.8);
    gr->SetMarkerStyle(21);
    grL[io]=gr;   
  }
}

#endif

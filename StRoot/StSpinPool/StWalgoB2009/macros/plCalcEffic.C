bool draw=false;

void plCalcEffic(char* iPath="outMC/noSect20Corrected/", char* core0="rcf10010"){

  //gStyle->SetOptFit(1);
  gStyle->SetOptStat(0);

  //load mc file
  TString fullInpName=iPath;  fullInpName+=core0;
  fullInpName+=".wana.hist.root";
  fd=new TFile(fullInpName);
  if(!fd->IsOpen()) {
    printf("EROR: input histo file not found, quit\n",fullInpName.Data());
    return;
  } else {
    printf("Opened: %s\n",fullInpName.Data());
  }
  
  //use rebin 1 for effic # calc, but rebin 2 for plot
  int etrebin=1;

  //trigger efficiency
  TH1F *hTrigET=doEfficiency(fd,"MCeleETall","MCeleETtrig","trigger efficiency",etrebin,1);
  TH1F *hTrigEta=doEfficiency(fd,"MCeleEtaAll","MCeleEtaTrig","trigger efficiency",2,0);
  TH1F *hTrigZvert=doEfficiency(fd,"MCeleZvertAll","MCeleZvertTrig","trigger efficiency",2,0);
  TH1F *hTrigPhi=doEfficiency(fd,"MCelePhiAll","MCelePhiTrig","trigger efficiency",1,0); 
  //TH1F *hTrigETJoe=doEfficiency(fd,"MCeleETallJoe","MCeleETtrigJoe","trigger efficiency",4,0);

  //vertex efficiency
  TH1F *hVertET=doEfficiency(fd,"MCeleETtrig","MCeleETvert","vertex efficiency",etrebin,1);
  TH1F *hVertEta=doEfficiency(fd,"MCeleEtaTrig","MCeleEtaVert","vertex efficiency",2,0);
  TH1F *hVertZvert=doEfficiency(fd,"MCeleZvertTrig","MCeleZvertVert","vertex efficiency",2,0);
  TH1F *hVertPhi=doEfficiency(fd,"MCelePhiTrig","MCelePhiVert","vertex efficiency",1,0);
  //TH1F *hVertETJoe=doEfficiency(fd,"MCeleETtrigJoe","MCeleETvertJoe","vertex efficiency",4,0); 

  //reconstruction efficiency
  TH1F *hRecoET=doEfficiency(fd,"MCeleETvert","MCeleETreco","reconstruction efficiency",etrebin,1);
  TH1F *hRecoEta=doEfficiency(fd,"MCeleEtaVert","MCeleEtaReco","reconstruction efficiency",2,0);
  TH1F *hRecoZvert=doEfficiency(fd,"MCeleZvertVert","MCeleZvertReco","reconstruction efficiency",2,0);
  TH1F *hRecoPhi=doEfficiency(fd,"MCelePhiVert","MCelePhiReco","reconstruction efficiency",1,0);
  //TH1F *hRecoETJoe=doEfficiency(fd,"MCeleETvertJoe","MCeleETrecoJoe","reconstruction efficiency",4,0);

  
  //plot 5% syst uncert for trigger effic
  hTrigET->Draw();
  hTrigET->GetXaxis()->SetRange(11,25);
  f=new TF1("a","[0]",25,100);
  hTrigET->Fit(f,"Q","Q",25,100);
  //hTrigET->SetStats(false);
  Lx=hTrigET->GetListOfFunctions();
  //float fitVal=f->GetParameter(0);
  //float fitVal=0.857;
  float fitVal=0.876;
  hTrigET->SetLineWidth(2);
  ln1=new TLine(20,fitVal*.95,50,fitVal*.95);
  ln2=new TLine(20,fitVal*1.05,50,fitVal*1.05);
  ln1->SetLineWidth(2); ln2->SetLineWidth(2);
  ln1->SetLineColor(kRed); ln1->SetLineStyle(2);
  ln2->SetLineColor(kRed); ln2->SetLineStyle(2);
  Lx->Add(ln1); Lx->Add(ln2); 


  //hf=new TFile("wplusEff.root","recreate");
  //hRecoETJoe->Write();
  //hTrigETJoe->Write();
  

  if(!draw) return;
  
  c=new TCanvas("trigger effic","trigger effic",800,600);
  c->Divide(2,2);
  c->cd(1);
  hTrigET->Draw();
  c->cd(2);
  hTrigEta->Draw();
  c->cd(3);
  hTrigZvert->Draw();
  c->cd(4);
  hTrigPhi->Draw();
  c->Print("0triggerEffic.ps");

  c2=new TCanvas("vertex effic","vertex effic",800,600);
  c2->Divide(2,2);
  c2->cd(1);
  hVertET->Draw();
  c2->cd(2);
  hVertEta->Draw();
  c2->cd(3);
  hVertZvert->Draw();
  c2->cd(4);
  hVertPhi->Draw();
  c2->Print("1vertexEffic.ps");

  c3=new TCanvas("reco effic","reco effic",800,600);
  c3->Divide(2,2);
  c3->cd(1);
  hRecoET->Draw();
  c3->cd(2);
  hRecoEta->Draw();
  c3->cd(3);
  hRecoZvert->Draw();
  c3->cd(4);
  hRecoPhi->Draw();
  c3->Print("2recoEffic.ps");

  return;
}

//calculate efficiencies
TH1F* doEfficiency(TFile *fd,char* name0, char* name1, char *tit, int reb=1, int etplot){
  
  TH1F * h0=(TH1F * )fd->Get(name0);  assert(h0);
  TH1F * h1=(TH1F * )fd->Get(name1);  assert(h1);

  ha=(TH1F*) h0->Clone(); assert(ha);
  hb=(TH1F*) h1->Clone(); assert(hb);

  ha->Rebin(reb);
  hb->Rebin(reb);

  hc=(TH1F*) hb->Clone(); assert(hc);
  hc->SetTitle(tit);
  hc->Reset();

  int num=0; int den=0;
  int nb=hb->GetNbinsX();
  int startBin=hb->GetXaxis()->FindBin(25.);
  //cout<<startBin<<endl;
  //cout<<hb->GetNbinsX()<<" "<<ha->GetNbinsX()<<endl;
  assert(nb==ha->GetNbinsX());
  for(int i=1; i<=nb; i++) {
    int n0=ha->GetBinContent(i);  // n0: # of thrown tracks
    int n1=hb->GetBinContent(i);  // n1: # of matched tracks
    if(n0==0) continue; // no thown tracks found
    float x=n1*(n0-n1);
    float eff=(float) n1/n0;
    if(n1==0 || n1==n0) x=n0;
    float errEff=sqrt(x/n0/n0/n0);
    // printf("bin#= %d, n0= %d, n1= %d, eff= %f, errEff=+/- %f\n",i,n0,n1,eff,errEff);
    hc->SetBinContent(i,eff);
    hc->SetBinError(i,errEff);
    if(i>=startBin){ //define ET range for efficiency
      num+=n1;
      den+=n0;
    }
      
  }
  
  if(etplot==1) 
    cout<<name0<<"="<<(float) num/den<<"+/-"<<sqrt((float)num*(den-num)/den/den/den)<<endl;
  hc->SetMinimum(0.0);
  hc->SetMaximum(1.2);

  return hc;
}

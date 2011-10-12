
ofstream spreadsheet("./effic.csv");

void plCalcEffic(int charge = 2){
  system("mkdir -p plots/");

  if(charge == 2){
    plCalcEfficX(0);
    plCalcEfficX(1);
  }
  else plCalcEfficX(charge);
  return;
}

void plCalcEfficX(int charge = 0, char* iPath="/star/u/stevens4/wAnalysis/efficXsec/outEmb/gainUp2/"){ 

  char* sign;
  if(charge==0) sign="+";
  else if(charge==1) sign="-";
  
  char* core0; //these have no vertex reweighting
  if(charge == 1) core0="Wminus";
  else if(charge == 0) core0="Wplus";

  ofstream latex(Form("./%s.txt",core0));

  //gStyle->SetOptFit(1);
  //gStyle->SetOptStat(0);
  gStyle->SetPalette(1);

  //load file
  TString fullInpName=iPath;  fullInpName+=core0; 
  fullInpName+=".wana.hist.root";
  fd=new TFile(fullInpName);
  if(!fd->IsOpen()) {
    printf("ERROR: input histo file not found, quit\n",fullInpName.Data());
    return;
  } else {
    printf("Opened: %s\n",fullInpName.Data());
  }
  
  //use rebin 1 for effic # calc, but rebin N for plot
  int etrebin=4;
  int etaRebin=50;
  int zRebin=4;
  int phiModuleRebin=6;
  int phiRebin=1;
  int zdcRebin=5;

  //get total efficiency and error
  float etplot[5]={1.0,1.0,1.0,1.0,1.0};
  float etplotErr[5]={1.0,1.0,1.0,1.0,0.0};
  float zdcplot[5]={2.0,2.0,2.0,2.0,2.0};
  float zdcplotErr[5]={1.0,1.0,1.0,1.0,0.0};

  //trigger efficiency
  TH1F *hTrigET=doEfficiency(fd,"MCeleETall","MCeleETtrig",Form("W%s trigger efficiency",sign),etrebin,etplot[0],etplotErr[0]);
  TH1F *hTrigEta=doEfficiency(fd,"MCeleEtaAll","MCeleEtaTrig",Form("W%s trigger efficiency",sign),etaRebin,0);
  TH1F *hTrigDetEta=doEfficiency(fd,"MCeleDetEtaAll","MCeleDetEtaTrig",Form("W%s trigger efficiency",sign),etaRebin,0);
  TH1F *hTrigZvert=doEfficiency(fd,"MCeleZvertAll","MCeleZvertTrig",Form("W%s trigger efficiency",sign),zRebin,0);
  TH1F *hTrigPhi=doEfficiency(fd,"MCelePhiModulePairAll","MCelePhiModulePairTrig",Form("W%s trigger efficiency",sign),phiModuleRebin,0); 
  TH1F *hTrigZDC=doEfficiency(fd,"MCeleZdcAll","MCeleZdcTrig",Form("W%s trigger efficiency",sign),zdcRebin,zdcplot[0],zdcplotErr[0]);
  //vertex efficiency
  TH1F *hVertET=doEfficiency(fd,"MCeleETtrig","MCeleETvert",Form("W%s vertex efficiency",sign),etrebin,etplot[1],etplotErr[1]);
  TH1F *hVertEta=doEfficiency(fd,"MCeleEtaTrig","MCeleEtaVert",Form("W%s vertex efficiency",sign),etaRebin,0);
  TH1F *hVertZvert=doEfficiency(fd,"MCeleZvertTrig","MCeleZvertVert",Form("W%s vertex efficiency",sign),zRebin,0);
  TH1F *hVertPhi=doEfficiency(fd,"MCelePhiTrig","MCelePhiVert",Form("W%s vertex efficiency",sign),phiRebin,0);
  TH1F *hVertZDC=doEfficiency(fd,"MCeleZdcTrig","MCeleZdcVert",Form("W%s vertex efficiency",sign),zdcRebin,zdcplot[1],zdcplotErr[1]);
  //tracking efficiency
  TH1F *hTrackET=doEfficiency(fd,"MCeleETvert","MCeleETTrack",Form("W%s tracking efficiency",sign),etrebin,etplot[2],etplotErr[2]);
  TH1F *hTrackEta=doEfficiency(fd,"MCeleEtaVert","MCeleEtaTrack",Form("W%s tracking efficiency",sign),etaRebin,0);
  TH1F *hTrackZvert=doEfficiency(fd,"MCeleZvertVert","MCeleZvertTrack",Form("W%s tracking efficiency",sign),zRebin,0);
  TH1F *hTrackPhi=doEfficiency(fd,"MCelePhiVert","MCelePhiTrack",Form("W%s tracking efficiency",sign),phiRebin,0);
  TH1F *hTrackZDC=doEfficiency(fd,"MCeleZdcVert","MCeleZdcTrack",Form("W%s tracking efficiency",sign),zdcRebin,zdcplot[2],zdcplotErr[2]);
  //algo efficiency
  TH1F *hRecoET=doEfficiency(fd,"MCeleETTrack","MCeleETreco",Form("W%s algo efficiency",sign),etrebin,etplot[3],etplotErr[3]);
  TH1F *hRecoEta=doEfficiency(fd,"MCeleEtaTrack","MCeleEtaReco",Form("W%s algo efficiency",sign),etaRebin,0);
  TH1F *hRecoZvert=doEfficiency(fd,"MCeleZvertTrack","MCeleZvertReco",Form("W%s algo efficiency",sign),zRebin,0);
  TH1F *hRecoPhi=doEfficiency(fd,"MCelePhiTrack","MCelePhiReco",Form("W%s algo efficiency",sign),phiRebin,0);
  TH1F *hRecoZDC=doEfficiency(fd,"MCeleZdcTrack","MCeleZdcReco",Form("W%s algo efficiency",sign),zdcRebin,zdcplot[3],zdcplotErr[3]);

  //total efficiency for overall stat uncertainty
  TH1F *hTotET=doEfficiency(fd,"MCeleETall","MCeleETreco",Form("W%s total efficiency",sign),etrebin,etplot[4],etplotErr[4]);
  TH1F *hTotZDC=doEfficiency(fd,"MCeleZdcAll","MCeleZdcReco",Form("W%s total efficiency",sign),zdcRebin,zdcplot[4],zdcplotErr[4]);
  float totEffic=etplot[0]*etplot[1]*etplot[2]*etplot[3];
  //cout<<totEffic<<endl;
  cout<<"Total Efficiency = "<<etplot[4]<<" $\\pm$ "<<etplotErr[4]<<endl;  
  //spreadsheet<<"Track Effic = "<<etplot[2]<<" +/- "<<etplotErr[2]<<endl;

  spreadsheet<<"W"<<sign<<" Total Efficiency"<<endl;
  for(int j=0; j<6; j++){
    spreadsheet<<25+j*4<<","<<29+j*4<<","<<Form("%.4f",hTotET->GetBinContent(7+j))<<","<<Form("%.4f",hTotET->GetBinError(7+j))<<endl;
  }

  //set axis range for ET plot
  int etAxisMin=0; int etAxisMax=70;

  //***************  Algo Efficiency plots ***************//
  cA=new TCanvas(Form("W%s algo effic",sign),"algo effic",800,600);
  cA->Divide(2,2);
  cA->cd(1);
  hRecoET->Draw(); 
  hRecoET->SetAxisRange(etAxisMin,etAxisMax);
  cA->cd(2);
  hRecoEta->Draw();
  cA->cd(3);
  hRecoZvert->Draw();
  cA->cd(4);
  hRecoZDC->Draw();
  //hRecoPhi->Draw();
  cA->Print(Form("plots/W%salgoEffic.eps",sign));
  cA->Print(Form("plots/W%salgoEffic.png",sign));

  //algo efficiency numbers
  spreadsheet<<"W"<<sign<<" Algo Efficiency"<<endl;
  latex<<"W"<<sign<<" Algo Efficiency"<<endl;
  for(int j=0; j<6; j++){
    //semi-latex format
    if(j<6) latex<<25+j*4<<"$<E_T<$"<<29+j*4<<" & "<<Form("%.4f",hRecoET->GetBinContent(7+j))<<" $\\pm$ "<<Form("%.4f",hRecoET->GetBinError(7+j))<<" $\\pm$  &  \\\\"<<endl;
    
    //for spreadsheet
    spreadsheet<<25+j*4<<","<<29+j*4<<","<<Form("%.4f",hRecoET->GetBinContent(7+j))<<","<<Form("%.4f",hRecoET->GetBinError(7+j))<<endl;
  }

  //***************  Tracking Efficiency plots ***********//
  cT=new TCanvas(Form("W%s track effic",sign),"track effic",800,600);
  cT->Divide(2,2);
  cT->cd(1);
  hTrackET->Draw(); 
  hTrackET->SetAxisRange(etAxisMin,etAxisMax);
  cT->cd(2);
  hTrackEta->Draw();
  cT->cd(3);
  //hTrackZvert->Draw();
  hTrackZDC->SetMinimum(0.6); 
  hTrackZDC->Draw();
  cT->cd(4);
  hTrackPhi->Draw();
  cT->Print(Form("plots/W%strackEffic.eps",sign));
  cT->Print(Form("plots/W%strackEffic.png",sign));

  //***************  Vertex Efficiency plots *************//
  //Draw trigger efficiency 
  cV=new TCanvas(Form("W%s vertex effic",sign),"vertex effic",800,600);
  cV->Divide(2,2);
  cV->cd(1);
  hVertET->Draw(); 
  hVertET->SetAxisRange(etAxisMin,etAxisMax);
  cV->cd(2);
  hVertEta->Draw();
  cV->cd(3);
  hVertZvert->Draw();
  cV->cd(4);
  hVertZDC->SetMinimum(0.6);
  hVertZDC->Draw();
  cV->Print(Form("plots/W%svertEffic.eps",sign));
  cV->Print(Form("plots/W%svertEffic.png",sign));

  //***************  Trigger Efficiency plots ************//
  //draw trigger efficiency to show why W- isn't const w/ ET
  c2D=new TCanvas(Form("W%s trigger effic ET bins",sign),"trigger effic ET bins",700,500);
  j1=(TH2F*)fd->Get("MCeleEta_ptPreTrig"); //j1->Rebin2D(2,5);
  TH1D* h25[10];
  c2D->Divide(2,2);
  spreadsheet<<"W"<<sign<<" Trigger Efficiency"<<endl;
  latex<<"W"<<sign<<" Trigger Efficiency"<<endl;
  for(int j=0; j<6; j++){
    h25[j] = j1->ProjectionX(Form("pt%d_%d",25+j*4,29+j*4),26+j*4,29+j*4);
    h25[j]->SetTitle(Form("Lepton detector #eta (from Geant): PT=[%d,%d]",25+j*4,29+j*4));
    h25[j]->Rebin(2); 
    
    //count frac. of events with |eta|<1 and effic for each bin
    float accepted = h25[j]->Integral(26,125);
    float total = h25[j]->Integral();
    //cout<<"ET=["<<25+j*4<<","<<29+j*4<<"] efficiency = "<<hTrigET->GetBinContent(7+j)<<" ; fraction with |eta| < 1 : "<<accepted<<" "<<total<<endl;

    //semi-latex format
    if(j<6) latex<<25+j*4<<"$<E_T<$"<<29+j*4<<" & "<<Form("%.4f",hTrigET->GetBinContent(7+j))<<" $\\pm$ "<<Form("%.4f",hTrigET->GetBinError(7+j))<<" $\\pm$  &  & "<<Form("%.4f",accepted/total)<<" &  \\\\"<<endl;
    spreadsheet<<25+j*4<<","<<29+j*4<<","<<Form("%.4f",hTrigET->GetBinContent(7+j))<<","<<Form("%.4f",hTrigET->GetBinError(7+j))<<endl;

    //add lines for detector cutoff at eta=1
    h25[j]->Rebin(5);
    Lx=h25[j]->GetListOfFunctions();
    int max=h25[j]->GetMaximum();
    ln1=new TLine(-1,0,-1,max);
    ln2=new TLine(1,0,1,max);
    ln1->SetLineWidth(2); ln2->SetLineWidth(2);
    ln1->SetLineColor(kRed); ln1->SetLineStyle(2);
    ln2->SetLineColor(kRed); ln2->SetLineStyle(2);
    Lx->Add(ln1); Lx->Add(ln2);
    if(j<4){ //only plot first four ET bins
      c2D->cd(j+1);
      h25[j]->Draw("h");
    }
  }
  c2D->Print(Form("plots/W%strigEfficNonConst.eps",sign));
  c2D->Print(Form("plots/W%strigEfficNonConst.png",sign));
  
  //Draw trigger efficiency 
  c=new TCanvas(Form("W%s trigger effic",sign),"trigger effic",800,600);
  c->Divide(2,2);
  c->cd(1);
  hTrigET->Draw(); 
  hTrigET->SetAxisRange(etAxisMin,etAxisMax);
  c->cd(2);
  hTrigEta->Draw();
  c->cd(3);
  hTrigDetEta->Draw();
  c->cd(4);
  hTrigPhi->Draw();
  c->Print(Form("plots/W%strigEffic.eps",sign));
  c->Print(Form("plots/W%strigEffic.png",sign));
  

  //other efficiency related histos
  TH2F * g0=(TH2F * )fd->Get("muBdist1");  assert(g0);
  TH2F * g1=(TH2F * )fd->Get("muBclET24R_ET");  assert(g1);
  TH2F * g2=(TH2F * )fd->Get("muBclEjetE2D_ET");  assert(g2);
  TH2F * g3=(TH2F * )fd->Get("musPtBalance_clust");  assert(g3);
  cAlgo2=new TCanvas(Form("W%s algo effic 2 ",sign),"algo effic 2",800,600);
  cAlgo2->Divide(2,2);
  cAlgo2->cd(1);
  g0->Draw("colz");
  g0->GetXaxis()->SetRange(0,70);
  cAlgo2->cd(2);
  g1->Draw("colz");
  g1->GetXaxis()->SetRange(0,70);
  cAlgo2->cd(3);
  g2->Draw("colz");
  g2->GetXaxis()->SetRange(0,70);
  cAlgo2->cd(4);
  g3->Draw("colz");
  g3->GetXaxis()->SetRange(0,70);
  cAlgo2->Print(Form("plots/W%salgoEffic2.eps",sign));
  cAlgo2->Print(Form("plots/W%salgoEffic2.png",sign));

  //cout<<g2->GetYaxis()->FindBin(0.88)<<endl;
  //cout<<g2->Integral(26,30,0,1000)<<" "<<g2->Integral(26,30,1,g2->GetYaxis()->FindBin(0.88))<<endl;
  //cout<<g2->Integral(26,1000,1,1000)<<" "<<g2->Integral(26,1000,1,g2->GetYaxis()->FindBin(0.88))<<endl;

  return;
}

//calculate efficiencies
TH1F* doEfficiency(TFile *fd,char* name0, char* name1, char *tit, int reb=1, float &etplot, float &etplotErr=0){
  
  TH1F * h0=(TH1F * )fd->Get(name0);  assert(h0);
  TH1F * h1=(TH1F * )fd->Get(name1);  assert(h1);

  ha=(TH1F*) h0->Clone(); assert(ha);
  hb=(TH1F*) h1->Clone(); assert(hb);

  ha->Rebin(reb);
  hb->Rebin(reb);

  hc=(TH1F*) hb->Clone(); assert(hc);
  hc->SetTitle(tit);
  hc->Reset();

  float num=0; float den=0;
  float matchErr2=0; float thrownErr2=0;
  int nb=hb->GetNbinsX();
  int startBin,endBin; 
  if(etplot==1.0) {
    startBin=hb->GetXaxis()->FindBin(25.);
    //endBin=hb->GetXaxis()->FindBin(48.0);
    //startBin=hb->GetXaxis()->FindBin(50.);
    endBin=hb->GetXaxis()->FindBin(100.0);
  }
  if(etplot==2.0) {
    endBin=hb->GetXaxis()->FindBin(200000.);
    startBin=hb->GetXaxis()->FindBin(0.);
  }
    
  //cout<<startBin<<endl;
  //cout<<hb->GetNbinsX()<<" "<<ha->GetNbinsX()<<endl;
  assert(nb==ha->GetNbinsX());
  for(int i=1; i<=nb; i++) {
    float n0=ha->GetBinContent(i);//n0: # of thrown tracks
    float n1=hb->GetBinContent(i);//n1: # of matched tracks
    float e0=ha->GetBinError(i);//e0: error on # of thrown tracks
    float e1=hb->GetBinError(i);//e1: error on # of matched tracks
    if(n0==0) continue; // no thown tracks found
    float eff=(float) n1/n0;
    
    //old method with un-weighted histos
    //float x=n1*(n0-n1);
    //if(n1==0 || n1==n0) x=n0;
    //float errEff=sqrt(x/n0/n0/n0); 
    
    float errEff=0;
    //method for using weighted histos (must use Sumw2)
    if(n1==n0) {//problem in effic=1 limit, statErr=0
      //cout<<name1<<" bin="<<i<<" problem with error"<<endl;
      errEff=1./n0; //just something big to see its low stats
    } 
    else errEff=sqrt(e1*e1*(n0-n1)*(n0-n1)+(e0-e1)*(e0-e1)*n1*n1)/n0/n0;
       
    //printf("name=%s: bin#= %d, n0= %f, n1= %f, e0=%f, e1=%f, eff= %f, errEff=+/- %f\n",name0,i,n0,n1,e0,e1,eff,errEff);
    hc->SetBinContent(i,eff);
    hc->SetBinError(i,errEff);
    if(i>=startBin && i<=endBin){//define ET range for efficTot
      num+=n1;
      den+=n0;
      matchErr2+=e1*e1;
      thrownErr2+=e0*e0;
    }
  }
  
  if(etplot==1.0){
    //old method with un-weighted histos
    //cout<<name1<<"="<<(float) num/den<<"+/-"<<sqrt((float)num*(den-num)/den/den/den)<<endl;
    
    //method for using weighted histos (must use Sumw2)
    if(etplotErr>0.0) cout<<name1<<"="<< num/den <<" $\\pm$ "<< sqrt(matchErr2*(den-num)*(den-num) + (thrownErr2-matchErr2)*num*num)/den/den <<endl;
    etplot=num/den;
    etplotErr=sqrt(matchErr2*(den-num)*(den-num) + (thrownErr2-matchErr2)*num*num)/den/den;
  }
  if(etplot==2.0){
    //method for using weighted histos (must use Sumw2)
    //cout<<name1<<"="<< num/den <<" $\\pm$ "<< sqrt(matchErr2*(den-num)*(den-num) + (thrownErr2-matchErr2)*num*num)/den/den <<endl;

    etplot=num/den;
    etplotErr=sqrt(matchErr2*(den-num)*(den-num) + (thrownErr2-matchErr2)*num*num)/den/den;
  }
  hc->SetMinimum(0.0);
  hc->SetMaximum(1.1);

  return hc;
}

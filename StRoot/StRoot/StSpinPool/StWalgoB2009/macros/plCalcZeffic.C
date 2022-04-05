
void plCalcZeffic(char* iPath="/star/u/stevens4/wAnalysis/efficXsec/outEmb/gainUp2/"){

  system("mkdir -p plots/");

  char* core0; 
  core0="Ze+e-Interf";
  
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
  int massRebin=4;
  int zdcRebin=5;

  //get total efficiency and error
  float massPlot[5]={1.0,1.0,1.0,1.0,1.0};
  float massPlotErr[5]={1.0,1.0,1.0,1.0,1.0};

  //trigger efficiency
  TH1F *hTrigMass=doEfficiency(fd,"MCzMassAll","MCzMassTrig",Form("Z trigger efficiency"),massRebin,massPlot[0],massPlotErr[0]);
  TH1F *hTrigZDC=doEfficiency(fd,"MCzZdcAll","MCzZdcTrig",Form("Z trigger efficiency"),zdcRebin,0);
  //vertex efficiency
  TH1F *hVertMass=doEfficiency(fd,"MCzMassTrig","MCzMassVert",Form("Z vertex efficiency"),massRebin,massPlot[1],massPlotErr[1]);
  TH1F *hVertZDC=doEfficiency(fd,"MCzZdcTrig","MCzZdcVert",Form("Z vertex efficiency"),zdcRebin,0);
  //tracking efficiency
  TH1F *hTrackMass=doEfficiency(fd,"MCzMassVert","MCzMassTrack",Form("Z tracking efficiency"),massRebin,massPlot[2],massPlotErr[2]);
  TH1F *hTrackZDC=doEfficiency(fd,"MCzZdcVert","MCzZdcTrack",Form("Z tracking efficiency"),zdcRebin,0);
  //algo efficiency
  TH1F *hRecoMass=doEfficiency(fd,"MCzMassTrack","MCzMassReco",Form("Z algo efficiency"),massRebin,massPlot[3],massPlotErr[3]);
  TH1F *hRecoZDC=doEfficiency(fd,"MCzZdcTrack","MCzZdcReco",Form("Z algo efficiency"),zdcRebin,0);
  
  //total efficiency for overall stat uncertainty
  TH1F *hTotMass=doEfficiency(fd,"MCzMassAll","MCzMassReco",Form("Z total efficiency"),massRebin,massPlot[4],massPlotErr[4]);
  cout<<"Total Efficiency = "<<massPlot[4]<<" $\\pm$ "<<massPlotErr[4]<<endl;
  TH1F *hTotZDC=doEfficiency(fd,"MCzZdcAll","MCzZdcReco",Form("Z total efficiency"),zdcRebin,0);
  hTotZDC->Draw();  

  //***************  Vertex+Trigger Efficiency plots ********//
  cV=new TCanvas(Form("Z vertex and trig effic"),"vertex and effic",800,600);
  cV->Divide(2,2);
  cV->cd(3);
  hVertMass->Draw(); 
  cV->cd(4);
  hVertZDC->Draw();
  cV->cd(1);
  hTrigMass->Draw();
  cV->cd(2);
  hTrigZDC->Draw();
  cV->Print("plots/ZVert-TrigEffic.eps");
  cV->Print("plots/ZVert-TrigEffic.png");

  //***************  Algo+Track Efficiency plots *************//
  cA=new TCanvas(Form("Z algo and track effic"),"algo and track effic",800,600);
  cA->Divide(2,2);
  cA->cd(3);
  hRecoMass->Draw(); 
  cA->cd(4);
  hRecoZDC->Draw();
  cA->cd(1);
  hTrackMass->Draw();
  cA->cd(2);
  hTrackZDC->Draw();
  cA->Print("plots/ZAlgo-TrkEffic.eps");
  cA->Print("plots/ZAlgo-TrkEffic.png");

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
  int startBin=hb->GetXaxis()->FindBin(70.);
  int endBin=hb->GetXaxis()->FindBin(109.9);
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
    if(i>=startBin && i<=endBin){//define mass range for efficTot
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
    cout<<name1<<"="<< num/den <<" $\\pm$ "<< sqrt(matchErr2*(den-num)*(den-num) + (thrownErr2-matchErr2)*num*num)/den/den <<endl;
    etplot=num/den;
    etplotErr=sqrt(matchErr2*(den-num)*(den-num) + (thrownErr2-matchErr2)*num*num)/den/den;
  }
  hc->SetMinimum(0.0);
  hc->SetMaximum(1.2);

  return hc;
}

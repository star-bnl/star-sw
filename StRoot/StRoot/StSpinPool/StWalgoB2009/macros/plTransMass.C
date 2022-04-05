//Display plots related to reconstructing transverse mass for data and MC.  Also shows correlations between "measured" quantities and values from the pythia record for W MC.

void plTransMass(int isMC=1,int type=1){

  //type: ps==1 and png==everything else;
  
  char* dirMC="/star/u/stevens4/wAnalysis/ver4.41/out/";
  char* dirData="/star/data05/scratch/stevens4/wAnalysisOut/ver4.41/data/";
  
  if(isMC) plMC(dirMC,type);
  else plData(dirData,type);
  
  return;
}


//_________________________________________
//_________________________________________
void plMC(char* iPath="/star/u/stevens4/wAnalysis/ver4.41/out/",int type=1){  

  char* core0="ppWprod_job";

  TString fullInpName=iPath;  fullInpName+=core0;
  fullInpName+=".wana.hist.root";
  fd=new TFile(fullInpName);
  if(! fd->IsOpen()) {
    printf("EROR: input histo file not found, quit\n",fullInpName.Data());
    return;
  } else {
    printf("Opened: %s\n",fullInpName.Data());
  }

  gStyle->SetPalette(1);

  char *nameA[]={"MCWpt","MCWpL","MChadRecoilPt"};
  char *nameB[]={"MCWminusHadRecoilpt","MChadRec_Wpt","MCdelPhi_Wpt","MCdelPhi_Recoilpt"};
  char *nameC[]={"MCelectronRecoPt","MCelectronGeantPt","MCdiffElectronPtvsGeantpt","MCelectronRecovsGeant"};
  char *nameD[]={"MCneutrinoRecoPt","MCneutrinoGeantPt","MCdiffNeutrinoPt","MCneutrinoRecovsGeant"};
  char *nameE[]={"MCeleG_neutrinoG","MCmT","MCgMT","MCGmTminusmT"};
  char *nameG[]={"MCRecoilEta_all","MCRecoilEta_WetaPos","MCRecoilEta_WetaNeg","MCRecoilEtaAll_Wpt"};

  cA=new TCanvas("aa","aa",800,600);
  cA->Divide(2,2);
  for(int i=0;i<3;i++) {
    cA->cd(i+1);
    h=(TH1F*)fd->Get(nameA[i]);  
    h->Draw();
  }
  TString titA="WMC1"; 
  if(type==1) titA+=".ps";
  else titA+=".png";
  cA->Print(titA);

  cB=new TCanvas("bb","bb",800,600);
  cB->Divide(2,2);
  for(int i=0;i<4;i++) {
    cB->cd(i+1);
    h=(TH1F*)fd->Get(nameB[i]);  
    h->Draw();
    if(i!=0) h->Draw("colz");
  }
  TString titB="WMC2"; 
  if(type==1) titB+=".ps";
  else titB+=".png";
  cB->Print(titB);

  cC=new TCanvas("cc","cc",800,600);
  cC->Divide(2,2);
  for(int i=0;i<4;i++) {
    cC->cd(i+1);
    h=(TH1F*)fd->Get(nameC[i]);  
    h->Draw();
    if(i==3 || i==2) h->Draw("colz");
  }
  TString titC="WMC3"; 
  if(type==1) titC+=".ps";
  else titC+=".png";
  cC->Print(titC);
  
  cD=new TCanvas("dd","dd",800,600);
  cD->Divide(2,2);
  for(int i=0;i<4;i++) {
    cD->cd(i+1);
    h=(TH1F*)fd->Get(nameD[i]);  
    h->Draw();
    if(i==3) h->Draw("colz");
  }
  TString titD="WMC4"; 
  if(type==1) titD+=".ps";
  else titD+=".png";
  cD->Print(titD);
  
  cE=new TCanvas("ee","ee",800,600);
  cE->Divide(2,2);
  for(int i=0;i<4;i++) {
    cE->cd(i+1);
    h=(TH1F*)fd->Get(nameE[i]);  
    h->Draw();
    if(i==0) h->Draw("colz");
  }
  TString titE="WMC5"; 
  if(type==1) titE+=".ps";
  else titE+=".png";
  cE->Print(titE);

  cG=new TCanvas("gg","gg",800,600);
  cG->Divide(2,2);
  for(int i=0;i<4;i++) {
    cG->cd(i+1);
    h=(TH1F*)fd->Get(nameG[i]);  
    h->Draw();
    if(i==3) h->Draw("colz");
  }
  TString titG="WMC6"; 
  if(type==1) titG+=".ps";
  else titG+=".png";
  cG->Print(titG);
  
}

//_________________________________________
//_________________________________________
void plData(char* iPath="/star/data05/scratch/stevens4/wAnalysisOut/ver4.41/",int type=1){

  char* core0="run9setABCD.wana.hist.root";
  TString fullInpName=iPath;  fullInpName+=core0;
  
  fdData=new TFile(fullInpName);
  if(! fdData->IsOpen()) {
    printf("EROR: input histo file not found, quit\n",fullInpName);
    return;
  } else {
    printf("Opened: %s\n",fullInpName.Data());
  }
  
  gStyle->SetPalette(1);

  char *nameH[]={"JShadRecoilPt","JSelectronRecoPt","JSneutrinoRecoPt","JSmT"};

  cH=new TCanvas("hh","hh",800,600);
  cH->Divide(2,2);
  for(int k=0;k<4;k++) {
    cH->cd(k+1);
    h=(TH1F*)fdData->Get(nameH[k]);
    h->Draw();
    if(k==1) h->Rebin(2);
    if(k>1) h->Rebin(4);
  }
  TString titH="RecoilData";
  if(type==1) titH+=".ps";
  else titH+=".png";
  cH->Print(titH);
}


// $Log: plTransMass.C,v $
// Revision 1.1  2009/11/23 23:00:20  balewski
// code moved spin-pool
//


void plTpcEffCorrect(bool print=true){
  gStyle->SetPalette(1);
  gStyle->SetOptStat(100000000);
  const float PI=TMath::Pi();
  TList *Lx;  TLine *ln;

  //input files for TPC effic correction
  TString core="/star/data05/scratch/stevens4/wAnalysisOut/apsXsec/";
  TFile *fdQcd = TFile::Open("outMC/tpcEffic/uncorrected/rcf10016.wana.hist.root");
  //TFile *fdQcd = TFile::Open("outMC/corrected/rcf10016.wana.hist.root");
  TFile *fdData = TFile::Open(core+"forJoe/data/run9setABCD.wana.hist.root");
  TFile *fdCorrected = TFile::Open("/star/data05/scratch/stevens4/wAnalysisOut/apsXsec/feb01/data/run9setABCD.wana.hist.root");

  //#if 0
  //plots for pt>1
  //hQcd=(TH2F*)fdQcd->Get("muTr2D1"); assert(hQcd);
  //hData=(TH2F*)fdData->Get("muTr2D1"); assert(hData);

  //plots for pt>5
  hQcd=(TH2F*)fdQcd->Get("muTr2D1pt5"); assert(hQcd);
  hData=(TH2F*)fdData->Get("muTr2D1pt5"); assert(hData);
  hCorr=(TH2F*)fdCorrected->Get("muTr2D1pt5weight"); assert(hCorr);

  //get plots rebinned in my (eta,phi) mapping
  hQcdRebin=(TH2F*) hQcd->Clone();
  hDataRebin=(TH2F*) hData->Clone();
  hCorrRebin=(TH2F*) hCorr->Clone();
  hQcdRebin->Rebin2D(10,10); hDataRebin->Rebin2D(10,10);
  hCorrRebin->Rebin2D(10,10);
  //hDataRebin->Draw("colz"); hDataRebin->SetMinimum(0);
  

  //add etabin lines to original plot
  Lx=hQcd->GetListOfFunctions();
  //Lx=hData->GetListOfFunctions();
  for(int i=0; i<9; i++){
    float binlim=(i*.22)-.88;
    ln=new TLine(binlim,-PI,binlim,PI);
    ln->SetLineColor(kRed);
    ln->SetLineWidth(2);
    Lx->Add(ln);
  }
  //hQcd->Rebin2D(3,2); hQcd->Draw("colz");
  //hData->Draw("colz");

  //ratio of QCD MC to Data
  hRatio=(TH2F*) hQcdRebin->Clone();
  hRatio->Divide(hDataRebin);
  //hRatio->Draw("colz"); hRatio->SetMaximum(.13);
  
#if 0
  //final slices in |eta| to compare ratio
  gStyle->SetOptStat(1011);
  hRatio->ProjectionY("hRatioFin1",1,2);
  hRatio->ProjectionY("hRatioFin2",3,4);
  hRatio->ProjectionY("hRatioFin3",5,6);
  hRatio->ProjectionY("hRatioFin4",7,8);
  hRatio->ProjectionY("hRatioFin5",9,10);
  hRatioFin1->SetMinimum(0);hRatioFin1->SetMaximum(0.25); hRatioFin1->SetTitle("Ratio MC/Data tracks eta<-0.66");
  hRatioFin2->SetMinimum(0);hRatioFin2->SetMaximum(0.25); hRatioFin2->SetTitle("Ratio MC/Data tracks -0.66<eta<-0.22");
  hRatioFin3->SetMinimum(0); hRatioFin3->SetMaximum(0.25); hRatioFin3->SetTitle("Ratio MC/Data tracks |eta|<0.22");
  hRatioFin4->SetMinimum(0);hRatioFin4->SetMaximum(0.25); hRatioFin4->SetTitle("Ratio MC/Data tracks 0.22<eta<0.66");
  hRatioFin5->SetMinimum(0);hRatioFin5->SetMaximum(0.25); hRatioFin5->SetTitle("Ratio MC/Data tracks eta>0.66");
  c3=new TCanvas("cc","cc",1500,500);
  c3->Divide(5,1);
  c3->cd(1);
  hRatioFin1->Draw();
  c3->cd(2);
  hRatioFin2->Draw();
  c3->cd(3);
  hRatioFin3->Draw();
  c3->cd(4);
  hRatioFin4->Draw();
  c3->cd(5);
  hRatioFin5->Draw();
#endif

  //make projections to get ratio of MC/Data for each eta bin
  TH1 *hRatioAll[10];
  hRatio->ProjectionY("hRatioA",1,1);   hRatioAll[0]=hRatioA;
  hRatio->ProjectionY("hRatioB",2,2);   hRatioAll[1]=hRatioB;
  hRatio->ProjectionY("hRatioC",3,3);   hRatioAll[2]=hRatioC;
  hRatio->ProjectionY("hRatioH",8,8);   hRatioAll[7]=hRatioH;
  hRatio->ProjectionY("hRatioI",9,9);   hRatioAll[8]=hRatioI;
  hRatio->ProjectionY("hRatioJ",10,10); hRatioAll[9]=hRatioJ;

  float normal[10]={0.07,0.075,0.08,0,0,0,0,0.08,0.075,0.07};//determined from looking at data where yield was high
  //float normal[10]={0.049,0.065,0.069,0,0,0,0,0.068,0.062,0.055};//for syst error chosen for lowest phi bin in each eta slice
  if(print) c=new TCanvas("aa","aa",800,600);
  for(int j=0; j<10; j++){
    if(j>2 && j<7) continue;
    hRatioAll[j]->SetTitle(Form("Eta bin %c: Ratio of tracks QCD MC to run 9 data (track pt > 5)",'A'+j));
    hRatioAll[j]->SetMinimum(0); hRatioAll[j]->SetLineWidth(2);
    Lx=hRatioAll[j]->GetListOfFunctions(); hRatioAll[j]->SetStats(false);
    ln=new TLine(-PI,normal[j],PI,normal[j]);
    ln->SetLineColor(kRed); ln->SetLineWidth(2);  
    Lx->Add(ln);
    if(print){
      hRatioAll[j]->Draw();
      c->Print(Form("etabin%c.png",'A'+j));
    }
  }
  
  
  float phiRad=999;
  float etaDet=999;
  
  //initialize and clear eff values
  float effic[10][24];
  float efficSec[10][24];
  for(int i=0;i<10;i++){ 
    for(int l=0;l<24;l++){
      effic[i][l]=0;
      efficSec[i][l]=0;
    }
  }
  
  //calculate inefficiency
  for(int ieta=0; ieta<10; ieta++){
    if(ieta>2 && ieta<7) continue;
    for(int iphi=0; iphi<24; iphi++){
      float binVal=hRatioAll[ieta]->GetBinContent(iphi+1);
      effic[ieta][iphi]=binVal/normal[ieta];
      //cout<<"Eta "<<Form("%c",ieta+'A')<<" phi "<<iphi<<" effic "<<binVal<<endl;
      phiRad=hRatioAll[ieta]->GetXaxis()->GetBinCenter(iphi+1);
      
      //find tpc sector
      if(ieta<3) etaDet=-1;
      else if(ieta>6) etaDet=1;
      const float PI=TMath::Pi();
      int sec=0;
      float phi=phiRad/PI*180; // now in degrees
      if (etaDet>0) { // West TPC
	float x=75-phi;
	while(x<0) x+=360;
	sec=1+(int)( x/30.);
      } else {
	float x=phi-105;
	while(x<0) x+=360;
	sec=13+(int)( x/30.);
      }
      
      efficSec[ieta][sec-1]+=effic[ieta][iphi]/2; //average 2 bins in sector
    }
#if 0 //print weight arrays to use on data in W algo
    cout<<"mWeight"<<ieta+1<<"={";
    for(int isec=0; isec<24; isec++){
      //cout<<"sector "<<isec+1<<" eta "<<ieta<<" effic "<<efficSec[ieta][isec]<<endl;
      cout<<efficSec[ieta][isec]<<",";
    }
    cout<<"};"<<endl;
#endif
    //set weight arrays to use on MC in W algo
    for(int isec=0; isec<24; isec++){
      if(efficSec[ieta][isec]!=0) efficSec[ieta][isec]=1/efficSec[ieta][isec];
    //  cout<<"sector "<<isec+1<<" eta "<<ieta<<" effic "<<efficSec[ieta][isec]<<endl;
    }


  }

  //#endif
  
  
  //Data corrected for efficiencies in outer eta bins
  //hCorr->Draw("colz");
  hCorr->Rebin2D(10,10);
  hRatioCorr=(TH2F*) hQcdRebin->Clone();
  hRatioCorr->Divide(hCorrRebin);
  //hRatioCorr->Draw("colz");

  //add sector across pi boundry separately
  hCorr->ProjectionX("h1",1,1);
  hCorr->ProjectionX("h24",24,24);
  TH1 * hMidCorr[12];
  hMidCorr[11]=h1;hMidCorr[11]->Add(h24);
  
  bool print2=false;
  if(print2) c2=new TCanvas("bb","bb",800,600);
  for(int iphi=1; iphi<22; iphi+=2){ // get sector slices
    hCorr->ProjectionX(Form("h",iphi+1),iphi+1,iphi+2);   
    hMidCorr[(iphi-1)/2]=h;
   
    //find tpc sector
    float phiRad=hCorr->GetYaxis()->GetBinCenter(iphi+1);
    float etaDet=1;
    const float PI=TMath::Pi();
    int sec1=0; int sec2=0;
    float phi=phiRad/PI*180; // now in degrees
    float x=75-phi;
    while(x<0) x+=360;
    sec1=1+(int)( x/30.);
    float x=phi-105;
    while(x<0) x+=360;
    sec2=13+(int)( x/30.);

    
    //#if 0
    //calculate efficiency at midrapidity
    float etaVal[10]; float etaEffic[10];
    for(int ieta=0; ieta<10; ieta++)
      etaVal[ieta]=hMidCorr[(iphi-1)/2]->GetBinContent(ieta+1);
    float midRapVal=(etaVal[2]+etaVal[7])/2;
    if(sec2==13) float midRapVal=etaVal[2];
    for(int ieta2=3; ieta2<7; ieta2++){
      etaEffic[ieta2]=etaVal[ieta2]/midRapVal;
      if(ieta2<5){
	sec=sec2;
	//cout<<"sec "<<sec2<<" iphi "<<iphi<<" ieta "<<ieta2<<" effic "<<etaEffic[ieta2]<<endl;
      }
      else{
	sec=sec1;
	//cout<<"sec "<<sec1<<" iphi "<<iphi<<" ieta "<<ieta2<<" effic "<<etaEffic[ieta2]<<endl;
      }
      efficSec[ieta2][sec-1]=etaEffic[ieta2];
    }
    //#endif

    if(print2){
      Lx=h->GetListOfFunctions();
      h->SetStats(false);
      ln=new TLine(-0.44,midRapVal,0.44,midRapVal);
      ln->SetLineColor(kRed);  ln->SetLineWidth(2);
      Lx->Add(ln);
      h->SetMinimum(0); h->SetFillColor(kBlue);
      h->Draw();
      h->SetTitle(Form("Sectors %d and %d Tracks vs Eta",sec1,sec2)); 
      //c2->Print(Form("sec%d_%d.png",sec1,sec2));
    }
  }

  //do sector 9,15 separately
  for(int ieta=0; ieta<10; ieta++)
    etaVal[ieta]=hMidCorr[11]->GetBinContent(ieta+1);
  float midRapVal=(etaVal[2]+etaVal[7])/2;
  if(print2) {
    hMidCorr[11]->SetTitle("Sectors 9 and 15 Tracks vs Eta"); 
    hMidCorr[11]->Draw(); hMidCorr[11]->SetMinimum(0); c2->Print("sec9_15.ps");
    Lx=h->GetListOfFunctions();
    ln=new TLine(-PI,midRapVal,PI,midRapVal);
    ln->SetLineColor(kRed);  
    Lx->Add(ln);
  }
  for(int ieta2=3; ieta2<7; ieta2++){
    etaEffic[ieta2]=etaVal[ieta2]/midRapVal;
    if(ieta2<5){
      sec=15;
      //cout<<"sec "<<sec2<<" iphi "<<iphi<<" ieta "<<ieta2<<" effic "<<etaEffic[ieta2]<<endl;
    }
    else{
      sec=9;
      //cout<<"sec "<<sec1<<" iphi "<<iphi<<" ieta "<<ieta2<<" effic "<<etaEffic[ieta2]<<endl;
    }
    efficSec[ieta2][sec-1]=etaEffic[ieta2];
  }

#if 0  
  //print effic numbers
  for(int jeta=0;jeta<10;jeta++){
    cout<<"float mWeight"<<jeta+1<<"={";
    for(int jsec=0;jsec<24;jsec++){
      //cout<<"sec "<<jsec+1<<" eta "<<jeta<<" effic "<<efficSec[jeta][jsec]<<endl;
      cout<<efficSec[jeta][jsec]<<",";
    }
    cout<<"};"<<endl;
  }
#endif
  

}

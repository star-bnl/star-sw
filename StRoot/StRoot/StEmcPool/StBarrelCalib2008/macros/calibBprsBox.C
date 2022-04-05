// cat ps/bprs* |ps2pdf - BPM12.pmt1x.pdf
//cat psV6/soft* | ps2pdf - allTower.pdf 
//cat psV6/bprsBPM* | ps2pdf - allBPM.pdf 

#include <vector>
const int mxBTile=2;

vector <int> softL; // list of Bprs softID's belonging to given PMT
TH2F *h2DoutRaw=0; // raw 2D spectrum
TH2F *h2DoutMip=0; // mip 2D spectrum
TH1F *h1MaP=0,*h1MaT=0; // MIP ADC
TH2F *h2Cr=0; // MIP ADC vs. slopw

TH2F* h2DinRawP, *h2DinTrP, *h2DinMipP;
TH2F* h2DinRawT, *h2DinTrT, *h2DinMipT;
TH1F* janDb_mipMean[mxBTile], * janDb_mipSig[mxBTile], * janDb_mipStat[mxBTile];
int doPS=2; // postscript : 1=per MPT, 2=per softID
TFile *fout=0;

calibBprsBox(int box=11, int pmt=5) {
  gStyle->SetOptStat(1110);
  gStyle->SetOptFit(1);
  gStyle->SetPalette(1,0);
 

  char *fname="calib-nov-21-2008/sumD43-70V6.hist.root";
  fd=new TFile(fname); assert(fd->IsOpen());
  printf("Work with %s\n", fd->GetName()); //fd->ls();
 
  char *fnameO="barrelMipSpectV6.hist.root";
  fout=new TFile(fnameO,"update"); assert(fout->IsOpen());
  printf("Write output to %s\n", fout->GetName()); 

  //... load used calibration ....
  char txt[100];
  sprintf(txt,"calib-nov-20-2008/mipGainBprs+Btow_v2.hist.root");
  TFile* fd5=new TFile(txt);  assert(fd5->IsOpen());
  fd5->ls();   
  char *core2[mxBTile]={"btow","bprs"};
  for(int ibp=0;ibp<mxBTile;ibp++) {
    TString tit=core2[ibp]; tit+="MipGain";
    janDb_mipMean[ibp]=(TH1F *)fd5->Get(tit); assert(   janDb_mipMean[ibp]);
    tit=core2[ibp]; tit+="MipSig";
    janDb_mipSig[ibp]=(TH1F *)fd5->Get(tit); assert(   janDb_mipSig[ibp]);
    tit=core2[ibp]; tit+="MipStat";
    janDb_mipStat[ibp]=(TH1F *)fd5->Get(tit); assert(   janDb_mipStat[ibp]);
  }
 

  h2DinRawP=(TH2F*)fd->Get("BPRS_c0"); assert(h2DinRawP);
  h2DinTrP=(TH2F*)fd->Get("mipBprsTr"); assert(h2DinTrP);
  h2DinMipP=(TH2F*)fd->Get("mipBprsTrBt"); assert(h2DinMipP);

  h2DinRawT=(TH2F*)fd->Get("BTOW_c0"); assert(h2DinRawT);
  h2DinTrT=(TH2F*)fd->Get("mipBtowTr"); assert(h2DinTrT);
  h2DinMipT=(TH2F*)fd->Get("mipBtowTrPr"); assert(h2DinMipT);

  buildSoftList(box,pmt);
  
  char core[1000];
  sprintf(core, "BPRS PMB=%d pmt=%d",box,pmt);
  int page=0;
  can=new TCanvas("aa","aa",800,820);   
  TPad *c=makeTitle(can,core,page);
  c->Divide(1,3);
 
  axY=h2DinRawP->GetYaxis();
  float y1=axY->GetXmin();
  float y2=axY->GetXmax();
  int nbY=axY->GetNbins();
  printf("Y-axis range  --> [%.1f, %.1f], nb=%d\n",y1,y2,nbY);

  char tt1[100], tt2[1000];
  sprintf(tt1, "rawBPM%d_%d",box,pmt);
  h2DoutRaw=new TH2F(tt1,"aa2;;ADC-ped",16,0.5,16.5,nbY,y1,y2);
  h2DoutRaw->GetXaxis()->SetTitleSize(0.05);
  sprintf(tt1, "mipBPM%d_%d",box,pmt);
  h2DoutMip=new TH2F(tt1,"aa1;;ADC-ped",16,0.5,16.5,nbY,y1,y2);
  h2DoutMip->GetXaxis()->SetTitleSize(0.05);

  sprintf(tt1, "gainBPM%d_%d",box,pmt);
  sprintf(tt2, "BPRS gain BPM-%d_%d, all tiles; average MIP (ADC)",box,pmt);
  h1MaP=new TH1F(tt1,tt2,40,0,40);
  h1MaT=new TH1F("aa4","BTOW tower: mean MIP; ADC-ped",25,0,50);
  h2Cr=new TH2F("aa5","BPRS comparison (both axis); slope (raw) ; average MIP (fit)",20,-0.15,-0.015,20,0,40);

  TString xtit=core; xtit+="; softID:";
  for(int i=0;i<softL.size();i++) {
    int softID=softL[i];
    xtit+=softID; xtit+=" ,  ";
    processOne(softID,core,i+1);
    // break;
    //if(i>1) break;
  }
  // return;
  ln00=new TLine(0,0,17,0); ln00->SetLineStyle(2);
  TString tit;
  c->cd(3); 
  h2DoutRaw->Draw("colz"); gPad->SetLogz(); h2DoutRaw->SetAxisRange(-10,60,"y");
  tit=core; tit+=", all values"+xtit;
  h2DoutRaw->SetTitle(tit);
  ln00->Draw();

  c->cd(2); 
  h2DoutMip->Draw("colz"); 
  tit=core; tit+=", reqire MIP @ TPC & BTOW"+xtit;
  h2DoutMip->SetTitle(tit);
  h2DoutMip->Rebin2D(1,3);
  ln00->Draw();
  h2DoutMip->SetAxisRange(-10,60,"y");
  
  c->cd(1);
  pad = new TPad("pad1", "apd1",0.0,0.0,1,.95);
  pad->Draw();
  pad->Divide(3,1);
  pad->cd(1);
  h1MaP->Draw(); 

  pad->cd(2);
  h2Cr->Draw("box");

  pad->cd(3);
  h1MaT->Draw(); //  h1MaT->SetLineColor(kGreen);
 
  if(doPS) {
    sprintf(core, "ps/bprsPmtPix%02d.ps",0);
    if(doPS==2) sprintf(core, "ps/bprsBPM%03d_%d.ps", box , pmt);
   can->Print(core);
  }

  //....... write summary histos to output file
  fout->cd();
  h2DoutRaw->Write();
  h2DoutMip->Write();
  h1MaP->Write();
  h2Cr->Write();
  //  fout->ls();

}

//------------------------
//------------------------
//------------------------
void processOne(int softID, char *core0,int pix) {
  TString ttx=core0; ttx+=pix;
  can=new TCanvas(ttx,ttx,800,700);
  char core[1000];
  sprintf(core, "%s, softID=%d",core0,softID);
 
  c=makeTitle(can,core,pix);
  c->Divide(2,2);
  c->cd(1);

  TH1F * hrP= getSlice( h2DinRawP,softID,"bprsR");
  TH1F * htP= getSlice( h2DinTrP,softID,"bprsT");
  TH1F * hmP= getSlice( h2DinMipP,softID,"bprsM");

  TH1F * hrT= getSlice( h2DinRawT,softID,"btowR");
  TH1F * htT= getSlice( h2DinTrT,softID,"btowT");
  TH1F * hmT= getSlice( h2DinMipT,softID,"btowM");

  hrP->SetTitle("BPRS spectrum,  all events");
  hrP->Fit("expo","","R",13,50);
  TF1 *ff=hrP->GetFunction("expo");
  float slope=ff->GetParameter(1);
  hrP->Fit("gaus","+","R",-10,10);
 
  hmP->SetLineColor(kRed); hmP->SetFillColor(kRed);
  hmP->SetTitle("BPRS spectrum,  MIP : TPC, TPC+BTOW");
  hmP->Fit("gaus","","R",-10,50);

  TF1 *ff=hmP->GetFunction("gaus");
  ff->SetLineColor(kBlue);   ff->SetLineWidth(1);
  float mean=ff->GetParameter(1);  
  float meanS=ff->GetParameter(2);
  float cut_adcL=2;
  float cut_sig=1.8;
  if(mean>cut_adcL && meanS>cut_sig) { // cut if MIP peak is too close to ped
    h1MaP->Fill(mean);
    h2Cr->Fill(slope,mean);
  }

  hrT->SetTitle("BTOW spectrum,  all events");
  hrT->Fit("expo","","R",10,40);
  hmT->SetLineColor(kGreen);hmT->SetFillColor(kGreen);
  hmT->Fit("gaus","","R",-10,60);
  TF1 *ffT=hmT->GetFunction("gaus");
  ffT->SetLineColor(kBlue);   ffT->SetLineWidth(1);
  float meanT=ffT->GetParameter(1);
  float meanST=ffT->GetParameter(2);
  h1MaT->Fill(meanT);
 
  hmT->SetTitle("BTOW spectrum,  MIP : TPC, TPC+BPRS");

  //..... BPRS .....
  c->cd(1); 
  hrP->Draw(); gPad->SetLogy(); hrP->SetMinimum(0.7);
  hmP->Draw("same");  gPad->SetGrid();
  float xC=mean-meanS,yC=1e7;
  ln=new TLine(xC,0,xC,yC); ln->SetLineColor(kMagenta); ln->Draw();

  c->cd(3);
  hmP->Draw(); hmP->SetMaximum(htP->GetMaximum()/3); 
  htP->Draw("same"); 
 
  //..... BTOW.....
  c->cd(2); 
  hrT->Draw(); gPad->SetLogy(); hrT->SetMinimum(0.7);
  hmT->Draw("same");gPad->SetGrid();

  c->cd(4);
  hmT->Draw(); hmT->SetMaximum(htT->GetMaximum()/3); 
  htT->Draw("same"); 
 
  ln=new TLine(10,-3,25,-3); ln->SetLineColor(kMagenta); ln->Draw();ln->SetLineWidth(2);
  int nb=hmP->GetXaxis()->GetNbins();
  for(int b=1;b<=nb;b++) {
    h2DoutRaw->SetBinContent(pix,b,hrP->GetBinContent(b));
    h2DoutMip->SetBinContent(pix,b,hmP->GetBinContent(b));
  }
  
  FILE *fcsv=fopen("bprs+btowMipGain.csv","a");
  // fprintf(fcsv,"average MIP ADC for BPRS tiles and BTOW towers; ver=1.6\n");
  //fprintf(fcsv,"softID,PMB_pmt, pmt pix #, , bprs nMIP,  bprs avrMIP (adc), bprs sigMIP (adc), , btow nMIP,  btow avrMIP (adc), btow sigMIP (adc)\n");
  fprintf(fcsv,"%d, %s, %d,  , %d, %.2f, %.2f, ,  %d, %.2f, %.2f\n", 
	  softID,core0+9,pix,(int)hmP->GetEntries(),mean,meanS,
	  (int)hmT->GetEntries(),meanT,meanST	  );
  fclose(fcsv);
  //..... compute ADC limmits
  for(int ibp=0;ibp<mxBTile;ibp++) {
    float adcL=5., adcH=40.; // default for _BTOW_
    if(ibp==1) adcL=3.5; // BPRS
    int statGain=(int)janDb_mipStat[ibp]->GetBinContent(softID);
     if(!statGain){ //valid gain
      float mean=janDb_mipMean[ibp]->GetBinContent(softID);
      float sig=janDb_mipSig[ibp]->GetBinContent(softID);
      adcL=mean-sig;
      adcH=mean+sig;
      if(ibp==0 && adcL<5) adcL=5; // BTOW
      if(ibp==1 && adcL<3.5) adcL=3.5;// BPRS
      if(adcH>2*mean) adcH=2*mean;
      printf("ibp=%d id=%4d MIP mean=%.1f sig=%.1f adcL=%.1f H=%.1f\n",ibp,softID,mean,sig,adcL,adcH);      
    }
    float y=1e5;
    TH1F *h1=hmT; if (ibp==1) h1=hmP;
    Lx=h1->GetListOfFunctions();    assert(Lx);
    lnL=new TLine( adcL,0, adcL,y); lnL->SetLineColor(kBlue);lnL->SetLineWidth(2);lnL->SetLineStyle(2);
    Lx->Add(lnL);
    lnH=new TLine( adcH,0, adcH,y); lnH->SetLineColor(kBlue);lnH->SetLineWidth(2);lnH->SetLineStyle(2);
    Lx->Add(lnH);
 } // Loop over B/P 

  //....... write 1D histos to output file
  fout->cd();
  char tt1[100], tt2[1000];
  sprintf(tt2, "id=%d %s pix=%d, ",softID,core0,pix);
  sprintf(tt1, "bprs%d",softID);

  TString Tt=tt1, T2=tt2;
  hrP->SetName(Tt+"a"); hrP->SetTitle(T2+"inclusive"); hrP->Write();
  htP->SetName(Tt+"t"); htP->SetTitle(T2+"MIP in TPC"); htP->Write();
  hmP->SetName(Tt+"m"); hmP->SetTitle(T2+"MIP in TPC+BTOW");hmP->Write();
 
  sprintf(tt2, "id=%d BTOW , ",softID);
  sprintf(tt1, "btow%d",softID);
  Tt=tt1, T2=tt2;
  hrT->SetName(Tt+"a"); hrT->SetTitle(T2+"inclusive"); hrT->Write();
  htT->SetName(Tt+"t"); htT->SetTitle(T2+"MIP in TPC"); htT->Write();
  hmT->SetName(Tt+"m"); hmT->SetTitle(T2+"MIP in TPC+BPRS");hmT->Write();
 //...... FINISH .....
  sprintf(core, "softID=%d",softID);
  can->SetTitle(core);
  if(doPS) {
    sprintf(core, "ps/bprsPmtPix%02d.ps",pix);
    if(doPS==2) sprintf(core, "ps/softID%04d.ps",softID);
    can->Print(core);
  }


 }


//=================
TH1F * getSlice(TH2F * h2, int id, char *ctit) {
  axX=h2->GetXaxis();
  float x1=axX->GetXmin();
  float x2=axX->GetXmax();
  int nbX=axX->GetNbins();
  printf("X-axis range  --> [%.1f, %.1f], nb=%d %s\n",x1,x2,nbX,axX->GetTitle());

  axY=h2->GetYaxis();
  float y1=axY->GetXmin();
  float y2=axY->GetXmax();
  int nbY=axY->GetNbins();
  printf("Y-axis range  --> [%.1f, %.1f], nb=%d\n",y1,y2,nbY);

  assert(id>=1 && id<=nbX);
  // do projections
  char txt1[100], txt2[1000];
  sprintf(txt1,"%s_id%d",ctit,id);
  sprintf(txt2,"%s soft id=%d;%s ",ctit, id,axY->GetTitle());
  TH1F*h=new TH1F(txt1,txt2,nbY,y1,y2); // working histo for 1-D spectrum
  
  int i;
  for(i=1;i<=nbY;i++) h->SetBinContent(i,h2->GetBinContent(id,i));
  float x1=-20, x2=70;
  h->SetAxisRange(x1,x2);
  h->SetEntries(h->Integral());
 
  return h;
}


//------------------------
void buildSoftList(int box=11, int pmt=1) {
   fd1=new TFile("calib-nov-8-2008/map-softID-bprsPmt-Rory.root"); assert(fd1->IsOpen());  
  printf("Opened: %s\n",fd1->GetName()); fd1->ls();
  TH1I * mapBprsPmt=(TH1I*) fd1->Get("bprsPmt"); assert(mapBprsPmt);
  //mapBprsPmt->Draw();
  
  int y= box*10+pmt;
  for(int b=1;b<=4800;b++) {
    if(y!=(int)mapBprsPmt->GetBinContent(b)) continue;
    softL.push_back(b);
  }
  printf("found pmb=%d pmt=%d Bprs tiles =%d\n",box,pmt,softL.size());
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
  txt2+=", pixel=";
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


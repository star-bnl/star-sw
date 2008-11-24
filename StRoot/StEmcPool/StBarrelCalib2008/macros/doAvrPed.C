/* notes, R9067013 OLD
   BPRS:
   kBad=0x1:  yield in ADC range [100,300] 100 counts, input was ~220 events
          7 tiles:  softID=3301-4,3322-4
   kBad=0x2: yiled @ [mean +/- 9 ADC] below 70% 
	  0 tiles
   kBad=0x4: bad chi2/dof >10          
          42-7=35 tiles, softID=3863 ...3940, about 50%
   kBad=x8 = ped low & high, set wide [105,295]
          0 tiles
   kBad=0x10  sticky low bit @ pedestal area
          85-42=43 , tiles softID=3862 ...3940, about 50%
   kBad=0x20  sig ped >2.5 ADC
          132-85=47 tiles, softID=3021-3032, 3041-3048, 3052,3103,3621-3652


*/

const int  mxTP=2, mxBTiles=4800;
char  cTile[mxTP]={'T','P'};
char  *cTile4[mxTP]={"BTOW","BPRS"};
TH1F * hPed, *sigP, *hChi,*hSig;
TH1I * hStat;
TGraphErrors *grP;
TFile *fd2=0, *fd1=0;
TH1F *hPeak; // yield of pedestal
TCanvas *c=0;
int isRawAdc=0; // 1=raw, 0=pedSub

doAvrPed( int k=10,int run=9067013) {
  gStyle->SetPalette(1,0);
  gStyle->SetOptFit(1);
  gStyle->SetOptStat(1001110);

  char *pathIn="outA3/";
  char *pathOut="iter4/";
  char txt[1000], txt2[1000];

  sprintf(txt,"%s/R%dp.barCal.-1.hist.root",pathIn,run);

  fd1=new TFile(txt);  assert(fd1->IsOpen());
  //  fd1.ls(); return;
  sprintf(txt,"%s/pedBprsR%davr.hist.root",pathOut,run);
  fd2=new TFile(txt,"recreate"); 
  assert(fd2->IsOpen());
  int i;
   grP=new TGraphErrors;
   //  gr->SetMarkerStyle(20+m);
   grP->SetMarkerSize(0.5);

  TH2F * h2T= (TH2F *)fd1->Get("BTOW_c0"); assert(h2T);
  TH2F * h2P= (TH2F *)fd1->Get("BPRS_c0"); assert(h2P);
  h2P->GetXaxis()->SetLabelSize(0.08);
  h2P->GetYaxis()->SetLabelSize(0.08);
  h2P->Draw("colz");
  //  Printf("XXX %f\n", h2P->GetYaxis()->SetTitleSize());

  // return;
 int id1=1, id2=id1+k-1;
  if(k<0) { id1=1; id2=mxBTiles; }
  c=new TCanvas("aa","aa",400,300);
  // id1=id2=k;//tmp
  fitPedBT(1,run,h2P,id1,id2 );
  gPad->SetLogy();

  // save output histo
  fd2->cd();  hPed->Write();
  hStat->Write();
  hChi->Write();
  hSig->Write();
  hPeak->Write();
  
  // return;
  c=new TCanvas(fd1->GetName(),fd1->GetName(),1000,800);
  c->Divide(1,5);
  gStyle->SetOptStat(10);
  c->cd(1);  h2P->Draw("colz");  h2P->SetAxisRange(id1,id2); 
  hPed->Draw("same");

  c->cd(2); hStat->Draw(); hStat->SetAxisRange(id1,id2); 
   hStat->SetMinimum(.5);
   if(hStat->GetMaximum()>10) gPad->SetLogy();
   gPad->SetGrid();
 
   c->cd(3);
   hChi->Draw();  hChi->SetAxisRange(id1,id2);// hChi->SetMaximum(20);
   gPad->SetGrid();
   
  c->cd(4);    
  hSig->Draw();  hSig->SetAxisRange(id1,id2); hSig->SetMaximum(5);
  gPad->SetGrid();

  c->cd(5);
  hPeak->Draw();  hPeak->SetAxisRange(id1,id2); hPeak->SetMinimum(0.7);
  gPad->SetGrid();

  //  gPad->SetLogy();
  // c->cd(1); hPed->Draw(); hPed->SetAxisRange(id1,id2); 
  

  return;
  fd2->cd();
  hres->Write();
  grPedR[0]->Write();

}



//==============================================
//==============================================
//==============================================
void  fitPedBT( int itp,int run,TH2F *h2, int id1, int id2) {
  if(id2>18000) id2=18000;
    char txt[1000], txt2[1000];
    
    sprintf(txt,"ped%s",cTile4[itp]);
    sprintf(txt2,"ped %s R%d; %s soft ID; pedestal +/- fit err (ADC)",cTile4[itp],run,cTile4[itp]);
    hPed=new TH1F(txt,txt2,mxBTiles,0.5,mxBTiles+0.5);
    hPed->Sumw2();
    hPed->GetXaxis()->SetLabelSize(0.08);
    hPed->GetYaxis()->SetLabelSize(0.08);

    sprintf(txt,"stat%s",cTile4[itp]);
    sprintf(txt2,"status %s R%d; %s soft ID; jan status",cTile4[itp],run,cTile4[itp]);
    hStat=new TH1I(txt,txt2,mxBTiles,0.5,mxBTiles+0.5); hStat->Reset(-1); // default is bad
    hStat->GetXaxis()->SetLabelSize(0.08);
    hStat->GetYaxis()->SetLabelSize(0.08);

    sprintf(txt,"chi%s",cTile4[itp]);
    sprintf(txt2,"chi2/DOF %s R%d; %s soft ID; ped Chi2/DOF",cTile4[itp],run,cTile4[itp]);
    hChi=new TH1F(txt,txt2,mxBTiles,0.5,mxBTiles+0.5); hStat->Reset(-1); // default is bad
    hChi->GetXaxis()->SetLabelSize(0.08);
    hChi->GetYaxis()->SetLabelSize(0.08);
    hChi->GetYaxis()->SetTitleSize(0.2);

    sprintf(txt,"sigPed%s",cTile4[itp]);
    sprintf(txt2,"sigma(ped) %s R%d; %s soft ID; sig(ped) ADC",cTile4[itp],run,cTile4[itp]);
    hSig=new TH1F(txt,txt2,mxBTiles,0.5,mxBTiles+0.5); hStat->Reset(-1); // default is bad
    hSig->GetXaxis()->SetLabelSize(0.08);
    hSig->GetYaxis()->SetLabelSize(0.08);

    hPeak=new TH1F("hpedPeak", "integral of pedestal peak;soft ID", mxBTiles,0.5,mxBTiles+0.5);
    hPeak->GetXaxis()->SetLabelSize(0.08);
    hPeak->GetYaxis()->SetLabelSize(0.08);

  
  float par_nsig=3; // integration range for QA
  float par_rms=2; // to approximate fit range
  float cut_yield0=100; 
  float cut_yieldR1=0.7;
  float cut_ch2min=0.1; 
  float cut_ch2ndf=200.; 
  float cut_pedL=105;
  float cut_pedH=295;
  float cut_pedH=295;
  float cut_sigPed=2.7;

  //  float cut_minYiled=50;

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

  TH1F*h=new TH1F("h1","h1",nbY,y1,y2); // working histo for 1-D spectrum
  // do projections
  int ih;
  for(ih=id1; ih<=id2; ih++) {
    char txt1[100], txt2[1000];
    sprintf(txt1,"id%d",ih);
    sprintf(txt2,"%s soft id=%d;%s ",cTile4[itp],ih,axY->GetTitle());
    h->SetTitle(txt2);
    h->Reset();  h->SetAxisRange(y1,y2);
    int i;
    int kBad=1;
    for(i=1;i<=nbY;i++) h->SetBinContent(i,h2->GetBinContent(ih,i));
    //for(i=1;i<=nbY;i++) printf("%d %f \n",i,h2->GetBinContent(ih,i));
    float mean=h->GetMean();
    float yield=h->Integral();
    h->SetEntries(yield);
    printf("******** work on %s  mean=%.1f rms=%.1f yield=%.1f\n",txt1,mean,h->GetRMS(),yield);

    if(yield<cut_yield0) {  hStat->SetBinContent(ih,kBad); continue; }
    kBad<<=1;

    if(isSticky(h,mean)) {  hStat->SetBinContent(ih,kBad); continue; }
    kBad<<=1;

    // if(ih>=12367) return;
    float adc1=mean-par_rms*par_nsig;
    float adc2=mean+par_rms*par_nsig-1;
    h->SetAxisRange( adc1,adc2);
    float yield2=h->Integral();
    float r1=yield2/yield;
    printf(" range=%d,%d  r1=%.3f yield2=%.1f\n",adc1,adc2,r1,yield2);
    
    if(r1< cut_yieldR1) {  hStat->SetBinContent(ih,kBad); continue; }
    kBad<<=1;

    h->Fit("gaus","Q","Rh", adc1, adc2);
    TF1 *ff=h->GetFunction("gaus"); assert(ff);
    ff->SetLineColor(kRed);
    ff->SetLineWidth(1.);
    h->Draw();
    
    float ped=ff->GetParameter(1);
    float pedErr=ff->GetParError(1);
    float sig=ff->GetParameter(2);
    float chi2=ff->GetChisquare();
    float ndf=ff->GetNDF();
  
    printf("chi2=%f ndf=%f\n", chi2,ndf);
    if(chi2<cut_ch2min)  {  hStat->SetBinContent(ih,kBad); continue; }
    // keep the same flag
    hChi->SetBinContent(ih, chi2/ndf);
    if(chi2/ndf> cut_ch2ndf)  {  hStat->SetBinContent(ih,kBad); continue; }
    kBad<<=1;
 
    adc1=ped-sig*par_nsig;
    adc2=ped+sig*par_nsig;
    h->SetAxisRange( adc1,adc2);
    float yield3=h->Integral();
    float r2=yield2/yield;
    printf("ped=%f sig=%f range=%d,%d r2=%.3f\n",ped,sig,adc1,adc2,r2);

    hSig->SetBinContent(ih,sig);
    if(sig>cut_sigPed || sig<0) {  hStat->SetBinContent(ih,kBad); continue; }
    kBad<<=1;

    if(isRawAdc){
      if(ped< cut_pedL)  {  hStat->SetBinContent(ih,kBad); continue; }
      if(ped> cut_pedH)  {  hStat->SetBinContent(ih,kBad); continue; }
      kBad<<=1;
    }
     
    hPed->SetBinContent(ih,ped);
    hPed->SetBinError(ih,pedErr);

    int np=grP->GetN();
    grP->SetPoint(np,ih,ped);
    grP->SetPointError(np,0,pedErr);
    
    hPeak->SetBinContent(ih,r2);
    hStat->SetBinContent(ih,0); // good  
  }
  h->SetAxisRange(y1,y2);
  
  printStat(id1,id2);
  fd2->cd();
  h->Write();
}

//================
void isSticky(TH1F *h, float ped) {
  assert(h);
  axX=h->GetXaxis();
  int kPed=axX->FindBin(ped);
  printf("%s ped=%f kPed=%d\n",h->GetName(),ped,kPed);
  float sum1=0,sum2=0;
  for(int i=kPed-10;i<=kPed+10;i++) {
    float val=h->GetBinContent(i);
    // printf("i=%d val=%f\n",i,val);
    if(i%2==0) sum1+=val;
    if(i%2==1) sum2+=val;
  }
  float r=-1;
  if(sum1>sum2) r=sum2/sum1;
  else r=sum1/sum2;
  printf("sum1=%f sum2=%f, r=%f\n",sum1,sum2,r);
  if(r<0.1) return true; // is sticky bit
  return false; // good spectrum

}


//================
void printStat(int id1,int id2) {
  int nBad=0, nGood=0;
  printf("softID stat \n",);
  
  for(int i=id1; i<=id2;i++) {
    int val=hStat->GetBinContent(i);
    if(val<0.5) { 
      if(val==0)nGood++; 
      continue;
    }
    printf("%d 0x%0x\n",i,val);
  //  if(val>0x10)  printf("softID=%d stat=0x%0x\n",i,val);
    nBad++;
  }
  printf("\nstat summary: tot=%d good=%d bad=%d\n",id2-id1+1,nGood, nBad);
}

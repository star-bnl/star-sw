TH2F *h2scan=0;
const int mxBTiles=4800;

float cut_minZ=40; // for BPRS use 40 if nRun=124
int isBprs=0;

void findMap( ){
  
  char *crun="calib-nov-8-2008/sum-swapB";
  crun="sumD43-70";
  gStyle->SetPalette(1,0);
  gStyle->SetOptStat(0);

 
  char *pathIn="./";
  char *pathOut="ourX/";
  char txt[1000], txt2[1000];
  
  sprintf(txt,"%s/%s.hist.root",pathIn,crun);
  fd1=new TFile(txt); assert(fd1->IsOpen());  
  printf("Opened: %s\n",fd1->GetName());
  h2scan= (TH2F *)fd1->Get("swapScan"); assert(h2scan);

  int id1=1, id2=4800;
  id2=41;
  c=new TCanvas("aa","aa",1400,300);
 
  TH1F *h1=findBest(h2scan, id1,id2,crun );
  h1->Draw();
  // gPad->SetLogy();

}



//==============================================
//==============================================
//==============================================
TH1F*  findBest( TH2F *h2, int id1, int id2, char *crun) {
  
  char txt[1000], txt2[1000];
  
  sprintf(txt,"mapCor");
  sprintf(txt2,"mapping crection  %d; old soft ID; new soft ID", crun);
  hMapCor=new TH1I(txt,txt2,mxBTiles,0.5,mxBTiles+0.5);
 

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

  TH1F*h1=new TH1F("h1","h1",nbX,x1,x2); // working histo for 1-D spectrum
  // do projections

  for(int iy=id1; iy<=id2; iy++) {
    char txt1[100], txt2[1000];
    sprintf(txt1,"id%d",iy);
    sprintf(txt2,"soft id=%d; %s",iy,axX->GetTitle());
    h1->SetTitle(txt2);
    h1->Reset();  //h->SetAxisRange(y1,y2);
    int i;
    int kBad=1;
    for(i=1;i<=nbX;i++) h1->SetBinContent(i,h2->GetBinContent(i,iy));
    float peakX=h1->GetMaximumBin();
    float peakZ=h1->GetMaximum();
    float yield=h1->Integral();
    h1->SetEntries(yield);
    printf("     work on %s  peakX=%.1f peakZ=%.1f\n",txt1,peakX,peakZ);
    float idL=peakX-20;
    float idH=peakX+20;
    h1->SetAxisRange( idL,idH);
    float yieldR=h1->Integral();

    float r1=peakZ/yieldR;
    h1->SetAxisRange( 0,5000);

    printf("    ID range=%d,%d  r1=%.3f yieldR=%.1f\n",idL,idH,r1,yieldR);
    int idBest=(int)peakX;
    if(peakZ<cut_minZ) { printf("##   DEAD/MISSING  softID %d ",iy); 
    } else   if(idBest==iy) { printf("#  OK  softID %d ",iy); 
    } else {
      printf("ss  hReMap->SetBinContent( %4d, %4d); // nMIP=%.0f, PDF page=%d\n",idBest,iy,peakZ, 1+((iy-1)/200));
      printf("## CHANGE old softID %d --> %d , nMIP=%.0f-->",idBest,iy,h1->GetBinContent(iy)); 
      assert(isBprs==0); // I changed old<-->new for towers
    }
    printf(" nMIP=%.0f\n",peakZ);
    
  }

  return h1;
}

#if 0
    if(yield<cut_yield0) {  hStat->SetBinContent(ih,kBad); continue; }
    kBad<<=1;

    if(isSticky(h,mean)) {  hStat->SetBinContent(ih,kBad); continue; }
    kBad<<=1;

    // if(ih>=12367) return;
    
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

#endif

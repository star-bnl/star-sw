int A='A', B='B',C='C',D='D',E='E';
int a='A', b='B',c='C',d='D',e='E';
TFile *f;
TCanvas *cc;

plPresMap(int sect=3){
  TString path="./";
  path="/star/data05/scratch/balewski/2005-eemcCal/";
  //path+="/day49-hist/iter2-out/";
  // path+="/day171-hist/iter5-pp/";
  path+="/mc-hist/iter8-mc/";
  path="iter14-pp/";
  path+="sum-sect";  path+=sect;
  //path+="R112";
  path+=".hist.root";
  
  f=new TFile(path);
  assert(f->IsOpen());
  //f=new TFile("iter4-out/R6173068.hist.root");
  gStyle->SetPalette(1,0);
  return;
}

plPresSect(int sec=1) {
  int sub=B,eta=4;
  for(eta=1;eta<=12;eta++)
  for(sub=A;sub<=E;sub++) {
    plPres(sec,sub,eta);
    cc->Print();
  }
}

plPres(  int sec=3,  int sub=c,  int eta=10 ) {
  int reb0=4;
  char name[100];
  sprintf(name,"tower-%02d%c%02d",sec,sub,eta);
   
  cc=new TCanvas(name,name,550,560);  cc->Divide(2,2);  // 2x2
  // cc=new TCanvas(name,name,300,900);  cc->Divide(1,4); // slim

  int binL=5,binH=50;
  const int nT=4;
  char tA[nT]={'T','P','Q','R'};
  char *tN[nT]={"Tow","Pre1","Pre2","Post"};
  int pedA[nT]={20,50,50,50};
  int it;
  for(it=0;it<nT;it++) {
    int reb=reb0;
    if(it==0) reb=2;
    int ped=pedA[it];
    cc->cd(1+it);
    char name[100];

    sprintf(name,"a%02d%c%c%02d",sec,tA[it],sub,eta);
    // printf("name=%s=\n",name);
    TH1F *h=(TH1F *)f->Get(name); assert(h);
    int aN= h->Integral(binL+ped,binH+ped);
    h->Draw();
    h->SetMinimum(0.2);
    float yMax=h->GetMaximum()/20.;

    gPad->SetLogy();
    if(it==0) h->SetAxisRange(-50,120);

    sprintf(name,"d%02d%c%c%02d",sec,tA[it],sub,eta);
    h=(TH1F *)f->Get(name); assert(h);
    int dN= h->GetEntries();
    h->SetFillColor( kRed);  h->SetLineColor( kRed);
    h->Draw("same");
    h->Rebin(reb);

    sprintf(name,"e%02d%c%c%02d",sec,tA[it],sub,eta);// printf("name=%s=\n",name);
    h=(TH1F *)f->Get(name); assert(h);
    int eN= h->GetEntries();
    h->Draw("same");
    h->Rebin(reb);
    h->SetFillColor( h->GetLineColor( )); 
    h->SetFillStyle(3644);

    float r=0;
    if(dN) r=1.*eN/dN;

    printf("%s [%d,%d]--> a=%5d  d=%3d  e=%3d  r=%.2f\n",name+1,binL,binH,aN,dN,eN,r);

    sprintf(name,"%s   R=%.2f",tN[it],r);// printf("name=%s=\n",name);
    TText *tx=new TText(20,yMax,name);
    tx->SetTextSize(0.08);
    tx->SetTextColor( h->GetLineColor( ) );
  
    tx->Draw();
    // break;
 }
}


plotDead(){
  TGraph *grT=new TGraph(); 
  grT->SetMarkerStyle(22);  grT->SetMarkerSize(2);grT->SetMarkerColor(kMagenta);
  grT->SetPoint(0,65,115); // 1TC06
  grT->SetPoint(1,21,-93); // 6TA03
  grT->SetPoint(2,-60,-100); // 7TC05
  grT->Draw("P");
  return;

  TGraph *grR=new TGraph(); grR->SetMarkerSize(2);grR->SetMarkerColor(kBlue);
  grR->SetMarkerStyle(24);
  grR->SetPoint(0,120,168); // 1TD12
  grR->SetPoint(1,201, -45); // 3TE12
  grR->SetPoint(2,-55,-75); // 7TD03
  grR->Draw("P");

  TGraph *grP=new TGraph(); grP->SetMarkerSize(2);grP->SetMarkerColor(kRed);
  grR->SetMarkerStyle(25);
  grR->SetPoint(0,25,-85); // 5PE2
  grR->SetPoint(1,27,-88);// 5PE3
  grR->SetPoint(2,75,-175); // 5PD11
  grR->SetPoint(3,90,-152); // 5PC10
  grR->Draw("P");
}

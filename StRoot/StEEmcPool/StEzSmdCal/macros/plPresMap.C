int A='A', B='B',C='C',D='D',E='E';
int a='A', b='B',c='C',d='D',e='E';
TFile *f;
TCanvas *cc;

plPresMap(TString hFile="m1"){
  f=new TFile("smdCal-X"+hFile+".hist.root");
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

plPres(  int sec=5,  int sub=E,  int eta=3 ) {
  int reb0=4;
  char name[100];
  sprintf(name,"tower-%02d%c%02d",sec,sub,eta);
   
  cc=new TCanvas(name,name,450,460);
  cc->Divide(2,2);

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

//====================
pl2D(int sec=1, float zMax=10.) {
  char name[100];
  sprintf(name,"mip%02d",sec);
  cc=new TCanvas(name,name,1000,700);
  cc->Range(0,0,1,1);
  cc->cd();
  TPad *c1 = new TPad("pad0", "apd0",0.0,0.,.35,1.);
  c1->Draw();
  c1->Divide(1,2);

  //sector:      1   2    3    4     5    6    7     8      9   10   11   12
  float x1[]={ -20, 40,  60,  40,  -20, -80, -170, -220, -240, -220,-160, -80};
  float y1[]={ 40, -20, -80, -160,-220,-250, -220, -160,  -80,   0,  40,  60};
  int is=sec-1;

  int i;
  for(i=0;i<2;i++) {
    sprintf(name,"xy%02d",sec);
    if(i==0) sprintf(name,"xy%02dm",sec);

    TH2F* h=(TH2F *)f->Get(name); assert(h);
    c1->cd(1+i);
    h->Draw("colz");
    if(i==0)  h->SetMaximum(zMax);
    h->SetAxisRange(x1[is],x1[is]+180,"X");
    h->SetAxisRange(y1[is],y1[is]+180,"Y");
  }

  cc->cd();
  c1 = new TPad("pad0", "apd0",0.35,0.,1.,1.);
  c1->Draw(); 
  c1->cd();
  c1->Divide(1,2);


  //------------  Do Integrals  -----------
  int binL=5,binH=50;   int ped=20; char cT='T';//towers
  // int binL=5,binH=150;   int ped=50; char cT='R';//pre/post
  
  sprintf(name,"y%02d",sec);
  TH1F * hy=new TH1F(name,"MIP  yield per tower; tower ID=sub+eta*5,  1=A12,2=B12,3=C12,...6=A11,..,11=A10, 21=A8, 31=A6, 41=A4, 51=A2",60,.5,60.5);
  sprintf(name,"m%02d",sec); 
  TH1F * hm=new TH1F(name,"MIP yiled / UxV  yield ; tower ID=sub+eta*5, ....",60,.5,60.5); hm->SetMarkerStyle(20); 
  hm->SetMarkerColor(kRed);

  int sub,eta=5;

  sprintf(name,"can%02d",sec);
  // f2=new TFile("smdCal-Xk6.hist.root");
  TH1F * huxv=(TH1F *)f->Get(name); assert(huxv);

  int ix=0;
  for(eta=12;eta>=1;eta--)
  for(sub='A';sub<='E';sub++) {
    ix++;
    sprintf(name,"e%02d%c%c%02d",sec,cT,sub,eta);
    TH1F *h1=(TH1F *)f->Get(name); assert(h1);
    //h1->Draw();
    float xx=h1->Integral(binL+ped,binH+ped);
    
    // get normalization
    int iPhi=(sec-1)*5+sub-'A';
    int iSpir=iPhi+(eta-1)*60;
    float nUxV=huxv->GetBinContent(iSpir+1);
    printf("x=%2d  %s Y=%.0f  iSpir=%d  N=%.1f\n",ix,name,xx,iSpir,nUxV);

    if(xx==0) xx=0.2;
    hy->Fill(ix,xx);

    hm->SetBinContent(ix,xx/nUxV);
    hm->SetBinError(ix,sqrt(xx)/nUxV);
    //break; 
  }
  c1->cd(1);
  hy->Draw("e");
  gPad->SetGrid();
  // gPad->SetLogy();
  hy->SetMinimum(-0.9);
  hy->SetMarkerStyle(23);
  hy->SetStats(0);
  c1->cd(2);
  // huxv->Draw();
  hm->Draw();
  hm->SetStats(0);

  sprintf(name,"mip%02d.gif",sec); cc->Print(name);
}

//=============================================
pl2Dall(float zMax=10.) {
  int sec=8;
  c=new TCanvas("xy","xy",600,600);
  for(sec=1;sec<=12;sec++) {
    TString opt="colz";
    //  if(sec%2==0) opt="box";
    if(sec>1) opt+=" same";
    
    char txt[100];
    sprintf(txt,"xy%02dm",sec);
    printf("=%s=%s\n",txt,opt.Data());
    TH2F *h=f->Get(txt);
    if(sec==1) { h->SetStats(0);h->SetTitle("MIP position, UxV & Tw & pre/post");}
    assert(h);
    h->Draw(opt.Data());
    h->SetMaximum(zMax);
    float phi=(75-30*(sec-1))/180.*3.1416;
    float y=50*sin(phi);
    float x=50*cos(phi);
    ln=new TLine(x,y,5*x,5*y);
    ln->Draw();
    continue;
    sprintf(txt,"%02d",sec);
    tx=new TText(5*x,5*y,txt);
    tx->Draw(); tx->SetTextSize(0.03);
    //  break;
  }
  gPad->SetGrid();
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

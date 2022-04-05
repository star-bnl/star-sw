/* varouis utility plots for SMD callibration
   
*/

TFile *f;
TCanvas *cc;
TString  inPath;

//=========================
plSmdCal(TString  hFile="R7089008"){
  inPath="/star/u/wissink/cal2006/iter4-pp/";
  // inPath="/star/data05/scratch/balewski/2005-eemcCal/day171-hist/iter5-pp/";
  f=new TFile(inPath+hFile+".hist.root");
  
  gStyle->SetPalette(1,0);
  return;
}


//====================
plOne(int sec=1, float zMax=10., int sepSect=1) {
  if(sepSect) {// read each sector from different input file
    TString fname="sect01/sum-sect"; fname+=sec;
    f=new TFile(inPath+fname+".hist.root");
  }
  
  printf("\n MIP yield for sector=%d --------------------\n",sec);
  char name[100], title[500];
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
  sprintf(title,"MIP  yield per tower, sector=%d; tower ID=sub+eta*5,  1=A12,2=B12,3=C12,...6=A11,..,11=A10, 21=A8, 31=A6, 41=A4, 51=A2",sec);
  TH1F * hy=new TH1F(name,title,60,.5,60.5);
  sprintf(name,"m%02d",sec); 
  TH1F * hm=new TH1F(name,"MIP yiled / UxV  yield ; tower ID=sub+eta*5, ....(as above)",60,.5,60.5); hm->SetMarkerStyle(20); 
  hm->SetMarkerColor(kRed);
  hm->SetMinimum(-0.001);
  // hm->SetMaximum(0.03);
  int sub,eta=5;

  sprintf(name,"ca%02d",sec);
  // f2=new TFile("smdCal-Xk6.hist.root");
  TH1F * huxv=(TH1F *)f->Get(name); assert(huxv);

  int ix=0;
  for(eta=12;eta>=1;eta--)
  for(sub='A';sub<='E';sub++) {
    ix++;
    sprintf(name,"e%02d%c%c%02d",sec,cT,sub,eta);
    TH1F *h1=(TH1F *)f->Get(name); assert(h1);
    //h1->Draw();
    float nMip=h1->Integral(binL+ped,binH+ped);
    
    // get normalization
    int iPhi=(sec-1)*5+sub-'A';
    int iSpir=iPhi+(eta-1)*60;
    float nUxV=huxv->GetBinContent(iSpir+1);
    float rat=-1.;
    if(nUxV>0) rat=nMip/nUxV;

    printf("x=%2d  %s  iSpir=%3d  nUxV=%d  nMip=%d  -->R=%.3f\n",ix,name,iSpir,nUxV,nMip,rat);

    if(nMip==0) nMip=0.2;
    hy->Fill(ix,nMip);

    hm->SetBinContent(ix,rat);
    hm->SetBinError(ix,sqrt(nMip)/nUxV);
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

  sprintf(name,"mip%02d.ps",sec); cc->Print(name);
}

//=============================================
pl2Dall(float zMax=10., int bestMip=0, int sepSect=1) {

  char *tit="12secUxV";
  if(bestMip) tit="12secMIP";
  cc=new TCanvas(tit,tit,600,630);
  for(int sec=1;sec<=12;sec++) {
    char txt[100];
    if(sepSect) {//
      if(sec<10) {TString fname=inPath+"sect0"; fname+=sec; fname+="/sum-sect"; fname+=sec; fname+=".hist.root";}
      if(sec>9)  {TString fname=inPath+"sect"; fname+=sec; fname+="/sum-sect"; fname+=sec; fname+=".hist.root";}
      printf("read each sector from different input file=%s\n",fname.Data());
     f=new TFile(fname);
    }
    TString opt="colz";
    //  if(sec%2==0) opt="box";
    if(sec>1) opt+=" same";
    
    
    sprintf(txt,"xy%02d",sec);
    if(bestMip) sprintf(txt,"xy%02dm",sec);
    printf("=%s=%s\n",txt,opt.Data());
    TH2F *h=(TH2F *)f->Get(txt);
    if(sec==1) {
      h->SetStats(0);
      if(bestMip) 
	h->SetTitle("MIP position, UxV & Tw & pre/post");
      else
	h->SetTitle("MIP position, only UxV");
    }
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


//-------------
void do12(){
  int i;
  for(i=1;i<=12;i++) plOne(i,30);
}

/* notes

LSF CPU per event
- full histos  MPV=47 kTics, mean 54 kTics
- ~all histos OFF : MPV=44kTics, mean 51 kTics

*/
class TCanvas;
TCanvas *c=0;
TFile *fd=0;

// root.exe plJetsL2.C'("wrk/hist/run7076054.l2jet.hist.root")'

//======================================
//======================================
plJetsL2(char *hFile="xx.hist.root", char  *pathPS=0) {
//open hist file & empty canvas, no plots

  gStyle->SetPalette(1,0);
  gStyle->SetOptStat(10);

  TString rootHistFname=hFile;
  fd=new TFile(rootHistFname);
  assert(fd->IsOpen());
  c=new TCanvas("aa","aa",600,500);
  printf("To view plots exec:   plJetsL2(1) ,2,3,4,5,6,10,20,21,22\n");
  if(pathPS) plAll(pathPS);
}

//======================================
//======================================  
plAll(char  *pathPS) {
  int pgA[]={1,2,3,4,5,6,10,20,21,22};
  int i;
  int k=0;
  for(i=0;i<10;i++) {
    char txt[100];
    int page=pgA[i];
    sprintf(txt,"%s/L2jet_Page%02d.ps",pathPS,page);
    plJetsL2(page);
    c->Print(txt);
    k++;
  }
  printf("#L2jet_plALL_done_%d\n",k);// confirmation string for the outside script
}


//======================================
//======================================
plJetsL2(int page) {

  if(c==0) { 
    printf(" execute plJetsL2() first to open the Canvas, QUIT\n");
    return;
  }

  TString tit="L2Jet-page"; tit+=page;
  c->SetTitle(tit);
  c->Clear();

  switch (page) {

  case 1: { //...........................
    gStyle->SetOptStat(1110);
    char *names[]={"h10","h11","h12"};
    c->Divide(2,2);
    int i;
    for(i=0;i<3;i++) {
      c->cd(i+1);
      TH1F *h=(TH1F *)fd->Get(names[i]);
      h->Draw();
      if(i==0) { 
	gPad->SetLogy();
	h->SetFillColor(kBlue);
	h->SetXTitle("Input: 1=btow, 2=etaw      Accept: 4=oneJet,  5=diJet,  6=rnd" );	
	h->SetTitle("total events  0=anyInput  8=anyAccept");
      }
      if(i==2) { 
	int nbin=5; 
	h->Rebin(nbin); h->Scale(1./nbin);
      }
 }
    
    // frequency plot
    c->cd(4);
    const int mxh=3;
    int nbin=50;
    float mxT=50.;
    TH1F *hA[mxh];
    float yMax=0;

    char *names2[]={"h13","h14","h15"};
    for(i=0;i<3;i++) {
      TH1F *h=(TH1F *)fd->Get(names2[i]);
      h->Rebin(nbin); h->Scale(1./nbin);
      if(i==0) {
	h->Draw();  //h->SetAxisRange(0,mxT); }
	h->SetTitle("accpet rate    blue:monoJet,  red:diJet,  black:random");
      } else h->Draw("same");
      if(i==0) {h->SetLineColor(kBlue);}
      if(i==1) {h->SetLineColor(kRed); h->SetLineStyle(2);}
      if(i==2) {h->SetLineColor(kBlack); h->SetLineStyle(2);}
      if(yMax<h->GetMaximum()) yMax=h->GetMaximum();
      h->SetLineWidth(2.);
      hA[i]=h;
    }
    for(i=0;i<mxh;i++) hA[i]->SetMaximum(yMax*1.1);
    if(yMax>0.5) gPad->SetLogy();
  }  break;


  case 2: { //   BTOW vs. softID
    gStyle->SetOptStat(0);
    TH1F *h0=(TH1F *)fd->Get("h21"); 
    float yMax=h0->GetMaximum();
    h0->SetMaximum(yMax*1.1);

    h0->SetFillColor(kRed);
    h0->SetLineColor(kRed);
    int nd=6;
    int nx=4800/nd;
    c->Divide(1,nd);
    int i;
    for(i=0;i<nd;i++) { 
      TH1F *ha=(TH1F *)h0->Clone();
      ha->SetAxisRange(nx*i, nx*(i+1));
      c->cd(i+1);
      ha->Draw();

      // print extra label
      TString aaa="soft=";
      aaa+=nx*i+1;  aaa+="+";
      tx=new TText(nx*i-100,1.,aaa);
      tx->SetTextSize(0.15); 
      tx->Draw();	
      
    }
  }    break;
  

  case 3: { //   ETOW vs. crate/cannel, common ymax
  gStyle->SetOptStat(0);
    TH1F *h0=(TH1F *)fd->Get("h31"); 
    h0->SetFillColor(kBlue);
    float yMax=h0->GetMaximum();
    h0->SetMaximum(yMax*1.1);
    int nd=6;
    int nx=h0->GetNbinsX()/nd;
    c->Divide(1,nd);
    int i;
    for(i=0;i<nd;i++) { 
      TH1F *ha=(TH1F *)h0->Clone();
      ha->SetAxisRange(nx*i, nx*(i+1));
      c->cd(i+1);
      ha->Draw();
 
      // print extra label
      TString aaa="crate=";
      aaa+=i+1; 
      tx=new TText(nx*i-15,1.,aaa);
      tx->SetTextSize(0.2);  
      tx->Draw();	      
   }
  }    break;
  

  case 4: { //   BTOW+ETOW  2D eta-phi

   gStyle->SetOptStat(10);
    TH1F *hb=(TH1F *)fd->Get("h22"); 
    TH1F *he=(TH1F *)fd->Get("h32"); 
    he->SetTitle("ETOW");
    c->Divide(0);
    c->Range(0,0,1,1);
    float xDiv=0.7;

    pdL = new TPad("padL", "padL",0.0,0.,xDiv,1.);
    pdL->Draw();

    pdR = new TPad("padR", "padR",xDiv,0.,1.,1.);
    pdR->Draw();

    float zMx=hb->GetMaximum();
    if(zMx<he->GetMaximum()) zMx=he->GetMaximum();
    hb->SetMaximum(zMx*1.2);
    he->SetMaximum(zMx*1.2);

    pdL->cd();
    hb->Draw("colz");
    TLine* ln=new TLine(20.,-10.,20., 130);
    ln->SetLineColor(kMagenta);
    ln->Draw();
    tx=new TText(5,121,"BTOW-East"); tx->Draw();tx->SetTextColor(kMagenta);
    tx=new TText(25,121,"BTOW-West"); tx->Draw();tx->SetTextColor(kMagenta);
    
    pdR->cd();
    he->Draw("colz");
    tx=new TText(4,61,"Endcap"); tx->Draw();tx->SetTextColor(kMagenta);
    tx->SetTextSize(0.1);
  } ;break;

 case 5: { //...........................
   gStyle->SetOptStat(10);
   char *names[]={"h40","h41","h43","h42"};
    c->Divide(2,2);
    int i;
    for(i=0;i<4;i++) {
      c->cd(i+1);
      TH1F *h=(TH1F *)fd->Get(names[i]);
      h->Draw("colz");
      if(i%2) 	draw_eta_phi_text();	
    }
  }    break;


 case 6: { //...........................
   gStyle->SetOptStat(1110);
    char *names[]={"h44","h45","h47","h46","h48"};
    c->Divide(2,3);
    int i;
    for(i=0;i<5;i++) {
      c->cd(i+1);
      TH1F *h=(TH1F *)fd->Get(names[i]);
      h->Draw(); 
      h->SetLineWidth(2.);
      if(i!=2 && i!=4) gPad->SetLogy();
    }
  }    break;


  case 10: {   //  one-jet accepted
    gStyle->SetOptStat(10);
    char *names[]={"h50","h51","h53","h52"};
    c->Divide(2,2);
    int i;
    for(i=0;i<4;i++) {
      c->cd(i+1);
      TH1F *h=(TH1F *)fd->Get(names[i]);
      if(i==1)  h->Draw("colz");
      else h->Draw();
      if(i==0) gPad->SetLogy();
      if(i==1) 	draw_eta_phi_text();	
      if(i>1) h->SetMinimum(0);
    }
  }    break;
  
 case 20: { //...........................
   gStyle->SetOptStat(10);
   char *names[]={"h60","h61","h63","h62"};
   c->Divide(2,2);
   int i;
   for(i=0;i<4;i++) {
     c->cd(i+1);
     TH1F *h=(TH1F *)fd->Get(names[i]);
     h->Draw("colz");
     if(i%2) 	draw_eta_phi_text();
   }
 }    break;
 
  case 21: {   //  di-jet accepted
    gStyle->SetOptStat(1110);
    char *names1[]={"h64","h66","h68"};
    char *names2[]={"h65","h67","h69"};
    c->Divide(2,2);
    int i;
    for(i=0;i<3;i++) {
      c->cd(i+1);
      TH1F *h1=(TH1F *)fd->Get(names1[i]);
      h1->Draw();  h1->SetLineColor(kRed);
      h1->SetLineWidth(2.);
      TH1F *h2=(TH1F *)fd->Get(names2[i]);
      h2->SetLineWidth(2.); h2->Draw("same"); 
      h2->SetLineColor(kBlue);  h2->SetLineStyle(2);
   
      if(i==0) 	gPad->SetLogy();

      
      if(i<3) {	// make it nicer
	h1->SetMinimum(0.9);
	if(i==0)h1->SetTitle("diJet Et   (accepted)");
	if(i==1)h1->SetTitle("diJet eta  (accepted)");
	if(i==2)h1->SetTitle("diJet phi  (accepted)");
	lg=new TLegend(0.4,0.42,.7,0.55);
	lg->AddEntry(h1,"High Et jet");
	lg->AddEntry(h2,"Low Et jet");
	lg->Draw();
      }

      else   h1->SetMinimum(0);

      float yMax=h1->GetMaximum();
      if(yMax<h2->GetMaximum()){
	yMax=h2->GetMaximum();
	h1->SetMaximum(yMax*1.1);
      }
      if(i==2) {
	t=new TText(2.8,yMax/5,"RED Jet1 ET > GREEN Jet2 ET ");
	t->Draw();
      }
      
    }
    TH1F *h=(TH1F *)fd->Get("h70");
    c->cd(4);
    h->Draw();
 
    t=new TLatex(3,h->GetMaximum()*.3,"#Delta #zeta=phi(J1,J2) * sign( K_{T} x S_{Y} )");
    t->Draw();
    
  }    break;

  case 22: { //...........................
    gStyle->SetOptStat(110);
   char *names[]={"h71","h72","h73","h74"};
    c->Divide(2,2);
    int i;
    for(i=0;i<4;i++) {
      c->cd(i+1);
      TH1F *h=(TH1F *)fd->Get(names[i]);
      if(i<=2) h->Draw("colz");
      if(i==3) {  
	h->Draw();
	gPad->SetLogy();
      }
      if(i==1) 	draw_eta12_text(); 
   }

  } break;
  
  
  default: ;
  }
  

}

//===============================
void draw_eta_phi_text() {
  ln=new TLine(5.,0.,5., 33);	ln->SetLineColor(kMagenta); ln->Draw();
  ln=new TLine(10.,0.,10., 33);	ln->SetLineColor(kMagenta); ln->Draw();
  tx=new TText(.8,29.8,"BTOW-East"); tx->Draw();tx->SetTextColor(kMagenta);
  tx=new TText(5.8,30.5,"BTOW-West"); tx->Draw();tx->SetTextColor(kMagenta);
  tx=new TText(10.5,30.5,"Endcap"); tx->Draw();tx->SetTextColor(kMagenta);
}


//===============================
void draw_eta12_text() {
  ln=new TLine(10.,0.,10., 16);	ln->SetLineColor(kMagenta); ln->Draw();
  ln=new TLine(-1.,10.,15., 10);	ln->SetLineColor(kMagenta); ln->Draw();

  tx=new TText(4.8,14.8,"BTOW"); tx->Draw();tx->SetTextColor(kMagenta);
  tx=new TText(10.5,15.1,"Endcap"); tx->Draw();tx->SetTextColor(kMagenta);

  tx=new TText(14.5,7,"BTOW"); tx->Draw();tx->SetTextColor(kMagenta);
  tx=new TText(14.5,12,"Endcap"); tx->Draw();tx->SetTextColor(kMagenta);
}


//====================================
//======================================
//======================================
//pl$carl() {

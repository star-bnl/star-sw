class EEBlueLedBox;
EEBlueLedBox *led;
TFile *f;
TCanvas *c;

//=============================================
//=============================================

init() {
  gStyle->SetPalette(1,0);
  gSystem -> Load("../ezGames/EEBlueLedBox/libEEBlueLedBox.so");
  led= new EEBlueLedBox();
  // led->print();
  f=new TFile("smdCal-Xm1.hist.root");
}

//=============================================
plBlue(int iuv=0,int sect=1) {
  gStyle->SetOptStat(11);
  init();
  //plBlue1(iuv, sect);
  // plPre();

}
//=============================================
plSmd() {
  int iuv,sec=8;
  for(sec=1;sec<=12;sec++)
  for(iuv=0;iuv<2;iuv++){
    plBlue1(iuv,sec);
    c->Print();
  }
}

//=============================================
plPre(float zMax=15.) {
  int sec=8;
  c=new TCanvas("xy","xy",590,600);
  for(sec=1;sec<=12;sec++) {
    TString opt="col z";
    //  if(sec%2==0) opt="box";
    if(sec>1) opt="col same";
    
    char txt[100];
    sprintf(txt,"xy%02d",sec);
    printf("=%s=%s\n",txt,opt.Data());
    TH2F *h=(TH2F *)f->Get(txt);
    assert(h);
    h->Draw(opt.Data());   
    h->SetMaximum(zMax); 
    if(sec==1) { h->SetStats(0); h->SetTitle("MIP position, UxV only");}
  }
  gPad->SetGrid();
}

//=============================================
plBlue1(int iuv=0,int sect=1) {
  char txt[100];
  sprintf(txt,"fr%02d%c",sect,iuv+'U');
  printf("=%s=\n",txt);
  TH1F *h0=f->Get(txt);
  assert(h0);
  c=new TCanvas(txt,txt,600,700);
  c->Divide(1,4);
  
  h0->SetFillColor(kGreen);
  int i;
  for(i=1;i<=4;i++) {
    c->cd(i);
    h=(TH1F*) h0->Clone();
    // h->SetMaximum(ym);
    h->Draw("b");
    h0->Draw("same e");
    h->SetAxisRange((i-1)*80,i*80);
    float ym=h->GetMaximum();
    gPad->SetLogy();
    //  fr05U->Draw();
    plotLed(iuv,ym*.8);
  }
  TString Txt=txt;
  //  c->Print(Txt+".gif");

}

//=============================================
//=============================================

plotLed(int iuv=0,float ym) {
  int colA[]={kBlack,kRed,kBlue,kMagenta};

  Int_t board = 0;
  
  Int_t iplane =iuv;
  int totStrip=0;

  for ( Int_t board = 0; board < 8; board++ ) {
    
    TObjArray fibers = led->getFibersToSmdPlane( board, iplane );
    std::cout << "Strips illuminated by LED board " << board+1 << std::endl;
    // Loop over all fibers and draw a box where we "expect"
    // a signal from the LED's.
    for ( Int_t ifiber = 0; ifiber < fibers.GetEntries(); ifiber++ ) {
      
      EEBlueLedFiber *fiber = (EEBlueLedFiber *)fibers[ifiber];
      std::cout << "min strip = " << fiber->getMinStrip() << " "
		<< "max strip = " << fiber->getMaxStrip() << " "
		<< "led board = " << fiber->getLedBoard() << " "
		<< "led fiber = " << fiber->getLedFiber() << " "
		<<std::endl;
      
      totStrip+=1+fiber->getMaxStrip()-fiber->getMinStrip();
      float y1=0.1,y2=log(ym)/2.;
      float x1=fiber->getMinStrip()-0.4;
      float x2=fiber->getMaxStrip()+0.4;
      
      TBox *box = new TBox(x1,y1,x2,y2);
      int col=colA[board%4];
      box -> SetLineColor(col);
      box -> SetFillStyle(0); // 75% transparent
      //      box -> SetFillColor(2);
      
      box -> Draw();
      char txt[100];
      sprintf(txt,"br=%d fb=%d", fiber->getLedBoard(),fiber->getLedFiber() );
      tt=new TText(x1+1.,y2*1.01,txt);
      //    tt->Draw();
      
    }// end of one beoard
  }// endo fo loop over boards
  cout <<" totStrip="<<totStrip<<endl;
}

TFile *fd=0;
TCanvas *cc;
TPad *pd1=0;
class   EemcTwMask;
EemcTwMask *mm; 
const Char_t *eemcTwMaskFile = 0;

//==========================
//==========================
void plPanitkin(TString fname="./wrk/hist/run7067154.1.hist.root", int flag=0) {
  gROOT -> LoadMacro("StRoot/StEEmcPool/muEztPanitkin/EEqaPresenter.h");
  gROOT -> LoadMacro("StRoot/StEEmcPool/muEztPanitkin/EEqaPresenter.cxx");
 

  gStyle->SetPalette(1,0);

  eemcTwMaskFile = gEnv->GetValue("OnLine.eemcMask","eemcTwMask.dat");

  printf("input Histo=%s=\n",fname.Data());  
  fd=new TFile(fname);
  if(!fd->IsOpen()) {
    printf(" faild to open file=%s=, exit\n",fname.Data()); 
    return;  
  }
  assert(fd->IsOpen());

  // j1(); return;

  cc=new TCanvas("pani","pani",500,510);
  // cc=new TCanvas("pani","pani",700,710);
  cc->Range(0,0,1,1);
  TPad *pad0 = new TPad("pad0", "apd0",0.0,0.95,1.,1.);
  pad0->Draw();
  pad0->cd();

 TPaveText *pt = new TPaveText(0,0.,1,1);
  pt->Draw();
  TDatime dt;
  TString txt2="P-plot,  ";
  txt2+=fname+"  ";
  txt2+=dt.AsString();
  pt->AddText(txt2);
  txt2="--";
  pt->AddText(txt2);

  cc->cd();
  pd1 = new TPad("pad1", "apd1",0.0,0.0,1,.95);
  pd1->Draw();

  printf(" Ready. Type: plTw(1) ,...  plSmd(1) ,... plDSM(1) ,...  plAllps()  flag=%d\n",flag);

  if(flag==1) plTw(1);

  //plSummary();
  //  plTw(1);
  //  pd1->Print("tw.ps");
  // pd1->Print("tw.gif");
}


//==========================
//==========================
void plTw(int panel=1) {
  pd1->Clear();
  eePlot(10,panel,fd,pd1,eemcTwMaskFile);
}

//==========================
//==========================
void plSmd(int panel=1) {
  pd1->Clear();
  eePlot(11,panel,fd,pd1,eemcTwMaskFile);
}

//==========================
//==========================
void plDSM(int panel=1) {
  pd1->Clear();
  eePlot(12,panel,fd,pd1,eemcTwMaskFile);
}



//=========================
//  back door functions for testing

void j1() {
  ESMDCorrBytes->Draw();
}

void plAll() {
 plTw(1); pr("0jpQA");   cc->Print("jpQA.gif");
  plTw(3); pr("1tw-3");cc->Print("etaPhi.gif");

  plTw(4); pr("1tw-4");
  plTw(5); pr("1tw-5");
  plDSM(1); pr("2dsm-1"); 
  plDSM(2); pr("2dsm-2");
  plDSM(8); pr("2dsm-3");

  return;
  plTw(1); pr("corrT");
  plTw(3); pr("tw");
  plSmd(1); pr("corrS");
  plSmd(2) ; pr("smd12-1");
  plSmd(3) ; pr("smd2-3");
  plSmd(4) ; pr("smd4-5");
  plSmd(5) ; pr("smd6-7");
  plSmd(6) ; pr("smd8-9");
  plSmd(7) ; pr("smd10-11");
}


void pr(TString core) {
  cc->Print(core+".ps");
  //
}


void plAllDsm(char  *path) {
  int i;
  for(i=1;i<=11;i++) {
     plDSM(i); 
     TString nn=path;
     nn+="fig-dsm";
     char bb='a'+i;
     nn+=bb;
     cc->Print(nn+".ps");
  }

}

void plAllps() {
  // int nPan[3]={5,12,11};
  int nPan[3]={5,1,11};
  //  int nPan[3]={2,2,2};
  int i,k;
  for(k=0;k<3;k++) {
    int tab=10+k;
    for(i=1;i<=nPan[k];i++) {
      eePlot(tab,i,fd,pd1,eemcTwMaskFile);
      char tit[100];
      sprintf(tit,"out/pani-%d-%02d.ps",tab,i);
      cc->Print(tit);
    }
  }
  printf("\ncat out/*.ps |ps2pdf -  all.pdf\n\n");
}

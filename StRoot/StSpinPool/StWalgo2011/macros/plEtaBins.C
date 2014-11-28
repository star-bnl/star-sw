enum {mxF=4}; // number of files to compare
enum {M=1}; // number of eta bins to comare
TFile *fdA[mxF];
int etaBin[M]={1};
/*

 cat out/comp_page01*ps |ps2pdf - ~/0x/comp_pg1.pdf
*/

void plEtaBins( int page=1) {
  TString simuPath="8.28.12/";
  TString dataPath="8.29.12/";
  TString psPath="out/";

  fdA[0]=new TFile(dataPath+"run12long.wana.hist.root");   
  fdA[1]=new TFile(simuPath+"jba310.wana.hist.root");   
  fdA[2]=new TFile(simuPath+"jbb330.wana.hist.root");   
  fdA[3]=new TFile(simuPath+"jba311.wana.hist.root");   
  
  TString dataNameA[mxF]={": STAR data 2012", "Pythia W+", "filter Pythia QCD" , "Pythia W-"};

  float lumScale[mxF]={72.,192/0.65,27*2.2,198/0.84}; 
  float absLT=lumScale[0];
  for(int k=0;k<mxF;k++) {
    assert(fdA[k]->IsOpen());
    lumScale[k]/=absLT;
  }  

  TString namePerPageB[]={"dumm","muBclET24R","muBdist4","muBjetETR","muWET","musPtBalance_clust", "musPtBalance_awayTot","muWcar3","muWeta","muChRecPNp","muWdedx"};
  TString namePerPageE[]={"dumm","muEclET24R","muEdist4","muETEMCjetETR","muE_WET","muEsPtBalance_clust", "muEsPtBalance_awayTot","muE_Wcar3","muE_Weta","muEchRecPNp","muE_Wdedx"};
  TString padTit;
  TString hName;
  gStyle->SetOptStat(1001100);
  gStyle->SetOptFit(1);

  for(int etaBin=1; etaBin<=8;etaBin++) {
    if(etaBin==6) continue;
    //if(etaBin!=7) continue;
     hName=namePerPageB[page];
      if(etaBin==7) hName=namePerPageE[page]; // switch to full endcap
      padTit=Form("#eta-bin=%d,  absLT=%.1f/pb  : ",etaBin, absLT)+hName;
      can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,padTit,page); 
      c->Divide(2,2);
      TH1F *h0=0;
      float yMax=0;
  
   switch (page) {
    case 1:
    case 2:
    case 3:
    case 4:
    case 7:
    case 8:
      { //............................
      for(int k=0;k<mxF;k++){
	TH1F *h=(TH1F *)fdA[k]->Get(Form("Eta%d/",etaBin)+hName); assert(h);
	if(page==4 ||page==7 ) h->Rebin();
	if(page==8) h->Rebin(4);
	if(k==0) { yMax=1.1*h->GetMaximum(); h->SetLineWidth(2.);}
	if(k==1 && page!=7) { h->SetFillColor(kRed); h->SetLineColor(kYellow); h->Draw("same"); h0->Draw("same");}
	if(k==3) h->SetFillColor(kBlue);
	h0=h;
	c->cd(k+1);
	h->Draw();
	TString tit=""; 
	if(k==0) tit+=h->GetTitle();
	h->SetTitle(tit+dataNameA[k]);
	h->Scale(1./lumScale[k]);
	if(page>=4) {	h->SetMaximum(yMax); }
 	if(page==4) h->SetAxisRange(0,70);
 	if(page==8) h->SetAxisRange(-1.3,1.8);
	if(page==7){ h->Fit("gaus"); ln=new TLine(0,0,0,1e4); ln->Draw();}
	//	printf("%d %f\n",k,lumScale[k]);
      }
      
    } break;
      
    case 5:
    case 6:
    case 9:
    case 10:
      { //............................
      for(int k=0;k<mxF;k++){
	TH2F *h2=(TH2F *)fdA[k]->Get(Form("Eta%d/",etaBin)+hName); assert(h);
	c->cd(k+1);
	h2->Draw("colz");
	TString tit=""; 
	if(k==0) tit+=h2->GetTitle();
	h2->SetTitle(tit+dataNameA[k]);
	h2->Scale(1./lumScale[k]);
 	if(page==9 ||page==10 ) {   h2->Rebin2D(2,2); h2->SetAxisRange(0,70);}
	// 	if(page==10 ) {  h2->SetAxisRange(0,70);}
      }
      
    } break;
      
    default:
      printf("page=%d NOT defined\n",page);
      return;
    } // end of switch 


    TString tit=psPath+Form("comp_page%02d_etaBin%d",page,etaBin);
    can->SetTitle(tit);
    can->SetName(tit);
    can->Print(tit+".ps");
 

  }// end of eta bins

  
  
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
  txt2+=", page=";
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


const int mxTheo=4, mxPN=2;
TString theoNameA[mxTheo]={"dns_k", "dns_kkp","dssv08","dssv08/mrst02"};
TString pnS[mxPN]={"+","-"};
TString pnL[mxPN]={"p","n"};
TH1F *hTheoA[mxPN][mxTheo];
int  theoCol[mxTheo]={kBlue,kRed, 8,kMagenta};
TGraphErrors *grP[mxPN]; // STAR data point(s)
TString  iPath="asyWALfinalAPS/";

int justTheo=0;  // defalut=0: show theory& data, 2 theory w/ sacel error

void plWAL_APS() {
  gStyle->SetOptStat(0); 
 
 
  c=new TCanvas("aa","aa",500,700);
  c->SetFillColor(kWhite);  c->SetFrameBorderMode(0);
  gPad->SetLeftMargin(0.10);  gPad->SetRightMargin(0.05);

  h0=new TH1F("bb", "; lepton #eta                                 ",10,-2,2);
  h0->SetMaximum(0.59);  h0->SetMinimum(-0.59);h0->Draw();
  h0->GetXaxis()->SetNdivisions(5); h0->GetXaxis()->SetTickLength(0.02);
  if(justTheo){
  ln=new TLine(-1,-0.59,-1,0.3); ln->Draw(); ln->SetLineStyle(3);
  ln=new TLine(1,-0.59,1,0.4); ln->Draw(); ln->SetLineStyle(3);
  }
  uploadData();


  uploadTheoryRhicbos();
  uploadTheoryDeFlor(); 
  plotTheory();
  if(!justTheo) plotSysErr();  
 if(!justTheo)  plotData();
  plotMiscText();

}


//====================================================
//====================================================
//====================================================

void   plotMiscText() {

  // STAR description
  float x0=0.14, y0=0.86;
  tlx=new TLatex(x0,y0,"STAR  Preliminary  Run9   #sqrt{s}=500 GeV");  tlx->SetNDC(); tlx->SetTextSize(0.048);
  if(!justTheo) tlx->Draw();

  tlx=new TLatex(x0+0.05,y0-0.05,"#vec{p}+p #rightarrow W^{#\pm}  #rightarrow e^{#\pm} + #nu"); tlx->Draw(); tlx->SetNDC(); tlx->SetTextSize(0.05);

  tlx=new TLatex(x0+0.05,y0-0.1,"E_{T}^{e} >25 GeV"); tlx->Draw(); tlx->SetNDC(); tlx->SetTextSize(0.045);
   
  // Ws labels
  tlx=new TLatex(0.4,0.68,"W^{ -}"); tlx->Draw(); tlx->SetNDC(); tlx->SetTextSize(0.06);
  tlx=new TLatex(0.4,0.22,"W^{+}"); tlx->Draw(); tlx->SetNDC(); tlx->SetTextSize(0.06);

  // Yaxis labels
  tlx=new TLatex(0.025,.85,"A_{L}"); tlx->Draw(); tlx->SetNDC(); tlx->SetTextSize(0.06);

  // Yaxis labels
  tlx=new TLatex(0.73,.53,"A_{L}= #frac{#sigma_{+} - #sigma_{-}}{#sigma_{+} + #sigma_{-}}"); tlx->Draw(); tlx->SetNDC(); tlx->SetTextSize(0.04);

  

  //........ 
  if(justTheo==2) {
    TLatex *lat3 = new TLatex(0.25,0.87,"Theory scaled by #pm 4.7%,   #sqrt{s}=500 GeV");
    lat3->SetNDC();     lat3->Draw("same");    lat3->SetTextSize(0.035);    lat3->SetTextColor(12);
  }

  return; // no printing date
  TDatime dt;
  tx=new TText(-2,-0.72,dt.AsString()); tx->SetTextSize(0.035);
  if(!justTheo) tx->Draw(); 
}

//---------------------------------------------------
//---------------------------------------------------
//---------------------------------------------------


void plotTheory() {

  float x0=0.68, y0=0.46;
  tlx=new TLatex(x0,y0,"W-     W+  RHICBOS"); tlx->Draw(); tlx->SetNDC(); tlx->SetTextSize(0.03);
  TLegend *thLg[mxPN*2];
  float x,y,dy=0.117;
  x=x0+0.06; y=y0-0.08;
  thLg[0]=new TLegend(x,y,x+0.27,y+0.07); 
  thLg[2]=new TLegend(x,y0-dy,x+0.27,y0-dy); //deFlor W+
  x=x-0.07;  thLg[1]=new TLegend(x,y,x+0.27,y+0.07); 
  thLg[3]=new TLegend(x,y0-dy,x+0.27,y0-dy); //deFlor W-

  for( int iPN=0; iPN<mxPN*2;iPN++) {
    TLegend *lg=thLg[iPN];
    lg->SetFillColor(kWhite);lg->SetLineColor(kWhite);  lg->SetTextSize(0.03);
  }

  char *theoL[mxTheo]={"DNS-K","DNS-KKP","DSSV08","DSSV08"};
  for( int iPN=0; iPN<mxPN;iPN++) {
    for (int ith=0;ith<mxTheo; ith++){
      TH1F *hTheo=hTheoA[iPN][ith];
      if(justTheo==2) plotOneBandTheory(hTheo,ith);// add scaling of theory 
      hTheo->Draw("same");  
      int ko=0; if(ith==3) ko=2;
      if(iPN==0) 
	thLg[iPN+ko]->AddEntry(hTheo,theoL[ith],"l");
      else 
	thLg[iPN+ko]->AddEntry(hTheo," ","l");
      // break;
    }
    gPad->SetGrid(0,0);
   }
  thLg[1]->Draw(); 
  thLg[0]->Draw(); 
  thLg[3]->Draw(); 
  thLg[2]->Draw(); 
  tlx=new TLatex(x0,y0-dy+0.015," deFlorian & Vogelsang"); tlx->Draw(); tlx->SetNDC(); tlx->SetTextSize(0.025);

  // fix the right edge
  ln=new TLine(2,-0.4,2,0); ln->Draw();
  // add box for legende
  tb=new TBox(0.62,-0.26,1.96,-0.01); tb->Draw();
  tb->SetFillStyle(0); tb->SetLineColor(12);

}

//---------------------------------------------------
//---------------------------------------------------
//---------------------------------------------------
void uploadData() {
// read data
  
  TString inpCore="run9setP1234";
  TString fullInpName=iPath;  fullInpName+=inpCore;
  fullInpName+=".wasy.hist.root";
  fdd=new TFile(fullInpName);
  if(! fdd->IsOpen()) {
    printf("EROR: input histo file not found, quit\n",fullInpName.Data());
    return;
  } else {
    printf("Opened: %s\n",fullInpName.Data());
  }

 
  for(int iq=0;iq<mxPN;iq++) {
    TGraphErrors *gr=new TGraphErrors;
    gr->SetMarkerStyle(21-iq);
    gr->SetMarkerSize(1.5);
    gr->SetMarkerColor(kBlack);
    gr->SetLineWidth(3);
    gr->SetLineColor(kBlack);

    char *tt1="hAsyP";
    if(iq==1) tt1="hAsyN";
    TH1F *hAsy=(TH1F *)fdd->Get(tt1); assert( hAsy);

    double asy=hAsy->GetBinContent(3);
    double asyEr=hAsy->GetBinError(3);
    printf("W %s AL=%.3f +/- %.3f nSig=%.1f\n", pnS[iq].Data(),asy,asyEr,asy/asyEr);
    gr->Clear();
    gr->SetPoint(0,0,asy);
    gr->SetPointError(0,1.,asyEr);
    
    grP[iq]=gr;

  }
}

//---------------------------------------------------
//---------------------------------------------------
//---------------------------------------------------
void plotData(){

  for(int iq=0;iq<mxPN;iq++) {
    grP[iq]->Draw("p");

  } 



}
//---------------------------------------------------
//---------------------------------------------------
//---------------------------------------------------
void plotSysErr() {

  
  float dx=0.08;
  for(int iq=0;iq<mxPN;iq++) {
    float x=grP[iq]->GetX()[0];
    float y=grP[iq]->GetY()[0];
    
    
    float facYup=0.89, facYdw=1.13; // W+ , hardcoded
    if(iq==1) facYup=1.21, facYdw=0.83;  // W- , hardcoded
    
    bx=new TBox(x-dx,y*facYdw,x+dx,y*facYup);
    bx->Draw(); bx->SetFillColor(kBlack);
    bx->SetFillStyle(3001);
  } 
  
  float x0=-1.8,y0=-0.57;
  bx=new TBox(x0,y0,x0+2*dx,y0+0.08);
  bx->Draw(); bx->SetFillColor(kBlack);
  bx->SetFillStyle(3001);
  tlx=new TLatex(x0+0.2,y0+.045,"Syst. uncertainty due to abs. "); tlx->Draw();   tlx->SetTextSize(0.03);
  tlx=new TLatex(x0+0.2,y0+.01,"polarization and background"); tlx->Draw();   tlx->SetTextSize(0.03);

}

//---------------------------------------------------
//---------------------------------------------------
//---------------------------------------------------



//======================
void   uploadTheoryRhicbos(){
  for( int iPN=0; iPN<mxPN;iPN++) { 
  TString nameUn="rb400_w"+pnL[iPN]+"_unp_ct5m_pt25.root";
  funp=new TFile( iPath+nameUn); assert(funp->IsOpen());
  hunp= (TH1F*)funp->Get("h3"); assert(hunp);

 
  for (int ith=0;ith<mxTheo-1; ith++){
    TString namePol="rb400_w"+pnL[iPN]+"_pol_"+theoNameA[ith]+"_pt25.root";
  
    fpol=new TFile( iPath+namePol); assert(fpol->IsOpen());
    TH1F* hpol=(TH1F*) fpol->Get("h3"); assert(hpol);
      
    hal=(TH1F*) hpol->Clone(); 
    hal->Divide(hunp);
    TString hName="w"+pnL[iPN]+"_"+theoNameA[ith];
    TString hTitle="W"+pnS[iPN]+"  "+theoNameA[ith]+"; lepton #eta";

    hal->SetNameTitle(hName,hTitle);
    

    hTheoA[iPN][ith]=hal;
    hal->SetLineColor( theoCol[ith]);
    if(iPN) hal->SetLineStyle(2);
    hal->SetLineWidth(2);

   
    continue;

    c=new TCanvas();
    c->Divide(2,2);
    c->cd(1);hunp->Draw();
    c->cd(2);hpol->Draw();
    
    c->cd(3);hal->Draw(); hal->SetLineColor(kRed);;
    hal->SetMaximum(0.6);  hal->SetMinimum(-0.6);  
    
  }
  }
  return;
}




//======================
void   uploadTheoryDeFlor(){
  for( int iPN=0; iPN<mxPN;iPN++) { 
  TString nameUn="deFlor_w"+pnL[iPN]+"_unp_mrst02_pt25_full.root";
  funp=new TFile( iPath+nameUn); assert(funp->IsOpen());
  hunp= (TH1F*)funp->Get("h3"); assert(hunp);

  char * theoName="dssv08";
  int ith=mxTheo-1;
  TString namePol="deFlor_w"+pnL[iPN]+"_pol_"+theoName+"_pt25_full.root";
  fpol=new TFile( iPath+namePol); assert(fpol->IsOpen());
  TH1F* hpol=(TH1F*) fpol->Get("h3"); assert(hpol);
    
  hal=(TH1F*) hpol->Clone(); 
  hal->Divide(hunp);
  TString hName="w"+pnL[iPN]+"_deFlor_dssv";
  TString hTitle="W"+pnS[iPN]+"  deFlor dssv; lepton #eta";
  
  hal->SetNameTitle(hName,hTitle);
  
  
  hTheoA[iPN][ith]=hal;
  hal->SetLineColor( theoCol[ith]);
  if(iPN) hal->SetLineStyle(2);
  hal->SetLineWidth(2);
  // hal->Draw(); break;
  }
}


//---------------------------------------------------
//---------------------------------------------------
//---------------------------------------------------


void plotOneBandTheory(TH1F*hIn , int it) {
  //if(iq)return;
  float sysErr=.047;
  for(int ib=0;ib<2;ib++){
    TH1F * hu=(TH1F*) hIn->Clone();
    //  hu->SetLineColor(0);	
    hu->Draw("same e3");
    TAxis *ax=hu->GetXaxis();
    for(ib=1;ib<=ax->GetNbins();ib++){
      float y= hu->GetBinContent(ib);
      hu->SetBinError(ib,y*sysErr);
    }
    hu->SetFillStyle(3003+it);
    
    hu->SetFillColor( hu->GetLineColor());
  }
}
/*
ln -s ../Prelim/ct5m/histo/rb400.w+unp_ct5m.ye.-1.1.pte25.rz rb400_wp_unp_ct5m_y1_pt25.rz
ln -s ../Prelim/dssv08/histo/rb400.w+pola_dssv08.ye.-1.1.pte25.rz  rb400_wp_pol_dssv08_y1_pt25.rz
*/

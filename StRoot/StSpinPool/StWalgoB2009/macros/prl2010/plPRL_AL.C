const int mxTheo=4, mxPN=2;
TString theoNameA[mxTheo]={"dns_k", "dns_kkp","dssv08","dssv08/mrst02"};
TString pnS[mxPN]={"+","-"};
TString pnL[mxPN]={"p","n"};
TH1F *hTheoA[mxPN][mxTheo];
int  theoCol[mxTheo]={kBlue,kRed, kBlack,kMagenta};
TGraphErrors *grP[mxPN]; // STAR data point(s)
TString  iPath="asyWALfinalPRL/";

int justTheo=0;  // defalut=0: show theory& data, 2 theory w/ scale error

void plPRL_AL(TString cCore="WAL_PRL_AL") {
  gStyle->SetOptStat(0); 
  gStyle->SetLineStyleString(2,"15 14");
  gStyle->SetLineStyleString(3,"4 9");
  gStyle->SetLineStyleString(4,"10 10 4 10");

  const Char_t *title="Longitudinal single-spin asymmetry A_L for W boson production at midrapidity";
  c=new TCanvas(cCore,title,500,550); 
    setPadPaperSize(245.0); // column width, approx. 3.4 in = 245 pt
    setPadMarginWidthPoint(gPad, 27, 1, 2, (2.0*12.0) + 2, gPad);
    setPadLineWidthPoint(gPad, 0.5);


  h0=new TH1F("bb", ";Lepton #eta",10,-2,2);
  h0->SetMaximum(0.59);
  h0->SetMinimum(-0.59);
  h0->GetXaxis()->SetNdivisions(5);
  h0->GetXaxis()->CenterTitle(true);
        stripCmdHist(h0);
        setHistFontSize(h0, 10.0);
        setLineWidthPoint(h0, 0.5);
        setMarkerSizePoint(h0, 3.5);
        setHistTickLength(h0, 4.0, 4.0);
        setHistTitleLabelOffset(h0, 2.0*12.0 - 2.0, 28, 12.0 - 2.0, 4.5 - 2.0);
  h0->Draw();
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
  float x0=0.14, y0=0.93;
  tlx=new TLatex(x0,y0,"#font[82]{STAR}  2009  " + SYMBOL_sqrts + "_{_{ }}#font[122]{=}_{ }_{_{ }}500 " + SYMBOL_GeV);
  tlx->SetNDC();
    stripCmdLatex(tlx);
    setTextFontSize(tlx, 10.0);
  if(!justTheo) tlx->Draw();

  tlx=new TLatex(x0+0.05,y0-0.06,SYMBOL_parrowplusp + "_{ }#rightarrow_{ }#font[12]{W}^{#pm}^{ }#rightarrow_{ }#font[12]{e}^{^{ }#pm}^{ }#font[122]{+}_{ }#nu");
  tlx->Draw();
  tlx->SetNDC();
    stripCmdLatex(tlx);
    setTextFontSize(tlx, 10.0);

  tlx=new TLatex(x0+0.05,y0-0.11,SYMBOL_ET + "^{^{ }#font[12]{e}} > 25 " + SYMBOL_GeV);
  tlx->Draw();
  tlx->SetNDC();
    stripCmdLatex(tlx);
    setTextFontSize(tlx, 10.0);
   
  // Ws labels
  tlx=new TLatex(0.4,0.75,"#font[12]{W}^{#font[122]{-}}");
  tlx->Draw();
  tlx->SetNDC();
    stripCmdLatex(tlx);
    setTextFontSize(tlx, 10.0);
  tlx=new TLatex(0.4,0.22,"#font[12]{W}^{#font[122]{+}}");
  tlx->Draw();
  tlx->SetNDC();
    stripCmdLatex(tlx);
    setTextFontSize(tlx, 10.0);

  // Yaxis labels
  tlx=new TLatex(0.025,.92,"#font[12]{A_{L}}");
  tlx->Draw();
  tlx->SetNDC();
    stripCmdLatex(tlx);
    setTextFontSize(tlx, 10.0);

  // Yaxis labels
  tlx=new TLatex(0.73,.59,"#font[12]{A_{L}}_{ }#font[122]{=} #frac{ " + SYMBOL_sigmaplus + "_{ }#font[122]{-}_{ }" + SYMBOL_sigmaminus + "}{ #shifty[0.1]{" + SYMBOL_sigmaplus + "_{ }#font[122]{+}_{ }" + SYMBOL_sigmaminus + "}}");
  tlx->Draw();
  tlx->SetNDC();
    stripCmdLatex(tlx);
    setLineWidthPoint(tlx, 0.5);
    setTextFontSize(tlx, 10.0);
  
    
  //........ 
  if(justTheo==2) {
    TLatex *lat3 = new TLatex(0.25,0.87,"Theory scaled by #pm 4.7%,   #sqrt{s}=500 GeV");
    lat3->SetNDC();
    lat3->Draw("same");
    lat3->SetTextColor(12);
    stripCmdLatex(lat3);
    setTextFontSize(lat3, 10.0);
  }

  return; // no printing date
  TDatime dt;
  tx=new TLatex(-2,-0.72,dt.AsString());
    stripCmdLatex(tx);
    setTextFontSize(tx, 10.0);
  if(!justTheo) tx->Draw(); 
}

//---------------------------------------------------
//---------------------------------------------------
//---------------------------------------------------


void plotTheory() {

  float x0=0.655, y0=0.508;
  tlx=new TLatex(x0,y0," #font[12]{W}^{#font[122]{-}}   _{_{ }}#font[12]{W}^{#font[122]{+}}  _{_{ }}#font[82]{RHICBOS:}");
  tlx->Draw();
  tlx->SetNDC();
    stripCmdLatex(tlx);
    setTextFontSize(tlx, 8.0);
  TLegend *thLg[mxPN*2];
  float x,y,dy=0.14;
  x=x0+0.06; y=y0-0.095;
  thLg[0]=new TLegend(x,y,x+0.27,y+0.09); 
  thLg[2]=new TLegend(x,y0-dy,x+0.27,y0-dy); //deFlor W+
  x=x-0.07;  thLg[1]=new TLegend(x,y,x+0.27,y+0.09);
  thLg[3]=new TLegend(x,y0-dy,x+0.27,y0-dy); //deFlor W-

  for( int iPN=0; iPN<mxPN*2;iPN++) {
    TLegend *lg=thLg[iPN];
    lg->SetFillColor(kWhite);
    lg->SetLineColor(kWhite);
    lg->SetShadowColor(kWhite);
  }

  char *theoL[mxTheo]={"  #font[82]{DNS}#shiftx[0.2]{-}K","  #shifty[0.13]{#font[82]{DNS}#shiftx[0.2]{-}#font[82]{KKP}}","  #font[82]{DSSV}08","  #font[82]{DSSV}08"};
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
            stripCmdLegend(thLg[iPN+ko]);
            setTextFontSize(thLg[iPN+ko], 8.0);
      // break;
    }
    gPad->SetGrid(0,0);
   }
  thLg[1]->Draw(); 
  thLg[0]->Draw(); 
  thLg[3]->Draw(); 
  thLg[2]->Draw(); 
  tlx=new TLatex(x0+.142,y0-dy+0.019,"#font[82]{CHE:}");
  tlx->Draw();
  tlx->SetNDC();
    stripCmdLatex(tlx);
    setTextFontSize(tlx, 8.0);

  // fix the right edge
  ln=new TLine(2,-0.4,2,0); ln->Draw();
  // add box for legende
  // tb=new TBox(0.39,-0.265,1.96,-0.01); tb->Draw();
  //tb->SetFillStyle(0); tb->SetLineColor(12);

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
    setMarkerSizePoint(gr, 3.5);
    gr->SetMarkerColor(kBlack);
    setLineWidthPoint(gr, 1);
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
    
    
    float facYup=0.88, facYdw=1.12; // W+ , hardcoded
    if(iq==1) facYup=1.16, facYdw=0.84;  // W- , hardcoded
    
    bx=new TBox(x-dx,y*facYdw,x+dx,y*facYup);
    bx->Draw(); bx->SetFillColor(16);
    //bx->SetFillStyle(3001);
  } 
  
  float x0=-1.8,y0=-0.57;
  bx=new TBox(x0,y0,x0+2*dx,y0+0.08);
  bx->Draw(); bx->SetFillColor(16);
  //bx->SetFillStyle(3001);
  tlx=new TLatex(x0+0.2,y0+.05,"Syst. uncertainty due to abs. ");
  tlx->Draw();
    stripCmdLatex(tlx);
    setTextFontSize(tlx, 8.0);
  tlx=new TLatex(x0+0.2,y0+.01,"polarization and background");
  tlx->Draw();
    stripCmdLatex(tlx);
    setTextFontSize(tlx, 8.0);

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
    if(iPN) hal->SetLineStyle(2+ith);
    setLineWidthPoint(hal, 0.5);

   
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
    setLineWidthPoint(hal, 0.5);
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


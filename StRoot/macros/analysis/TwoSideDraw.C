//////////////////////////////////////////////////////////////////////////
//                                                             
// Macro for plotting hits and pixels in combination with bfc.C
//            plotting both sides of tpc seperately
//
// $Id: TwoSideDraw.C,v 1.3 1999/09/23 21:52:18 snelling Exp $
//
// $Log: TwoSideDraw.C,v $
// Revision 1.3  1999/09/23 21:52:18  snelling
// added gSystem->Load("StAnalysisUtilities") because that's not in bfc anymore
//
// Revision 1.2  1999/08/10 19:01:25  snelling
// changed for new bfc
//
// Revision 1.1.1.1  1999/08/10 18:48:22  snelling
// macro to draw pixels and hits
//
//////////////////////////////////////////////////////////////////////////

int DrawPixels(Text_t* varexp, Text_t* selection, Text_t* options) {
  /* 
   * -----------------------------------------------------------------
   *	draw TPC pixels
   * -----------------------------------------------------------------*/

  St_DataSetIter Itpc_raw(chain->GetDataSet("tpc_raw"));
  St_tfc_adcxyz *phtfc = 0;
  tfc_adcxyz_st *ptadcxyz = 0;
  phtfc = (St_tfc_adcxyz *) Itpc_raw.Find("adcxyz");

  if (phtfc) {ptadcxyz = phtfc->GetTable();}
  else { cout << "Warning: adcxyz table header does not exist " << endl; return kStWarn; }
  if (!ptadcxyz) { cout << "Warning: adcxyz table does not exist " << endl; return kStWarn; }

  St_TableNtuple *adcxyz = new St_TableNtuple(*phtfc);
  adcxyz.Fill(*phtfc);
  // define plot options
  adcxyz.SetMarkerStyle(26);
  adcxyz.SetMarkerColor(4);
  adcxyz.Draw(varexp,selection,options);
  return kStOK;
}

//-----------------------------------------------------------------------

int DrawHits(Text_t* varexp, Text_t* selection, Text_t* options) {
  /* 
   * -----------------------------------------------------------------
   *	draw TPC hits
   * -----------------------------------------------------------------*/

  // get pointers to tpc hit table
  St_DataSetIter Itpc_hits(chain->GetDataSet("tpc_hits"));
  St_tcl_tphit *phtcl = 0;
  tcl_tphit_st *pttphit = 0;
  phtcl = (St_tcl_tphit *) Itpc_hits.Find("tphit");
  if (phtcl) {pttphit = phtcl->GetTable();}
  else { cout << "Error: tphit table header does not exist " << endl; return kStWarn; }
  if (!pttphit) { cout << "Error: tphit table does not exist " << endl; return kStWarn; }

  St_TableNtuple *tphit = new St_TableNtuple(*phtcl);
  tphit.Fill(*phtcl);
  // define plot options
  tphit.SetMarkerStyle(20);
  tphit.SetMarkerColor(2);
  tphit.Draw(varexp,selection,options);
  return kStOK;
}

//-----------------------------------------------------------------------

int DrawGeantHits(Text_t* varexp, Text_t* selection, Text_t* options) {
  /* 
   * -----------------------------------------------------------------
   *	draw TPC geant hits
   * -----------------------------------------------------------------*/

  St_DataSetIter Igeant_g2t(chain->GetDataSet("g2t_tpc_hit"));
  St_g2t_tpc_hit *phg2t = 0;
  g2t_tpc_hit_st *ptg2t_tpc_hit = 0;
  phg2t = (St_g2t_tpc_hit *) Igeant_g2t.Find("g2t_tpc_hit");
  if (phg2t) {ptg2t_tpc_hit = phg2t->GetTable();}
  else { cout << "Warning: g2t tpc hit table header does not exist " << endl; return kStWarn; }
  if (!ptg2t_tpc_hit) { cout << "Warning: g2t tpc hit table does not exist " << endl; return kStWarn; }

  St_TableNtuple *g2t_tpc_hit = new St_TableNtuple(*phg2t);
  g2t_tpc_hit.Fill(*phg2t);
  // define plot options
  g2t_tpc_hit.SetMarkerStyle(24);
  g2t_tpc_hit.SetMarkerColor(5);
  g2t_tpc_hit.Draw(varexp,selection,options);
  return kStOK;
}

//-----------------------------------------------------------------------

void TwoSideDraw() {
  /* 
   * -----------------------------------------------------------------
   *	draw TPC sectors with some options 
   * -----------------------------------------------------------------*/
  gSystem->Load("StAnalysisUtilities");


  static int DrawPixels(Text_t*, Text_t*, Text_t*);
  static int DrawHits(Text_t*, Text_t*, Text_t*);
  static int DrawGeantHits(Text_t*, Text_t*, Text_t*);

  /*
  if (chain->GetOption(kMINIDAQ)) {
    chain->SetInput("BEGIN_RUN",".make/xdfin/.const/BEGIN_RUN");
    chain->SetInput("ChainFlags[kTPC]_DATA",".make/xdfin/.data/ChainFlags[kTPC]_DATA");
  }
  */

  gStyle->SetCanvasColor(10);              // white
  gStyle->SetPadColor(10);                 // white
  gStyle->SetFrameFillColor(10);           // white
  gStyle->SetOptFit(1);

  TCanvas *canvas1 = new TCanvas("canvas1","pixels, hits, geant data",100,100,700,700);
  canvas1->SetFillColor(0);
  canvas1->SetBorderMode(0);
  canvas1->SetBorderSize(0);

  TPaveText *title = new TPaveText(.2,0.96,.8,.995);
  title->SetFillColor(33);
  title->AddText("pixels and hits");
  title->Draw();

  TPad *pad1 = new TPad("pad1","x vs y",0.01,0.48,0.49,0.95);
  TPad *pad2 = new TPad("pad2","x vs z",0.51,0.48,0.98,0.95);
  TPad *pad3 = new TPad("pad3","tpc view 1",0.01,0.01,0.49,0.48);
  TPad *pad4 = new TPad("pad4","tpc view 2",0.51,0.01,0.98,0.48);

  pad1->SetFillColor(0);
  pad1->SetBorderMode(0);
  pad1->SetBorderSize(0);
  pad1->Draw();

  pad2->SetFillColor(0);
  pad2->SetBorderMode(0);
  pad2->SetBorderSize(0);
  pad2->Draw();

  pad3->SetFillColor(0);
  pad3->SetBorderMode(0);
  pad3->SetBorderSize(0);
  pad3->Draw();

  pad4->SetFillColor(0);
  pad4->SetBorderMode(0);
  pad4->SetBorderSize(0);
  pad4->Draw();

  int ierror = 1;

  pad1->cd();
  if (chain->GetOption(kTSS) || chain->GetOption(kTRS) || chain->GetOption(kMINIDAQ) ||
      chain->GetOption(kTDAQ)) {
    ierror = DrawPixels("y:x","(adc>2 && z>0)*adc","box");
  }
  if (ierror == 0) { 
    DrawHits("y:x","z>0","same,scat"); 
  }
  else { 
    DrawHits("y:x","z>0","scat"); 
  }
  if (!chain->GetOption(kMINIDAQ) && !chain->GetOption(kTDAQ)) {
      DrawGeantHits("x1:x0","x2>0","same,scat");
  }  

  pad2->cd();
  if (chain->GetOption(kTSS) || chain->GetOption(kTRS) || chain->GetOption(kMINIDAQ) ||
      chain->GetOption(kTDAQ)) {
    ierror = DrawPixels("z:x","(adc>2 && z>0)*adc","box");
  }
  if (ierror == 0) { 
    DrawHits("z:x","z>0","same,scat"); 
  }
  else { 
    DrawHits("z:x","z>0","scat"); 
  }
  if (!chain->GetOption(kMINIDAQ) && !chain->GetOption(kTDAQ)) {
    DrawGeantHits("x2:x0","x2>0","same,scat");
  }

  pad3->cd();
  if (chain->GetOption(kTSS) || chain->GetOption(kTRS) || chain->GetOption(kMINIDAQ) ||
      chain->GetOption(kTDAQ)) {
    ierror = DrawPixels("y:x","(adc>2 && z<0)*adc","box");
  }
  if (ierror == 0) { 
    DrawHits("y:x","z<0","same,scat"); 
  }
  else { 
    DrawHits("y:x","z<0","scat"); 
  }
  if (!chain->GetOption(kMINIDAQ) && !chain->GetOption(kTDAQ)) {
    DrawGeantHits("x1:x0","x2<0","same,scat");
  }  

  pad4->cd();
  if (chain->GetOption(kTSS) || chain->GetOption(kTRS) || chain->GetOption(kMINIDAQ) ||
      chain->GetOption(kTDAQ)) {
    ierror = DrawPixels("z:x","(adc>2 && z<0)*adc","box");
  }
  if (ierror == 0) { 
    DrawHits("z:x","z<0","same,scat"); 
  }
  else { DrawHits("z:x","z<0","scat"); }
  if (!chain->GetOption(kMINIDAQ) && !chain->GetOption(kTDAQ)) {
    DrawGeantHits("x2:x0","x2<0","same,scat");
  }
}

//////////////////////////////////////////////////////////////////////////
// $Id: TwoSideDraw.C,v 1.11 2000/01/20 02:19:41 snelling Exp $
//
// $Log: TwoSideDraw.C,v $
// Revision 1.11  2000/01/20 02:19:41  snelling
// removed tss dependencies
//
// Revision 1.10  2000/01/10 22:06:09  kathy
// add owner name and comments
//
// Revision 1.9  1999/12/11 20:13:53  snelling
// Fixed logic between DAQ data and similators
//
// Revision 1.8  1999/12/06 21:49:58  snelling
// Fixed bug (exchanged x and z)
//
// Revision 1.7  1999/11/23 19:49:54  snelling
// Changed from table draw to loop to fill histograms
// (Valeri suggestion to speed things up)
//
// Revision 1.6  1999/11/20 23:45:51  snelling
// Used Table member function Draw instead of making TableNtuple
//
// Revision 1.5  1999/11/16 19:28:25  snelling
// Changed kOption to "Option" in chain->GetOption()
//
// Revision 1.4  1999/11/10 00:15:36  snelling
// removed drawing of pseudo-padrow hits for geant points
//
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
//
// owner: Raimond Snellings
//
// what it does: 
//     Macro for plotting hits and pixels in combination with bfc.C
//            plotting both sides of tpc separately
//                                   
//////////////////////////////////////////////////////////////////////////
St_tfc_adcxyz *GetPixels() {
  /* 
   * -----------------------------------------------------------------
   *	draw TPC pixels
   * -----------------------------------------------------------------*/
  
  St_tfc_adcxyz* phtfc = (St_tfc_adcxyz*) chain->GetDataSet("adcxyz");
  if (!phtfc) 
    cout << "Warning: adcxyz table header does not exist " << endl;
  else if (!phtfc->GetNRows()) 
    cout << "Warning: adcxyz table does not exist " << endl;
  return phtfc;
}

//-----------------------------------------------------------------------

St_tcl_tphit *GetHits() {
  /* 
   * -----------------------------------------------------------------
   *	draw TPC hits
   * -----------------------------------------------------------------*/
  
  // get pointers to tpc hit table
  St_tcl_tphit* phtcl = (St_tcl_tphit*) chain->GetDataSet("tphit");
  if (!phtcl) 
     cout << "Error: tphit table header does not exist " << endl;
  else if (!phtcl->GetNRows()) 
    cout << "Error: tphit table does not exist " << endl;
  return phtcl;
}
//-----------------------------------------------------------------------
St_g2t_tpc_hit *GetGeantHits() {
  /* 
   * -----------------------------------------------------------------
   *	Look for g2t_tpc table pointer 
   * -----------------------------------------------------------------*/
  
  St_g2t_tpc_hit* phg2t = (St_g2t_tpc_hit*) chain->GetDataSet("g2t_tpc_hit");
  if (!phg2t)  
    cout << "Warning: g2t tpc hit table header does not exist " << endl;
  else if (!phg2t->GetNRows())  
    cout << "Warning: g2t tpc hit table does not exist " << endl; 
  return phg2t; 
}

//-----------------------------------------------------------------------

void TwoSideDraw() {
  /* 
   * -----------------------------------------------------------------
   *	draw TPC sectors with some options 
   * -----------------------------------------------------------------*/
  
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
  
  TPad *pad1 = new TPad("pad1","x vs y positive z",0.01,0.48,0.49,0.95);
  TPad *pad2 = new TPad("pad2","x vs z positive z",0.51,0.48,0.98,0.95);
  TPad *pad3 = new TPad("pad3","x vs y negative z",0.01,0.01,0.49,0.48);
  TPad *pad4 = new TPad("pad4","x vs z negative z",0.51,0.01,0.98,0.48);

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
  
  // Book histograms 
  TH2F *hyx        = 0; TH2F *hzx        = 0;
  TH2F *hyx_g2t    = 0; TH2F *hzx_g2t    = 0;
  TH2F *hzx_pixels = 0; TH2F *hyx_pixels = 0;
  
  TH2F *nhyx        = 0; TH2F *nhzx        = 0;
  TH2F *nhyx_g2t    = 0; TH2F *nhzx_g2t    = 0;
  TH2F *nhzx_pixels = 0; TH2F *nhyx_pixels = 0;
  
  St_Table *hits = 0;   Int_t nHitsRows = 0;
  
  // ---------- Fill Hits -------------
  if (hits = GetHits() && nHitsRows = hits->GetNRows()) {
    
    hyx  = new TH2F("x vs y (hits pos z)","x vs y (z > 0)",50,-100,100,50,-100,100);
    hzx  = new TH2F("x vs z (hits pos z)","x vs z (z > 0)",50,-100,100,50,-100,100);
    hyx->SetMarkerStyle(20); hzx->SetMarkerStyle(20);
    hyx->SetMarkerColor(2);  hzx->SetMarkerColor(2);
    hyx->SetXTitle("y [cm]"); hyx->SetYTitle("x [cm]");
    hzx->SetXTitle("z [cm]"); hzx->SetYTitle("x [cm]");
    
    
    nhyx  = new TH2F("x vs y (hits neg z)","x vs y (z < 0)",50,-100,100,50,-100,100);
    nhzx  = new TH2F("x vs z (hits neg z)","x vs z (z < 0)",50,-100,100,50,-100,100);
    nhyx->SetMarkerStyle(20); nhzx->SetMarkerStyle(20);
    nhyx->SetMarkerColor(2);  nhzx->SetMarkerColor(2);
    nhyx->SetXTitle("y [cm]"); nhyx->SetYTitle("x [cm]");
    nhzx->SetXTitle("z [cm]"); nhzx->SetYTitle("x [cm]");
    
    tcl_tphit_st *pttphit = ((St_tcl_tphit *)hits)->GetTable();
    for (Int_t i = 0; i < nHitsRows; i++) {
      if (pttphit[i].z > 0) {
	hyx->Fill(pttphit[i].y, pttphit[i].x); 
	hzx->Fill(pttphit[i].z, pttphit[i].x);
      } 
      else {
	nhyx->Fill(pttphit[i].y, pttphit[i].x); 
	nhzx->Fill(pttphit[i].z, pttphit[i].x);
      }
    } // Fill hits
  }
  
  // --------- Fill Geant Hits ------------
  Bool_t noDAQ = !chain->GetOption("miniDAQ") && !chain->GetOption("TDAQ");
  if (noDAQ && hits = GetGeantHits() && nHitsRows = hits->GetNRows() ) {
    
    hyx_g2t    = new TH2F("x vs y (geant pos z)","x vs y (z > 0)",50,-100,100,50,-100,100);
    hzx_g2t    = new TH2F("x vs z (geant pos z)","x vs z (z > 0)",50,-100,100,50,-100,100);
    hyx_g2t->SetMarkerStyle(24); hzx_g2t->SetMarkerStyle(24);
    hyx_g2t->SetMarkerColor(3);  hzx_g2t->SetMarkerColor(3);
    
    nhyx_g2t    = new TH2F("x vs y (geant neg z)","x vs y (z < 0)",50,-100,100,50,-100,100);
    nhzx_g2t    = new TH2F("x vs z (geant neg z)","x vs z (z < 0)",50,-100,100,50,-100,100);
    nhyx_g2t->SetMarkerStyle(24); nhzx_g2t->SetMarkerStyle(24);
    nhyx_g2t->SetMarkerColor(3);  nhzx_g2t->SetMarkerColor(3);
    
    g2t_tpc_hit_st *ptg2t = ((St_g2t_tpc_hit *)hits)->GetTable();
    for (Int_t i = 0; i < nHitsRows; i++) {
      if (ptg2t[i].volume_id < 2446) {
	if (ptg2t[i].x[2] > 0) {
	  hyx_g2t->Fill(ptg2t[i].x[1], ptg2t[i].x[0]);
	  hzx_g2t->Fill(ptg2t[i].x[2], ptg2t[i].x[0]);
	}
	else {
	  nhyx_g2t->Fill(ptg2t[i].x[1], ptg2t[i].x[0]);
	  nhzx_g2t->Fill(ptg2t[i].x[2], ptg2t[i].x[0]);
	}
      }
    }
  } // Fill GEANT hits
  
  // -------- Fill pixels ----------
  Bool_t isSimulator = chain->GetOption("trs");
  
  if ((!noDAQ || isSimulator) && hits = GetPixels() && nHitsRows = hits->GetNRows() ) {
    
    hyx_pixels = new TH2F("x vs y (pixels pos z)","x vs y (z > 0)",50,-100,100,50,-100,100);
    hzx_pixels = new TH2F("x vs z (pixels pos z)","x vs z (z > 0)",50,-100,100,50,-100,100);
    hzx_pixels->SetMarkerStyle(26); hyx_pixels->SetMarkerStyle(26);
    hzx_pixels->SetMarkerColor(4);  hyx_pixels->SetMarkerColor(4);
    
    nhyx_pixels = new TH2F("x vs y (pixels neg z)","x vs y (z < 0)",50,-100,100,50,-100,100);
    nhzx_pixels = new TH2F("x vs z (pixels neg z)","x vs z (z < 0)",50,-100,100,50,-100,100);
    nhzx_pixels->SetMarkerStyle(26); nhyx_pixels->SetMarkerStyle(26);
    nhzx_pixels->SetMarkerColor(4);  nhyx_pixels->SetMarkerColor(4);
    
    tfc_adcxyz_st *ptpixel = ((St_tfc_adcxyz *)hits)->GetTable();
    for (Int_t i = 0; i < nHitsRows; i++) {
      if (ptpixel[i].adc>2) {
	if (ptpixel[i].z > 0) {
	  hzx_pixels->Fill(ptpixel[i].z, ptpixel[i].x, ptpixel[i].adc);
	  hyx_pixels->Fill(ptpixel[i].y, ptpixel[i].x, ptpixel[i].adc);
	}
	else {
	  nhzx_pixels->Fill(ptpixel[i].z, ptpixel[i].x, ptpixel[i].adc);
	  nhyx_pixels->Fill(ptpixel[i].y, ptpixel[i].x, ptpixel[i].adc);
	}
      }
    }
  } // Fill pixels
  
  // Draw histograms

  pad1->cd();
  
  if (hyx)        hyx->Draw();
  if (hzx_pixels) hyx_pixels->Draw("box,same");
  if (hyx_g2t)    hyx_g2t->Draw("same");

  pad2->cd();
  
  if (hzx)        hzx->Draw();
  if (hzx_pixels) hzx_pixels->Draw("box,same");
  if (hyx_g2t)    hzx_g2t->Draw("same");
  
  pad3->cd();
  if (nhyx)        nhyx->Draw();
  if (nhyx_pixels) nhyx_pixels->Draw("box,same");
  if (nhyx_g2t)    nhyx_g2t->Draw("same");
  
  pad4->cd();
  if (nhzx)        nhzx->Draw();
  if (nhzx_pixels) nhzx_pixels->Draw("box,same");
  if (nhzx_g2t)    nhzx_g2t->Draw("same");
  
  canvas1->Update();
}



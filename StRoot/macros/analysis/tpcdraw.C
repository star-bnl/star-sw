// $Id: tpcdraw.C,v 1.14 2000/03/08 22:52:49 snelling Exp $
// $Log: tpcdraw.C,v $
// Revision 1.14  2000/03/08 22:52:49  snelling
// Changed View settings (thanks Ian)
//
// Revision 1.13  2000/01/20 02:18:28  snelling
// fixed St_TableSorter under Linux still crash under SUN (CINT related)
//=======================================================================
// owner: Raimond Snellings
// what it does: Macro for plotting hits and pixels in combination with bfc.C 
//=======================================================================

//int draw_sector(TPad &padname, Float_t theta, Float_t phi) {
int InitDraw() {
  /* 
   * -----------------------------------------------------------------
   *	draw TPC sectors, two sections, inner and outer.
   * -----------------------------------------------------------------*/
  Float_t RminInner = 51.9/2;
  Float_t RmaxInner = 126.5/2;
  Float_t RminOuter = 127.5/2;
  Float_t RmaxOuter = 202.9/2;
  Float_t Zmin = -210.;
  Float_t Zmax = 210.;
  
  // Define the TPC sector outlines. 
  TPGON   *InnerSector = new TPGON("InnerSector","TPC inner sector","void",-15,360,12,2);
  InnerSector->DefineSection(0, Zmin, RminInner, RmaxInner);
  InnerSector->DefineSection(1, Zmax, RminInner, RmaxInner);
  InnerSector->SetLineColor(2);
  TNode *m_node1 = new TNode("m_node1","inner sector",InnerSector);
  
  TPGON   *OuterSector = new TPGON("OuterSector","TPC outer sector","void",-15,360,12,2);
  OuterSector->DefineSection(0,Zmin,RminOuter,RmaxOuter);
  OuterSector->DefineSection(1,Zmax,RminOuter,RmaxOuter);
  OuterSector->SetLineColor(4);
  TNode *m_node2 = new TNode("m_node2","outer sector",OuterSector);

  return kStOK;
}
//______________________________________________________________________

int DrawEvent(TPad *padname, Float_t theta, Float_t phi) {

  //   * Plot hits and tracks in the TPC */
  
  char tkstring[10];
  Float_t  zmin=-210;
  Float_t  zmax=210;
  Float_t rmin[3]={ -200., -200., -200.};
  Float_t rmax[3]={ 200., 200., 200.};

  TView *view = new TView(rmin,rmax);
  padname->SetView(view);
  view->RotateView(phi,theta);
  //  view->ShowAxis();
  view->Draw();

  m_node2->Draw("same");
  m_node1->Draw("same");

  // Create iterators for the two datasets
  St_DataSetIter tpc_data(chain->DataSet("tpc_hits"));
  St_tcl_tphit  *hits = NULL;
  tcl_tphit_st *hit1 = NULL; 
  hits = (St_tcl_tphit* ) tpc_data.Find("tphit");
  if (hits) {hit1 = hits->GetTable();}
  else { cout << "Error: tphit table header does not exist " << endl; return kStWarn; }
  if (!hit1) { cout << "Error: tphit table does not exist " << endl; return kStWarn; }
  Int_t nhits = hits->GetNRows();
  if (nhits == 0) {cout << "Error: tphit table contains zero rows " << endl; return kStWarn;}
  St_TableSorter sortrk(hits,"track",0,nhits-1);

  St_DataSetIter tpc_tracks(chain->DataSet("tpc_tracks"));
  St_tpt_track *track = 0;
  tpt_track_st *track1 = 0;
  track = (St_tpt_track *) tpc_tracks.Find("tptrack");
  if (track) {track1 = track->GetTable();}
  else { cout << "Error: tptrack table header does not exist " << endl; return kStWarn; }
  if (!track1) { cout << "Error: tptrack table does not exist " << endl; return kStWarn; }
  Int_t ntracks = track->GetNRows();
  if (ntracks == 0) {cout << "Error: tptrack table contains zero rows " << endl; return kStWarn;}


  // Draw hits
  Int_t k=0; 
  Float_t *pts = new Float_t[3*nhits];
  for (Int_t j = 0; j < nhits; j++) {
    if (hit1[j]->z > zmin && hit1[j]->z < zmax) {
      pts[3*k]   = hit1[j]->x; 
      pts[3*k+1] = hit1[j]->y; 
      pts[3*k+2] = hit1[j]->z;
      k++;
    }
  }
  cout << "total hits " << k << endl; 
  TPolyMarker3D *hit = new TPolyMarker3D(k,pts,2);
  hit->SetMarkerColor(1);
  hit->Draw("same");

  //Draw Hits on track with different colors
  Int_t ngoodtrks = 0;
  for (j = 0; j < ntracks; j++) {
    Int_t trkid = track1[j]->id;
    Int_t trkflag = track1[j]->flag;
    Int_t trknrec = track1[j]->nrec;
    cout <<" Track #" << j << " "<< trknrec <<" hits" << endl;
    Float_t *tkpts = new Float_t[3*trknrec];
    // define colors and markers for different tracks
    Int_t mark = j + 20;
    if (j>11) {mark = j + 8;}
    if (j>22) {mark = j - 3;}
    if (j>33) {mark = j - 14;}
    if (j>44) {mark = j - 25;}
    Int_t incolor = j + 2;
    //  colors 0 and 10 are white - invisible
    if (incolor == 10) {incolor = 2;}
    if (incolor > 50) {incolor -= 50;} 
    // loop over hits belonging to a track
    int k =0;
    for (Int_t i = 0; i < trknrec; i++) {
      Int_t hitid = 1000*trkid + i + 1; // because table start at 1
      Int_t irow_hit = sortrk[hitid];
      if (irow_hit < 0) continue;
      if (hit1[irow_hit]->z > zmin && hit1[irow_hit]->z < zmax) {
	tkpts[3*k]   = hit1[irow_hit]->x;
	tkpts[3*k+1] = hit1[irow_hit]->y;
	tkpts[3*k+2] = hit1[irow_hit]->z;
	k++;
      }
    }
    TPolyMarker3D *thit = new TPolyMarker3D(k,tkpts,mark);
    thit->SetMarkerColor(incolor);
    thit->Draw("same");


    // Draw good tracks as polylines.
    Float_t p[6];
    if (trkflag < 0) {continue;}
    Float_t ay = 1.0/tan(0.0174533*track1[j]->psi);
    Float_t x0 = track1[j]->r0*cos(0.0174533*track1[j]->phi0);
    Float_t y0 = track1[j]->r0*sin(0.0174533*track1[j]->phi0);
    if (track1[j]->z0 >zmin && track1[j]->z0 < zmax) {
      p[1] = rmin[1];
      p[4] = rmax[1];
      p[0] = x0 + ay*(p[1]-y0);
      if (p[0] < rmin[0]) {
	p[0] = rmin[0];
	p[1] = y0 + (p[0]-x0)/ay;
      }
      if (p[0] > rmax[0]) {
	p[0] = rmax[0];
	p[1] = y0 + (p[0]-x0)/ay;
      }
      p[2] = track1[j]->tanl*(y0-p[1]) + track1[j]->z0;
      p[3] = x0 + ay*(p[4]-y0);
      if (p[3] < rmin[0]) {   
	p[3] = rmin[0];
	p[4] = y0 + (p[3]-x0)/ay;
      }
      if (p[3] > rmax[0]) {
	p[3] = rmax[0];
	p[4] = y0 + (p[3]-x0)/ay;
      }
      p[5] = track1[j]->tanl*(y0-p[4]) + track1[j]->z0;
      TPolyLine3D *trk = new TPolyLine3D(2,p);
      trk->SetLineColor(incolor);
      trk->Draw("same");
      ngoodtrks++;
    }
  }


  padname->Modified();
  padname->Update();

  return kStOK;
}

//-----------------------------------------------------------------------

int DrawPixels(Text_t* varexp, Text_t* selection, Text_t* options) {
  /* 
   * -----------------------------------------------------------------
   *	draw TPC pixels
   * -----------------------------------------------------------------*/

  St_DataSetIter Itpc_raw(chain->DataSet("tpc_raw"));
  //  St_DataSetIter Itpc_raw(GetDataSet("tpc_raw"));
  St_tfc_adcxyz *phtfc = 0;
  tfc_adcxyz_st *ptadcxyz = 0;
  phtfc = (St_tfc_adcxyz *) Itpc_raw.Find("adcxyz");

  if (phtfc) {ptadcxyz = phtfc->GetTable();}
  else { cout << "Warning: adcxyz table header does not exist " << endl; return kStWarn; }
  if (!ptadcxyz) { cout << "Warning: adcxyz table does not exist " << endl; return kStWarn; }

  TH1* pPixelHist = phtfc->Draw(varexp,selection,options);
  pPixelHist->SetMarkerStyle(26);
  pPixelHist->SetMarkerColor(4);

  return kStOK;
}

//-----------------------------------------------------------------------

int DrawHits(Text_t* varexp, Text_t* selection, Text_t* options){
  /* 
   * -----------------------------------------------------------------
   *	draw TPC hits
   * -----------------------------------------------------------------*/

  // get pointers to tpc hit table
  St_DataSetIter Itpc_hits(chain->DataSet("tpc_hits"));
  St_tcl_tphit *phtcl = 0;
  tcl_tphit_st *pttphit = 0;
  phtcl = (St_tcl_tphit *) Itpc_hits.Find("tphit");
  if (phtcl) {pttphit = phtcl->GetTable();}
  else { cout << "Error: tphit table header does not exist " << endl; return kStWarn; }
  if (!pttphit) { cout << "Error: tphit table does not exist " << endl; return kStWarn; }

  TH1* pHitHist = phtcl->Draw(varexp,selection,options);
  pHitHist->SetMarkerStyle(20);
  pHitHist->SetMarkerColor(2);

  return kStOK;
}

//-----------------------------------------------------------------------

int DrawGeantHits(Text_t* varexp, Text_t* selection, Text_t* options) {
  /* 
   * -----------------------------------------------------------------
   *	draw TPC geant hits
   * -----------------------------------------------------------------*/

  St_DataSetIter Igeant_g2t(chain->DataSet("g2t_tpc_hit"));
  St_g2t_tpc_hit *phg2t = 0;
  g2t_tpc_hit_st *ptg2t_tpc_hit = 0;
  phg2t = (St_g2t_tpc_hit *) Igeant_g2t.Find("g2t_tpc_hit");
  if (phg2t) {ptg2t_tpc_hit = phg2t->GetTable();}
  else { cout << "Warning: g2t tpc hit table header does not exist " << endl; return kStWarn; }
  if (!ptg2t_tpc_hit) { cout << "Warning: g2t tpc hit table does not exist " << endl; return kStWarn; }

  TH1* pGeantHist = phg2t->Draw(varexp,selection,options);
  pGeantHist->SetMarkerStyle(24);
  pGeantHist->SetMarkerColor(3);

  return kStOK;
}

//-----------------------------------------------------------------------

void tpcdraw() {
  /* 
   * -----------------------------------------------------------------
   *	draw TPC sectors with some options 
   * -----------------------------------------------------------------*/

  /*
  if (chain->GetOption("miniDAQ")) {
    chain->SetInput("BEGIN_RUN",".make/xdfin/.const/BEGIN_RUN");
    chain->SetInput("ChainFlags[kTPC]_DATA",".make/xdfin/.data/ChainFlags[kTPC]_DATA");
  }
  */

  gStyle->SetCanvasColor(10);              // white
  gStyle->SetPadColor(10);                 // white
  gStyle->SetFrameFillColor(10);           // white
  gStyle->SetOptFit(1);

  TCanvas *c1 = new TCanvas("tpcDraw","x y z info",100,100,700,700);
  c1->SetFillColor(0);
  c1->SetBorderMode(0);
  c1->SetBorderSize(0);

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

  pad1->cd();
  if (chain->GetOption("trs") || 
      chain->GetOption("miniDAQ") || chain->GetOption("TDAQ")) {
    DrawPixels("x:y","(adc>2)*adc","box");
  }
  if (DrawPixels == 0) { DrawHits("y:x","","same,scat"); }
  else { DrawHits("x:y","","scat"); }
  if (!chain->GetOption("miniDAQ") && !chain->GetOption("TDAQ")) {
    DrawGeantHits("x[0]:x[1]","volume_id<2446","same,scat");
  }  

  pad2->cd();
  if (chain->GetOption("trs") || 
      chain->GetOption("miniDAQ") || chain->GetOption("TDAQ")) {
    DrawPixels("x:z","(adc>2)*adc","box");
  }
  if (DrawPixels == 0) { DrawHits("z:x","","same,scat"); }
  else { DrawHits("x:z","","scat"); }
  if (!chain->GetOption("miniDAQ") && !chain->GetOption("TDAQ")) {
    DrawGeantHits("x[0]:x[2]","volume_id<2446","same,scat");
  }
  
  InitDraw();
  
  pad3->cd();
  DrawEvent(pad3,0.,-90.);
  
  pad4->cd();
  DrawEvent(pad4,90.,-90.);
}


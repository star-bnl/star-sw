//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// Macro for plotting hits and pixels in combination with bfc.C         //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
void draw_sector(TPad &padname, Float_t theta, Float_t phi){
  /* 
   * -----------------------------------------------------------------
   *	draw TPC sectors, two sections, inner and outer.
   * -----------------------------------------------------------------*/
  Float_t r1=51.9;
  Float_t r2=126.5;
  Float_t r3=127.5;
  Float_t r4=202.9;
  
  // Define the TPC sector outlines. 
  TPGON   *insect = new TPGON("insect","TPC inner sector","void",-15,360,12,2);
  insect->DefineSection(0,-210,r1,r2);
  insect->DefineSection(1,210,r1,r2);
  insect->SetLineColor(2);
  
  m_node1 = new  TNode("m_node1","inner sector",insect);
  m_node1->BuildListOfNodes();
  m_node1->cd();
  
  TPGON   *outsect = new TPGON("outsect","TPC outer sector","void",-15,360,12,2);
  outsect->DefineSection(0,-210,r3,r4);
  outsect->DefineSection(1,210,r3,r4);
  outsect->SetLineColor(2);
  TNode *node2 = new  TNode("node2","outer sector",outsect);

  padname.SetTheta(theta);
  padname.SetPhi(phi);
  padname.Modified();
  padname.Update();
}
//______________________________________________________________________
int draw_event(TPad &padname, Int_t nslice){
  /*-----------------------------------------------------------------------
   * Plot hits and tracks in the TPC */
  
  char slstring[10], tkstring[10];
  int value; 

  // seven slices for the full laser event - These for the 97 test data.
  Float_t zsep[8] = {-210.0,15.0,45.0,75.0,105.0,140.0,170.0,210.0};
  // And these for the 1998 sector 18-19 test data.
  // Float_t zsep[8] = {-210.0,-165.0,-135.0,-105.0,-75.0,-45.0,-15.0,210.0};
  //  Int_t m_nslice=-7;
  Int_t m_nslice= nslice;
  Int_t first_slice = 0;
  Int_t last_slice =  1;
  if(m_nslice<0){
    zsep[1]= 210.0;
  }
  else{
    if(m_nslice==7)
      last_slice = 7;
    else{
      first_slice = m_nslice;
      last_slice =  m_nslice+1;
    }
  }
  cout << "from " << first_slice << " to " << last_slice << endl;
  
  Int_t i=0;
  Int_t incolor;
  
  // Plot the part of the TPC with tracks from the tpctest test
  m_node1->Draw();

  TView *view = padname.GetView();
  //  Float_t rmin[3]={-40, -180,-210};
  //  Float_t rmax[3]={ 40,  -60, 210};
  Float_t rmin[3]={ -180, -180,-210};
  Float_t rmax[3]={ 180,  180, 210};
  Float_t p[6];
  Float_t pts[7500],tkpts[75][150]; //allow for 75, 50 hit tracks
  Int_t ntkpt[75];                  // and 75 track hit counters 
  
  view->SetRange(rmin,rmax);
  view->SetLongitude(15.0);
  view->SetLatitude(90.0);


  // Create iterators for the two datasets
  St_DataSetIter tpc_data(chain->DataSet("tpc_hits"));
  St_tcl_tphit  *hits = 0;
  tcl_tphit_st *hit1 = 0; 
  hits = (St_tcl_tphit *) tpc_data.Find("tphit");
  if (hits) {hit1 = hits->GetTable();}
  else { cout << "Error: tphit table header does not exist " << endl; return kStWarn; }
  if (!hit1) { cout << "Error: tphit table does not exist " << endl; return kStWarn; }
  Int_t nhits = hits->GetNRows();
  if (nhits == 0) {cout << "error: tphit table contains zero rows " << endl; return kStWarn;}
  St_TableSorter sortrk(*hits,"track",0,nhits-1);

  St_DataSetIter tpc_tracks(chain->DataSet("tpc_tracks"));
  St_tpt_track *track = 0;
  tpt_track_st *track1 = 0;
  track = (St_tpt_track *) tpc_tracks.Find("tptrack");
  if (track) {track1 = track->GetTable();}
  else { cout << "Error: tptrack table header does not exist " << endl; return kStWarn; }
  if (!track1) { cout << "Error: tptrack table does not exist " << endl; return kStWarn; }
  Int_t ntracks = track->GetNRows();

  // Show the tpc event in z slices.
  for (int islice=first_slice;islice<last_slice;islice++) {
    Float_t  zmin=zsep[islice];
    Float_t  zmax=zsep[islice+1];
    cout << " slice "<< islice <<" z limits  "<< zmin <<" to "<< zmax << endl;
    //    tcl_tphit_st *h = hit1;
    for (int mm=0;mm<75;mm++) ntkpt[mm] = 0;
    Int_t k=0, savnum[75]; 
    Int_t oldie=0, itkno =0;

    for (i = 0; i < hits->GetNRows(); i++) {
      //      h = hit1[sortrk->GetIndex(i)];
      if (hit1[i]->z > zmin && hit1[i]->z < zmax) {
	pts[3*k]=hit1[i]->x; pts[3*k+1]=hit1[i]->y; pts[3*k+2]=hit1[i]->z;
	k++;
        if(hit1[i].track !=0){
          Int_t newtk = hit1[i]->track / 1000;
          if( newtk != oldie){
            oldie= newtk;
            itkno++;
            savnum[itkno]=oldie;
          }
          tkpts[itkno][3*ntkpt[itkno]]=hit1[i]->x;
          tkpts[itkno][3*ntkpt[itkno]+1]=hit1[i]->y;
          tkpts[itkno][3*ntkpt[itkno]+2]=hit1[i]->z;
          ntkpt[itkno]++;
        }
      }
    }

    cout << "total hits " << k << endl; 

    TPolyMarker3D *hit = new TPolyMarker3D(k,pts,2);
    hit->SetMarkerColor(1);
    hit->Draw();

    //Now plot the tracks hits in color ;
    TPolyMarker3D *thit[75];
    for(int m=0; m < 75; m++){
      if(ntkpt[m] !=0){
	//    
	cout <<" Track #" << savnum[m] << " "<< ntkpt[m] <<" hits" << endl;
	Int_t mark   = m+19;
	if(m>11)mark = m+8;
	if(m>22)mark = m-3;
	if(m>33)mark = m-14;
	if(m>44)mark = m-25;
	thit[m] = new TPolyMarker3D(ntkpt[m],tkpts[m],mark);
	incolor =m;
	//  colors 0 and 10 are white - invisible
	if(m==10)incolor=1;
	if(m> 50)incolor=m-50; 
	
	thit[m]->SetMarkerColor(incolor);
	thit[m]->Draw();
      }
    }
    // Now plot all the tracks as polylines.  
    TPolyLine3D *trk[75];
    Int_t tin=0;
    Int_t tid[75];
    tpt_track_st *t = track1;

    for( i=0;i<ntracks;i++){
      //    cout << "track flag " << i <<", " << t->nfit <<", " << t->flag <<endl;
      //skip any fake tracks (negative flag?)
      if(t->flag < 0){ t++;continue;}
      Float_t ay = 1.0/tan(0.0174533*t->psi);
      Float_t x0 = t->r0*cos(0.0174533*t->phi0);
      Float_t y0 = t->r0*sin(0.0174533*t->phi0);
      if(t->z0 >zmin && t->z0 < zmax){
	p[1] = rmin[1];
	p[4] = rmax[1];
	p[0] = x0 + ay*(p[1]-y0);
	if(p[0] < rmin[0]){
	  p[0] = rmin[0];
	  p[1] = y0 + (p[0]-x0)/ay;
	}
	if(p[0] > rmax[0]){
	  p[0] = rmax[0];
	  p[1] = y0 + (p[0]-x0)/ay;
	}
	p[2] = t->tanl*(y0-p[1]) + t->z0;
	p[3] = x0 + ay*(p[4]-y0);
	if(p[3] < rmin[0]){   
	  p[3] = rmin[0];
	  p[4] = y0 + (p[3]-x0)/ay;
	}
	if(p[3] > rmax[0]){
	  p[3] = rmax[0];
	  p[4] = y0 + (p[3]-x0)/ay;
	}
	p[5] = t->tanl*(y0-p[4]) + t->z0;
	trk[tin] = new TPolyLine3D(2,p);
	tid[tin]=t->id;
	tin++;
      }
      t++;
    }
    for( i=0;i<tin;i++){
      incolor=tid[i];
      //  colors 0 and 10 are white - invisible
      if(tid[i]==10)incolor=1;
      if(tid[i]> 50)incolor=tid[i]-50; 
      
      trk[i]->SetLineColor(incolor);
      trk[i]->Draw();
    }
    // put the slice number and number of tracks in a separate pad
    TPad *event = new TPad("event","event",0.75,0.84,0.95,0.95);
    event->Draw();
    event->cd();
    event->Range(0,0,1,1);
    event->SetFillColor(19);
    event->SetBorderSize(0);
    event->SetBorderMode(0);

    if(m_nslice>=0){
      sprintf(slstring,"Slice %d",islice);
      TText *stext = new TText(0.125,0.40,slstring);
      stext->SetTextSize(0.5);
      stext->Draw();
    }

    //label counts only the good tracks.
    sprintf(tkstring," %d Tracks",tin);
    TText *ttext = new TText(0.125,0.11,tkstring);
    ttext->SetTextSize(0.5);
    ttext->Draw();
    event->Modified();
    padname.cd();
    padname.Modified();
    padname.Update();
  }
}

//-----------------------------------------------------------------------

int draw_pixels(Text_t* varexp, Text_t* selection, Text_t* options){
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

  St_TableNtuple *adcxyz = new St_TableNtuple(*phtfc);
  adcxyz.Fill(*phtfc);
  // define plot options
  adcxyz.SetMarkerStyle(26);
  adcxyz.SetMarkerColor(4);
  adcxyz.Draw(varexp,selection,options);

  return 0;
}

//-----------------------------------------------------------------------

int draw_hits(Text_t* varexp, Text_t* selection, Text_t* options){
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

  St_TableNtuple *tphit = new St_TableNtuple(*phtcl);
  tphit.Fill(*phtcl);
  // define plot options
  tphit.SetMarkerStyle(20);
  tphit.SetMarkerColor(2);
  tphit.Draw(varexp,selection,options);

  return 0;
}

//-----------------------------------------------------------------------

int draw_geant_hits(Text_t* varexp, Text_t* selection, Text_t* options){
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

  St_TableNtuple *g2t_tpc_hit = new St_TableNtuple(*phg2t);
  g2t_tpc_hit.Fill(*phg2t);
  // define plot options
  g2t_tpc_hit.SetMarkerStyle(24);
  g2t_tpc_hit.SetMarkerColor(5);
  g2t_tpc_hit.Draw(varexp,selection,options);

  return 0;
}

//-----------------------------------------------------------------------

void tpcdraw(){
  /* 
   * -----------------------------------------------------------------
   *	draw TPC sectors with some options 
   * -----------------------------------------------------------------*/


  if (ChainFlags[kMINIDAQ]) {// defined for ChainFlags[kMINIDAQ]
    chain->SetInput("BEGIN_RUN",".make/xdfin/.const/BEGIN_RUN");
    chain->SetInput("ChainFlags[kTPC]_DATA",".make/xdfin/.data/ChainFlags[kTPC]_DATA");
  }

  // defined for MINIDAQ
  //  chain->SetInput("TPC_DATA",".make/xdfin/.data/TPC_DATA");
  //  chain->SetInput("BEGIN_RUN",".make/xdfin/.const/BEGIN_RUN");

  //#ifndef FZIN
  //#ifndef GTRACK
  //  chain->SetInput("g2t_tpc_hit",".make/xdfin/.data/event/geant/Event/g2t_tpc_hit");
  //#endif
  //#endif

  gStyle->SetCanvasColor(10);              // white
  gStyle->SetPadColor(10);                 // white
  gStyle->SetFrameFillColor(10);           // white
  gStyle->SetOptFit(1);

  TCanvas *c1 = new TCanvas("c1","x y z info",100,100,700,700);
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
  if (ChainFlags[kTSS] || ChainFlags[kTRS] || ChainFlags[kMINIDAQ]) {
    draw_pixels("y:x","adc>2","");
  }
  if (draw_pixels == 0) { draw_hits("y:x","","same"); }
  else { draw_hits("y:x","",""); }
  if (!ChainFlags[kMINIDAQ]) {
    draw_geant_hits("x1:x0","","same");
  }  

  pad2->cd();
  if (ChainFlags[kTSS] || ChainFlags[kTRS] || ChainFlags[kMINIDAQ]) {
    draw_pixels("z:x","adc>2","");
  }
  if (draw_pixels == 0) { draw_hits("z:x","","same"); }
  else { draw_hits("z:x","",""); }
  if (!ChainFlags[kMINIDAQ]) {
    draw_geant_hits("x2:x0","","same");
  }
  
  pad3->cd();
  draw_sector(*pad3,90.0,0.0);
  draw_event(*pad3,-7);
  
  pad4->cd();
  draw_sector(*pad4,0.0,0.0);
  draw_event(*pad4,-7);
}


















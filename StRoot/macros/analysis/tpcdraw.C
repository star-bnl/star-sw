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
void draw_event(TPad &padname, Int_t nslice){
  /*-----------------------------------------------------------------------
   * Plot hits and tracks in the TPC */
  
  char slstring[10], tkstring[10];

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
  int value; 
  
  view->SetRange(rmin,rmax);
  view->SetLongitude(15.0);
  view->SetLatitude(90.0);

  // Create iterators for the two datasets
  St_DataSetIter tpc_data(chain->DataSet("tpc_hits"));
  St_DataSetIter tpc_tracks(chain->DataSet("tpc_tracks"));
  St_tpt_track *track = (St_tpt_track *) tpc_tracks["tptrack"];
  St_tcl_tphit  *hits = (St_tcl_tphit *) tpc_data["tphit"];
  St_TableSorter sortrk(*hits,"track");
  tcl_tphit_st *hit1 = hits->GetTable(); 
  Int_t nhits = hits->GetNRows();
  tpt_track_st *track1 = track->GetTable();
  Int_t ntracks = track->GetNRows();
  
  // Show the tpctest event in z slices.
  for(int islice=first_slice;islice<last_slice;islice++){
    Float_t  zmin=zsep[islice];
    Float_t  zmax=zsep[islice+1];
    cout << " slice "<< islice <<" z limits  "<< zmin <<" to "<< zmax << endl;
    // cin >>  value ;  // this is a pause just before plotting each slice.
    //  Now draw the hits - mark those assigned to tracks.
    tcl_tphit_st *h = hit1;
    for(int mm=0;mm<75;mm++)ntkpt[mm] = 0;
    Int_t k=0, savnum[75]; 
    Int_t oldie=0, itkno =0;
    for (i = 0;i<nhits;i++){
      h= hit1[sortrk->GetIndex(i)];
      if(h->z > zmin && h->z < zmax){
	pts[3*k]=h->x; pts[3*k+1]=h->y; pts[3*k+2]=h->z;
	k++;
        if(h->track !=0){
          int newtk = h->track/1000;
          if( newtk != oldie){
            oldie= newtk;
            itkno++;
            savnum[itkno]=oldie;
          }
          tkpts[itkno][3*ntkpt[itkno]]=h->x;
          tkpts[itkno][3*ntkpt[itkno]+1]=h->y;
          tkpts[itkno][3*ntkpt[itkno]+2]=h->z;
          ntkpt[itkno]++;
        }
      }
    } 
    TPolyMarker3D *thit[75];
    cout << "total hits " << k << endl; 
    TPolyMarker3D *hit = new TPolyMarker3D(k,pts,2);
    hit->SetMarkerColor(1);
    hit->Draw(); 
    //Now plot the tracks hits in color ;
    for(int m=0;m<75;m++){
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

void draw_pixels(Text_t* varexp, Text_t* selection, Text_t* options){
  /* 
   * -----------------------------------------------------------------
   *	draw TPC pixels
   * -----------------------------------------------------------------*/

  St_DataSetIter Itpc_raw(chain->DataSet("tpc_raw"));
  St_tfc_adcxyz *pytfc = (St_tfc_adcxyz *) Itpc_raw.Find("adcxyz");
  tfc_adcxyz_st *ptadcxyz = pytfc->GetTable();
  //  St_TableNtuple adcxyz(*pytfc);
  St_TableNtuple *adcxyz = new St_TableNtuple(*pytfc);
  adcxyz.Fill(*pytfc);
  // define plot options
  adcxyz.SetMarkerStyle(26);
  adcxyz.SetMarkerColor(4);
  //  adcxyz.Draw("y:x","adc>1");
  adcxyz.Draw(varexp,selection,options);
}
//______________________________________________________________________

void draw_hits(Text_t* varexp, Text_t* selection, Text_t* options){
  /* 
   * -----------------------------------------------------------------
   *	draw TPC hits
   * -----------------------------------------------------------------*/

  St_DataSetIter Itpc_hits(chain->DataSet("tpc_hits"));
  St_tcl_tphit *pytcl = (St_tcl_tphit *) Itpc_hits.Find("tphit");
  tcl_tphit_st *pttphit = pytcl->GetTable();
  St_TableNtuple tphit(*pytcl);
  tphit.Fill(*pytcl);
  // define plot options
  tphit.SetMarkerStyle(20);
  tphit.SetMarkerColor(2);
  //  tphit.Draw("y:x","","same");
  tphit.Draw(varexp,selection,options);
}
//______________________________________________________________________

void draw_geant_hits(Text_t* varexp, Text_t* selection, Text_t* options){
  /* 
   * -----------------------------------------------------------------
   *	draw TPC geant hits
   * -----------------------------------------------------------------*/

  St_DataSetIter Igeant_g2t(chain->DataSet("g2t_tpc_hit"));
  St_g2t_tpc_hit *pyg2t = (St_g2t_tpc_hit *) Igeant_g2t.Find("g2t_tpc_hit");
  g2t_tpc_hit_st *ptg2t_tpc_hit = pyg2t->GetTable();
  St_TableNtuple g2t_tpc_hit(*pyg2t);
  g2t_tpc_hit.Fill(*pyg2t);
  // define plot options
  g2t_tpc_hit.SetMarkerStyle(24);
  g2t_tpc_hit.SetMarkerColor(5);
  g2t_tpc_hit.Draw(varexp,selection,options);
}
//______________________________________________________________________

void tpcdraw(){
  /* 
   * -----------------------------------------------------------------
   *	draw TPC 
   * -----------------------------------------------------------------*/

//#define GTRACK
//#define FZIN
#define MINIDAQ
#define TPC
  //#define TRS
  //#define TSS
  //#define FTPC
  //#define FSS
  //#define SVT
  //#define EMC
  //#define CTF
#ifndef GTRACK
//#define StMagF
#endif
  //#define XDFOUT
  //#define GLOBAL



#ifdef MINIDAQ
  // defined for MINIDAQ
  chain->SetInput("TPC_DATA",".make/xdfin/.data/TPC_DATA");
  chain->SetInput("BEGIN_RUN",".make/xdfin/.const/BEGIN_RUN");
#endif

#ifndef FZIN
#ifndef GTRACK
  chain->SetInput("g2t_tpc_hit",".make/xdfin/.data/event/geant/Event/g2t_tpc_hit");
#endif
#endif

  gStyle->SetCanvasColor(10);              // white
  gStyle->SetPadColor(10);                 // white
  // gStyle->SetFillColor(0);                 // clear (no-fill)
  // gStyle->SetTitleColor(0);                // clear (no-fill)
  // gStyle->SetStatColor(0);                 // clear (no-fill)
  // gStyle->SetHistFillColor(0);             // clear (no-fill)
  gStyle->SetFrameFillColor(10);           // white
  gStyle->SetOptFit(1);

  TCanvas *c1 = new TCanvas("c1","x y z info",100,100,700,700);
  c1->SetFillColor(0);
  c1->SetBorderMode(0);
  c1->SetBorderSize(0);
  //c1->GetFrame()->SetFillColor(21);
  //c1->GetFrame()->SetBorderMode(-1);
  //c1->GetFrame()->SetBorderSize(5);

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
#ifdef TSS
  draw_pixels("y:x","adc>2","");
#endif
#ifdef TRS
  draw_pixels("y:x","adc>2","");
#endif
#ifdef MINIDAQ
  draw_pixels("y:x","adc>2","");
#endif
  draw_hits("y:x","","same");
#ifndef MINIDAQ
  draw_geant_hits("x1:x0","","same");
#endif
  
  pad2->cd();
#ifdef TSS
  draw_pixels("z:x","adc>2","");
#endif
#ifdef TRS
  draw_pixels("z:x","adc>2","");
#endif
#ifdef MINIDAQ
  draw_pixels("z:x","adc>2","");
#endif
  draw_hits("z:x","","same");
#ifndef MINIDAQ
  draw_geant_hits("x2:x0","","same");
#endif
  
  pad3->cd();
  draw_sector(*pad3,90.0,0.0);
  draw_event(*pad3,-7);
  
  pad4->cd();
  draw_sector(*pad4,0.0,0.0);
  draw_event(*pad4,-7);
}

// $Id: tpctest.C,v 1.5 1999/03/31 13:53:31 fisyak Exp $
// $Log: tpctest.C,v $
// Revision 1.5  1999/03/31 13:53:31  fisyak
// Set exactly no field option
//
// Revision 1.4  1999/03/17 02:59:47  fisyak
// New makers
//


//
/*************************************************************\
							      tpctest99.C is a CINT script package to analyse data from the
							      1997 (LBL) and 1998 (BNL) TPC tests which included both
							      Cosmic Ray triggers and laser events.  The 99 version uses
							      StMinidaqmaker and the 99 versions of tcl, tph and the tpt
							      tracking code.
							      There are four functions:  start() defines and
							      initializes the system.  loop(n) analyses the next n events
							      on the input file.  skip(n) skips n input records.  end()
							      writes the final ntuple to the output.-- WALove 22 Feb 1999
							      \*************************************************************/

// Define globals over this package
TFile *f=0;
//TBrowser b=0;
Int_t ievt=0;  //local event counter 
#include "iostream.h"
#pragma includepath "/afs/rhic/star/packages/dev/StRoot/StChain"
class  StChain;
StChain *chain = 0;
class St_xdfin_Maker;
St_xdfin_Maker *xdfin = 0;
void Load(){
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("xdf2root");
  gSystem->Load("St_Tables");
  gSystem->Load("StMagF");
  gSystem->Load("libmsg");
  gSystem->Load("libtls");
  gSystem->Load("St_db_Maker");
  gSystem->Load("St_xdfin_Maker");
#if 0
  gSystem->Load("geometry");
  gSystem->Load("St_g2r");
  gSystem->Load("St_geant_Maker");
#endif
  gSystem->Load("St_tpc");
  //  gSystem->Load("St_tss_Maker");
  gSystem->Load("St_tcl_Maker");
  gSystem->Load("St_tpt_Maker");
  gSystem->Load("StMinidaqMaker");
}
void start(){
#ifndef __CINT__
#include "Rtypes.h"
#include "St_XDFFile.h"
#include "St_DataSet.h"
#include "St_Module.h"
#include "St_Table.h"
#endif
  // The following files are 1998 test cosmic ray runs.
  // Char_t *filename="/star/tpctest/SD98/tpc_s24e_981027_22h_cos_t13_f3.xdf";
  // Char_t *filename="/star/tpctest/SD98/tpc_s18e_981104_15h_cos_t21_f4.xdf";
  // Char_t *filename="/star/tpctest/SD98/tpc_s18e_981110_14h_cos_t23_f13.xdf";
  // Char_t *filename="/star/tpctest/SD98/tpc_s18e_981111_19h_cos_t25_f4.xdf";
  // Char_t *filename="/star/tpctest/SD98/tpc_s18e_981102_11h_cos_t20_f5.xdf";
  // Char_t *filename="/star/tpctest/SD98/tpc_s23e_981024_12h_cos_t10_f10.xdf";
  // Char_t *filename="/star/tpctest/SD98/tpc_s24e_981026_22h_cos_t12_f4.xdf";
  // Char_t *filename="/star/tpctest/SD98/tpc_s18e_981105_03h_cos_t22_f1.xdf";
  
  // The following files are 1998 test laser runs.
  // Char_t *filename="/star/tpctest/SD98/tpc_s18e_981109_19h_las_t23_f8.xdf"; 
  // Char_t *filename="/star/tpctest/SD98/tpc_s18e_981112_17h_las_t26_f6.xdf"; 
  // Char_t *filename="/star/tpctest/SD98/tpc_s18e_981116_16h_las_t30_f4.xdf"; 
  // The following files are 1997 test laser runs.
  
  // Char_t *filename="/star/tpctest/SD97/laser/log-p285-t28-f1-las.xdf";
  // Char_t *filename="/star/tpctest/SD97/laser/log-p285-t28-f2-las.xdf";
  // Char_t *filename="/star/tpctest/SD97/laser/log-p285-t28-f3-las.xdf";
  // Char_t *filename="/star/tpctest/SD97/laser/log-p285-t28-f4-las.xdf";
  Char_t *filename="/star/tpctest/SD97/laser/log-p285-t28-f5-las.xdf";
  // Char_t *filename="/star/tpctest/SD97/laser/log-p285-t28-f6-las.xdf";
  // this file has 44 events with the membrane only.
  // Char_t *filename="/star/tpctest/SD97/laser/log-p274-t23-f4-las.xdf";
  // Char_t *filename="/star/tpctest/SD97/laser/log-p274-t23-f5-las.xdf";
  // Char_t *filename="/star/tpctest/SD97/laser/log-p274-t23-f6-las.xdf";
  // Char_t *filename="/star/tpctest/SD97/laser/log-p246-t13-f2-las.xdf";
  
  // Create the main chain object
  chain = new StChain("StChain");
  chain->SetDebug();
  chain->SetInput("EvtHddr",".make/geant/.const/EvtHddr");    
  chain->SetInput("TPC_DATA",".make/xdfin/.data/TPC_DATA");
  chain->SetInput("BEGIN_RUN",".make/xdfin/.const/BEGIN_RUN");
  
  const char *mainDB = "$STAR/StDb";
  St_db_Maker *dbMk = new St_db_Maker("db",mainDB);
  chain->SetInput("params","db:StDb/params");
  dbMk->SetDebug();  
  const char *calibDB = "$STAR_ROOT/calib";
  St_db_Maker *calibMk = new St_db_Maker("calib",calibDB);
  chain->SetInput("calib","calib:calib");
  calibMk->SetDebug();  
  //  Create the makers to be called by the current chain
  xdfin = new St_xdfin_Maker("xdfin",filename);
#if 0
  geant = new St_geant_Maker("geant");
  geant->SetNwGEANT(10 000 000);
  geant->SetDebug();
  chain->SetInput("geom","geant:geom");
  geant->LoadGeometry("detp geometry field_only");
  
#endif
  StMagF         *field   = new StMagFC("field","STAR no field",0.);
  StMinidaqMaker *tpc_raw = new StMinidaqMaker("tpc_raw");
  
  St_tcl_Maker *tpc_hits = new St_tcl_Maker("tpc_hits");
  St_tpt_Maker *tpt_tracks = new St_tpt_Maker("tpc_tracks");
  tpt_tracks->Set_final(kTRUE); // Turn on the final ntuple.
  //
  //  chain->SetDebug(StChain::kDebug);
  chain->PrintInfo();
  
  // Init the chain and all its makers
  chain->Init();
}
void loop(Int_t nevt=1){
  gBenchmark->Start("TPCtest"); // time the loop
  
  for (Int_t i=ievt;i<ievt+nevt;i++){
    chain->Clear();
    chain->Make(i+1);//Tell Make the event number - starts from 1.
    draw_event();
  }
  ievt += nevt;
  gBenchmark->Stop("TPCtest");
  gBenchmark->Print("TPCtest");
  gBenchmark->Reset();
}
void skip(Int_t nskip=1){
  xdfin->Skip(nskip); 
  ievt += nskip;// keep the count of events straight.
}
void end(){
  // chain->Maker("tpctest")->Histograms()->Write();
  f->Write(); f->Close(); delete f; f=0;}
void draw_sector(){
  /* 
   * -----------------------------------------------------------------
   *	draw TPC sectors, two sections, inner and outer.
   * -----------------------------------------------------------------*/
  Float_t r1=51.9;
  Float_t r2=126.5;
  Float_t r3=127.5;
  Float_t r4=202.9;
  m_TreeD= new TCanvas("m_TreeD","tpctest event",10,10,600,600);
  
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
  m_TreeD->SetTheta(90.0);
  m_TreeD->SetPhi(0.0);
  m_TreeD->Modified();
  m_TreeD->Update();
}
//______________________________________________________________________
void draw_event(){
  /*-----------------------------------------------------------------------
   * Plot hits and tracks in the TPC */
  
  char evstring[10], slstring[10], tkstring[10];
  // seven slices for the full laser event - These for the 97 test data.
  Float_t zsep[8] = {-210.0,15.0,45.0,75.0,105.0,140.0,170.0,210.0};
  // And these for the 1998 sector 18-19 test data.
  // Float_t zsep[8] = {-210.0,-165.0,-135.0,-105.0,-75.0,-45.0,-15.0,210.0};
  Int_t m_nslice=7;
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
  TView *view = m_TreeD->GetView();
  Float_t rmin[3]={-40, -180,-210};
  Float_t rmax[3]={ 40,  -60, 210};
  Float_t p[6];
  //TPolyMarker3D *hit  = new TPolyMarker3D();
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
    //    cin >>  value ;  // this is a pause just before plotting each slice.
    //  Now draw the hits - mark those assigned to tracks.
    tcl_tphit_st *h = hit1;
    for(int mm=0;mm<75;mm++)ntkpt[mm] = 0;
    Int_t k=0, savnum[75]; 
    Int_t oldie=0, itkno =0;
    Int_t j=0;
    for (i = 0;i<nhits;i++){
      j = sortrk->GetIndex(i);
      h= hit1+j;
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
    for ( i=0;i<75;i++){
      m_TreeD->RecursiveRemove(thit[i]);
    }
    // plot all the points as a + sign
    TPolyMarker3D *hit;
    m_TreeD->RecursiveRemove(hit);
    cout << "total hits " << k << endl; 
    hit= new TPolyMarker3D(k,pts,2);
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
    for ( i=0;i<75;i++){
      m_TreeD->RecursiveRemove(trk[i]);
    }
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
    // put the event number in a separate pad
    TPad *event = new TPad("event","event",0.75,0.84,0.95,0.95);
    event->Draw();
    event->cd();
    event->Range(0,0,1,1);
    event->SetFillColor(19);
    event->SetBorderSize(2);
    // go get event number from the event data
    St_DataSet *raw = chain->DataSet("TPC_DATA");
    St_DataSetIter nex(raw);
    St_type_index *I1 = (St_type_index *) nex("IT1");
    type_index_st *ii = I1->GetTable();
    Int_t evno = ii->data_row;
    sprintf(evstring,"Event %d",evno);
    TText *etext = new TText(0.125,0.69,evstring);
    etext->SetTextSize(0.3);
    etext->Draw();
    if(m_nslice>=0){
      sprintf(slstring,"Slice %d",islice);
      TText *stext = new TText(0.125,0.40,slstring);
      stext->SetTextSize(0.3);
      stext->Draw();
    }
    //label counts only the good tracks.
    sprintf(tkstring," %d Tracks",tin);
    TText *ttext = new TText(0.125,0.11,tkstring);
    ttext->SetTextSize(0.3);
    ttext->Draw();
    event->Modified();
    m_TreeD->cd();
    m_TreeD->Modified();
    m_TreeD->Update();
  }
}

void tpctest(){
  // Dynamically link some shared libs
  if (gClassTable->GetID("StChain") < 0) Load();
  // Create a root file to hold the ntuples. 
  f=new TFile("ntup99.root","RECREATE");
  start();
  skip(4);
  draw_sector();
  loop(1);
}


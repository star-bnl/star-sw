// $Id: Las04YF.C,v 1.1 2004/06/08 22:06:54 fisyak Exp $
// $Log: Las04YF.C,v $
// Revision 1.1  2004/06/08 22:06:54  fisyak
// Freeze
//
// Revision 1.0  1999/11/08 Love
//
//
/*************************************************************\
 Las03.C is a CINT script package to analyse data from the
 2003 TPC test laser events.  Uses new DAQ code
 and the 2000 versions of tcl, tph, and the tpt tracking code.
 and willy-nilly the new TPC database routines.
 There are four functions:  start() defines and
 initializes the system.  loop(n) analyses the next n events
 on the input file.  skip(n) skips n input records.  end()
 summarizes data errors encountered and writes the laser
 tree to the output.-- WALove 20 July 2001.

  For running 2004 data changed the name to Las04.C
\*************************************************************/

// Define globals over this package
  TFile *f=0;  // file variable for the root output..
//TBrowser b=0;
  TCanvas *m_TreeD = 0;
  TNode *m_node1 = 0;
 Int_t islice=0;
#include "iostream.h"
#pragma includepath "/afs/rhic/star/packages/dev/StRoot/StChain"
class  StBFChain;
StBFChain *chain = 0;

class St_tpcdaq_Maker;
St_tpcdaq_Maker *tpc_raw=0;
class StIOMaker;
StIOMaker *inpMk=0;
class StLaserEventMaker;
StLaserEventMaker *las_tracks=0;
//#define OneLaser

void Load(){
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("StUtilities");
  gSystem->Load("StBFChain");
}
//_____________________________________________________________________
void start(){
#ifndef __CINT__
#include "Rtypes.h"
#include "St_XDFFile.h"
#include "St_DataSet.h"
#include "St_Module.h"
#include "St_Table.h"
#endif

 // The following files are Feb 2003 Laser runs et. seq.

  // Char_t *filename="/star/data03/daq/2003/045/st_laser_4045020_raw_0030001.daq";
#if 0
 Char_t *filename="/star/data03/daq/2003/laser/st_laser_4364002_raw_1050003.daq";
#else
 Char_t *filename=0;
#endif
//____________________________________________________________________ 
  // Create the main chain object
#if 0
  chain = new StBFChain("StChain");
  chain->SetFlags("in db tpc_daq tpcDb tcl tpt corr2 globT ");
  chain->Set_IO_Files(filename,0);
#else
  gROOT->LoadMacro("bfc.C"); 
  bfc(-1,"gstar,Y2004,trsMini,tpc,corr2",filename,0,"R4364002_153.root");
#endif
  chain->Load();
  chain->Instantiate();  // chain->SetDebug();
#if 1
#ifndef OneLaser
  gInterpreter->ProcessLine(".L laserGukine.C+");
#endif
#if 0
  chain->SetTFile(f);
#endif
  // 
  St_tcl_Maker *tpc_hits = (St_tcl_Maker *) chain->GetMaker("tpc_hits"); 
  //tpc_hits->tclPixTransOn(); //make the adcxyz table. (need for pixels)
  //
   gSystem->Load("StLaserEvent");
   gSystem->Load("StLaserEventMaker");
  
  las_tracks = new StLaserEventMaker("las_tracks");
  las_tracks->Set_laser(kTRUE); // Turn on/off the laser Tree.
  las_tracks->Set_lasers(kTRUE); // Do a real laser run or event data.
  las_tracks->Set_UndoExB(kFALSE); // Turn on/off the ExB corrections.
  //las_tracks->Set_UndoDistort(kFALSE,0); // Turn on/off Jim Thomas' corrections.

  Int_t rowmin=1; Int_t rowmax=1; 
  las_tracks->SetRows(rowmin,rowmax); // range for the pixel branch.
  cout << "store pixels for rows "<<rowmin<< " through " <<rowmax<<endl;
  //
  chain->SetDebug(StChain::kDebug);
  chain->PrintInfo();
  
  // Init the chain and all its makers
  chain->Init();
#if 1
  St_geant_Maker *geant = (  St_geant_Maker * ) chain->Maker("geant");
  if (geant) {
    gSystem->Load("gstar.so");
    geant->Do("call gstar");
    geant->Do("debug on");
    geant->Do("swit 1 2");
    geant->Do("swit 2 2");
#ifdef OneLaser
    //  geant->Do("gvert     56     0        0.0");
    geant->Do("gkine 1  171   100.     100. 0.     0.    0. 0.  10.   10.;");  
    geant->Do("gvert      0     54       0.0");
    //    geant->Do("gkine 1  170     10.     10. 0.     0.   0. 0.  10.   10.;");  
    //    geant->Do("gkine 1    5     10.     10. 0.     0.   0. 0.  10.   10.;");  
#endif
  }
#endif
}
//_____________________________________________________________________
void loop(Int_t nevt=1){
  gBenchmark->Start("TPCtest"); // time the loop
  
  for (Int_t i=0;i<nevt;i++){
    chain->Clear();
   int evng =  chain->Make();
    //        check_event();
   if(evng == kStFatal){
    printf("Ran out of data after %d events - quit\n",i+1);
    return;
   }
   //        draw_event();
  }
  gBenchmark->Stop("TPCtest");
  gBenchmark->Print("TPCtest");
  gBenchmark->Reset();
}
//_____________________________________________________________________
void skip(Int_t nskip=1){
  //Skip is done by the IOMaker.
  StIOMaker *io = (StIOMaker *)chain->GetMaker("inputStream");
     io->Skip(nskip);
     cout << "skip " << nskip  <<endl;
}
//_____________________________________________________________________
void end(){
  // We need to make sure the tree is closed here.
  // las_tracks->EndRun();  No EndRun in StLaserEventMaker ?
  // Close the output file because chain->Finish will delete it.
#if 0
  f->Write(); f->Close();
#endif
  chain->Finish();
}

//_____________________________________________________________________
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
  
  m_node1 = new TNode("m_node1","inner sector","insect");
  
  m_node1->BuildListOfNodes();
  m_node1->cd();
  
  TPGON   *outsect = new TPGON("outsect","TPC outer sector","void",-15,360,12,2);
  outsect->DefineSection(0,-210,r3,r4);
  outsect->DefineSection(1,210,r3,r4);
  outsect->SetLineColor(2);
  TNode *node2 = new  TNode("node2","outer sector","outsect");
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

  // These for the 1998 sector 18-19 test data.
  // Float_t zsep[8] = {-210.0,-165.0,-135.0,-105.0,-75.0,-45.0,-15.0,210.0};
  // These for the 1999-2000 laser tests.
     Float_t zsep[14] = {-210.0,-165.0,-135.0,-105.0,-75.0,-45.0,-15.0,
      15.0,45.0,75.0,105.0,135.0,165.0,210.0};
  
   
   Int_t first_slice = islice;
   Int_t last_slice = islice+1;
   islice++;
   if(islice>12) islice=0;
  
  cout << "from " << first_slice << " to " << last_slice << endl;
  
  Int_t i=0;
  Int_t incolor;
  
  // Plot the part of the TPC with tracks from the tpctest test
  m_node1->Draw();
  TView *view = m_TreeD->GetView();
  Double_t rmin[3]={-120, -120,-210};
  Double_t rmax[3]={ 120,  120, 210};
  Float_t p[6];
  //TPolyMarker3D *hit  = new TPolyMarker3D();
  Float_t pts[480000],tkpts[2000][240]; //allow for 2000, 80 hit tracks
  Int_t ntkpt[2000];                  // and 2000 track hit counters 
  int value; 
  
  view->SetRange(rmin,rmax);
  view->SetLongitude(15.0);
  view->SetLatitude(90.0);
  // Create iterators for the two datasets
  St_DataSetIter tpc_data(chain->DataSet("tpc_hits"));
  St_DataSetIter tpc_tracks(chain->DataSet("las_tracks"));
  St_tpt_track *tracks = (St_tpt_track *) tpc_tracks["tptrack"];
  St_tcl_tphit  *hits = (St_tcl_tphit *) tpc_data["tphit"];
  St_TableSorter sortrk(hits,"track");
  tcl_tphit_st *hit1 = hits->GetTable(); 
  Int_t nhits = hits->GetNRows();
  
  // Show the tpctest event in z slices.
  for(int islice=first_slice;islice<last_slice;islice++){
    Float_t  zmin=zsep[islice];
    Float_t  zmax=zsep[islice+1];
    cout << " slice "<< islice <<" z limits  "<< zmin <<" to "<< zmax << endl;
    //    cin >>  value ;  // this is a pause just before plotting each slice.
    //  Now draw the hits - mark those assigned to tracks.
    tcl_tphit_st *h = hit1;
    for(int mm=0;mm<2000;mm++)ntkpt[mm] = 0;
    Int_t k=0, savnum[2000]; 
    Int_t oldie=0, itkno =0;
    for (i = 0;i<nhits;i++){
      h= &(hit1[sortrk->GetIndex(i)]);
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
    TPolyMarker3D *thit[2000];
    for ( i=0;i<2000;i++){
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
    for(int m=0;m<2000;m++){
      if(ntkpt[m] !=0){
	//    
	//cout <<" Track #" << savnum[m] << " "<< ntkpt[m] <<" hits" << endl;
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
    //plot a marker at the source of each bundle.  Only for laser events
    //    Float_t pots[18];
    //   Float_t zori[6] = {32.0,60.0,90.0,120.0,152.0,179.0};
    //    TPolyMarker3D *origin;
    //    for(int m=0;m<6;m++){
    //      pots[3*m]=-2.5; pots[3*m+1]=-198.0; pots[3*m+2]=zori[m];
    //    }  
    //    origin = new TPolyMarker3D(6,pots,1);
    //    origin->SetMarkerColor(1);
    //    origin->SetMarkerStyle(3);
    //    origin->SetMarkerSize(1.0);
    //    origin->Draw();    
    // Now plot all the tracks as polylines.  
    TPolyLine3D *trk[2000];
    for ( i=0;i<2000;i++){
      m_TreeD->RecursiveRemove(trk[i]);
    }
    Int_t tin=0;
    Int_t tid[2000];
    // This code for regular tpt tracking
    tpt_track_st *t = tracks->GetTable();
    Int_t ntracks = tracks->GetNRows();
    for( i=0;i<ntracks;i++) {
      //skip any fake tracks (negative flag?)
      if(t->flag < 0){ t++;continue;}
      if(t->nfit <6){ t++;continue;}
      Float_t ay = 1000.0;
      if(fabs(t->psi) > 0.01)  
      ay = 1.0/tan(0.0174533*t->psi);
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
    int idevt= chain->GetEventNumber();
    sprintf(evstring,"Event %d",idevt);
    TText *etext = new TText(0.125,0.69,evstring);
    etext->SetTextSize(0.3);
    etext->Draw();  

    //label counts only the good tracks.
    sprintf(tkstring," %d Tracks",tin);
    TText *ttext = new TText(0.125,0.11,tkstring);
    ttext->SetTextSize(0.3);
    ttext->Draw();

    sprintf(slstring,"z slice %d",islice);
    TText *ztext = new TText(0.125,0.40,slstring);
    ztext->SetTextSize(0.3);
    ztext->Draw();

    event->Modified();
    m_TreeD->cd();
    m_TreeD->Modified();
    m_TreeD->Update();
  }
}
  //____________________________________________________________________
void Las04YF(){
  // Dynamically link some shared libs
  if (gClassTable->GetID("StChain") < 0) Load();
   gMessMgr->SetLimit("special case",10);
   gMessMgr->SetLimit("Fit_I_Cluster",10);
   gMessMgr->SetLimit("root limit!",10);
   gMessMgr->SetLimit("in empty cluster",10);
   gMessMgr->SetLimit("in the outliers",10);
   gMessMgr->SetLimit("TPH_3POINT_GAUSS bad shape",10);
   // Create a root file to hold the tree or ntuples. 
#if 0
  f=new TFile("/star/data05/scratch/love/roots/R4364002_153.root","RECREATE");
  skip(1);
  //  draw_sector();
  loop(230);
#else
  start();
  loop(1);
#endif
  end();
}

// $Id: Dst.C,v 1.1 2004/03/30 23:26:50 fisyak Exp $
void Dst(const Int_t nevents = 10,
	 const Char_t *MainFile="/star/data14/reco/DEV2/2000/08/*dst.root",
	 const Char_t *root_File="all2.root")
{  
  //#ifndef __CINT__
  gROOT->LoadMacro("bfc.C");
  bfc(0,"in globT NoDb",MainFile,0,root_File);
  
  // Create "histograms"
  
  TFile  *root_out = chain->GetTFile();
  root_out->cd();
  Bool_t drawinit=kFALSE;
  Int_t np = 100;
  Int_t ndedx = 100;
  Int_t fcounter = 0;  // counter of the files
  Double_t minp = -2.;
  Double_t maxp = 1.;
  Double_t mindedx = 0.0;
  Double_t maxdedx =  10;
  // Create 2D histogram
  root_out->cd(); 
  m_p_dedx_rec = new TH2F("p_dedx_rec","dE/dx versus p (reconstructed)",
                           np,minp,maxp,ndedx,mindedx,maxdedx);
  m_p_dedx_rec->SetYTitle("dE/dx(keV/cm)");
  m_p_dedx_rec->SetXTitle("Log10 (p) (GeV)");
  // Create canvas to plot that histogram
  c1 = new TCanvas("c1","Particle p by reading STAF table: \"globtrk.h\"",20,10,600,400);
  c1->SetGrid();

  c1->SetLogz();


//*-* Create a canvas to show the result (based on  root/tutorials/hsum.C )

//      c1->Divide(1,2);
  TSlider *slider = 0;

  St_DataSet *event = 0;
  gBenchmark->Start("hsum");
  Int_t kUPDATE = 0;
  cout <<"Starting Event Loop"<<endl;
  int istat=0,iev=1;
 EventLoop: if (iev<=nevents && !istat) {
   chain->Clear();
   cout << "---------------------- Processing Event : " << iev << endl;
   istat = chain->Make(iev); // 
   if (istat) {
     cout << "Last Event Processed. Status = " << istat << endl;
   }
   
   St_DataSet *Tdst = chain->GetDataSet("dst"); 
   if (!Tdst) continue;
   St_DataSet *dst = Tdst->Find(".data/dst");
   if (!dst) dst = Tdst;
   
   //	   event->ls("*");
     
   St_dst_dedx *dedx     = (St_dst_dedx *)  dst->Find("dst_dedx");
   St_dst_track *primtrk = (St_dst_track *) dst->Find("primtrk");
   if (dedx && primtrk) { 
     // define how often we will upgrade the graphical view
     kUPDATE = dedx->GetNRows()/50;
     TTableSorter     *primtrkS  = new TTableSorter(primtrk,"id");
     TTableSorter     *dedxS     = new TTableSorter(dedx,"id_track");
     Int_t k = 0, kdedx = 0;
     dst_track_st  *trk = primtrk->GetTable();
     dst_dedx_st  *de   = dedx->GetTable();
     for (; k < dedx->GetNRows(); k++, trk++){
       Int_t l = primtrkS->GetIndex(k);
       dst_track_st  Trk = primtrk->GetTable() + l;
       Int_t Id = 
	 for (;  kdedx<dedx->GetNrows(); kdedx++) {
	   Int_t ldedx = dedxS->GetIndex(kdedx);
	   dst_dedx_st *d = de + ldedx
       if (d->det_id=2 && d->ndedx > 15) {
	 Double_t dedx_m = d->dedx[0]*1.e6;
	 if (dedx_m <0.01) continue;
	 Int_t igl = d->id_track;
	 Int_t igl_use = igl - 1;
	 Double_t invpt = Trk[igl_use]->invpt;
	 Double_t pT = 9999.;
	 if (invpt) pT = 1./TMath::Abs(invpt);
	 Double_t pz = pT*Trk[igl_use]->tanl;
	 Double_t pp = TMath::Sqrt((Float_t)(pT*pT+pz*pz));
         if (pp < 0.01) continue;
	 Double_t  p = TMath::Log10(pp);
	 m_p_dedx_rec->Fill(p,dedx_m);
       }
       // Update the view of these histograms (just for fun)
       if (kUPDATE && l && (l%kUPDATE) == 0) {
	 if ( l==kUPDATE && !drawinit) {
	   drawinit = kTRUE;
	   c1->cd();
	   m_p_dedx_rec->Draw("col");
	   slider = new TSlider("slider","test",1.05,0,1.1,m_p_dedx_rec->GetMaximum()*1.3,38);
	   slider->SetFillColor(46);
	   c1->Update();
	 }
       }
     }
     delete dst; dst= 0;
   }
   iev++; goto EventLoop;
 } // Event Loop
  c1->Modified();
  root_out->Write();
  printf("\n");
  printf(" %d XDF files have been analysed from <%s> directory \n",fcounter,Path);
  gBenchmark->Show("hsum");
  printf(" This is a finish \n");
}

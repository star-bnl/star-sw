// $Id: dedx_root.C,v 1.1 1998/12/12 02:38:43 fisyak Exp $
// $Log: dedx_root.C,v $
// Revision 1.1  1998/12/12 02:38:43  fisyak
// Clean up
//
// Revision 1.1  1998/11/19 23:40:04  fine
// $Id: dedx_root.C,v 1.1 1998/12/12 02:38:43 fisyak Exp $
// $Log: dedx_root.C,v $
// Revision 1.1  1998/12/12 02:38:43  fisyak
// Clean up
//
// A macro "dedx_root.C" to plot dedx from root dst files 
//
//#pragma security level3
void dedx_root(const Char_t *Path="/disk1/star/auau200/hijing135/default/",
               const Char_t *root_file="all2.root")
{
// This begin_html <a href=http://www.rhic.bnl.gov/cgi-bin/star-cgi/cvsweb.pl/StRoot/macros/dedx.C> macro</a> by  <a href="mailto:OGILVIE@mitlns.mit.edu"> Craig Ogilvie </a> end_html
  gROOT->Reset();
  if (gSystem.Load("St_base"))      printf(" Loading DLL \"St_base\" failed \n"); 
  // if (gSystem.Load("xdf2root"))      printf(" Loading DLL \"xdf2root\" failed \n");
  if (gSystem.Load("St_Tables"))    printf(" Loading DLL \"St_Tables\" failed \n");
  // Create "histograms"
  TFile  *root_out = 0;
  // Create a new ROOT file to save the histograms
  root_out = new TFile(root_file,"RECREATE");
  Bool_t drawinit=kFALSE;
  Int_t np = 100;
  Int_t ndedx = 100;
  Int_t fcounter = 0;  // counter of the files
  Float_t minp = 0.0;
  Float_t maxp = 1.5;
  Float_t mindedx = 0.0;
  Float_t maxdedx =  0.1e-04;
  // Create 2D histogram
  m_p_dedx_rec = new TH2F("p_dedx_rec","p versus dedx (reconstructed)",
			  np,minp,maxp,ndedx,mindedx,maxdedx);
  m_p_dedx_rec->SetYTitle("dedx");
  m_p_dedx_rec->SetXTitle("p (GeV)");
  // Create canvas to plot that histogram
  c1 = new TCanvas("dedx","Particle p by reading STAF table: \"globtrk.h\"",20,10,600,400);
  c1->SetGrid();
  c1->SetLogz();
//*-* Create a canvas to show the result (based on  root/tutorials/hsum.C )
  TSlider *slider = 0;

  St_DataSet *event = 0;
  gBenchmark->Start("dedx");
  St_FileSet dstdirs(Path);
  St_DataSetIter nextroot(&dstdirs,0);
  St_DataSet *set = 0; 
  TFile *root = 0;
  Int_t kUPDATE = 0;
  TString  path = Path;
  Char_t *name;
  Char_t *title;
  while (set = nextroot()){
    if (strcmp(set->GetTitle(),"file") == 0 && strstr(set->GetName(),".root")){
      name = set->GetName();
      title = set->GetTitle();
      printf("%s %s \n",name,title);
      if (root) delete root; // This closes the previous root file if any
      //  Define the absolute path of the next XDF file
      path  = Path;
      path += set->Path();
      Char_t *rootfilename= path.Data();
      // Open next XDF file to read
      root =  new TFile(rootfilename);
      printf ("Open %s\n", rootfilename);
      root->ls();
      fcounter++;
      TList *list =  gDirectory->GetListOfKeys();
      TIter next(list);
      int i = 0;
      TKey *k = 0;
      while (k = (TKey *) next()) {
	printf(" %d. %s  %s %d\n",++i,k->GetName(),k->GetName(),k->GetCycle());
	if (strcmp(k->GetName(),"dst") != 0) continue;
	event = (St_DataSet *)k->ReadObj(); event->ls();
	St_DataSetIter *dst = 0;
	dst = new St_DataSetIter(event);
	
	//	   event->ls("*");
	
	St_dst_dedx *dedx     = (St_dst_dedx *)  dst->Next("dst/dst_dedx");
	St_dst_track *globtrk = (St_dst_track *) dst->Next("dst/globtrk");
	
	if (dedx && globtrk) { 
	  
	  dst_dedx_st  *de   = dedx->GetTable();
	  
	  dst_track_st  *trk   = globtrk->GetTable();
	  // define how often we will upgrade the graphical view
	  kUPDATE = dedx->GetNRows()/50;
	  
	  for (Int_t l = 0; l < dedx->GetNRows(); l++){
	    // Pick the next STAF table row
	    dst_dedx_st *d = de + l;
	    
	    if (d->det_id=2 && d->ndedx > 15) {
	      Float_t dedx_m = d->dedx[0];
	      Int_t igl = d->id_track;
	      Int_t igl_use = igl - 1;
	      // this is bad style, since it assumes the global track has not been sorted
	      // it works for now
	      Float_t invpt = trk[igl_use]->invpt;
	      Float_t pT = 9999.;
	      if (invpt) pT = 1./TMath::Abs(invpt);
	      Float_t pz = pT*trk[igl_use]->tanl;
	      Float_t  p = TMath::Sqrt(pT*pT+pz*pz);
	      
	      // Filling histogram
	      
	      m_p_dedx_rec->Fill(p,dedx_m);
	    }
	    
	    // Update the view of these histograms (just for fun)
	    if (kUPDATE && l && (l%kUPDATE) == 0) {
	      if ( l==kUPDATE && !drawinit) {
		drawinit = kTRUE;
		c1->cd();
		//		   m_p_dedx_rec->Draw("lego3");
		
		// Plot histogram:
		// Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/dedx.gif"> </P> End_Html //
		//     	           m_p_dedx_rec->Draw("scat");  
		m_p_dedx_rec->Draw("col");
		
		
		
		slider = new TSlider("slider","test",1.05,0,1.1,m_p_dedx_rec->GetMaximum()*1.3,38);
		slider->SetFillColor(46);
		c1->Update();
	      }
	    }
	  }
	  if (slider) slider->SetRange(0,Float_t(l)/dedx->GetNRows());
	  c1->Modified();
	  c1->Update();
	}
	delete dst; dst= 0;
	// End of Event. Now we can delete it
	if (event) delete event;
	event = 0;
	c1->Modified();
      }
    }
  }
  root_out->Write();
  printf("\n");
  printf(" %d ROOT files have been analysed from <%s> directory \n",fcounter,Path);
  gBenchmark->Show("hsum");
  printf(" This is a finish \n");
}

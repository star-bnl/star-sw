// $Id: dedx.C,v 1.6 1999/06/03 16:40:37 kathy Exp $
// $Log: dedx.C,v $
// Revision 1.6  1999/06/03 16:40:37  kathy
// put back old dedx.C and restore original macros that use non-existent input file
//
// Revision 1.4  1999/05/21 15:33:57  kathy
// made sure Log & Id are in each file and also put in standard comment line with name of owner
//
// Revision 1.3  1999/04/08 17:12:21  fisyak
// Change file names to MDC2 dst
//
// Revision 1.2  1999/01/02 19:08:25  fisyak
// Add ctf
//
// Revision 1.1  1998/11/19 23:40:04  fine
// A new macro "dedx.C" by Craig Ogilvie has been introduced
//=======================================================================
// owner: Craig Ogilvie
// what it does: 
//=======================================================================
//
void dedx(const Char_t *Path="/disk00001/star/auau200/venus412/default/b6_9/year_1b/hadronic_on/tss/",const Char_t *root_file="all2.root")
{  

// This begin_html <a href=http://www.rhic.bnl.gov/cgi-bin/star-cgi/cvsweb.pl/StRoot/macros/dedx.C> macro</a> by  <a href="mailto:OGILVIE@mitlns.mit.edu"> Craig Ogilvie </a> end_html

  gROOT->Reset();
  if (gSystem.Load("St_base"))      printf(" Loading DLL \"St_base\" failed \n"); 
  if (gSystem.Load("xdf2root"))      printf(" Loading DLL \"xdf2root\" failed \n");
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
  c1 = new TCanvas("c1","Particle p by reading STAF table: \"globtrk.h\"",20,10,600,400);
  c1->SetGrid();

  c1->SetLogz();


//*-* Create a canvas to show the result (based on  root/tutorials/hsum.C )

//      c1->Divide(1,2);
  TSlider *slider = 0;

  St_DataSet *event = 0;
  gBenchmark->Start("hsum");
  St_FileSet dstdirs(Path);
  St_DataSetIter nextxdf(&dstdirs,0);
  St_DataSet *set = 0; 
  St_XDFFile *xdf = 0;
  Int_t kUPDATE = 0;
  TString  path = Path;
  while (set = nextxdf()){
    if (strcmp(set->GetTitle(),"file") == 0){
      // Select all "*dst.xdf" files but "set174_02_48evts_h_dst.xdf"
      // since the last has a corrupted table
      if ( strstr(set->GetName(),"dst.xdf") &&
           !strstr(set->GetName(),"set174_02_48evts_h_dst.xdf")
         ){
 	 if (xdf) delete xdf; // This closes the previous XDF file if any
 	 //  Define the absolute path of the next XDF file
	 path  = Path;
	 path += set->Path();
	 Char_t *xdffilename= path.Data();
	 // Open next XDF file to read
         xdf =  new St_XDFFile(xdffilename,"r");
         printf ("Open %s\n", xdffilename);
         fcounter++;
         while (event = xdf.NextEventGet() ) {
           St_DataSetIter *dst = 0;
	   dst = new St_DataSetIter(event);
	   
	   //	   event->ls("*");
     
           St_dst_dedx *dedx     = (St_dst_dedx *)  dst->Find("dst/dst_dedx");
           St_dst_track *globtrk = (St_dst_track *) dst->Find("dst/globtrk");

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
   }
   root_out->Write();
   printf("\n");
   printf(" %d XDF files have been analysed from <%s> directory \n",fcounter,Path);
   gBenchmark->Show("hsum");
   printf(" This is a finish \n");

}

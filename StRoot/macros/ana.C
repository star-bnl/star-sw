void ana(const Char_t *Path="/disk1/star/auau200/hijing135/default/b0_3/year1a/hadronic_off/tfs_dst/",const Char_t *root_file="b0_3_y1a_off.root")
{
//Char_t *xdffilename="/disk1/star/auau200/hijing135/default/b0_3/year2a/hadronic_on/tfs_dst/psc148_02_48evts_h_dst.xdf"
  gROOT->Reset();
  Bool_t NT = kFALSE;
  if (strcmp(gSystem.GetName(),"WinNT")==0){
    NT = kTRUE;
    if (gSystem.Load("St_base.dll")) printf(" Loading DLL \"St_base.dll\" failed \n");
    if (gSystem.Load("St_Tables.dll")) printf(" Loading DLL \"St_Tables.dll\" failed \n");
  }
  else{
    if (gSystem.Load("St_base.so"))      printf(" Loading DLL \"St_base.so\" failed \n");
    if (gSystem.Load("xdf2root.so"))      printf(" Loading DLL \"xdf2root.so\" failed \n");
    if (gSystem.Load("St_Tables.so"))    printf(" Loading DLL \"St_Tables.so\" failed \n");
  }
 // Create "histograms"
  TFile  *root_out = 0;
  root_out = new TFile(root_file,"RECREATE");
  Bool_t drawinit=kFALSE;
  Int_t npT = 100;
  Int_t neta = 40;
  Float_t minpT = 0.0;
  Float_t maxpT = 10.0;
  Float_t mineta = -2.0;
  Float_t maxeta =  2.0;
  m_pT_eta_rec = new TH2F("pT_eta_rec","pT versus eta (reconstructed)",
                           neta,mineta,maxeta,npT,minpT,maxpT);
  m_pT_eta_rec->SetXTitle("eta");
  m_pT_eta_rec->SetYTitle("pT (GeV)");
  c1 = new TCanvas("c1","Particle pT by reading STAF table: \"globtrk.h\"",20,10,600,400);
  c1->SetGrid();
  m_pT_eta_gen = new TH2F("pT_eta_gen","pT versus eta (generated)",
			  neta,mineta,maxeta,npT,minpT,maxpT);
  c1->SetLogz();
#if 0
  c2 = new TCanvas("c2","Particle eta by reading STAF table: \"hepe_gent.h\"",20,410,600,400);
  c2->SetGrid();
  m_pT_eta_gen->SetXTitle("eta");
  m_pT_eta_gen->SetYTitle("pT (GeV)");
#endif
  m_No   = new TH1F("No","number of points",50,0,50);
  m_NoF  = new TH1F("NoF","number of points used in fit",50,0,50);
  m_Leng = new TH1F("Leng","track length from first to last point",100,0,100);
  m_NDF  = new TH1F("NDF","No. deg. of freedom for track fit.",100,0,100);
  m_chi2 = new TH2F("chi2","Chi-square for fit in x-y vs path-z",50,0,100,50,0,100);
 
  m_pT   = new TH1F("pT","pT distribution",npT,minpT,maxpT);
  m_pT->SetFillColor(16);
  m_pT->SetMarkerStyle(21);
  m_pT->SetMarkerSize(0.2);
  c3 = new TCanvas("c3","pT distribution by reading STAF table: \"globtrk.h\"",700,10,600,400);
  c3->SetLogy();

  m_pTL  = new TH1F("pTL","pT distribution with length gt 15 cm",npT,minpT,maxpT);
  m_pTL->SetFillColor(42);
  m_pTN  = new TH1F("pTN","pT distribution with NDF gt 10",npT,minpT,maxpT);
  m_pTN->SetFillColor(46);
  m_pTC  = new TH1F("pTC","pT distribution with chisq/NDF lt 3",npT,minpT,maxpT);
  m_pTC->SetFillColor(50);
  m_eta  = new TH1F("eta","eta distribution",neta,mineta,maxeta);
  m_eta->SetFillColor(16);
  m_eta->SetMarkerStyle(21);
  m_eta->SetMarkerSize(0.2);

  c4     = new TCanvas("c4","eta distribution by reading STAF table: \"globtrk.h\"",700,410,600,400);
  m_etaL  = new TH1F("etaL","eta distribution with length gt 15 cm",neta,mineta,maxeta);
  m_etaL->SetFillColor(42);
  m_etaN  = new TH1F("etaN","eta distribution with NDF gt 10",neta,mineta,maxeta);
  m_etaN->SetFillColor(46);
  m_etaC  = new TH1F("etaC","eta distribution with chisq/NDF lt 3",neta,mineta,maxeta);
  m_etaC->SetFillColor(50);

//*-* Create a canvas to show the result (based on  root/tutorials/hsum.C )

//      c1->Divide(1,2);
  TSlider *slider = 0;

  St_DataSet *event = 0;
  gBenchmark->Start("hsum");
  St_FileSet dstdirs(Path);
  St_DataSetIter nextxdf(&dstdirs,0);
  St_DataSet *set = 0; 
  St_XDFFile *xdf = 0;
  TString  path = Path;
  while (set = nextxdf()){
    if (strcmp(set->GetTitle(),"file") == 0){
      printf ("Title = %s Name = %s\n",set->GetTitle(),set->GetName());
      if (strstr(set->GetName(),"dst.xdf")){
	if (xdf) delete xdf;
	 path = Path;
	 path +=  set->Path();
	 Char_t *xdffilename= path.Data();
         xdf =  new St_XDFFile(xdffilename,"r");
         printf ("Open %s\n", xdffilename);
         while (event = xdf.NextEventGet() ) {
           St_DataSetIter dst(event);
           St_dst_track *globtrk = (St_dst_track *) dst("globtrk");
	   if (globtrk) { 
	     table_head_st *trk_h = globtrk->GetHeader();
	     dst_track_st  *trk   = globtrk->GetTable();
	     Int_t kUPDATE = globtrk->GetNRows()/50;
	     for (Int_t l = 0; l < globtrk->GetNRows(); l++){
	       dst_track_st *t = trk + l;
	       Float_t pT = 9999.;
	       if (t->invpt) pT = 1./TMath::Abs(t->invpt);
	       Float_t theta = asin(1.) - atan(t->tanl);
	       Float_t eta   =-log(tan(theta/2.));
	       m_pT->Fill(pT);
	       m_eta->Fill(eta);
	       m_pT_eta_rec->Fill(eta,pT);
	       m_No->Fill(t->n_point);
	       m_NoF->Fill(t->n_fit_point);
	       m_Leng->Fill(t->length);
	       m_NDF->Fill(t->ndegf);
	       m_chi2->Fill(t->chisq[0],t->chisq[1]);
	       if (t->length > 15.0){
		 m_pTL->Fill(pT);
		 m_etaL->Fill(eta);
		 if (t->n_fit_point > 10) {
		   m_pTN->Fill(pT);
		   m_etaN->Fill(eta);
		   if ((t->chisq[0]+t->chisq[1])/t->ndegf < 3) {
		     m_pTC->Fill(pT);
		     m_etaC->Fill(eta);
		   }
		 }
	       }
    
    // Update the view of these histograms (just for fun)
	       if (l && (l%kUPDATE) == 0) {
		 if ( l==kUPDATE && !drawinit) {
		   drawinit = kTRUE;
		   c1->cd();
		   m_pT_eta_rec->Draw("lego3");
		   c3->cd();
		   m_pT->Draw("e1p"); 
		   m_pTL->Draw("same");
		   m_pTN->Draw("same");
		   m_pTC->Draw("same");
		   c4->cd();
		   m_eta->Draw("e1p");
		   m_etaL->Draw("same");
		   m_etaN->Draw("same");
		   m_etaC->Draw("same");
		   slider = new TSlider("slider","test",1.05,0,1.1,m_pT_eta_rec->GetMaximum()*1.3,38);
		   slider->SetFillColor(46);
		   c1->Update();
		   c3->Update();
		   c4->Update();
		 }
	       }
	     }
	     if (slider) slider->SetRange(0,Float_t(l)/globtrk->GetNRows());
	     c1->Modified();
	     c1->Update();
	     c3->Modified();
	     c3->Update();
	     c4->Modified();
	     c4->Update();
	   }
	   St_hepe_gent *hepev = (St_hepe_gent *) dst("hepe_gent");
#if 0
	   if (hepev) {
	     Int_t kUPDATE = hepev->GetNRows()/5;
	     printf ("Nrow %d kUPDATE %d \n",hepev->GetNRows(), kUPDATE);
	     table_head_st *t1_h = hepev->GetHeader();
	     hepe_gent_st *particle = hepev->GetTable();
	     for (Int_t l=0; l < hepev->GetNRows(); l++){
	       hepe_gent_st *p = particle+l;
	       if (p->isthep == 1) {
		 Float_t px = p->phep[0];
		 Float_t py = p->phep[1];
		 Float_t pz = p->phep[2];
		 Float_t pT    =  sqrt(px*px+py*py);
//           printf ("px py pz %f %f %f l = %d\n",px,py,pz,l);
//         Double_t theta =  TMath::Atan2 ( pT, pz );
		 Double_t theta =  atan2 ( pT, pz );
		 Float_t  eta  = -log(tan(theta/2.));
		 m_pT_eta_gen->Fill(eta,pT);
		 if (l && (l%kUPDATE) == 0) {
		   if ( l==kUPDATE && !drawinit) {
		     printf ("l =%d\n",l);
		     drawinit = kTRUE;
		     c2->cd();
		     m_pT_eta_gen->Draw("lego3");
		   }
		 }
	       }
	       if (slider) slider->SetRange(0,Float_t(l)/hepev->GetNRows());
	       c2->Modified();
	       c2->Update();
	     }
	   }
#endif
     // End of Event. Now we can delete it
	   if (event) delete event;
	   event = 0;
	   c1->Modified();
#if 0
	   c2->Modified();
#endif
	   c3->Modified();
	   c4->Modified();
	 }
       }
     }
   }
   root_out->Write();
   printf("\n");
   gBenchmark->Show("hsum");
   printf(" This is a finish \n");
   printf(" You may pick up this example from /afs/rhic/star/packages/dev/StRoot/macros/ana.C\n");
}


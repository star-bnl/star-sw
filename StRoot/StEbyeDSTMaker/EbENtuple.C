{
  //
  // a macro to extract an ntuple from a STAR EbyE DST
  //  cuts are same as those used for pt fluctuation analysis
  //
  // by Jeff Reid
  //

  // load StEbye microDST shared library
  gSystem.Load("libEvent.so");

  Int_t i,j,q;
  Int_t NumberOfTracks;
  StEbyeTrack *currentTrack;

  // define Ntuple variables
  Float_t pp,pm;    // P+ and P-
  Float_t pp2,pm2;    // P+ and P-
  Float_t pp3,pm3;    // P+ and P-
  Float_t pp4,pm4;    // P+ and P-

  Float_t Np,Nm;    // N+ and N-
  Float_t Np2,Nm2;    // N+ and N-
  Float_t Np3,Nm3;    // N+ and N-
  Float_t Np4,Nm4;    // N+ and N-

  Float_t No,Nc;    // N_original and N_centrality

  Float_t p2p,p2m;  // p+,- squared mean
  Float_t p2p2,p2m2;  // p+,- squared mean
  Float_t p2p3,p2m3;  // p+,- squared mean
  Float_t p2p4,p2m4;  // p+,- squared mean

  Float_t nfi,nma,n;   // Nfit, Nmax, Nfit/Nmax
  Int_t eid,rid;       // event and run ids

  Float_t cpt,chi2;   // temporary pt,chi2  variable

  // create the root Ntuple 'EbyeMicroDST'
  TNtuple *n0 = new TNtuple("ntuple","EbyE","rid:eid:pp:pm:p2p:p2m:Np:Nm:No:Nc");
  TNtuple *n2 = new TNtuple("ntuple","EbyE","rid:eid:pp2:pm2:p2p2:p2m2:Np2:Nm2:No:Nc");
  TNtuple *n3 = new TNtuple("ntuple","EbyE","rid:eid:pp3:pm3:p2p3:p2m3:Np3:Nm3:No:Nc");
  TNtuple *n4 = new TNtuple("ntuple","EbyE","rid:eid:pp4:pm4:p2p4:p2m4:Np4:Nm4:No:Nc");

  // histos of inclusive pt and N distributions
  TH1F *jim = new TH1F("ptincl","Jim",200,0,2);
  TH1F *jin = new TH1F("nincl","Jin",200,200,1200);

  TChain chain("EbyeTree");

  // This bit of code processes the directory listing of
  //  the traget analysis directory, finds all of the .ebe.root
  //  files in that directory and adds them to the analysis chain

  Int_t nfiles = 0;
  Char_t *file;
  Char_t path_file[100];
  Char_t dirname[]="~/central/";
  Char_t file_list[5000][128];

  void *dirhandle = gSystem->OpenDirectory(gSystem->ExpandPathName(dirname));
  while (file = gSystem->GetDirEntry(dirhandle)) {
    if (strstr(file,".evebe.root") != 0) {
      strcpy(path_file,dirname);
      strcat(path_file,file);
      sprintf(&file_list[nfiles],"%s",path_file);
      //printf("file %d: %s\n",nfiles,path_file);
      nfiles++;
    }
  }

  printf("%i files found.\n",nfiles);

  // nfiles = 2;

  if (nfiles == 0) {
    printf("No *.ebe.root files found in specified directory\n");
    break;
  }

  printf("Hold on a sec, adding %i files to chain...\n",nfiles);
  for (int q = 0; q < nfiles; q++) {
    chain.Add(file_list[q]);
  }

  //
  //

  //we create the event object once outside the loop
  StEbyeEvent *event = new StEbyeEvent();   

  TClonesArray *tracks = 0;

  chain.SetBranchAddress("EbyeDSTBranch",&event);
  Int_t nevent = chain.GetEntries();

  printf("There are %i events in this DST chain.\n",nevent);
  Int_t nb = 0;
  Int_t counter = 0;

  // ** begin event loop ** //
  for (int i = 0; i < nevent; i++) {

    counter++;    

    nb += chain.GetEvent(i);   //read complete event in memory
    Int_t numberOfTracks = event->OrigMult();
    No = numberOfTracks;
    Nc = event->CentMult();
    tracks = event->Tracks();     //get pointer to the TClonesArray object
    
    // Use iterator to traverse the track list and
    //   extract the track data
    TIter next(tracks);
    
    /// *** cut #1 : z-vertex cut *** ///
    if (fabs(event->Vz()) < 75) {
      eid = event->EventID();
      rid = event->RunID();

      pp = 0; pm = 0;
      p2p = 0; p2m = 0;
      Np = 0; Nm = 0;

      pp2 = 0; pm2 = 0;
      p2p2 = 0; p2m2 = 0;
      Np2 = 0; Nm2 = 0;

      pp3 = 0; pm3 = 0;
      p2p3 = 0; p2m3 = 0;
      Np3 = 0; Nm3 = 0;

      pp4 = 0; pm4 = 0;
      p2p4 = 0; p2m4 = 0;
      Np4 = 0; Nm4 = 0;

      // traverse the track list and access track info
      // ** begin track loop ** //
      for (j = 0; j < numberOfTracks; j++) {

        // get the current track
        currentTrack = (StEbyeTrack*)next();

        // *** cut A : track goodness cut ***
        if (currentTrack->Flag() > 0) {

        nfi = currentTrack->NFitPoints();
        nma = currentTrack->NMaxPoints();
        if (nma > 0) n = (nfi/nma); else n = 0;

        /// *** cut #2 : track splitting cut *** ///
        if (n > 0.5) {

          /// *** cut #3 : eta acceptance cut *** ///
          if (fabs(currentTrack->Eta()) < 1.0) {

            cpt = currentTrack->Pt();
            chi2 = currentTrack->Chi2();

            /// *** cut #4 : pt acceptance cut *** ///
            if ((cpt > 0.150) && (cpt < 2.0)) {
              jim->Fill(cpt);
              
              if (currentTrack->Charge() > 0) {
                pp += cpt; p2p += cpt*cpt; Np++;
                //if (chi2 < 2.0) { pp2 += cpt; p2p2 += cpt*cpt; Np2++; }
                //if (chi2 < 3.0) { pp3 += cpt; p2p3 += cpt*cpt; Np3++; }
                //if (chi2 < 4.0) { pp4 += cpt; p2p4 += cpt*cpt; Np4++; }
              } else {
                pm += cpt; p2m += cpt*cpt; Nm++;
                //if (chi2 < 2.0) { pm2 += cpt; p2m2 += cpt*cpt; Nm2++; }
                //if (chi2 < 3.0) { pm3 += cpt; p2m3 += cpt*cpt; Nm3++; }
                //if (chi2 < 4.0) { pm4 += cpt; p2m4 += cpt*cpt; Nm4++; }
              }     

	    } /// *** cut #4 *** ///
	  } /// *** cut #3 *** ///
	} /// *** cut #2 *** ///
        } /// *** cut A *** ///

      } // ** end track loop ** //

      tracks->Clear();             //clear it

      /// *** cut #5 : minimum quality track cut *** ///
      if ((Np > 0) && (Nm > 0)) {

        pp /= Np;
        pm /= Nm;
        pp2 /= Np2;
        pm2 /= Nm2;
        pp3 /= Np3;
        pm3 /= Nm3;
        pp4 /= Np4;
        pm4 /= Nm4;

        p2p /= Np;
        p2m /= Nm;
        p2p2 /= Np2;
        p2m2 /= Nm2;
        p2p3 /= Np3;
        p2m3 /= Nm3;
        p2p4 /= Np4;
        p2m4 /= Nm4;

        n0->Fill(rid,eid,pp,pm,p2p,p2m,Np,Nm,No,Nc);
        n2->Fill(rid,eid,pp2,pm2,p2p2,p2m2,Np2,Nm2,No,Nc);
        n3->Fill(rid,eid,pp3,pm3,p2p3,p2m3,Np3,Nm3,No,Nc);
        n4->Fill(rid,eid,pp4,pm4,p2p4,p2m4,Np4,Nm4,No,Nc);

        printf("ntuples filled for event %i in run %i - #%i.\n",eid,rid,counter);

        jin->Fill((Np+Nm));

      } /// *** cut #5 *** ///

    } /// *** cut #1 *** ///

  } // ** end event loop ** //

  // write ntuple out to a file

  TFile ntupleFile("jim.root","recreate");

  n0->Write("n0");
  //n2->Write("n2");
  //n3->Write("n3");
  //n4->Write("n4");
  //jim->Write();
  //jin->Write();

  ntupleFile.Close();

}


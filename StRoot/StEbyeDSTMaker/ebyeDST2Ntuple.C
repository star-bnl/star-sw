{

/**********************************************************************
 *
 * $Id: ebyeDST2Ntuple.C,v 1.1.1.1 2000/08/01 13:57:56 jgreid Exp $
 *
 * Author: Jeff Reid, UW, July 2000
 *
 **********************************************************************
 *
 * Description:  This is a ROOT macro for reading in an EbyEDST file once
 *               it has been written out and creating a global variables 
 *               ntuple which is then written out to a file. This macro is
 *               provided as a template for accessing the DST and making
 *               your own ntuples.  This macro operates independent of
 *               STAR software and can be used to read EbyEDSTs from other
 *               experiments.
 *
 **********************************************************************
 *
 * $Log: ebyeDST2Ntuple.C,v $
 * Revision 1.1.1.1  2000/08/01 13:57:56  jgreid
 * EbyE DST creation and access tools
 *
 *
 **********************************************************************/

  gROOT->Reset();

  // load StEbye microDST shared library
  gSystem.Load("libEvent.so");

  Int_t i,j,q;
  Int_t NumberOfTracks;
  StEbyeTrack *currentTrack;

  // define Ntuple variables
  Float_t pp,pm;  // P+ and P-
  Float_t Np,Nm;  // N+ and N-
  Float_t etab;   // eta mean
  Float_t p2p,p2m;  // p+,- squared mean
  Float_t etasqb; // eta squared mean
  Float_t n;      // eventwise mean Nfit/Nmax
  Float_t nfi,nma;     // Nfit, Nmax
  Int_t eid,rid;       // event and run ids
  Float_t z,ctb,zdce,zdcw;  // event vertex, and trigger info

  Float_t ppm,pmm;

  Float_t cpt;   // temporary pt variable

  // create the root Ntuple 'EbyeMicroDST'
  ntuple = new TNtuple("ntuple","EbyE","rid:eid:pp:pm:p2p:p2m:Np:Nm:etab:etasqb:n:z:ctb:zdce:zdcw");

  // open the DST file
  TFile f("EbyeDST.root");

  // Create a timer object to benchmark this loop
  TStopwatch timer;
  timer.Start();

  //we create the event object once outside the loop
  StEbyeEvent *event = new StEbyeEvent();   

  // Get the tree, the branch, and the entries
  ebyeTree = (TTree*)f.Get("EbyeTree");
 
  TClonesArray *tracks = 0;

  TBranch *branch = ebyeTree.GetBranch("EbyeDSTBranch");
  branch->SetAddress(&event);
  Int_t nevent = ebyeTree.GetEntries();

  printf("There are %i events in this DST file.\n",nevent);
  Int_t nb = 0;
  for (i = 0; i < nevent; i++) {
    nb += ebyeTree.GetEvent(i);   //read complete event in memory
    Int_t numberOfTracks = event->OrigMult();
    tracks = event->Tracks();     //get pointer to the TClonesArray object
    
    // Use iterator to traverse the track list and
    //   extract the track data
    TIter next(tracks);
    
    zdcw = event->ZDCw();
    zdce = event->ZDCe();
    ctb = 0;
    for (q = 0; q < 32 ; q++) ctb += event->CTBarray(q);
    z = event->Vz();
    eid = event->EventID();
    rid = event->RunID();

    pp = 0; pm = 0;
    p2p = 0; p2m = 0;
    Np = 0; Nm = 0;
    n = 0;
    etab = 0; etasqb = 0;
    ppm = 0; pmm = 0;
    // traverse the track list and access track info
    for (j = 0; j < numberOfTracks; j++) {

      // get the current track
      currentTrack = (StEbyeTrack*)next();

      nfi = currentTrack->NFitPoints();
      nma = currentTrack->NMaxPoints();
      if (nma > 0) n += (nfi/nma);
      etab += currentTrack->Eta();
      etasqb += (currentTrack->Eta()*currentTrack->Eta());
      cpt = currentTrack->Pt();
      if (currentTrack->Charge() > 0) {
        if (cpt > ppm) ppm = cpt;
        pp += cpt;
        p2p += cpt*cpt;
        Np++;
      } else {
        if (cpt > pmm) pmm = cpt;
        pm += cpt;
        p2m += cpt*cpt;
        Nm++;
      }     

    }

    tracks->Clear();             //clear it

    n /= numberOfTracks;
    etab /= numberOfTracks;
    etasqb /= numberOfTracks;

    Np--;
    Nm--;

    p2p = (p2p-(ppm*ppm));
    p2m = (p2m-(pmm*pmm));
    pp -= ppm;
    pm -= pmm;

    ntuple->Fill(rid,eid,pp,pm,p2p,p2m,Np,Nm,etab,etasqb,n,z,ctb,zdce,zdcw);

    printf("ntuple filled for event %i in run %i.\n",eid,rid);

  }

  // Stop timer and print results
  timer.Stop();
  Float_t mbytes = 0.000001*nb;
  Double_t rtime = timer.RealTime();
  Double_t ctime = timer.CpuTime();
  printf("%d events and %d bytes read.\n",nevent,nb);
  printf("file compression factor = %f\n",f.GetCompressionFactor());
  printf("RealTime=%f seconds, CpuTime=%f seconds\n",rtime,ctime);
  printf("You read %f Mbytes/Realtime seconds\n",mbytes/rtime);
  printf("You read %f Mbytes/Cputime seconds\n",mbytes/ctime);

  // close the DST file
  f.Close();

  // write ntuple out to a file

  TFile ntupleFile("globalVarNtuple.root","recreate");

  // you _must_ set the format to one when using root
  //  on rcf otherwise it will only be readable on rcf!!
  //
  ntupleFile.SetFormat(1);  

  ntuple->Write();

  ntupleFile.Close();

}

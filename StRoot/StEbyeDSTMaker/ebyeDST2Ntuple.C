/**********************************************************************
 *
 * $Id: ebyeDST2Ntuple.C,v 1.6 2000/11/01 22:51:06 jgreid Exp $
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
 * Revision 1.6  2000/11/01 22:51:06  jgreid
 * fixed repeat file-loading bug
 *
 * Revision 1.5  2000/09/15 22:41:17  jgreid
 * made file array even larger
 *
 * Revision 1.4  2000/09/15 22:19:04  jgreid
 * made filename array larger in ebyeDST2Ntuple.C
 *
 * Revision 1.3  2000/09/15 16:41:38  jgreid
 * bug fix in multiple file name handler
 *
 * Revision 1.2  2000/09/01 22:59:11  jgreid
 * version 1 revision ; multiple file handling + additional data members added
 *
 * Revision 1.1.1.1  2000/08/01 13:57:56  jgreid
 * EbyE DST creation and access tools
 *
 *
 **********************************************************************/

void ebyeDST2Ntuple() {
  printf("Usage : ebyeDST2Ntuple.C(\"directory\")\n");
}

void ebyeDST2Ntuple(char *dirname) {

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

  TChain chain("EbyeTree");

  // This bit of code processes the directory listing of
  //  the traget analysis directory, finds all of the .ebe.root
  //  files in that directory and adds them to the analysis chain

  Int_t nfiles = 0;
  Char_t *file;
  Char_t path_file[100];
  //Char_t dirname[]="~/pwg/dst/aug/";
  Char_t file_list[5000][128];

  void *dirhandle = gSystem->OpenDirectory(gSystem->ExpandPathName(dirname));
  while (file = gSystem->GetDirEntry(dirhandle)) {
    if (strstr(file,".ebe.root") != 0) {
      strcpy(path_file,dirname);
      strcat(path_file,file);
      sprintf(&file_list[nfiles],"%s",path_file);
      printf("file %d: %s\n",nfiles,path_file);
      nfiles++;
    }
  }

  if (nfiles == 0) {
    printf("No *.ebe.root files found in specified directory\n");
    break;
  }

  printf("Hold on a sec, adding %i files to chain...\n",nfiles);
  for (int q = 0; q < nfiles; q++) {
    chain.Add(file_list[q]);
    printf(".");
  }

  //
  //

  //we create the event object once outside the loop
  StEbyeEvent *event = new StEbyeEvent();   

  TClonesArray *tracks = 0;

  TBranch *branch = chain.GetBranch("EbyeDSTBranch");
  branch->SetAddress(&event);
  Int_t nevent = chain.GetEntries();

  printf("There are %i events in this DST chain.\n",nevent);
  Int_t fCurrent = chain.GetTreeNumber();
  Int_t nb = 0;

  for (int i = 0; i < nevent; i++) {

    // here we load the Tree, determine which file it is in
    //  get the branch from that file and connect the local
    //  copy of event to it to access the data
   
    Int_t centry = chain.LoadTree(i);
    if (chain.GetTreeNumber() != fCurrent) {
       fCurrent = chain.GetTreeNumber();
       branch = chain.GetBranch("EbyeDSTBranch");
       branch->SetAddress(&event);
    }

    nb += chain.GetEvent(i);   //read complete event in memory
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

  // write ntuple out to a file

  TFile ntupleFile("globalVarNtuple.root","recreate");

  // you _must_ set the format to one when using root
  //  on rcf otherwise it will only be readable on rcf!!
  //
  ntupleFile.SetFormat(1);  

  ntuple->Write();

  ntupleFile.Close();

}

TChain *fChain = 0;
void TMerge(Int_t N1 = 1, Int_t N2 = 5000) {  
  Char_t *TreeName = "dEdxT";
#if 1
  gInterpreter->ProcessLine(Form(".X %s.C",TreeName));
  dEdxT *event = new dEdxT();
#endif
  TIter  next(gROOT->GetListOfFiles());
  TFile *file = 0;
  Int_t N = 0;
  Int_t n2 = 0;
  Int_t Ntotal = 0;
  Long_t Size = 0;
  fChain = new TChain(TreeName);
#if 1
  //  fChain->SetBranchAddress(TreeName, &event);
  event->Init(fChain);
#endif
  while ( (file = (TFile*)next()) ) {    // bad runs  1228*       
    if (!strstr(file->GetName(),".root")) continue;
    N++;
    //    cout << "File " << N << ":" << file->GetName() << endl;
    if (N1 != 0 && N < N1) continue;
    if (N2 != 0 && N > N2) break;
    Long_t id, size, flags, modtime;
    gSystem->GetPathInfo(file->GetName(), &id, &size, &flags, &modtime);
    Size += size;
    printf("\nFile %s \tsize \t%i \tSize \t%i\t\n",file->GetName(),size, Size);
    if (Size > 1900000000) break;
    printf("Open :\t%i ",N);
    TTree *tree = (TTree *) file->Get(TreeName);
    if (!tree) {continue;}
    UInt_t nEvents = tree->GetEntries();
    Ntotal += nEvents;
    fChain->Add(file->GetName());
    printf("\tchained with %i events",nEvents);
    n2 = N;
  }
  Char_t line[80] = Form("%s_%04i_%04i.root",TreeName,N1,n2);
  printf("\nMerge from %04i to %04i to %s\n",N1,n2,line);
  fChain->Merge(line);
  printf("Chained %i events\n",Ntotal); //  1.824.869.221
}

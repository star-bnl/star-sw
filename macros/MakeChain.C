const Char_t *TreeName="dEdxT";
class dEdxT;
dEdxT *event = 0;
TCahin *chain = 0;
//___________________________________
void dEdxTChain() {// make chain from attached files
  TChain *chain = 0;
  if (! TreeName) return;
  TString cmd(".L B");
  cmd += TreeName;
  cmd += ".C++";
  cout << "Load " << cmd.Data() << endl;
  gInterpreter->ProcessLine(cmd.Data());
  
#if 0
  TObject *event = 0;
  TClass *cl = gROOT->GetClass(TreeName);
  if (cl) {
    event = (TObject *)cl->New();
    if (event) {
      if (name && strlen(name)) event->SetName(name);
      if (title) event->SetTitle(title);
    }
    chain = new TChain(TreeName);
    chain->SetBranchAddress(TreeName, &event);
  }
#else
  event = new dEdxT();
  chain = new TChain(TreeName);
  chain->SetBranchAddress(TreeName, &event);
#endif
  TIter  next(gROOT->GetListOfFiles());
  TFile *file = 0;
  Int_t N = 0, Size = 0, Ntotal = 0;
  while ( (file = (TFile*)next()) ) {    // bad runs  1228*       
    if (!strstr(file->GetName(),".root")) continue;
    N++;
    //    cout << "File " << N << ":" << file->GetName() << endl;
    Long_t id, size, flags, modtime;
    gSystem->GetPathInfo(file->GetName(), &id, &size, &flags, &modtime);
    Size += size;
    printf("\nFile %s \tsize \t%i \tSize \t%i\t",file->GetName(),size, Size);
    printf("Open :\t%i ",N);
    TTree *tree = (TTree *) file->Get(TreeName);
    if (!tree) {continue;}
    UInt_t nEvents = tree->GetEntries();
    Ntotal += nEvents;
    chain->Add(file->GetName());
    printf("\tchained with %i events",nEvents);
  }
  //  chain->Merge(line);
  printf("Chained %i events\n",Ntotal); //  1.824.869.221
  
  return;
}

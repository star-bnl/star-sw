void Example_tags(const char* topDir = "/star/rcf/GC/daq/tags/")
{
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// Example_tags.C                                                       //
//                                                                      //
// shows how to use the STAR tags files                                 //
// Input: top level directory (must end with "/")                       //
//                                                                      //
// what it does:                                                        //
// 1. creates TChain from all tags files down from the topDir           //
// 2. loops over all events in the chain                                //
//                                                                      //
// owner: Alexandre V. Vaniachine <AVVaniachine@lbl.gov>                //
//////////////////////////////////////////////////////////////////////////

  gSystem->Load("libStar");
  gSystem->Load("St_base");
  // start benchmarks
  gBenchmark = new TBenchmark();
  gBenchmark->Start("total");
   
  // set loop optimization level
  gROOT->ProcessLine(".O4");
  // gather all files from the same top directory into one chain
  St_FileSet dirs(topDir);
  St_DataSetIter next(&dirs,0);
  St_DataSet *set = 0; 
  TChain chain("Tag");
  while ( (set = next()) ) {           
    if (strcmp(set->GetTitle(),"file") || !(strstr(set->GetName(),".tags.root"))) continue;
    TString p = set->Path();
    Char_t *tagsfile = gSystem->ConcatFileName(topDir,p.Data());
    chain.Add(tagsfile);
  } 
  cout<<"chained "<<chain->GetEntries()<<" events "<<endl;

  TObjArray *files = chain.GetListOfFiles();
  UInt_t nFiles = files->GetEntriesFast();
  cout << "chained " << nFiles << " files from " << topDir << endl;

  TObjArray *branches = chain.GetListOfBranches();
  TObjArray *leaves = chain.GetListOfLeaves();
  Int_t nleaves = leaves->GetEntriesFast();

  TString file;
  Int_t l;
  TLeaf *leaf;
  TBranch *branch;

  TString *tName = new TString(" ");
  TNamed *tableName;
  TObjArray *tagTable = new TObjArray;
  Int_t tableCount = 0;
  Int_t *tableIndex = new Int_t[nleaves];
  Int_t tagCount = 0;

  //count number of tag tables encoded in the TTree branch names
  for (Int_t l=0;l<nleaves;l++) {
    leaf = (TLeaf*)leaves->UncheckedAt(l);
    tagCount+=leaf->GetNdata();
    branch = leaf->GetBranch();
    //new tag table name
    if ( strstr(branch->GetName(), tName->Data()) == 0 ) {
      tName = new TString(branch->GetName());
      tName->Resize(tName->Index("."));
      //the tableName is encoded in the branch Name before the "."
      tableName = new TNamed(tName->Data(),"Tag");
      tagTable->AddLast(tableName);
      tableCount++;
    }
    tableIndex[l]=tableCount-1;
  }

  cout << " tot num tables,tags = " << tableCount << "   " 
       << tagCount << endl << endl;

  //print out names of all tags and the first value
  for (l=0;l<nleaves;l++) {
    leaf = (TLeaf*)leaves->UncheckedAt(l);
    branch = leaf->GetBranch();
    branch->GetEntry();
    //tag comment is in the title
    TString Title = leaf->GetTitle();
    Int_t dim = leaf->GetNdata();
    if (dim==1) {
      Title.ReplaceAll('['," '"); 
      Title.ReplaceAll(']',"'"); 
    }
    cout << "\n Table: ";
    cout.width(10);
    cout << ((TNamed*)tagTable->UncheckedAt(tableIndex[l]))->GetName()<< 
      " -- has tag: " << Title<<endl;
    for (Int_t i=0;i<dim;i++) {
      cout <<"                               "<< leaf->GetName();
      if (dim>1) cout << '['<<i<<']';
      cout << " = " << leaf->GetValue(i) << endl; 
    }
  }

  //example of a plot:
  c1 = new TCanvas("c1","Beam-Gas Rejection",600,1000);
  gStyle->SetMarkerStyle(8);
  chain->Draw("n_trk_tpc[0]:n_trk_tpc[1]");

  ntrk = chain.GetLeaf("n_trk_tpc");
  TLeaf *event = chain.GetLeaf("mEventNumber");
  TLeaf *ivent = chain.GetLeaf("mIventNumber");
  TBranch *ebranch = event->GetBranch();

  //example of a selection: write event numbers for collisions on the plot
  Int_t ncoll=0;
  char aevent[10];
  TText t(0,0,"a");
  t.SetTextFont(52);
  t.SetTextSize(0.02);
  Float_t cut = 0.35;
  //loop over all events: READ ONLY REQUIRED BRANCHES!
  gBenchmark->Start("loop");
  for (Int_t i=0;i<nFiles;i++) {
    file = (files->UncheckedAt(i))->GetTitle();
    cout <<"Start of file "<< i+1 <<": "<< file.Data() <<endl;
    chain.LoadTree(*(chain.GetTreeOffset()+i));
    TTree *T = chain.GetTree();
    //must renew leaf pointer for each tree
    ntrk = T->GetLeaf("n_trk_tpc");
    event = T->GetLeaf("mEventNumber");
    ivent = T->GetLeaf("mIventNumber");
    for (Int_t j = 0; j<T->GetEntries();j++){
      ntrk->GetBranch()->GetEntry(j);
      event->GetBranch()->GetEntry(j);
      ivent->GetBranch()->GetEntry(i);
      
      Int_t Nm=ntrk->GetValue(0);
      Int_t Np=ntrk->GetValue(1);
      Int_t Ntrk = Np+Nm;
      Float_t asim=(Np-Nm)/float(Np+Nm);
      if (-cut < asim&&asim < cut && Ntrk>400) {
	cout
	  <<" bfc event number for this file = "<<ivent->GetValue()
	  <<" DAQ event number = "<<event->GetValue()
	  <<endl;
	ncoll++;
	sprintf(aevent,"%d",event->GetValue());
	t.DrawText(Np+10,Nm+10,aevent);
      }
    }
  }
  gBenchmark->Stop("loop");
  t.SetTextSize(0.05);
  t.DrawText(50,2550,"ntrk>400 and |(Np-Nm)/(Np+Nm)| < 0.35 ");
  t.DrawText(500,-300,"Ntrk with tanl<0 ");
  cout <<" number of events with ntrk>400 and |asim|<"<<cut<<" = "<<ncoll<<endl;
  // stop timer and print benchmarks
  gBenchmark->Print("loop");  
  gBenchmark->Stop("total");
  gBenchmark->Print("total");  
}

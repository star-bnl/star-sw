void Example_tags(TString topDir = "/star/rcf/GC/daq/tags")
{
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// Example_tags.C                                                       //
//                                                                      //
// shows how to use the STAR tags files                                 //
// Input: top level directory                                           //
//                                                                      //
// what it does:                                                        //
// 1. creates TChain from all tags files down from the topDir           //
// 2. loops over all events in the chain                                //
//                                                                      //
// owner: Alexandre V. Vaniachine <AVVaniachine@lbl.gov>                //
//////////////////////////////////////////////////////////////////////////

  gSystem->Load("libTable");
  gSystem->Load("St_base");
  // start benchmarks
  gBenchmark = new TBenchmark();
  gBenchmark->Start("total");
   
  // set loop optimization level
  gROOT->ProcessLine(".O4");
  // gather all files from the same top directory into one chain
  // topDir must end with "/"
  topDir +='/';
  St_FileSet dirs(topDir);
  St_DataSetIter next(&dirs,0);
  St_DataSet *set = 0; 
  TChain chain("Tag");
  while ( (set = next()) ) {           
    if (strcmp(set->GetTitle(),"file") || 
	!(strstr(set->GetName(),".tags.root"))) continue;
    chain.Add(gSystem->ConcatFileName(topDir,set->Path()));
  }
  UInt_t nEvents = chain->GetEntries();
  cout<<"chained "<<nEvents<<" events "<<endl;

  TObjArray *files = chain.GetListOfFiles();
  UInt_t nFiles = files->GetEntriesFast();
  cout << "chained " << nFiles << " files from " << topDir << endl;

  TObjArray *leaves = chain.GetListOfLeaves();
  Int_t nleaves = leaves->GetEntriesFast();

  TString tableName = " ";
  TObjArray *tagTable = new TObjArray;

  Int_t tableCount = 0;
  Int_t *tableIndex = new Int_t[nleaves];
  Int_t tagCount = 0;

  // decode tag table names
  for (Int_t l=0;l<nleaves;l++) {
    TLeaf *leaf = (TLeaf*)leaves->UncheckedAt(l);
    tagCount+=leaf->GetNdata();
    TBranch *branch = leaf->GetBranch();
    // new tag table name
    if ( strstr(branch->GetName(), tableName.Data()) == 0 ) {
      tableName = branch->GetName();
      // the tableName is encoded in the branch Name before the "."
      tableName.Resize(tableName->Last('.'));
      tagTable->AddLast(new TObjString(tableName.Data()));
      tableCount++;
    }
    tableIndex[l]=tableCount-1;
  }
  cout << " tot num tables, tags = " << tableCount << "   " 
       << tagCount << endl << endl;

  //EXAMPLE 1: how to print out names of all tags and values for first event
  for (l=0;l<nleaves;l++) {
    leaf = (TLeaf*)leaves->UncheckedAt(l);
    branch = leaf->GetBranch();
    branch->GetEntry();
    // tag comment is in the title
    TString Title = leaf->GetTitle();
    Int_t dim = leaf->GetNdata();
    if (dim==1) {
      Title.ReplaceAll('['," '"); 
      Title.ReplaceAll(']',"'"); 
    }
    cout << "\n Table: ";
    cout.width(10);
    cout << ((TObjString*)tagTable->UncheckedAt(tableIndex[l]))->GetString()
	 <<" -- has tag: " << Title << endl;
    for (Int_t i=0;i<dim;i++) {
      cout <<"                               "<< leaf->GetName();
      if (dim>1) cout << '['<<i<<']';
      cout << " = " << leaf->GetValue(i) << endl; 
    }
  }

  // EXAMPLE 2: how to make a plot
  c1 = new TCanvas("c1","Beam-Gas Rejection",600,1000);
  gStyle->SetMarkerStyle(8);
  chain->Draw("n_trk_tpc[0]:n_trk_tpc[1]");

  // EXAMPLE 3: how to make a selection (write selected event numbers on the plot)
  Int_t ncoll=0;
  char aevent[10];
  TText t(0,0,"a");
  t.SetTextFont(52);
  t.SetTextSize(0.02);
  Float_t cut = 0.35;
  cout <<"\n Events with ntrk>400 and |asim|<"<<cut<<endl;
  //loop over all events: READ ONLY n_trk_tpc AND mEventNumber BRANCHES!
  gBenchmark->Start("loop");
  for (Int_t i=0;i<nFiles;i++) {
    chain.LoadTree(*(chain.GetTreeOffset()+i));
    TTree *T = chain.GetTree();
    //must renew leaf pointer for each tree
    TLeaf *ntrk = T->GetLeaf("n_trk_tpc");
    TLeaf *run = T->GetLeaf("mRunNumber");
    TLeaf *event = T->GetLeaf("mEventNumber");
    for (Int_t j=0; j<T->GetEntries(); j++){
      ntrk->GetBranch()->GetEntry(j);
      event->GetBranch()->GetEntry(j);
      run->GetBranch()->GetEntry(j);
      Int_t Nm=ntrk->GetValue(0);
      Int_t Np=ntrk->GetValue(1);
      Int_t Ntrk = Np+Nm;
      // avoid division by 0
      Float_t asim = Np-Nm;
      if (Ntrk>0) asim /= Ntrk;
      if (-cut < asim&&asim < cut && Ntrk>400) {
	cout<<"   Run "<<(UInt_t)run->GetValue()
	    <<", Event "<<event->GetValue() <<endl;
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
  cout << " Selected " << ncoll << " collision candidates out of "
       << nEvents << " events" << endl;
  // stop timer and print benchmarks
  gBenchmark->Print("loop");  
  gBenchmark->Stop("total");
  gBenchmark->Print("total");  
}

void Example_tags()
{
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// Example_tags.C                                                       //
//                                                                      //
// shows how to use the STAR tags files                                 //
// Input: directory                                                     //
// used to select all tags files in the same directory                  //
//                                                                      //
// what it does:                                                        //
// 1. creates ROOT TChain from all selected tagDB files                 //
// 2. loops over all events in the chain                                //
//                                                                      //
// owner: Alexandre V. Vaniachine <AVVaniachine@lbl.gov>                //
//////////////////////////////////////////////////////////////////////////


  // start benchmarks
  gBenchmark = new TBenchmark();
  gBenchmark->Start("total");
   
  // set loop optimization level
  gROOT->ProcessLine(".O4");
  // gather all files from the same Run into one chain
  TChain chain("Tag");

  chain.Add("st_physics_1164056_raw_0003.tags.root");
//   chain.Add("/star/rcf/test/dst/run9/st_physics_1164056_raw_0001.tags.root");
//   chain.Add("/star/rcf/test/dst/run9/st_physics_1164056_raw_0002.tags.root");
//   chain.Add("/star/rcf/test/dst/run9/st_physics_1164056_raw_0003.tags.root");
//   chain.Add("/star/rcf/test/dst/run9/st_physics_1164056_raw_0004.tags.root");

  cout<<"chained "<<chain->GetEntries()<<" events "<<endl;

  TObjArray *files = chain.GetListOfFiles();
  cout<<"chained "<<files->GetEntriesFast()<<" files from the tagDB"<<endl;

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

    Int_t dim = leaf->GetNdata();
    for (Int_t i=0;i<dim;i++) {
      cout << 
	" Table#,name: " << tableIndex[l] <<
	", " << ((TNamed*)tagTable->UncheckedAt(tableIndex[l]))->GetName()<< 
	" -- has  tag: " << leaf->GetName();
      if (dim>1) cout << '['<<i<<']';
      cout << " = " << leaf->GetValue(i) << endl; 
      
    }
  }

  //example of a histogram:
  c1 = new TCanvas("c1","Beam-Gas Rejection",600,1000);
  gStyle->SetMarkerStyle(8);
  chain->Draw("n_trk_tpc[0]:n_trk_tpc[1]");
  //   return;

  gBenchmark->Start("loop");

  leaf = chain.GetLeaf("n_trk_tpc");
  TLeaf *event = chain.GetLeaf("mEventNumber");
  TLeaf *ivent = chain.GetLeaf("mIventNumber");
  TBranch *ebranch = event->GetBranch();

  //count collisions
  Int_t ncoll=0;
  char aevent[10];
  //loop over all events
  for (Int_t k=0;k<chain->GetEntries();k++) {
    chain.GetEntry(k);
    //print values only for the first event in each file of the chain
    if (k == *(chain.GetTreeOffset()+chain.GetTreeNumber())) {
      file = (files->UncheckedAt(chain.GetTreeNumber()))->GetTitle();
      cout<<"chain event "<< k 
	  <<", start of file "<< chain.GetTreeNumber()+1 <<": "<< file.Data() <<endl;
      
      leaf = chain.GetLeaf("n_trk_tpc");
      event = chain.GetLeaf("mEventNumber");
      ivent = chain.GetLeaf("mIventNumber");
    }
    Int_t Nm=leaf->GetValue(0);
    Int_t Np=leaf->GetValue(1);
    Int_t ntrk = Np+Nm;
    Float_t asim=(Np-Nm)/float(Np+Nm);
    
    TText t(0,0,"a");
    t.SetTextFont(52);
    t.SetTextSize(0.02);
    Float_t cut = 0.35;
    
    if (-cut<asim && asim< cut && ntrk>400) {
      cout
 	<<" bfc event number for this file = "<<ivent->GetValue()
	<<" DAQ event number = "<<event->GetValue()
	<<endl;
      ncoll++;
      sprintf(aevent,"%d",event->GetValue());
      t.DrawText(Np+10,Nm+10,aevent);
    }
  }

  gPad->Update();
  t.SetTextSize(0.05);
  t.DrawText(50,2550,"ntrk>400 and |(Np-Nm)/(Np+Nm)| < 0.35 ");
  t.DrawText(500,-300,"Ntrk with tanl<0 ");
  
  cout <<" number of events with ntrk>400 and |asim|<"<<cut<<" = "<<ncoll<<endl;

  // stop timer and print benchmarks
  gBenchmark->Stop("loop");
  gBenchmark->Print("loop");  
  gBenchmark->Stop("total");
  gBenchmark->Print("total");  
}

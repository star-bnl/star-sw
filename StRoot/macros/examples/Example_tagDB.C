void Example_tagDB(const Char_t * selection = NULL)
{
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// Example_tagDB.C                                                      //
//                                                                      //
// shows how to use the STAR MDC3 tag database                          //
// Input: selection                                                     //
// used to select all datasets with names matching the selection string //
//                                                                      //
// what it does:                                                        //
// 1. selects tagDB files from the fileCatalog database in mysql        //
// 2. creates ROOT TChain from all selected tagDB files                 //
// 3. loops over all events in the chain                                //
//                                                                      //
// owner: Alexandre V. Vaniachine <AVVaniachine@lbl.gov>                //
//////////////////////////////////////////////////////////////////////////


  // start benchmarks
  gBenchmark = new TBenchmark();
  gBenchmark->Start("total");
   
  TSQLServer *db = TSQLServer::
       Connect("mysql://duvall.star.bnl.gov/fcMDC3", "", "");

  TSQLResult *res;
  TSQLRow *row;
  TStringLong query= "SELECT CONCAT(path,'/',fName)
                      FROM fileCatalog
                      WHERE fName LIKE '%tags.root'
                      AND readable='Y'
                      AND dataset LIKE '%";
  if (selection!=NULL)
    query+=selection;
  query+="%'";

  res = db->Query(query.Data());
  Int_t nFiles = res->GetRowCount();

  cout<<"Selected "<<nFiles<<" files from the tagDB"<<endl;
  if  (nFiles==0) return;

  // set loop optimization level
  gROOT->ProcessLine(".O4");
  // gather all files from the same Run into one chain for loading to tagDB
  TChain chain("Tag");
  TString fileName;
  UInt_t nF=0;
  while ((row = res->Next())) {
    fileName=row->GetField(0);
    fileName.ReplaceAll("/home/starreco/reco","/star/rcf/GC/tags");

    nF++;
    if (nF%100==0)
      cout<<"Chained "<<nF<<" files, Total events = "
	  <<chain->GetEntries()<<endl;

    //cout<<"trying "<<fileName<<endl;
    chain.Add(fileName.Data());
    }
  cout<<"chained "<<chain->GetEntries()<<" events from the tagDB"<<endl;

  //close the db connection
  db->Close();

  TObjArray *files = chain.GetListOfFiles();
  cout<<"chained "<<files->GetEntriesFast()<<" files from the tagDB"<<endl;

  //  return;

  TObjArray *branches = chain.GetListOfBranches();
  TObjArray *leaves = chain.GetListOfLeaves();
  Int_t nleaves = leaves->GetEntriesFast();

  TString file;
  Int_t l;
  TLeaf *leaf;
  TBranch *branch;

  //print out names of all tags
  for (l=0;l<nleaves;l++) {
    leaf = (TLeaf*)leaves->UncheckedAt(l);
    branch = leaf->GetBranch();
    cout<< "StructureName: "<<branch->GetName()
	<< ", tagName: "<<leaf->GetName()<<endl;
  }

  //example of a histogram:
  chain->Draw("NLa");
  gPad->Update();
  //return;

  gBenchmark->Start("loop");
  //loop over all events
  for (Int_t k=0;k<chain->GetEntries();k++) {

    //test that all events can be read in (read in all the tags)
    chain->GetEntry(k);

    //print values only for the first event in each file of the chain
    if (k == *(chain.GetTreeOffset()+chain.GetTreeNumber())) {
      file = (files->UncheckedAt(chain.GetTreeNumber()))->GetTitle();
      cout<<"chain event "<< k 
	  <<", start of file "<< chain.GetTreeNumber()+1 <<endl;
//       cout<<": "<< file.Data() <<endl;

      //printout NLa value
      leaf = chain.GetLeaf("NLa");
      cout<<leaf->GetName()<<" = "<<leaf->GetValue()<<endl;
	
      //uncomment lines below to printout all tag values
//       branches = chain.GetListOfBranches();//renew pointers for each file
//       leaves = chain.GetListOfLeaves();
//       for (l=0;l<nleaves;l++) {
// 	leaf = (TLeaf*)leaves->UncheckedAt(nleaves-l-1);
// 	branch = leaf->GetBranch();
// 	if(branches->IndexOf(branch)!=2)
// 	  cout<<leaf->GetName()<<" = "<<leaf->GetValue()<<endl;
//       }
    }
  }
  
  // stop timer and print benchmarks
  gBenchmark->Stop("loop");
  gBenchmark->Print("loop");  
  gBenchmark->Stop("total");
  gBenchmark->Print("total");  
}

// $Id: bfcread_tagsBranch.C,v 1.3 2000/03/21 15:45:08 kathy Exp $
// $Log $

//======================================================================
// owner:  Kathy Turner 
//   taken from code provided by Sasha Vanyashin
//
// what it does:  reads .tags.root file produced from bfc & prints out info
//                - a file name is given as input to the macro
//                - this is a flat file (a TTree file)
//                - for first event, prints out values of all tables & tags
//                - for rest of events, counts # tags for each table
//
// branches -> tables
// leaves   -> entries (tags) in tables
//
//=======================================================================

void bfcread_tagsBranch(const char *MainFile=
 "/afs/rhic/star/data/samples/gstar.tags.root")
{
  // fill tags table

  // start timer
  TStopwatch timer;
  timer.Start();

  cout << " Input Tags File Name = " << MainFile << endl;
   

  // gather all files from the same Run into one chain for loading to tagDB
  // can .Add more on here and then we will loop over them all 

  TChain chain("Tag");
  chain.Add(MainFile);

  cout << "   Total # events  = " << chain->GetEntries() << endl;

  TObjArray *files = chain.GetListOfFiles();
  TObjArray *branches = chain.GetListOfBranches();
  TObjArray *leaves = chain.GetListOfLeaves();

  Int_t nleaves = leaves->GetEntriesFast();
  Int_t nbranches = branches->GetEntriesFast();
  cout << "    tot num tables,tags = " << nbranches << "   " << nleaves << endl;

  TString file;

//Loop over entries (events written to the tags.root file)
//test that all events can be read in & print out values for first event

  Int_t countEvents=0;
  Int_t countTables=0;
  Int_t countTagsTot[4]={0,0,0,0};

  for (Int_t k=0;k<chain->GetEntries();k++) {

    chain->GetEntry(k);

    countEvents++;

    //print values only for the first event in each fileof the chain
    //    if (k == *(chain.GetTreeOffset()+chain.GetTreeNumber()))

	file = (files->UncheckedAt(chain.GetTreeNumber()))->GetTitle();
        if (!k) cout <<"    now reading file: " << file.Data() << endl;

        cout <<" ----- Event # " << countEvents << endl;
    
	//must renew leaves for each file
	leaves = chain.GetListOfLeaves();

// Now loop over leaves (values or tags in tables)

        Int_t countTags[4]={0,0,0,0};

	for (Int_t l=0;l<nleaves;l++) {
	  leaf = (TLeaf*)leaves->UncheckedAt(l);
          branch = leaf->GetBranch();

          countTags[branches->IndexOf(branch)]++;
          countTagsTot[branches->IndexOf(branch)]++;
         
 
//print out branch,tag name and value - first event only
	  if (!k) 
          { 
            cout << 
	      " QAInfo: table#,name: " << branches->IndexOf(branch) <<
	      ", " << branch->GetName() << 
              " -- has  tag: " << leaf->GetName() <<
              " = " << leaf->GetValue() << endl; 
	  }
	}

// print out for all events
        for (Int_t m=0; m<4; m++){
          cout << "  table "<< m << " has " << countTags[m] << " tags" << endl;
          countTables++;
        }
	
  }

  cout << " QAInfo:  Read total # events = " << countEvents << endl;
  //  cout << " QAInfo:  Read total # tables = " << countTables << endl;
  countTables /= countEvents;
  cout << " QAInfo:  #tables/event = " << countTables << endl;

  for (Int_t j=0; j<4; j++){
    countTagsTot[j] /= countEvents;
     cout << " QAInfo: table "<< j << " had " << countTagsTot[j] << " tags per event" <<endl;
  }

  // stop timer and print results
  timer.Stop();
  cout<<"RealTime="<<timer.RealTime()<<" seconds, CpuTime="<<timer.CpuTime()<<" seconds"<<endl;
}




// $Id: bfcread_tagsBranch.C,v 1.11 2000/05/25 18:32:07 kathy Exp $
// $Log $

//======================================================================
// code written by: Sasha Vanyashin
// macro owner:  Kathy 
//
// what it does:  reads .tags.root file produced from bfc & prints out info
//                - a file name is given as input to the macro
//                - this is a flat file (a TTree file)
//                - for given #events, prints out values of all tables & tags
//                - for rest of events, counts # tags for each table
//
// branches -> tables
// leaves   -> entries (tags) in tables
//
//  Inputs to macro:
//     MainFile - input *.tags.root file
//     printEvent - event # to print out details for
//     fname - output file name to write QAInfo
//
//=======================================================================

void bfcread_tagsBranch( 
  const char *MainFile="/afs/rhic/star/data/samples/gstar.tags.root",
  Int_t printEvent=1,
  const char *fname="qa_tags.out") 
{
  // start timer
  TStopwatch timer;
  timer.Start();

  cout << endl << endl;
  cout << " bfcread_tagsBranch.C: input file  = " << MainFile << endl;
  cout << " bfcread_tagsBranch.C: print #evts = " << printEvent << endl;
  cout << " bfcread_tagsBranch.C: output file = " << fname << endl;
  cout << endl << endl;

  ofstream fout(fname);

  fout << endl << endl;
  fout << " bfcread_tagsBranch.C: input file  = " << MainFile << endl;
  fout << " bfcread_tagsBranch.C: print evt#  = " << printEvent << endl;
  fout << " bfcread_tagsBranch.C: output file = " << fname << endl;
  fout << endl << endl;

  TFile *file = TFile::Open(MainFile);
  TTree *tree = (TTree*)file->Get("Tag");

  Int_t nEntries = tree->GetEntries();

  TObjArray *leaves = tree->GetListOfLeaves();
  Int_t nleaves = leaves->GetEntriesFast();

  Float_t AcntLeaf0 = 0;
  Float_t AsumLeaf0 = 0;

  Int_t countEvents=nEntries;
  Int_t countLeaves = nleaves;
  Int_t countTags = 0;

  TBranch *branch;

  cout <<" Now reading file: " << file->GetName() << endl;
  fout <<" Now reading file: " << file->GetName() << endl;


// Now loop over leaves (to get values of tags)

	for (Int_t l=0;l<nleaves;l++) {

	  leaf = (TLeaf*)leaves->UncheckedAt(l);
	  branch = leaf->GetBranch();

	    Int_t dim = leaf->GetNdata();

	    cout << " QAInfo: leaf #, # dimensions(tags) = " 
                    << l << "  " << dim << endl;
	    fout << " QAInfo: leaf #, # dimensions(tags) = " 
                    << l << "  " << dim << endl;


// sums for all events, all dimensions
	  for (Int_t nev=0; nev<nEntries; nev++) {
	    branch->GetEntry(nev);

	    Int_t ndim = leaf->GetNdata();
	    for (Int_t ij=0;ij<ndim;ij++) {

              if (l==0){
                AcntLeaf0++;
                AsumLeaf0 += leaf->GetValue(ij);
              }

	    } 	
	  }

// print out full listing for # printEvents only:

	  for (Int_t k=0; k<printEvent; k++) {
	    branch->GetEntry(k);

            Int_t ik = k+1;
	    cout << " QAInfo:  event # " << ik << endl; 
	    fout << " QAInfo:  event # " << ik << endl; 

	    Int_t numdim = leaf->GetNdata();

	    for (Int_t i=0;i<numdim;i++) {

	      cout << " QAInfo:   tag: " << leaf->GetName();
	      if (dim>1) cout << '['<<i<<']';
	      cout << " = " << leaf->GetValue(i) << endl; 
	      
	      fout << " QAInfo:   tag: " << leaf->GetName();
	      if (dim>1) fout << '['<<i<<']';
	      fout << " = " << leaf->GetValue(i) << endl; 

	      countTags++;
	    }
 	  }


        }

// end of loop over all leaves

	cout << endl << endl << 
	  " QAInfo:  total # events = " << countEvents << endl;
	fout << endl << endl << 
	  " QAInfo:  total # events = " << countEvents << endl;

	cout << " QAInfo:   tot num leaves = " << countLeaves << endl;
	fout << " QAInfo:   tot num leaves = " << countLeaves << endl;

	cout << " QAInfo:   tot num tags = " << countTags << endl;
	fout << " QAInfo:   tot num tags = " << countTags << endl;


        AsumLeaf0 /= AcntLeaf0;

	cout << endl << endl;
        cout << " QAInfo: Average of all events & tags for Leaf 0 = " 
              << AsumLeaf0 << endl;

	fout << endl << endl;
        fout << " QAInfo: Average of all events & tags for Leaf 0 = " 
              << AsumLeaf0 << endl;

  // stop timer and print results
  timer.Stop();
  cout<< endl << endl <<"RealTime="<<timer.RealTime()<<
       " seconds, CpuTime="<<timer.CpuTime()<<" seconds"<<endl;

  //cleanup
  file->Close();
  fout.close();
}






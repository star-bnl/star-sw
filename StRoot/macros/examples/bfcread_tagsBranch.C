// $Id: bfcread_tagsBranch.C,v 1.12 2000/06/06 21:14:42 kathy Exp $
// $Log: bfcread_tagsBranch.C,v $
// Revision 1.12  2000/06/06 21:14:42  kathy
// updated to print out more info about tags
//
//
//======================================================================
// code written by: Sasha Vanyashin
// macro owner:  Kathy 
//
// what it does:  reads .tags.root file produced from bfc & prints out info
//                - a file name is given as input to the macro
//                - this is a flat file (a TTree file)
//                - prints out the branch name, leaf name & # tags
//                - for given event#, prints out values of all tags
//
// branches -> TTree tables --- get a branch (table) from each TagsMaker
//      (table names are encoded in branch.leaf names)
// leaves   -> many leaves can be in each branch
// tags     -> many tags (dimensions) can be in each leaf
//
//  loop over leaves, then #events, then tags 
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
  cout << " bfcread_tagsBranch.C: print event # " << printEvent << endl;
  cout << " bfcread_tagsBranch.C: output file = " << fname << endl;
  cout << endl;

  ofstream fout(fname);

  fout << endl << endl;
  fout << " bfcread_tagsBranch.C: input file  = " << MainFile << endl;
  fout << " bfcread_tagsBranch.C: print evt#  = " << printEvent << endl;
  fout << " bfcread_tagsBranch.C: output file = " << fname << endl;
  fout << endl;

  TFile *file = TFile::Open(MainFile);
  TTree *tree = (TTree*)file->Get("Tag");

  cout <<" read file: " << file->GetName() << endl << endl;

  Int_t nEntries = tree->GetEntries();
  cout << " Total # events  = " << nEntries << endl;

  TObjArray *leaves = tree->GetListOfLeaves();
  Int_t nLeaves = leaves->GetEntriesFast();

  cout << "  Total # leaves  = " << nLeaves << endl;

  TString *tName = new TString(" ");
  TNamed *tableName;
  TObjArray *tagTable = new TObjArray;
  Int_t tableCount = 0;
  Int_t *tableIndex = new Int_t[nLeaves];
  Int_t tagCount = 0;

  TBranch *branch;
  TLeaf *leaf;

//count number of tag tables encoded in the TTree branch names
  for (Int_t l=0;l<nLeaves;l++) {
    leaf = (TLeaf*)leaves->UncheckedAt(l);
    tagCount+=leaf->GetNdata();
    branch = leaf->GetBranch();
      cout << "leaf #  " << l << "  br name = " <<  
                                     branch->GetName() << endl;
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

  cout << endl << "  Total num tables(branches) ,tags = " 
              << tableCount << "   " << tagCount << endl << endl;

  Int_t *countTagsTable = new Int_t[tableCount];
  Int_t *countLeavesTable = new Int_t[tableCount];
  Float_t *sumTagsLeaf = new Float_t[nLeaves];
  Int_t *countTagsLeaf = new Int_t[nLeaves];

  Int_t ndim =0;

// Now loop over leaves (to get values of tags)

   for (Int_t l=0;l<nLeaves;l++) {

      leaf = (TLeaf*)leaves->UncheckedAt(l);
      branch = leaf->GetBranch();

      countTagsTable[tableIndex[l]]+=leaf->GetNdata();
      countLeavesTable[tableIndex[l]]++;
 
      ndim = leaf->GetNdata();

      countTagsLeaf[l]+=ndim;

      cout << " QAInfo: branch ";
      cout.width(2);
      cout << tableIndex[l] << " = ";
      cout.width(12);
      cout << ((TNamed*)tagTable->UncheckedAt(tableIndex[l]))->GetName();
      cout << ", leaf ";
      cout.width(3);
      cout << l << " = ";
      cout.width(24);
      cout << leaf->GetName();
      cout << ", #tags = ";     
      cout.width(4);
      cout << ndim;
      cout << endl;

      fout << 
       " QAInfo: branch " <<  tableIndex[l] << 
       " = "  << ((TNamed*)tagTable->UncheckedAt(tableIndex[l]))->GetName() <<
       ", leaf " << l << 
       " = " << leaf->GetName() <<
       ", #tags = " << ndim << endl;

//  loop over all events in each leaf

      for (Int_t nev=0; nev<nEntries; nev++) {
	branch->GetEntry(nev);

//  loop over all tags in each leaf for each event

	for (Int_t itag=0;itag<ndim;itag++) {

          Int_t ik = nev+1;
	  if (ik==printEvent) {

	     cout << " QAInfo:     tag - " << leaf->GetName();
	     if (ndim>1) cout << '['<<itag<<']';
             cout << " = " << leaf->GetValue(itag) << endl; 	

	     fout << " QAInfo:     tag - " << leaf->GetName();
	     if (ndim>1) fout << '['<<itag<<']';
             fout << " = " << leaf->GetValue(itag) << endl; 	  
	  }

          sumTagsLeaf[l]+=leaf->GetValue(itag);

	} 	
      }
    }

// end of loop over all leaves

     cout << endl  << endl;
     fout << endl  << endl;


     for (Int_t m=0; m<tableCount; m++){
          cout << " QAInfo: branch(table) ";
	  cout.width(10);
	  cout << ((TNamed*)tagTable->UncheckedAt(m))->GetName() << " has ";
	  cout.width(4);
	  cout << countLeavesTable[m] << " leaves,";
	  cout.width(4);
	  cout << countTagsTable[m] << " tags" << endl << endl << endl;

          fout << " QAInfo: branch(table) ";
	  fout.width(10);
	  fout << ((TNamed*)tagTable->UncheckedAt(m))->GetName() << " has ";
	  fout.width(4);
	  fout << countLeavesTable[m] << " leaves,";
	  fout.width(4);
	  fout << countTagsTable[m] << " tags" << endl << endl << endl;
     }



     for (Int_t m=0; m<nLeaves; m++){
       sumTagsLeaf[m]/=countTagsLeaf[m]*nEntries;
          cout << " QAInfo: leaf ";
	  cout.width(3);
          cout << m << "  has ";
	  cout.width(4);
	  cout << countTagsLeaf[m] << " tags, avg all tags+ev =  ";
          cout.width(24);
          cout << sumTagsLeaf[m] << endl;
     }


     cout << endl << endl << 
	  " QAInfo:  tot num events = " << nEntries << endl;
     fout << endl << endl << 
	  " QAInfo:  tot num events = " << nEntries << endl;

     cout << " QAInfo:   tot num branches = " << tableCount << endl;
     fout << " QAInfo:   tot num branches = " << tableCount << endl;

     cout << " QAInfo:   tot num leaves = " << nLeaves << endl;
     fout << " QAInfo:   tot num leaves = " << nLeaves << endl;

     cout << " QAInfo:   tot num tags = " << tagCount << endl;
     fout << " QAInfo:   tot num tags = " << tagCount << endl;


  // stop timer and print results
  timer.Stop();
  cout<< endl << endl <<"RealTime="<<timer.RealTime()<<
       " seconds, CpuTime="<<timer.CpuTime()<<" seconds"<<endl;

  //cleanup
  file->Close();
  fout.close();
}






// $Id: bfcread_tagsBranch.C,v 1.14 2000/06/07 18:21:21 kathy Exp $
// $Log: bfcread_tagsBranch.C,v $
// Revision 1.14  2000/06/07 18:21:21  kathy
// finalized printouts
//
// Revision 1.13  2000/06/06 22:04:31  kathy
// updated to print out more info about tags
//
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
//     printEvent - event # to print out details for (1 by default)
//     fname - output file name to write QAInfo
//     fullPrint - prints out lots more info if set to 1 (0 by default)
//
//=======================================================================

void bfcread_tagsBranch( 
  const char *MainFile="/afs/rhic/star/data/samples/gstar.tags.root",
  Int_t printEvent=1,
  const char *fname="qa_tags.out",
  Int_t fullPrint=0) 
{
  // start timer
  TStopwatch timer;
  timer.Start();

  cout << endl << endl;
  cout << " bfcread_tagsBranch.C: input file  = " << MainFile << endl;
  cout << " bfcread_tagsBranch.C: print event # " << printEvent << endl;
  cout << " bfcread_tagsBranch.C: output file = " << fname << endl;
  cout << " bfcread_tagsBranch.C: full printout = " << fullPrint << endl;
  cout << endl;

  ofstream fout(fname);

  fout << endl << endl;
  fout << " bfcread_tagsBranch.C: input file  = " << MainFile << endl;
  fout << " bfcread_tagsBranch.C: print evt#  = " << printEvent << endl;
  fout << " bfcread_tagsBranch.C: output file = " << fname << endl;
  fout << " bfcread_tagsBranch.C: full printout = " << fullPrint << endl;
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
  TNamed *tableName=0;
  TObjArray *tagTable = new TObjArray;
  Int_t tableCount = 0;
  Int_t *tableIndex = new Int_t[nLeaves];
  Int_t tagCount = 0;

  TBranch *branch=0;
  TLeaf *leaf=0;
  Int_t ndim =0;

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

  cout << endl << "  Total num tables(branches),tags = " 
              << tableCount << "   " << tagCount << endl << endl;

  Int_t *countTagsTable = new Int_t[tableCount];
  Int_t *countLeavesTable = new Int_t[tableCount];
  Float_t *sumTagsLeaf = new Float_t[nLeaves];
  Int_t *countTagsLeaf = new Int_t[nLeaves];

// Now loop over leaves (to get values of tags)

   Int_t setBranch = -1;
   Int_t nowBranch = -1;

   for (Int_t l=0;l<nLeaves;l++) {

      leaf = (TLeaf*)leaves->UncheckedAt(l);
      branch = leaf->GetBranch();
      ndim = leaf->GetNdata();

      nowBranch = tableIndex[l];

      //cout << " nowbranch, setBranch = " << 
      //  nowBranch << ", "<< setBranch << endl;

      if (nowBranch !=  setBranch){ 
          setBranch=nowBranch;
          cout << " QAInfo: branch ";
            cout.width(2);
            cout << tableIndex[l] << " = ";
            cout.width(10);
            cout << ((TNamed*)tagTable->UncheckedAt(tableIndex[l]))->GetName();
            cout << endl;
          fout << " QAInfo: branch ";
            fout.width(2);
            fout << tableIndex[l] << " = ";
            fout.width(10);
            fout << ((TNamed*)tagTable->UncheckedAt(tableIndex[l]))->GetName();
            fout << endl;
      }

      countTagsTable[tableIndex[l]]+=leaf->GetNdata();
      countLeavesTable[tableIndex[l]]++;
      countTagsLeaf[l]+=ndim;

      cout << " QAInfo:     leaf ";
        cout.width(3);
        cout << l << " has ";
        cout.width(1);
        cout << ndim << " tags:";
        cout << endl;
      fout << " QAInfo:     leaf ";
        fout.width(3);
        fout << l << " has ";
        fout.width(1);
        fout << ndim << " tags:";
        fout << endl;

//  loop over all events in each leaf

      for (Int_t nev=0; nev<nEntries; nev++) {
	branch->GetEntry(nev);

//  loop over all tags in each leaf for each event

	for (Int_t itag=0;itag<ndim;itag++) {

          Int_t ik = nev+1;
	  if (ik==printEvent) {

	     cout << " QAInfo:         " << leaf->GetName();
	     if (ndim>1) cout << '['<<itag<<']';
             cout << " = " << leaf->GetValue(itag) << endl; 	

	     fout << " QAInfo:         " << leaf->GetName();
	     if (ndim>1) fout << '['<<itag<<']';
             fout << " = " << leaf->GetValue(itag) << endl; 	  
	  }

          sumTagsLeaf[l]+=leaf->GetValue(itag);

	} 	
      }
    }
    cout << endl  << endl;
    fout << endl  << endl;


    if (fullPrint == 1){
// loop over leaves again for printout at end
     for (Int_t m=0; m<nLeaves; m++){

       leaf = (TLeaf*)leaves->UncheckedAt(m);
       branch = leaf->GetBranch();
       ndim = leaf->GetNdata();

      cout << " QAInfo: branch ";
        cout.width(2);
        cout << tableIndex[m] << " = ";
        cout.width(10);
        cout << ((TNamed*)tagTable->UncheckedAt(tableIndex[m]))->GetName();
        cout << ", leaf ";
        cout.width(3);
        cout << m << " = ";
        cout.width(24);
        cout << leaf->GetName();
        cout << ", #tags = ";     
        cout.width(1);
        cout << ndim;
        cout << endl;
      fout << " QAInfo: branch ";
        fout.width(2);
        fout << tableIndex[m] << " = ";
        fout.width(10);
        fout << ((TNamed*)tagTable->UncheckedAt(tableIndex[m]))->GetName();
        fout << ", leaf ";
        fout.width(3);
        fout << m << " = ";
        fout.width(24);
        fout << leaf->GetName();
        fout << ", #tags = ";     
        fout.width(1);
        fout << ndim;
        fout << endl;

     }
    }
     cout << endl  << endl;
     fout << endl  << endl;



// loop over leaves again for printout at end of averages
     cout << " QAInfo: each leaf's avg over all tags,evts: " << endl;
     fout << " QAInfo: each leaf's avg over all tags,evts: " << endl;

     for (Int_t m=0; m<nLeaves; m++){

      leaf = (TLeaf*)leaves->UncheckedAt(m);

      sumTagsLeaf[m]/=countTagsLeaf[m]*nEntries;

      cout << " QAInfo: avg leaf #";
        cout.width(2);
        cout << m << ", ";
        cout.width(23);
        cout << leaf->GetName() << " = ";
        cout.width(12);
        cout << sumTagsLeaf[m];
        cout << endl;

      fout << " QAInfo: avg leaf #";
        fout.width(2);
        fout << m << ", ";
        fout.width(23);
        fout << leaf->GetName() << " = ";
        fout.width(12);
        fout << sumTagsLeaf[m];
        fout << endl;

     }
     cout << endl  << endl;
     fout << endl  << endl;



// loop over all tables
     for (Int_t m=0; m<tableCount; m++){

        cout << " QAInfo: branch(table) ";
	  cout.width(10);
	  cout << ((TNamed*)tagTable->UncheckedAt(m))->GetName() << " has ";
	  cout.width(4);
	  cout << countLeavesTable[m] << " leaves,";
	  cout.width(4);
	  cout << countTagsTable[m] << " tags" << endl;
        fout << " QAInfo: branch(table) ";
	  fout.width(10);
	  fout << ((TNamed*)tagTable->UncheckedAt(m))->GetName() << " has ";
	  fout.width(4);
	  fout << countLeavesTable[m] << " leaves,";
	  fout.width(4);
	  fout << countTagsTable[m] << " tags" << endl;

     }
     cout << endl  << endl;
     fout << endl  << endl;



     cout << " QAInfo:  tot num events = " << nEntries << endl;
     fout << " QAInfo:  tot num events = " << nEntries << endl;

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






//
// $Id: RunAdditionOfHistos.C,v 1.2 2003/06/19 21:25:19 calderon Exp $
//
// This is nothing more than a copy of the original hadd.C example in the
// root tutorials, modified to take the input directory as argument
// the input prefix is hardwired at the moment
//
// $Log: RunAdditionOfHistos.C,v $
// Revision 1.2  2003/06/19 21:25:19  calderon
// Better handling of file names, don't assume the input files, but use
// options in the macros (keep setting the default values as before).
// Adding Zbigniew's scripts and macros.
// Modified FillHistos.C due to a bug in the handling of the file name,
// which was fortuitously counterbalanced by a call to TString::Data()
// which truncated the TString at just the right place.
//
// Revision 1.1  2002/11/27 00:09:29  calderon
// New version of evaluator using the minimctrees
// Macros for running the evaluation and perl script to coordinate
//
//
//______________________________________________________________________

TFile   *fnew;
TList   *flist;
TFile   *afile, *file1;

TH1     *h1, *h2;
TTree   *t1, *t2;
TObject *obj;
TKey    *key;

void AddRecursive(TDirectory *root,TDirectory* node);
//______________________________________________________________________
//
//
//
//______________________________________________________________________
void RunAdditionOfHistos(TString topDir="StiEvalOutputRawHistos/") {

    // use the vertex bin as the suffix for both input and output files
    char* prefixin = new char[25];
    sprintf(prefixin,"rawHistos");      // minimc hijing july 2002

    //create a support list for the input files
    flist = new TList();

    // open all input files with the same prefix 
    // and insert them in the list of files
    Int_t nfiles = 0;
    TFileSet dirs(topDir);
    TDataSetIter next(&dirs,0);

    cout << "Prefix Input " << prefixin << endl;
    cout << "Looking in   " << topDir   << endl;
//     while ( next() && nfiles<maxfiles) {   // Maxfiles was used because root can't handle too many open files
    while ( next() ) {
	//cout << (*next)->GetName() << endl;
	if (strcmp((*next)->GetTitle(),"file") || 
	    !(strstr((*next)->GetName(),prefixin)) ) continue;// ||
// 	    !(strstr((*next)->GetName(),"BigBin")) ||
// 	    !(strstr((*next)->GetName(),"mem")))continue;
	if (nfiles<5) cout << "File " << nfiles+1  << " : " << (*next)->GetName() << endl;
	afile = new TFile(gSystem->ConcatFileName(topDir,(*next)->GetName()));
	flist->Add(afile);
	nfiles++;
    }
    cout << "Entered " << nfiles << " files into list from " << topDir << endl;

    // create the result file
    TString outfile = "SumHistosEvalItTest.root";
    outfile.Prepend(topDir);
    cout << "Output File " << outfile.Data() << endl;
    fnew = new TFile(outfile.Data(),"RECREATE");

    //Get a pointer to the first file
    afile = file1 = (TFile*)flist->First();

    AddRecursive(fnew,file1);

    //fnew->ls();
    fnew->Write();
    fnew->Close();
    delete fnew;
    flist->Delete();
    delete flist;
}

//______________________________________________________________________
void AddRecursive(TDirectory *root,TDirectory* node) {
    
  static TDirectory *dact;

  TDirectory *dirsav;

  //We create an iterator to loop on all objects(keys) of first file
  TIter nextkey(node->GetListOfKeys());
  while (key = (TKey*)nextkey()) {
      node->cd();
      obj = key->ReadObj();
      if (obj->IsA()->InheritsFrom("TTree")) { //case of a TTree or TNtuple
	  //       t1 = (TTree*)obj;
//       // this part still to be implemented
//       // use TChain::Merge instead
      }
      else if (obj->IsA()->InheritsFrom("TH1")) { //case of TH1
	  h1 = (TH1*) obj;
	  afile = (TFile*) flist->After(file1);
	  while (afile) { //loop on all files starting at second file
 	      char* base=strstr(root->GetPath(),":"); base+=2;
	      
	      dirsav=gDirectory;
	      afile->cd(base);
	      h2 = (TH1*) gDirectory->Get(h1->GetName());
	      dirsav->cd();
	      if (h2) { // here we should check that we can add
		  h1->Add(h2);
		  delete h2;
	      }
	      afile = (TFile*) flist->After(afile);
	  }
	  cout << h1->GetName() << " : " <<  h1->GetEntries() << endl;
      }

    
    // write node object, modified or not into fnew
      if (obj) {
	  root->cd();
	  obj->Write(key->GetName());
	  delete obj;
	  obj=NULL;
      }
  }
  root->cd();
}

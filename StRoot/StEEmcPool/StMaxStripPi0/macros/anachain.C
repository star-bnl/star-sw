TChain *mTree = 0;
TCanvas *c = 0;

void anachain( const Char_t *ddname = "/star/data05/scratch/jwebb/checkin/",
	       const Char_t *pref="R5", 
	       const Char_t *ffname = ".root",
	       Int_t nmax=50000
	       ) 
{

  gROOT->LoadMacro("macros/loadlibs.C");
  loadlibs();

  c = new TCanvas("c","A canvas",500,500); 

  mTree = new TChain("mTree","A pi0 tree");

  TSystemDirectory *dir = new TSystemDirectory("pwd",ddname);
  TList *files = dir->GetListOfFiles();
  TIter next( files );
  TSystemFile *file = 0;
  Int_t nf = 0;

  TList *listOfEmpties=new TList();

  while ( (file = (TSystemFile *)next() ) ) {


    //-- Get the filename --
    TString fname = file -> GetName();
    if ( !fname.Contains(ffname) ) continue;
    if ( !fname.Contains(pref) ) continue;     
    Bool_t pi0tree = 0;

    /// Open the file and determine if it contains
    /// a TTree
    TFile pfile( fname );
    TTree *mytree = (TTree *)pfile.Get("mTree");
    if ( mytree ) pi0tree = 1;
    pfile.Close();

    //-- If we have the trees, add them
    if ( pi0tree ) {
      std::cout << "Adding " << fname 
		<< " npi0tree=" << mTree->GetEntries()
		<< std::endl;

      mTree -> Add(fname+"/mTree");
      nf++;
    }

    if ( nf > nmax ) break;
  }

  std::cout << "-- anachain -----------------------------------" << std::endl;
  std::cout << std::endl;
  std::cout << "loaded " << nf << " files" << std::endl; 
}





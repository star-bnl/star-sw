// $Id: doEvents.C,v 1.2 1999/02/11 16:22:51 wenaus Exp $
// $Log: doEvents.C,v $
// Revision 1.2  1999/02/11 16:22:51  wenaus
// load StEvent for Linux
//
// Revision 1.2  1999/02/11 16:22:51  wenaus
// load StEvent for Linux
//
// Revision 1.1  1999/02/11 15:44:28  wenaus
//          to fill StEvent and StAnalysisMaker to show example of analysis
//          
//=======================================================================
///////////////////////////////////////////////////////////////////////////////
//
// doEvents.C
//
// Description: 
// Chain to read events from files or database into StEvent and analyze
//
// Environment:
//  Software developed for the STAR Detector at Brookhaven National Laboratory
//
// Author List: 
//  Torre Wenaus, BNL  2/99
//
// History:
//
///////////////////////////////////////////////////////////////////////////////
void setFiles(const Char_t *path, const Char_t *file);
class St_XDFFile;
St_XDFFile* nextFile();

// Functions included below which retrieve a single file or all files
// under a path
// File-scope stuff needed by setFiles, nextFile. Someone ambitious
// can clean this up by putting it all into a nice clean class.

Int_t nFile = 0;
class St_FileSet;
St_FileSet *dstDirs = 0;
class St_DataSet;
St_DataSet *set = 0; 
class St_DataSetIter;
St_DataSetIter* nextXdf;
class St_XDFFile;
St_XDFFile *theFile = 0;
TString  originalPath;
// If you specify a path, all *dst.xdf files below that path will be

// If 'file ends in '.xdf', XDF DSTs are searched for.
// If 'file ends in '.dst.root', ROOT DSTs are searched for.

// If path begins with '-', 'file' will be taken to be a single file
// to be processed.

void doEvents(const Int_t nevents=999,
               const Char_t *path="-/disk00000/star/auau200/hijing135/",
               const Char_t *file="/disk00000/star/auau200/hijing135/default/b0_20/year2x/hadronic_on/tfs_dst/pet213_02_190evts_h_dst.xdf")
    cout << "       doEvents.C(nevents,\"-\",\"some_directory/some_dst_file.root\")" << endl;



  gSystem->Load("xdf2root");
  // Dynamically link needed shared libs
  gSystem->Load("StSclRoot");
  gSystem->Load("StUtilities");
  gSystem->Load("StEventReaderMaker");
//  gSystem->Load("StEventReaderMaker");
//  gSystem->Load("St_geom_Maker");
  TBrowser *b=0;

  // Set up the chain
  StChain chain("StChain");
  // Maker to read events from file or database into StEvent
  StEventReaderMaker readerMaker("events","title");
  // Sample analysis maker
  StAnalysisMaker analysisMaker("analysis","title");
//  Event Display Maker
  // Initialize chain
  Int_t iInit = chain.Init();
  if (iInit) chain.Fatal(iInit,"on init");

  setFiles(path,file);

  St_XDFFile *xdf_in = 0;
  while (xdf_in = nextFile()) {
    // Open XDF file and pass to reader
    xdf_in = new St_XDFFile(file,"r");
    readerMaker.setXdfFile(xdf_in);

    // Event loop
    for (Int_t i=1; i<=nevents; i++) {
      cout << "============================ Event " << i << " start" << endl;
      if (chain.Make(i)) break;
      cout << "============================ Event " << i << " finish" << endl;
      if (i != nevents) chain->Clear();
    }
  }
     chain->Clear();
    chain.Finish();
  if (nevents > 1) {
    if (!b) b = new TBrowser;
      //       gROOT->LoadMacro("PadControlPanel.C");
}

//************************************************************************
void setFiles(const Char_t *path, const Char_t *file)
{
  if ( path[0] != '-' ) {
    // Path has been specified. Run on all DST files found under
    // the path.
    cout << "Using fileset based on path " << path << endl;
    usePath = 1;
    originalPath = path;
    dstDirs = new St_FileSet(path);
    nextXdf = new St_DataSetIter(dstDirs,0);
  } else {
    // No path. Use single file.
    cout << "Using file " << file << endl;
    theFile = new St_XDFFile(file,"r");
  }
}

//************************************************************************
St_XDFFile* nextFile()
{
  St_XDFFile *nextF = 0;
  if (usePath) {
    // Loop until we find a file of form *dst.xdf
    while ( (set = nextXdf->Next()) && 
            ! (
            (strstr(set->GetName(),"dst.xdf") != 0) &&
            (strcmp(set->GetTitle(),"file") == 0) 
            ) ) { };
    if (set) {
      if (strcmp(set->GetTitle(),"file") == 0){
        if (strstr(set->GetName(),"dst.xdf")){
          if (nextF) delete nextF;
          thePath = originalPath;
          thePath +=  set->Path();
          Char_t *xdffilename= thePath.Data();
          nextF = new St_XDFFile(xdffilename,"r");
          cout << "Open file " << xdffilename << endl;
        }
      }
    } else {
      nextF = 0;
    }
  } else {
    if (0 == nFile) {
      nextF = theFile;
    } else {
      nextF = 0;
    }
  }
  nFile++;
  return nextF;
    fileListQQ[0] = gSystem->ConcatFileName(path,file);
}


// what it does: reads .dst.root or .dst.xdf file or files, fills StEvent &
//      then runs StAnalysisMaker 
//////////////////////////////////////////////////////////////////////////







// $Id: doEvents.C,v 1.14 1999/05/21 15:33:57 kathy Exp $
// $Log: doEvents.C,v $
// Revision 1.14  1999/05/21 15:33:57  kathy
// made sure Log & Id are in each file and also put in standard comment line with name of owner
//
// Revision 1.1  1999/05/22 19:38:12  perev
// temporary macro for RootEvents
//
// Revision 1.13  1999/04/15 18:03:14  wenaus
// clean out duplicate/conflicting declarations
//
// Revision 1.12  1999/04/01 23:39:47  fisyak
// Cleanup old macros
//
// Revision 1.11  1999/03/10 14:28:17  fisyak
// Clean up for SL99c
//
// Revision 1.10  1999/03/02 03:34:43  fisyak
// Set LD_LIBRARY_PATH to Root.DynamicPath
//
// Revision 1.9  1999/02/28 00:08:18  wenaus
// add multi-file handling for .root files. But, using multiple files doesn't work for ROOT files yet.
//
// Revision 1.8  1999/02/25 23:10:41  wenaus
// fix multi-file bug
//
// Revision 1.7  1999/02/25 02:51:42  wenaus
// make sure default file is a working one
//
// Revision 1.6  1999/02/25 02:42:58  wenaus
// input file options
//
// Revision 1.5  1999/02/24 23:21:19  wenaus
// add ROOT file handling
//
// Revision 1.4  1999/02/20 05:39:24  wenaus
// turn off TBrowser (gives bus errors) and don't count run header as an event
//
// Revision 1.3  1999/02/16 18:15:48  fisyak
// Check in the latest updates to fix them
//
// Revision 1.2  1999/02/11 16:22:51  wenaus
// load StEvent for Linux
//
// Revision 1.1  1999/02/11 15:44:28  wenaus
//=======================================================================
// owner: Torre Wenaus
// what it does: 
//=======================================================================
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

Int_t usePath = 0;
Int_t nFile = 0;
class St_FileSet;
St_FileSet *dstDirs = 0;
class St_DataSet;
St_DataSet *set = 0; 
class St_DataSetIter;
St_DataSetIter* nextDataSet;
St_XDFFile *theFile = 0;
TString  thePath;
TString  theFileName;
TString  originalPath;
TFile *rootFile=0;  
class StChain;
Bool_t isRoot=kFALSE;
Bool_t isXdf=kFALSE;
TTree* tree=0;
const char *xdfFile ="/afs/rhic/star/data/samples/psc0054_07_40evts_dst.xdf";
St_XDFFile* nextFile();
TFile *nextRootFile();
const char *fileList[] = {dstFile,xdfFile,mdcFile,0};
//========================================================================================================

// ----------- Ways to run -------------------------------------------
// If you specify a path, all DST files below that path will be
// If 'file ends in '.root', ROOT DSTs are searched for.
// The type of DST files searched for is taken from the 'file' parameter.
// If 'file ends in '.xdf', XDF DSTs are searched for.
// If 'file ends in '.dst.root', ROOT DSTs are searched for.
//
// If path begins with '-', 'file' will be taken to be a single file
// to be processed.
//
// example invocation:
// .x doEvents.C(10,"-","/disk00001/star/auau200/venus412/default/b0_3/year_1b/hadronic_on/gstardata/psc0033_01_40evts.root")
//
// example ROOT file invocation:
// .x doEvents.C(9999,"/disk00001/star/auau200/hijing/b0_3/jet05/year_1b/hadronic_on/tfs/","*.root")

void doEvents(const Int_t nevents=999,
              const Char_t *path="-/disk00001/star/auau200/hijing135/jetq_on/b0_3/year_1b/hadronic_on/tfs/",
              const Char_t *file="/afs/rhic/star/data/samples/psc0054_07_40evts_dst.xdf")
    cout << "       doEvents.C(nevents,\"-\",\"some_directory/some_dst_file.root\")" << endl;
  // const Char_t *file="/afs/rhic/star/data/samples/psc0016_05_35evts.root")
  // const Char_t *file="/disk00001/star/auau200/hijing135/jetq_on/b0_3/year_1b/hadronic_on/tfs/./set0022_01_56evts_dst.xdf")
  // const Char_t *file="/afs/rhic/star/strange/genevb/year1a_90evts_dst.xdf")
  // const Char_t *file="/disk00000/star/auau200/hijing135/default/b0_20/year2x/hadronic_on/tfs_dst/pet213_02_190evts_h_dst.xdf")
  // const Char_t *path="-/disk00000/star/auau200/hijing135/",




  gSystem->Load("xdf2root");
  // Dynamically link needed shared libs
  //  gSystem->Load("St_emc_Maker");
  gSystem->Load("St_io_Maker");
  St_io_Maker *rootIn=0;
  gSystem->Load("St_Tables");
  gSystem->Load("StUtilities");
  gSystem->Load("StEventReaderMaker");
//  gSystem->Load("StEventReaderMaker");
//  gSystem->Load("St_geom_Maker");
//  gSystem->Load("StEventDisplayMaker");
  theFileName = file;
  isRoot = theFileName.Contains(".root");
  isXdf  = theFileName.Contains(".xdf");
  if (isRoot) {
    cout << "Reading ROOT file" << endl;
    chain  = new StChain("StChain");
    rootIn = new St_io_Maker("Input","all");
  } else if (isXdf) {
    cout << "Reading XDF file" << endl;
    chain  = new StChain("StChain");
  } else {
    cout << "File type not recognized on file " << theFileName << endl;
    cout << "File type must be either .xdf or .root" << endl;
    return;
  }

  setFiles(path,file);  
  //  St_geom_Maker *geom = new St_geom_Maker; // this maker open its own TFile !!!
  // Maker to read events from file or database into StEvent
  StEventReaderMaker readerMaker("events","title");
  // Sample analysis maker
  StAnalysisMaker analysisMaker("analysis","title");
//  Event Display Maker
  if (isRoot) {
    while (rootFile = nextRootFile()) {
      // ROOT file handling -------------------------------
      TTree *newTree = 0;
      if (rootFile) newTree=(TTree *)rootFile->Get("Output");
      
      if (newTree) {
        newTree->Print();
        chain->SetTree(newTree);
        tree = newTree;
        TObjArray *list = tree->GetListOfBranches();
        if (list) {
          TIter next(list);
          TBranch *nextb = 0;
          while (nextb = (TBranch *)next()) 
            cout << "Branch: <"<< nextb->GetName() << ">;"
                 << "  File: <"<< nextb->GetFileName() << ">;"
                 << " Entries: " << nextb->GetEntries()
                 << "; Last event number: "<< nextb->GetEventNumber() << endl;
        }
        // Initialize chain
        Int_t iInit = chain->Init();
        if (iInit) chain->Fatal(iInit,"on init");
        chain->PrintInfo();
        
        // Event loop
        int istat;
        for (Int_t i=1; i<=nevents; i++) {
          cout << "============================ Event " << i << " start" << endl;
          istat = chain->Make(i);
          if (istat) {
            cout << "Last event processed. Status = " << istat << endl;
            chain->Clear();
            break;
          }
          St_DataSet *set = chain->DataSet("dst");
          if (set)  {
            St_DataSetIter dirt(set);
            dirt.Du();
          }
          cout << "============================ Event " << i << " finish" << endl;
          if (i != nevents) chain->Clear();
        }
        if (nevents > 1) chain->Finish();
      }
    }
    
  } else {
    // XDF file handling ------------------------------------

    // Initialize chain
    Int_t iInit = chain->Init();
    if (iInit) chain->Fatal(iInit,"on init");
    St_XDFFile *xdf_in = 0;
    while (xdf_in = nextFile()) {
      // Open XDF file and pass to reader
      readerMaker.setXdfFile(xdf_in);
      
      // Event loop
      for (Int_t i=1; i<=nevents; i++) {
        if (i == 1) chain->Make(0); // Read the run header
        cout << "============================ Event " << i << " start" << endl;
        if (chain->Make(i)) break;
        cout << "============================ Event " << i << " finish" << endl;      
        if (i != nevents) chain->Clear();
      }
    }
  }
     chain->Clear();
    cout << "============================ Event " << i << " finish" << endl;
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
    dstDirs     = new St_FileSet(path);
    nextDataSet = new St_DataSetIter(dstDirs,0);
  } else {
    // No path. Use single file.
    cout << "Using file " << file << endl;
    if (isRoot) {
      rootFile = new TFile(file);
    } else if (isXdf) {
      theFile = new St_XDFFile(file,"r");
    }
  }
}

//************************************************************************
St_XDFFile* nextFile()
{
  St_XDFFile *nextF = 0;
  if (usePath) {
    // Loop until we find a file of form *dst.xdf
    while ( (set = nextDataSet->Next()) && 
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

}

//************************************************************************
TFile *nextRootFile()
{
  TFile *fileFound = 0;
  if (usePath) {
    // Loop until we find a file of form *evts.root
    while ( (set = nextDataSet->Next()) && 
            ! (
            (strstr(set->GetName(),"evts.root") != 0) &&
            (strcmp(set->GetTitle(),"file") == 0) 
            ) ) { };
    if (set) {
      if (strcmp(set->GetTitle(),"file") == 0){
        if (strstr(set->GetName(),"evts.root")){
          thePath = originalPath;
          thePath +=  set->Path();
          Char_t *rootfilename= thePath.Data();
          cout << "Open file " << rootfilename << endl;
          if (rootFile) delete rootFile;
          fileFound = new TFile(rootfilename);
        }
      }
    } else 
      rootFile = 0;
    
  } else 
    if (0 == nFile) 
      fileFound = rootFile;  // No iterating done. File opened in main.
    else 
      fileFound = 0;
      
  nFile++;
  return fileFound;
    fileListQQ[0] = gSystem->ConcatFileName(path,file);
}


// what it does: reads .dst.root or .dst.xdf file or files, fills StEvent &
//      then runs StAnalysisMaker 
//////////////////////////////////////////////////////////////////////////







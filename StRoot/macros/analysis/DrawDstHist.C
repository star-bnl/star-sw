//  create chain to read in root dst file(s), run QA_Maker & 
//  send all histograms to a postscript file

TCanvas *QACanvas = 0;
TBrowser *QABrowser = 0;
class StChain;
class St_DataSet;
StChain  *chain=0;

// Load macro --------------------------------------------------------
void Load()
{
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("St_Tables");
    gSystem->Load("St_io_Maker");
    gSystem->Load("St_QA_Maker");
};


// DrawDstHist macro ---------------------------------------------------
// enter nevproc,firstHistName,lastHistName,input fileName,output psFile name

// this is an MDC2 dst:
//fileName="/disk00001/star/auau200/venus412/default/b0_3/year_1b/hadronic_on/tss/psc0064_07_40evts.root

void DrawDstHist(
     Int_t nevproc=50,
     const Char_t *firstHistName="*",const Char_t *lastHistName="*",
//     const Char_t *fileName="/disk1/star/test/hijing135/jetq_on/b0_3/year_1b/tfs_dst/set0016_01_51evts.dst.root",
     const Char_t *fileName="/disk00001/star/auau200/venus412/default/b0_3/year_1b/hadronic_on/tss/psc0064_07_40evts.root",
     const Char_t *psFile="QA_hist.ps")
{ 

// print out stuff:
    cout << endl   
         << " Usage:  DrawDstHist( " << endl
         << "                        Int_t nevproc= " << nevproc    << endl
         << "                        const Char_t *firstHistName=\"" << firstHistName << "\","   << endl
         << "                        const Char_t *lastHistName=\""  << lastHistName  << "\""    << endl
         << "                        const Char_t *fileName =\""     << fileName      << "\","   << endl
         << "                        const Char_t *psFile=\""        << psFile        << "\","   << endl
         << "                      );" << endl 
         << " Note: firstHistName = \"*\" is by default and means ALL histograms from the file should be drawn" << endl
         << " ----- lastHistName  = \"*\" is by default and means ALL histograms by the end of file should be drawn" << endl
         << "       fileName      - may define either file or entire directory tree." << endl
         << "                       For the directory all files from that will be used." << endl
         << endl ;

// execute Load macro
  if (gClassTable->GetID("StChain") < 0) Load();

//  check input file first
  Long_t id;
  Long_t size;
  Long_t flags; 
  Long_t modtime;
  Char_t *exFileName = gSystem->ExpandPathName(fileName);
  if (gSystem->GetPathInfo(exFileName, &id, &size, &flags, &modtime)) 
  {
   cerr << " *** Error ***  Can not find file: \"" << fileName << "\"" << endl;
   delete [] exFileName;
   exFileName=0;
   return;
  }

// check output file
  Char_t *exPsFile =  gSystem->ExpandPathName(psFile);
  const Char_t *excludedFiles[] = {"psc0055_07_40","psc0055_08_40","psc0059_07_40"};
  Int_t nExcluded =  sizeof(excludedFiles)/4;
  cout << nExcluded << " files will be excluded" << endl ;

// Now setup the actual chain
  if (!chain) {
    chain = new StChain("bfc");
    St_io_Maker *input    = new St_io_Maker("Input","all");
    Int_t  nFile = 0;

//*-*  Add a files or files to the list of dst ones 
    if ((flags & 2)) {
      St_FileSet dstDirs(exFileName,".");
      St_DataSetIter nextDataSet(&dstDirs,0);
      St_DataSet *set = 0;      
      while ( (set = nextDataSet()) ) {           
        if (strcmp(set->GetTitle(),"file") || !(strstr(set->GetName(),"evts.root"))) continue;
        TString p = set->Path();
        Char_t *rootfilename = gSystem->ConcatFileName(exFileName,p.Data());
        cout << "Including file " << rootfilename << " into list of the files " << endl;
        cout << "===" << p.Data() <<  endl;
        Int_t ie = 0;
        Bool_t isExcluded = kFALSE;
        while(ie < nExcluded) {
          // exculde one "wrong" file 
          if (strstr(rootfilename,excludedFiles[ie])) {isExcluded = kTRUE; break; }
          ie++;
        }
        if (isExcluded) continue;
        input->AddFile(rootfilename);
        nFile++;
      } 
    } else {
      input->AddFile(exFileName);
      nFile++;
    }
    cout << "Total: " << nFile << " files will be analysed" << endl ;

// set max # events to read from each file ()=default value of 10 million 
    input->SetMaxEvent();
    input->SetDebug();

// now setup the rest of the Makers in the chain 
    St_QA_Maker *QA   = new St_QA_Maker("QA","event/geant/Event");
    QA->SetHistsNames(firstHistName,lastHistName);
    QA->SetDraw();
    QA->SetPostScriptFile(psFile);
//    input->MakeDoc(); 
  }

// Now actually execute the init & make methods in the Makers 
  chain->Init();
  chain->PrintInfo();
  int i;
  for (i=1;i<=nevproc; i++)  { 
    cout <<  " !!! processing event !!! " << i << endl ;
    if (!chain->Make(i))  chain->Clear();
    else break;
  }
  cout <<  " !!! passed chain->Make" << endl ;
  chain->Finish();
  cout <<  " !!! passed chain->Finish" << endl ;

  if (QABrowser) delete QABrowser;
//  QABrowser = new TBrowser;

  delete [] exFileName;
  delete [] exPsFile;
  cout <<  " !!! This is last line of macro" << endl ;
}



// $Id: DrawDstHist.C,v 1.21 1999/05/21 21:05:43 kathy Exp $ 
// $Log: DrawDstHist.C,v $
// Revision 1.21  1999/05/21 21:05:43  kathy
// fixed comments for DrawDstHist.C
//
//
//======================================================================
// owner:  Kathy Turner
// what it does: 
//    - reads a root dst file created during MDC2
//    - runs St_QA_Maker
//    - draws QA histograms and then sends them to a postscript file
//
//===============================================================
//
TCanvas *QACanvas = 0;
TBrowser *QABrowser = 0;
class StChain;
class St_DataSet;
StChain  *chain=0;

// Load macro ==========================================================
void Load()
{
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("St_Tables");
    gSystem->Load("St_io_Maker");
    gSystem->Load("St_QA_Maker");
};


// DrawDstHist macro ======================================================
// enter nevproc,firstHist,lastHist,input fileName,output psFile name

// MDC2 dsts:
//   *fileName="/disk00001/star/auau200/venus412/default/b0_3/year_1b/hadronic_on/tss/psc0064_07_40evts.root
//   *fileName="/disk1/star/test/hijing135/jetq_on/b0_3/year_1b/tfs_dst/set0016_01_51evts.dst.root",

void DrawDstHist(
     Int_t nevproc=100,
     const Char_t *firstHist="*",
     const Char_t *lastHist="*",
     const Char_t *fileName="/disk00001/star/auau200/venus412/default/b0_3/year_1b/hadronic_on/tss/psc0064_07_40evts.root",
     const Char_t *psFile="QA_hist.ps")
{ 

// print out stuff:
    cout << endl   
         << " Usage:  DrawDstHist( " << endl
         << "                        Int_t nevproc= " << nevproc    << endl
         << "                        const Char_t *firstHist=\"" << firstHist << "\","   << endl
         << "                        const Char_t *lastHist=\""  << lastHist  << "\""    << endl
         << "                        const Char_t *fileName =\""     << fileName      << "\","   << endl
         << "                        const Char_t *psFile=\""        << psFile        << "\","   << endl
         << "                      );" << endl 
         << " Note: firstHist = \"*\" is by default and means ALL histograms from the file are drawn" << endl
         << " ----- lastHist  = \"*\" is by default and means ALL histograms by the end of file are drawn" << endl
         << "       fileName      - may define either file or entire directory tree." << endl
         << "                       For the directory, all files from that will be used." << endl
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
    cout << " DrawDstHist.C, Total: " << nFile << " files will be analysed" << endl ;

// set max # events to read from each file ()=default value of 10 million 
   
    cout << " DrawDstHist.C, Default max event per file is " << input->GetMaxEvent() << endl;
      input->SetMaxEvent(nevproc);
    cout << " DrawDstHist.C, Have set max event per file to " << input->GetMaxEvent() << endl;
      input->SetDebug();

// now setup the rest of the Makers in the chain 
    St_QA_Maker *QA   = new St_QA_Maker;


//    input->MakeDoc(); 
  }

// Now actually execute the init & make methods in the Makers 
  chain->Init();
  chain->PrintInfo();

// method to print out list of histograms - can do this anytime after they're booked
   Int_t NoHist=0;
   NoHist = QA->ListHists();
   cout << " DrawDstHist.C, No. of Hist we have == " << NoHist << endl;


// now looping over all events in input file
  int i=0;
  int iloop=0;
  iloop = input->GetMaxEvent();
// add 1 to iloop  or it doesn't work (due to "feature" in St_io_Maker??)
  iloop+=1;
  cout << " DrawDstHist, will now loop over # events = " << input->GetMaxEvent() << endl;
  cout << "   -- but really have to add 1 to loop to make it work! iloop =  " << iloop << endl;
  for (i=1;i<=iloop; i++)  { 
    cout <<  " DrawDstHist.C, processing event !!! " << i << endl ;
    if (!chain->Make(i))  chain->Clear();
    else break;
  }
  cout <<  " DrawDstHist.C, passed chain->Make !!!" << endl ;


// the following methods are already set to default values in St_QA_Maker::Init - now write over them
    QA->SetDraw(kTRUE);
    QA->SetHistsNamesDraw(firstHist,lastHist);
    QA->SetPostScriptFile(psFile);
    QA->SetZones();
    QA->SetPaperSize();

// Now add to the list of which histograms we want plotted with LogY scale
  const Char_t *LList[] = {"QaVertexX",
                           "QaVertexY",
                           "QaVertexZ"};
  Int_t lengOfList = 0;
    lengOfList = sizeof(LList)/4;
  Int_t ilg = 0;
  Int_t numLog = 0;
  for (ilg=0;ilg<lengOfList;ilg++) {
    cout <<  " DrawDstHist.C, adding histogram " << LList[ilg] << " to LogY list "  << endl ;
    numLog = QA->AddToLogYList(LList[ilg]);
  }
  cout <<" DrawDstHist.C, Number hist to plot with log scale = " << numLog << endl;

// Now remove a hist from the list:
// numLog = QA->RemoveFromLogYList("QaGlobtrkPt");
// cout <<" DrawDstHist.C, Number hist to plot with log scale = " << numLog << endl;
   numLog = QA->RemoveFromLogYList("Abcd");
   cout <<" DrawDstHist.C, Number hist to plot with log scale = " << numLog << endl;

  numLog = QA->ExamineLogYList();

// Finish method in St_QA_Maker is where the actual DrawHist is done
  chain->Finish();
  cout <<  "DrawDstHist.C, passed chain->Finish" << endl ;


  if (QABrowser) delete QABrowser;
//  QABrowser = new TBrowser;

  delete [] exFileName;
  delete [] exPsFile;
//  cout <<  " !!! This is last line of macro" << endl ;
}















//read root histogram file and send all histograms to a postscript file
TCanvas *QACanvas = 0;
TBrowser *QABrowser = 0;
class StChain;
class St_DataSet;
StChain  *chain=0;

void Load()
{
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("St_Tables");
    gSystem->Load("St_io_Maker");
    gSystem->Load("St_QA_Maker");
};

void DrawDstHist(
     const Char_t *firstHistName="*",const Char_t *lastHistName="*",
     const Char_t *fileName="/disk00001/star/auau200/venus412/default/b0_3/year_1b/hadronic_on/tss/psc0072_05_40evts.root",
     const Char_t *psFile="QA_hist.ps")
{ 
    cout << endl   
         << " Usage:  QA_Hist_Draw( " << endl
         << "                        const Char_t *firstHistName=\"" << firstHistName << "\","   << endl
         << "                        const Char_t *lastHistName=\""  << lastHistName  << "\""    << endl
         << "                        const Char_t *fileName =\""     << fileName      << "\","   << endl
         << "                        const Char_t *psFile=\""        << psFile        << "\","   << endl
         << "                      );" << endl 
         << " Note: firstHistName = \"*\" is by default and means ALL histograms from the file should be drawn" << endl
         << " ----- lastHistName  = \"*\" is by default and means ALL histograms by the end of file should be drawn" << endl
         << endl ;
  if (gClassTable->GetID("StChain") < 0) Load();
  //  check file first
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
  Char_t *exPsFile =  gSystem->ExpandPathName(psFile);

  if (!chain) {
    chain = new StChain("bfc");
    St_io_Maker *in    = new St_io_Maker("Input","all");
    in->AddFile(exFileName);
    in->SetMaxEvent(4);
    St_QA_Maker *QA   = new St_QA_Maker("QA","event/geant/Event");
    QA->SetHistsNames(firstHistName,lastHistName);
    QA->SetDraw();
//    in->MakeDoc(); 
  }

  chain->Init();
  chain->PrintInfo();
  int i;
  for (i=1;i<11111111;i++)  {
    if (!chain->Make(i))  chain->Clear();
    else break;
  }
  chain->Finish();
  if (QABrowser) delete QABrowser;
//  QABrowser = new TBrowser;
  delete [] exFileName;
  delete [] exPsFile;
}

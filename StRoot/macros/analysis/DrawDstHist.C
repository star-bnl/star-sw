// $Id: DrawDstHist.C,v 1.25 1999/06/11 20:07:35 kathy Exp $
// $Log: DrawDstHist.C,v $
// Revision 1.25  1999/06/11 20:07:35  kathy
// changed default output file name
//
// Revision 1.24  1999/06/03 23:34:49  kathy
// got macros working with current files
//
// Revision 1.23  1999/06/03 17:55:26  kathy
// changed DrawDstHist from using St_io_Maker to Victor's new StIOMaker - actually just copied doEvents to DrawDstHist and hacked it. Fixed comments in read_bfc_hist_list.C
//


//=======================================================================
// owner: Kathy Turner
// what it does: reads MDC2 dst-root file and runs St_QA_Maker
//               draws & prints QA histograms
//=======================================================================
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

// Functions included below which retrieve a single file or all files
// under a path


// File-scope stuff needed by setFiles, nextFile. Someone ambitious
// can clean this up by putting it all into a nice clean class.

Int_t usePath = 0;
Int_t nFile = 0;
TString  thePath;
TString  theFileName;
TString  originalPath;
class StChain;
StChain *chain=0;

TCanvas *QACanvas = 0;
TBrowser *QABrowser =  0;

const char *dstFile ="/disk00001/star/auau200/two_photon/starlight/twogam/year_1b/hadronic_on/tfs/ric0022_01_14552evts.dst.root";
const char *xdfFile ="/afs/rhic/star/data/samples/psc0054_07_40evts_dst.xdf";
const char *mdcFile ="/disk00001/star/auau200/venus412/default/b0_3/year_1b/hadronic_on/tss/psc0081_07_40evts.root";
const char *fileList[] = {dstFile,xdfFile,mdcFile,0};


  // const Char_t *file="/afs/rhic/star/data/samples/psc0016_05_35evts.root")
  // const Char_t *file="/disk00001/star/auau200/hijing135/jetq_on/b0_3/year_1b/hadronic_on/tfs/./set0022_01_56evts_dst.xdf")
  // const Char_t *file="/afs/rhic/star/strange/genevb/year1a_90evts_dst.xdf")
  // const Char_t *file="/disk00000/star/auau200/hijing135/default/b0_20/year2x/hadronic_on/tfs_dst/pet213_02_190evts_h_dst.xdf")
  // const Char_t *path="-/disk00000/star/auau200/hijing135/",

// If you specify a path, all DST files below that path will be
// found, and 'nevents' events from each will be analyzed.
// The type of DST files searched for is taken from the 'file' parameter.
// If 'file ends in '.xdf', XDF DSTs are searched for.
// If 'file ends in '.root', ROOT DSTs are searched for.
//
// If path begins with '-', 'file' will be taken to be a single file
// to be processed.
//
// example invocation:
// .x DrawDstHist.C(10,"-","/afs/rhic/star/strange/genevb/year1a_90evts_dst.xdf")
//
// example ROOT file invocation:
// .x DrawDstHist.C(10,"-","/disk00001/star/auau200/venus412/default/b0_3/year_1b/hadronic_on/gstardata/psc0033_01_40evts.root")
//
// example multi-ROOT file invocation:
// .x DrawDstHist.C(9999,"/disk00001/star/auau200/hijing/b0_3/jet05/year_1b/hadronic_on/tfs/","*.root")


// 6/3/99 Kathy tested following files to use:
//const Char_t *file="/afs/rhic/star/data/samples/psc0054_07_40evts_dst.xdf",
//const Char_t *file="/disk00000/star/auau200/hijing135/jetq_on/b0_3/year_1b/hadronic_on/tfs/set0029_08_49evts.root"

void DrawDstHistQQ(const Int_t nevents, 
              const Char_t **fileList,
              const Char_t *firstHist,     
              const Char_t *lastHist,   
              const Char_t *psFile);


void DrawDstHist(const Int_t nevents=1,
              const Char_t *path="-",
              const Char_t *file="/disk00000/star/auau200/hijing135/jetq_on/b0_3/year_1b/hadronic_on/tfs/set0029_08_49evts.root",
              const Char_t *firstHist="*",
              const Char_t *lastHist="*",   
              const Char_t *psFile="QA_hist_DrawDstHist.ps")
{
  const char *fileListQQ[]={0,0};
  if (path[0]=='-') {
    fileListQQ[0]=file;
  } else {
    fileListQQ[0] = gSystem->ConcatFileName(path,file);
  }
  cout << "DrawDstHist.C, nevents = " << nevents << endl;
  cout << "DrawDstHist.C, firstHist = " << firstHist << endl;
  cout << "DrawDstHist.C, lastHist = " << lastHist << endl;
  cout << "DrawDstHist.C, input file = " << file << endl;
  cout << "DrawDstHist.C, output ps file = " << psFile << endl;
  cout << "DrawDstHist.C, fileListQQ = " << fileListQQ[0] << endl;
// now execute the "real" code:  
  DrawDstHistQQ(nevents,fileListQQ,firstHist,lastHist,psFile);
}


void DrawDstHistQQ(const Int_t nevents,
              const Char_t **fileList, 
              const Char_t *firstHist, 
              const Char_t *lastHist,
              const Char_t *psFile)
{

  // Dynamically link needed shared libs
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("St_Tables");
  gSystem->Load("StIOMaker");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("St_QA_Maker");

  // Handling depends on whether file is a ROOT file or XDF file

  chain  = new StChain("StChain");

  StFile *setFiles= new StFile();

  for (int ifil=0; fileList[ifil]; ifil++)
  { setFiles->AddFile(fileList[ifil]);}
  
  StIOMaker *IOMk = new StIOMaker("IO","r",setFiles,"bfcTree");
  IOMk->SetDebug();

// now setup the rest of the Makers in the chain 
    St_QA_Maker *QA   = new St_QA_Maker;

  // Initialize chain
Int_t iInit = chain->Init();
  if (iInit) chain->Fatal(iInit,"on init");
  chain->PrintInfo();


// method to print out list of histograms - can do this anytime after they're booked
   Int_t NoHist=0;
   NoHist = QA->ListHists();
   cout << " DrawDstHist.C, No. of Hist we have == " << NoHist << endl;

  // Event loop
  int istat;
  for (Int_t i=1; i<=nevents; i++) {
    cout << "============================ Event " << i << " start" << endl;
    chain->Clear();
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


}

  
////////////////////////////////////////////////////////////////
//
// macro: clusterDisplay.C
// author: A.A.P.Suaide (2005)
//
// This macro loops over events in the muDST files, run the
// BEMC cluster finder and creates an event by event display
// of the hits and clusters in the detector. This is an important
// tool for cluster QA because you can test the cluster
// parameters and check the results online.
//
// it also has a method to do statistical cluster QA over many
// events
//
////////////////////////////////////////////////////////////////

class StChain;
class StClusterDisplay;

StChain            *chain = 0;
StClusterDisplay   *t = 0;
TMemStat           memory;
int                n = 0;
int                stat = 0;
int                count = 1;

///////////////////////////////////////////////////////////////
// This method displays a small help
void help()
{
  cout <<"////////////////////////////////////////////////////////////////"<<endl;
  cout <<"//"<<endl;
  cout <<"// macro: clusterDisplay.C"<<endl;
  cout <<"// author: A.A.P.Suaide (2005)"<<endl;
  cout <<"//"<<endl;
  cout <<"// This macro loops over events in the muDST files, run the"<<endl;
  cout <<"// BEMC cluster finder and creates an event by event display"<<endl;
  cout <<"// of the hits and clusters in the detector. This is an important"<<endl;
  cout <<"// tool for cluster QA because you can test the cluster"<<endl;
  cout <<"// parameters and check the results online."<<endl;
  cout <<"//"<<endl;
  cout <<"// it also has a method to do statistical cluster QA over many"<<endl;
  cout <<"// events"<<endl;
  cout <<"//"<<endl;
  cout <<"////////////////////////////////////////////////////////////////"<<endl;
  cout <<endl;
  cout <<"The commands available are:"<<endl;
  cout <<endl;
  cout <<"    setHitThreshold(Int_t det, Float_t th)"<<endl;
  cout <<"        // This method defines the energy threshold for displaying"<<endl;
  cout <<"        // a detector HIT in the display. The default value in the"<<endl;
  cout <<"        // code is 0.2 GeV. Values should de entered in GeV."<<endl;
  cout <<"        //"<<endl;
  cout <<"        // the parameters are:"<<endl;
  cout <<"        //       Int_t det = detector name. (BTOW = 1, BPRS = 2, BSMDE = 3 and BSMDP = 4)"<<endl;
  cout <<"        //       Int_t th  = hit energy threshold in GeV"<<endl;
  cout <<endl;
  cout <<"    next()"<<endl;  
  cout <<"        // This method displays the next event in the queue"<<endl;
  cout <<endl;
  cout <<"    qa(Int_t nevents)"<<endl;
  cout <<"        // This method loops over many events in order to fill some"<<endl;
  cout <<"        // clusters QA histograms. The user needs to open a TBrowser"<<endl;
  cout <<"        // in order to access the histograms"<<endl;
  cout <<"        //"<<endl;
  cout <<"        // the parameters are:"<<endl;
  cout <<"        //        Int_t nevents = number of events to be processed in QA"<<endl;
  cout <<endl;
  cout <<"    help()"<<endl;
  cout <<"        // Displays this help"<<endl;
  cout <<endl;
}
///////////////////////////////////////////////////////////////
// This method defines the energy threshold for displaying
// a detector HIT in the display. The default value in the
// code is 0.2 GeV. Values should de entered in GeV.
//
// the parameters are:
//       Int_t det = detector name. (BTOW = 1, BPRS = 2, BSMDE = 3 and BSMDP = 4)
//       Int_t th  = hit energy threshold in GeV
void setHitThreshold(Int_t det, Float_t th)
{
  t->setHitThreshold(det,th);
  cout <<"\nThreshold in detector = "<<det<<" changed to "<<th<<" GeV."<<endl;
  cout <<"This will have effect only in the next event displayed."<<endl;
  cout <<"\n******* TYPE next() for the next event\n";
  cout <<"******* TYPE help() for help about the methods available\n";
}

///////////////////////////////////////////////////////////////
// This method displays the next event in the queue
void next()
{
  chain->cd();
  t->setDraw(kTRUE);
  chain->Clear();
  stat = chain->Make();
  if(n%count==0) 
  {
    cout << "Finished processing event number "<<n <<endl;
    memory.PrintMem(NULL);
  }
  n++;    
  cout <<"\n******* TYPE next() for the next event\n";
  cout <<"******* TYPE help() for help about the methods available\n";
}

///////////////////////////////////////////////////////////////
// This method loops over many events in order to fill some
// clusters QA histograms. The user needs to open a TBrowser
// in order to access the histograms
//
// the parameters are:
//        Int_t nevents = number of events to be processed in QA
void qa(Int_t nevents = 100)
{
  chain->cd();
  t->setDraw(kFALSE);
  n = 0;
  while ( (stat==0 || stat==1) && n<nevents) 
  {
    chain->Clear();
    stat = chain->Make();
    if(n%count==0) 
    {
      cout << "Finished processing event number "<<n <<endl;
      memory.PrintMem(NULL);
    }
    n++;
  }
  cout <<"\n******* TYPE help() for help about the methods available\n";
}

///////////////////////////////////////////////////////////////
// This is the main method in the macro.
// It loads the libraries, creates the chain and
// instantiate the makers.
// This macro works only with MuDST events but it 
// is very simple for the user to modify it in order to
// run over .event.root files.
//
// the parameters are:
//     char* list = defines the list of data files
//     Int_t nFiles = defines how many files should be read from the list
void clusterDisplay(char* list = "./file.lis",Int_t nFiles  = 10)
{  
  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");
  gSystem->Load("StDaqLib");
  gSystem->Load("StEmcRawMaker");
  gSystem->Load("StEmcADCtoEMaker");

  gStyle->SetOptStat(0);

// create chain    
  chain = new StChain("StChain"); 
   
  maker = new StMuDstMaker(0,0,"",list,"",nFiles);
  StMuDbReader* db = StMuDbReader::instance();

  StMuDst2StEventMaker *m = new StMuDst2StEventMaker();  
  St_db_Maker *dbMk = new St_db_Maker("StarDb","MySQL:StarDb");
  
  StEmcADCtoEMaker *adc=new StEmcADCtoEMaker();
  adc->setPrint(kFALSE);
  
  StPreEclMaker *ecl=new StPreEclMaker();
 
  t = new StClusterDisplay();
       
  chain->Init(); 
  next();     
}


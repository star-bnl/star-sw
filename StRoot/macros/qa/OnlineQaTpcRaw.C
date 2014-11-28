// $Id: OnlineQaTpcRaw.C,v 1.8 2006/08/15 21:43:24 jeromel Exp $
// $Log: OnlineQaTpcRaw.C,v $
// Revision 1.8  2006/08/15 21:43:24  jeromel
// Fix rhic -> rhic.bnl.gov
//
// Revision 1.7  2000/06/20 19:57:38  kathy
// minor updates & cleanup
//
// Revision 1.6  2000/06/14 15:03:55  kathy
// changed default input daq file to be a current one
//
// Revision 1.5  2000/06/14 14:35:42  kathy
// comment out setdebug so zillions of daq messages aren't printed out
//
// Revision 1.4  2000/06/12 15:20:51  kathy
// put in Sergei's code to set output hist file name correctly
//
// Revision 1.2  2000/06/09 18:53:28  kathy
// update comments
//
// Revision 1.1  2000/06/09 16:27:30  kathy
// put new online QA tpc  macro in
//
//======================================================================
// owner:  Alex,Sergei
// what it does:  

// Inputs to macro:
//   nevents  -  # events to process
//   MainFile - input file
//   nevHistOut - #events to write to each output .hist.root file
//              - default is 0 which means it writes out all events 
//                present in input file
//    
//   
//
//======================================================================

class StChain;
StChain *chain;


void OnlineQaTpcRaw(
 Int_t nevents=2, 
 const char *MainFile=
 "/afs/rhic.bnl.gov/star/data/samples/june_2000_st_physics_1164058_raw_0001.daq",
 Int_t nevHistOut=0)
{
//
  cout << " OnlineQaTpcRaw.C: #events to process  = " << nevents << endl;
  cout << " OnlineQaTpcRaw.C: input file name = " << MainFile << endl;
  cout << " OnlineQaTpcRaw.C: #events to write to hist file (0=all) = " << 
            nevHistOut << endl;

// Generate a name for an outHistFile using input file name
//
  fInFile = new TString(MainFile);
  fFileOut = new TString(gSystem->BaseName(fInFile->Data()));
  fFileOut->ReplaceAll(".daq",""); // remove old file extension
  const char *histoDir = "./"; // directory for histos
  fFileOut->Prepend(histoDir);
  const Char_t *outHistFile= fFileOut->Data(); // done


// define other values needed:
   const Char_t *topTree="OnlQaTpcTree";
   const Char_t *makerName="RawTpcQa";


   cout << " OnlineQaTpcRaw.C: output hist file name set to " << 
           outHistFile << ".hist.root" << endl;
   cout << " OnlineQaTpcRaw.C: top level Tree directory set to " << 
           topTree << endl;
   cout << " OnlineQaTpcRaw.C: maker name set to " << 
           makerName << endl;

// load libraries - basic ones needed
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("StIOMaker");
    gSystem->Load("StTreeMaker");

// these 2 are for using the StHistUtil class
    gSystem->Load("StUtilities");
    gSystem->Load("StAnalysisUtilities");

// for raw tpc qa maker
    gSystem->Load("StRawTpcQaMaker");


//  Setup top part of chain
    chain = new StChain("OnlineQaTpcRawChain");
    //    chain->SetDebug(0);
   
// now any Maker that gets constructed will be added to the chain
  StIOMaker *IOMk = new StIOMaker("IO","r",MainFile,topTree);
  //IOMk->SetDebug();
    IOMk->SetIOMode("r");

// histogram utility class (this is not a Maker so not used in chain)
    StHistUtil   *HU  = new StHistUtil;
// now must set pointer to any StMaker so HistUtil can find histograms
//  with StHistUtil methods
// -- input any maker pointer but must cast as type StMaker
   HU->SetPntrToMaker((StMaker *)IOMk);


   StRawTpcQaMaker *RTQa = new StRawTpcQaMaker(makerName);

// output hist.root file:
   StTreeMaker* treeMk = new StTreeMaker("tree",outHistFile,topTree);
      treeMk->SetIOMode("w");
      treeMk->SetBranch("histBranch");


// --- now execute chain member functions
  chain->Init();


// method to print out list of histograms - 
//can do this anytime after they're booked
  Int_t NoHist=0;
  NoHist = HU->ListHists(makerName);
  cout << " !!! OnlineQaTpcRaw.C, No. of Hist Booked = " << NoHist << endl;

  int istat=0;
  int iev=0;
  int countev=0;

// Event loop
EventLoop: if (iev < nevents && !istat) {

    chain->Clear();
    istat = chain->Make(iev);
    
//  count # times Make is called
    iev++;

    cout << " Called Make # " << iev << endl; 
    cout << "     istat value returned from chain Make = " << istat <<

endl;

    if (!istat) {

    countev++;

    cout << " processed event # " << countev << endl;

    }  // istat

    else   // if (istat)
      {
      cout << "Last event processed. Status = " << istat << endl;
    }

    goto EventLoop;

}  // EventLoop
     
  cout << endl;
  cout << "QAInfo: End of Job " << endl; 
  cout << "QAInfo: # times Make called = " << iev << endl;
  cout << "QAInfo:  # events read = " << countev << endl;

  chain->Finish();

}

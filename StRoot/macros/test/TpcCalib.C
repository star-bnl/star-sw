//=======================================================================
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// Macro to perform the calibration of the TPC                          //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef __CINT__
#include "TBrowser.h"
#include "TString.h"
#include "TSystem.h"
//#include "St_XDFFile.h"
#include "StChain.h"
#endif
TBrowser *b = 0;
class StChain;        
StChain  *chain=0;

TString *InFile = 0;
Int_t NoEvents = 0;

//_____________________________________________________________________
//_____________________________________________________________________
void Load(){
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  //gSystem->Load("xdf2root");
  gSystem->Load("StDaqLib");
  gSystem->Load("St_Tables");
  gSystem->Load("StIOMaker");
  gSystem->Load("StDAQMaker");
  gSystem->Load("StUtilities");
  //gSystem->Load("/opt/star/lib/mysql/libmysqlclient");
  gSystem->Load("StDbLib.so");
  gSystem->Load("StDbMaker.so");
  gSystem->Load("StTpcDb.so");     // load StTpcDb library

  // 
  gSystem->Load("StTpcCalibrationMaker");
}
//_____________________________________________________________________

void TpcCalib(const Int_t NPulserEvents=85, const Int_t CalibType=1, 
	Char_t *pedfile= "/disk1/star/daq/990803.3200.01.daq", 
	Char_t *pulsfile="/disk1/star/daq/990803.3201.01.daq", 
	       //Char_t *pedfile="/disk1/star/daq/st_pedestal_0003704_raw_0001.daq",
	       //Char_t *pulsfile="/disk1/star/daq/st_physics_0003703_raw_0001.daq",
	Char_t *outfile="Calib")
{
  
// --- Dynamically link some shared libs
  if (gClassTable->GetID("StChain") < 0) Load();


// --- Calibration setup
  StTpcCalibSetup* CalibSetup = new StTpcCalibSetup;
  //CalibSetup->setMaxNumberOfCorruptedTB(300);
  //CalibSetup->setCalibOutFile("BadTest.txt");
  //CalibSetup->setRootOutFileName("BadTest.root");

// --- Start chain
    if (!chain) delete chain;
    chain = new StChain("TpcCalib");

// --- Data base
  //  StDbMaker* DbMk = new StDbMaker("TrsES99","StarDb");  
  //DbMk->SetTime(1);

// --- Declare makers 
    StIOMaker* IOMk;
    StMaker* CalibMk;
// ____________________________________________________
//                    BAD TAGGING
//
  if(CalibType==1) {
// --- IO : open the Pedestal RMS file
    StFile *tPedFile= new StFile();
    tPedFile->AddFile(pedfile);
    IOMk = new StIOMaker("IORMS","r",tPedFile);
    chain->SetInput("StDAQReader",".make/IORMS/.make/IORMS_DAQ/.const/StDAQReader");
// --- Instantiate the bad channel maker
    cout << "New bad maker" << endl;
    CalibMk = new StTpcBadChanMaker("TpcBadMaker",CalibSetup);
  }
//
// _______________________________________________________
//                    DEAD TAGGING
//
  else{
  if(CalibType==2){
// --- IO : open the zero supressed file
    StFile *tPulsFile= new StFile();
    tPulsFile->AddFile(pulsfile);
    IOMk = new StIOMaker("IORMS","r",tPulsFile);
    chain->SetInput("StDAQReader",".make/IORMS/.make/IORMS_DAQ/.const/StDAQReader");
// --- Instantiate the dead channel maker
    cout << "New dead maker" << endl;
    CalibMk = new StTpcDeadChanMaker("TpcDeadMaker",CalibSetup);
  }
//
// _______________________________________________________
//                    FEE GAIN CALIBRATION
//
  else{
  if(CalibType==3){
// --- IO : open the zero suppressed file
    StFile *tPulsFile= new StFile();
    tPulsFile->AddFile(pulsfile);
    IOMk = new StIOMaker("IORMS","r",tPulsFile);
    chain->SetInput("StDAQReader",".make/IORMS/.make/IORMS_DAQ/.const/StDAQReader");
// --- Instantiate the FEE gain calib maker
    cout << "New FEE gain calib maker" << endl;
    CalibMk = new StTpcFEEGainCalibMaker("TpcFEEGainCalibMaker",CalibSetup);
  }
// _______________________________________________________
  else{
    cout << "Wrong calib type " << CalibType << endl;
  }
  }
  }
//
// _______________________________________________________
//                  Run the chain
//
  chain->Init();
  int istat=0,i=1;
 EventLoop: if (i <= NPulserEvents && !istat) {
   cout << "============================ Event " << i << " start" << endl;
   chain->Clear();
   istat = chain->Make();
   if (istat) {cout << "Last event processed. Status = " << istat << endl;}
   i++; goto EventLoop;
 }
  
  Int_t iFinish=chain->Finish();
  printf ("Run completed ");
  gSystem->Exec("date");

}






//////////////////////////////////////////////////////////////////////////
//                                                                      //
//                   BAD CHANNEL FINDER                                 //
// For the time being, the finder look for corrupted pads :
// It makes histograms of the number of coorupted time bin as a function
// of the pad.
//                                                                      //
//////////////////////////////////////////////////////////////////////////
// C/C++
#include <iostream.h>
#include <fstream.h>
#include <stdio.h>
#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <math.h>
#include <string.h>
// Root
#include "TFile.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TNtuple.h"
// STAR
#include "StChain.h"
#include "St_DataSetIter.h"
//#include "StTpcDb/StTpcDb.h"
#include "StDAQMaker/StDAQReader.h"
#include "StRoot/StDaqLib/TPC/fee_pin.h"
// Local
#include "StTpcBadChanMaker.h"
#include "StTpcCalibSetup.h"
#include "StTpcCalibSector.h"
// root again
ClassImp(StTpcBadChanMaker)
//  
//_____________________________________________________________________
// --- Constructor
//
StTpcBadChanMaker::StTpcBadChanMaker(const char *aMakerName,       
			       const StTpcCalibSetup* aSetup)
:
  StMaker(aMakerName),
  mSetup(aSetup)
{

}
//
//_____________________________________________________________________
// --- Destructor
//
StTpcBadChanMaker::~StTpcBadChanMaker(){
delete [] mTpcCalibSector;
}
//_____________________________________________________________________
// --- Init
//
Int_t StTpcBadChanMaker::Init(){

  cout << "StTpcBadChanMaker::Init()" << endl;
  //int tNumberOfSector=gStTpcDb->Dimensions()->numberOfSectors();
  int tNumberOfSector=24;

  static int tNPadAtRow[45]={
  88,96,104,112,118,126,134,142,150,158,166,174,182,
  98,100,102,104,106,106,108,110,112,112,114,116,118,120,122,122,
  124,126,128,128,130,132,134,136,138,138,140,142,144,144,144,144};
  mTpcCalibSector = new StTpcCalibSector*[tNumberOfSector];
  for(int tiSect=0;
      tiSect<tNumberOfSector;
      tiSect++){
    mTpcCalibSector[tiSect]= new StTpcCalibSector(mSetup,(tiSect+1),tNPadAtRow);
  }

  return StMaker::Init();
}
//
//_____________________________________________________________________
// --- Make calculation
//
Int_t StTpcBadChanMaker::Make(){
  //int tNumberOfSector=gStTpcDb->Dimensions()->numberOfSectors();
  int tNumberOfSector=24;
  //cout << "StTpcBadChanMaker::Make()" << endl;
  St_DataSet * tDataSet=GetDataSet("StDAQReader");
  //cout << "Got the data set " << tDataSet<< endl;
  StDAQReader *tDAQReader=(StDAQReader*)(tDataSet->GetObject());
  //cout << "Got the daq reader " << tDAQReader<< endl;
  StTPCReader *tRMSReader=tDAQReader->getTPCReader(); 
  //cout << "Got the RMS reader " << tRMSReader<< endl;
  if(!tRMSReader) {cout << "Coudln't get the reader " << endl;}
  for(int tiSect=0;
      tiSect< tNumberOfSector;
      tiSect++){
    mTpcCalibSector[tiSect]->updateBad(tRMSReader);
  }
  delete tDataSet;
  delete tDAQReader;
  delete tRMSReader;

  return kStOK;
}
//_____________________________________________________________________________
void StTpcBadChanMaker::PrintInfo(){
  printf("**************************************************************\n");
  printf("* $Id: StTpcBadChanMaker.cxx,v 1.1 1999/09/09 17:41:45 fretiere Exp $\n");
  printf("**************************************************************\n");
  if (Debug()) StMaker::PrintInfo();
}

void StTpcBadChanMaker::CalcElectronicConvertor(int** aPadToFeeConvertor, 
					     int** aPadToRDOConvertor){


  for(int tiFee=0;tiFee<182;tiFee++){
    for(int tiPin=0;tiPin<32;tiPin++){
      if(row_vs_fee[tiFee][tiPin]!=0 && pad_vs_fee[tiFee][tiPin]!=0){
	//      cout << tiFee << " " << tiPin << " " << row_vs_fee[tiFee][tiPin] << " "
	//  << pad_vs_fee[tiFee][tiPin] << " " 
	//    << rdo_vs_fee[tiFee][tiPin] << endl;
	aPadToFeeConvertor[(row_vs_fee[tiFee][tiPin]-1)]
	                  [(pad_vs_fee[tiFee][tiPin]-1)]=(tiFee+1);
	aPadToRDOConvertor[(row_vs_fee[tiFee][tiPin]-1)]
	                  [(pad_vs_fee[tiFee][tiPin]-1)]=rdo_vs_fee[tiFee][tiPin];
      }
    }
  }
  /*
  int tNPadAtRow[45]={
  88,96,104,112,118,126,134,142,150,158,166,174,182,
  98,100,102,104,106,106,108,110,112,112,114,116,118,120,122,122,
  124,126,128,128,130,132,134,136,138,138,140,142,144,144,144,144};
  for(int tiRow=1;tiRow<=45;tiRow++){
    for(int tiPad=1;tiPad<=tNPadAtRow[tiRow-1];tiPad++){
      cout << tiRow << " " << tiPad << " " 
	   << aPadToFeeConvertor[tiRow-1][tiPad-1] << " "
	   << aPadToRDOConvertor[tiRow-1][tiPad-1] << endl;
    }
    }*/
}
//_____________________________________________________________________________
Int_t StTpcBadChanMaker::Finish(){
  cout << "StTpcBadChanMaker::Finish()" << endl;

  //int tNumberOfSector=gStTpcDb->Dimensions()->numberOfSectors();
  int tNumberOfSector=24;
  //  mHPercOfBad = new TH1D("HPerBad","Percentage of bad pads",24,0.5,24.5);
  TFile *tHFile = new TFile(mSetup->getRootOutFileName(),"recreate");
  ofstream* tBadFile = new ofstream(mSetup->getBadFileName());

  int** tPadToFeeConvertor= new int*[45];
  int** tPadToRDOConvertor= new int*[45];
  for(int tiRow=0;tiRow<45;tiRow++){
    tPadToFeeConvertor[tiRow]= new int[182];
    tPadToRDOConvertor[tiRow]= new int[182];
  }
  CalcElectronicConvertor(tPadToFeeConvertor,tPadToRDOConvertor);

  for(int tiSect=0;
      tiSect<tNumberOfSector;
      tiSect++){
    mTpcCalibSector[tiSect]->findBad();
    mTpcCalibSector[tiSect]->findBadElectronics(tPadToFeeConvertor,
					     tPadToRDOConvertor);
    mTpcCalibSector[tiSect]->writeBadHisto();
    mTpcCalibSector[tiSect]->writeBadTable(tBadFile);

  }
  //mHPercOfBad->Write();
  tHFile->Close();
  tBadFile->close();
  return kStOK;
}
//_____________________________________________________________________________
Int_t StTpcBadChanMaker::Clear(){
   return kStOK;
}




//////////////////////////////////////////////////////////////////////////
//                                                                      //
//                   DEAD CHANNEL FINDER                                //
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
#include "StTpcDeadChanMaker.h"
#include "StTpcCalibSetup.h"
#include "StTpcCalibSector.h"
// root again
ClassImp(StTpcDeadChanMaker)
//  
//_____________________________________________________________________
// --- Constructor
//
StTpcDeadChanMaker::StTpcDeadChanMaker(const char *aMakerName,       
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
StTpcDeadChanMaker::~StTpcDeadChanMaker(){
delete [] mTpcCalibSector;
}
//_____________________________________________________________________
// --- Init
//
Int_t StTpcDeadChanMaker::Init(){

  cout << "StTpcDeadChanMaker::Init()" << endl;
  //int tNumberOfSector=gStTpcDb->Dimensions()->numberOfSectors();
  int tNumberOfSector=24;

  static int tNPadAtRow[45]={
  88,96,104,112,118,126,134,142,150,158,166,174,182,
  98,100,102,104,106,106,108,110,112,112,114,116,118,120,122,122,
  124,126,128,128,130,132,134,136,138,138,140,142,144,144,144,144};
  ifstream* tBadFile = new ifstream(mSetup->getBadFileName());

  mTpcCalibSector = new StTpcCalibSector*[tNumberOfSector];
  for(int tiSect=0;
      tiSect<tNumberOfSector;
      tiSect++){
    mTpcCalibSector[tiSect]= new StTpcCalibSector(mSetup,(tiSect+1),
						  tNPadAtRow);
    mTpcCalibSector[tiSect]->readBadTable(tBadFile);
  }
  tBadFile->close();
  delete tBadFile;

  return StMaker::Init();
}
//
//_____________________________________________________________________
// --- Make calculation
//
Int_t StTpcDeadChanMaker::Make(){
  //int tNumberOfSector=gStTpcDb->Dimensions()->numberOfSectors();
  int tNumberOfSector=24;
  //  cout << "StTpcDeadChanMaker::Make()" << endl;
  static St_DataSet * tDataSet=GetDataSet("StDAQReader");
  //cout << "Got the data set " << tDataSet<< endl;
  static StDAQReader *tDAQReader=(StDAQReader*)(tDataSet->GetObject());
  //cout << "Got the daq reader " << tDAQReader<< endl;
  static StTPCReader *tZSupReader=tDAQReader->getTPCReader(); 
  //cout << "Got the zero suppressed reader " << tZSupReader<< endl;
  if(!tZSupReader) {cout << "Coudln't get the reader " << endl;}
  for(int tiSect=0;
      tiSect< tNumberOfSector;
      tiSect++){
    mTpcCalibSector[tiSect]->updateDead(tZSupReader);
  }
  //  delete tDataSet;
  //delete tDAQReader;
  //delete tZSupReader;

  return kStOK;
}
//_____________________________________________________________________________
void StTpcDeadChanMaker::PrintInfo(){
  printf("**************************************************************\n");
  printf("* $Id: StTpcDeadChanMaker.cxx,v 1.1 1999/09/14 12:42:14 fretiere Exp $\n");
  printf("**************************************************************\n");
  if (Debug()) StMaker::PrintInfo();
}

void StTpcDeadChanMaker::CalcElectronicConvertor(int** aPadToFeeConvertor, 
					     int** aPadToRDOConvertor){

  // Eventually this function should be replaced by a database function
  for(int tiFee=0;tiFee<182;tiFee++){
    for(int tiPin=0;tiPin<32;tiPin++){
      if(row_vs_fee[tiFee][tiPin]!=0 && pad_vs_fee[tiFee][tiPin]!=0){
	aPadToFeeConvertor[(row_vs_fee[tiFee][tiPin]-1)]
	                  [(pad_vs_fee[tiFee][tiPin]-1)]=(tiFee+1);
	aPadToRDOConvertor[(row_vs_fee[tiFee][tiPin]-1)]
	                  [(pad_vs_fee[tiFee][tiPin]-1)]=
	  rdo_vs_fee[tiFee][tiPin];
      }
    }
  }
}
//_____________________________________________________________________________
Int_t StTpcDeadChanMaker::Finish(){
  cout << "StTpcDeadChanMaker::Finish()" << endl;

  //int tNumberOfSector=gStTpcDb->Dimensions()->numberOfSectors();
  int tNumberOfSector=24;
  //  mHPercOfDead = new TH1D("HPerDead","Percentage of bad pads",24,0.5,24.5);
  TFile *tHFile = new TFile(mSetup->getRootOutFileName(),"recreate");
  ofstream* tDeadFile = new ofstream(mSetup->getDeadFileName());

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
    mTpcCalibSector[tiSect]->findDead();
    mTpcCalibSector[tiSect]->findDeadElectronics(tPadToFeeConvertor,
					     tPadToRDOConvertor);
    mTpcCalibSector[tiSect]->writeDeadHisto();
    mTpcCalibSector[tiSect]->writeDeadTable(tDeadFile);

  }
  //mHPercOfDead->Write();
  tHFile->Close();
  tDeadFile->close();
  return kStOK;
}
//_____________________________________________________________________________
Int_t StTpcDeadChanMaker::Clear(){
   return kStOK;
}




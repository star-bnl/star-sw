//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StRawTpcQaMaker class for Makers                                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include <iostream.h>
#include <stdio.h>
#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <math.h>


#include "StRawTpcQaMaker.h"
#include "StChain.h"
#include "St_DataSetIter.h"

#include "StDAQMaker/StDAQReader.h"
#include "StDaqLib/TPC/trans_table.hh"

#include "StDAQMaker/StDAQReader.h"

#include "TH1.h"
#include "TH2.h"

#include "hardWired.h"

StDAQReader *victorPrelim;
StTPCReader *victor;


ClassImp(StRawTpcQaMaker)
  
//_____________________________________________________________________________

  StRawTpcQaMaker::StRawTpcQaMaker(const char *name):
    StMaker(name)
{
  cout << endl << " in constructor for StRawTpcQaMaker " << endl << endl;
}


//_____________________________________________________________________________

StRawTpcQaMaker::~StRawTpcQaMaker(){}


//_____________________________________________________________________________

Int_t StRawTpcQaMaker::Init(){

  cout <<"  in StRawTpcQaMaker Init"<<endl;

  St_DataSet *herb;
  herb=GetDataSet("StDAQReader");
  assert(herb);

  victorPrelim=(StDAQReader*)(herb->GetObject());
  assert(victorPrelim);
   

   myH1 = new TH2F("RawTpcMxPad","Tpc - max pads vs row",
          50,0.,50.,50,0.,5000.); 

   myH2 = new TH1F("RawTpcSectNum","Tpc - sector number",25,0.,25.);   

 
  return StMaker::Init();

}

//_____________________________________________________________________________

Int_t StRawTpcQaMaker::Make(){

  cout << endl << " in StRawTpcQaMaker::Make() " << endl << endl;

  victor=victorPrelim->getTPCReader();
  assert(victor);

  // Now you have a pointer named "victor".  It is a pointer to class
  // StTPCReader, which is defined in $STAR/StDAQMaker/StDAQReader.h.
  // You can use "victor" as follows:
  //         victor->getPadList(....
  //         victor->getSequences(....

// this is a test:
  //  Int_t kathy30;
  //Int_t kathy45;
  //kathy30 = victor->getMaxPad(30);
  //  cout << " max pads in row 30 = " << kathy30 << endl;
  //kathy45 = victor->getMaxPad(45);
  //  cout << " max pads in row 45 = " << kathy45 << endl;

  Float_t mMxPd=0.0;
  for (Int_t nRow=1; nRow<=45; nRow++) {
    mMxPd=victor->getMaxPad(nRow);
    //    cout << " row#, max pads = " << nRow << " " << mMxPd << endl;
    myH1->Fill(nRow,mMxPd);
  }

  //
  int npad;
  unsigned char* padList;
   
// loop over sectors
  for (Int_t sectorIndex=0; sectorIndex < N__SECTORS; sectorIndex++) 
  {
    cout << "working on sector# ";
    cout << " " << sectorIndex << "===>"<< endl;

    myH2->Fill(sectorIndex);

// loop over rows
    for ( int r=0; r < ROWS__PER__SECTOR; r++) 
    { 
      npad = victor->getPadList(sectorIndex+1, r+1 ,padList);
      //  cout << "Row " << r << " Npad " << npad << endl;
    } // loop over rows 

  }
  return kStOK;
}

//_____________________________________________________________________________
void StRawTpcQaMaker::PrintInfo(){
  printf("**************************************************************\n");
  printf("* $Id: StRawTpcQaMaker.cxx,v 1.1 2000/06/09 18:21:50 kathy Exp $\n");
  printf("**************************************************************\n");
  if (Debug()) StMaker::PrintInfo();
}

//_____________________________________________________________________________

Int_t StRawTpcQaMaker::Finish(){

cout<<" StRawTpcQaMaker::Finish " << endl;

  return kStOK;
}
//_____________________________________________________________________________
Int_t StRawTpcQaMaker::Clear(){
   return kStOK;
}
















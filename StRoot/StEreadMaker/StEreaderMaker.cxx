/***************************************************************************
 *
 * $Id: StEreaderMaker.cxx,v 1.1 2001/07/17 00:05:30 perev Exp $
 *
 * Author:  bl
 ***************************************************************************
 *
 * Description: RICH offline software:
 *              StRchMaker.cxx - ROOT/STAR Maker for offline chain.
 *              Incorporation of cluster finder here
 ***************************************************************************
 *
 * See Log Comments at bottom
 ***************************************************************************/

#include <iostream.h>
#include <fstream.h>

#include "StEreaderMaker.h"
#include "StChain.h"
#include "St_DataSetIter.h"

#include "StGlobals.hh"

//
// Interfaces
//
// DAQ Libraries
#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDaqLib/EMC/EMC_Reader.hh"
#include "StDAQMaker/StDAQReader.h"

ClassImp(StEreadMaker) // macro
   
//-----------------------------------------------------------------

    StEreadMaker::StEreadMaker(const char *name, int daq)
	: StMaker(name), mDaq(daq)
{}

//-----------------------------------------------------------------

StEreadMaker::~StEreadMaker() {}

//-----------------------------------------------------------------

Int_t StEreadMaker::Init() {
    cout << "StEreaderMaker::init()" << endl;
    //
    // either DAQ or SIM data.  MACRO switchable!
    //

    return StMaker::Init();
}
//-----------------------------------------------------------------

Int_t StEreadMaker::Make() {
    cout << "EreaderMaker::Make()" << endl;

    //
	if(!mDaq) {
	    cout << "Sim Mode" << endl;
	}
	else {
	    cout << "DAQ" << endl;
	    mTheEmcData   = GetDataSet("StDAQReader");

	    if(!mTheEmcData) {
		cout << "StEreaderMaker::Maker()\n";
		cout << "\t DataSet: StDAQReader not there\n";
		cout << "\tSkip this event\n" << endl;
		return kStWarn;
	    }
	    else{cout<<" GOT DaqReader dataset **"<<endl;}

	    mTheDataReader = (StDAQReader*)(mTheEmcData->GetObject());
	    if(!mTheDataReader) {
		cout << "StEreaderMaker::Maker()\n";
		cout << "\tStDAQReader*: not there\n";
		cout << "\tSkip this event\n" << endl;
		return kStWarn;
	    }
	    else{cout<<" GOT DaqReader object, look for emc**"<<endl;}

	    if (!(mTheDataReader->EMCPresent())) {
		cout << "StEreaderMaker::Maker()\n";
		cout << "\tRICH not in datastream\n";
		cout << "\tSkip this event\n" << endl;
		return kStWarn;
	    }
	    else{cout<<"EMC prsent seen from datareader"<<endl;}

	    mTheEmcReader = mTheDataReader->getEMCReader();
	}
	cout<<"I am here now**"<<endl;

	if(mTheEmcReader) {
	    cout << "Got the Reader " << endl;
	    cout<<" getting ADCs***"<<endl;
/*
	    unsigned short *(AdcList[2]);  
       if(mTheEmcReader->getTowerADCR(mod,AdcList)){
       }
       else{cout<<" problem in ADCR extraction"<<endl;}

*/

    for(int i=0;i<120;i++){

//Tower
    for(int j=0;j<20;j++){
      for(int k=0;k<2;k++){
	
	unsigned short ADC=0;
	if(mTheEmcReader->getTowerADC(i+1,j+1, k+1,ADC)){
//	  	  if(ADC>0)cout<<"Eread::ADCDATA** mod"<<i+1<<"eta "<<j+1<<"phi "<<k+1<<"ADC "<<ADC<<endl;
	}
      }
    }
    //SMDE
   for(int j=0;j<150;j++){
	unsigned short ADC=0;
	if(mTheEmcReader->getSMDE_ADC(i+1,j+1,ADC)){
	  	  if(ADC>0)cout<<"Eread::SMDE,ADCDATA** mod"<<i+1<<"strip "<<j+1<<"ADC "<<ADC<<endl;
	}
        else{}
     }

    //SMDP

    for(int j=0;j<10;j++){
      for(int k=0;k<15;k++){
	unsigned short ADC=0;
	if(mTheEmcReader->getSMDP_ADC(i+1,j+1, k+1,ADC)){
	  if(ADC>0)cout<<"Eread::SMDP,ADCDATA** mod"<<i+1<<"Bin "<<j+1<<"strip "<<k+1<<"ADC "<<ADC<<endl;
	}
        else{}
      }
    }

  }

	}
	else {
	    cout << "StEreaderMaker::Make()\n";
	    cout << "\tCould not get a Reader\n";
	    cout << "\tSkip Event" << endl;
	    return kStWarn;
	}

	//
	// In possession of a RichReader
	//  --> use the interface to extract the pixels
	//     and load the PixelStore for the CF
	// The decoding is done such that it is a
	// saturated pad if the 11th bit is set
	//
		
    // Cleanup:  A new reader is made for each event
    //           if we are using the daq libraries
    //
    if(!mDaq) {
	if(mTheEmcReader) {
	    delete mTheEmcReader;
	}
	mTheEmcReader = 0;
    }

    return kStOK;
}

//-----------------------------------------------------------------
void StEreadMaker::PrintInfo() 
{
    printf("**************************************************************\n");
    printf("* $Id: StEreaderMaker.cxx,v 1.1 2001/07/17 00:05:30 perev Exp $\n");
    printf("**************************************************************\n");
    if (Debug()) StMaker::PrintInfo();
}

//-----------------------------------------------------------------


Int_t StEreadMaker::Finish() {

    cout << "StRchMaker::Finish()" << endl;
    return StMaker::Finish();
}

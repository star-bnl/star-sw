//////////////////////////////////////////////////////////////////////////
//
// You must select a data base initializer method
#define TPC_DATABASE_PARAMETERS
//#define ROOT_DATABASE_PARAMETERS
//#define aSCII_DATABASE_PARAMETERS
//
//////////////////////////////////////////////////////////////////////////

#include <stdio.h>      // For binary file input (the DAQ data file).
#include <string.h>     // For binary file input (the DAQ data file).
#include <sys/types.h>  // For binary file input (the DAQ data file).
#include <sys/stat.h>   // For binary file input (the DAQ data file).
#include <fcntl.h>      // For binary file input (the DAQ data file).

#include "Stiostream.h"
#include <unistd.h>    // needed for access()/sleep()
#include <math.h>
#include <string>
#include <algorithm>
#include <vector>
#include <list>
#include <utility>    // pair
#include <algorithm>  // min() max()
#include <ctime>      // time functions

#include "StMixerMaker.h"

// SCL
#include "StGlobals.hh"
#include "Randomize.h"
#include "TDataSetIter.h"
#include "TObjectSet.h"

// DataBase Initialization
// Dave's Header file
#include "StTpcDb/StTpcDb.h"
#include "StTrsMaker/include/StTpcDbGeometry.hh"
#include "StTrsMaker/include/StTpcDbSlowControl.hh"
#include "StTrsMaker/include/StTpcDbElectronics.hh"
#include "StTrsMaker/include/StTrsFastDigitalSignalGenerator.hh"
//#include "StTrsMaker/include/StDbMagneticField.hh" // To be done

// processes
#include "StTrsMaker/include/StTrsDigitalSignalGenerator.hh"

// containers
#include "StTrsMaker/include/StTrsAnalogSignal.hh"
#include "StTrsMaker/include/StTrsSector.hh"
#include "StTrsMaker/include/StTrsDigitalSector.hh"
#include "StTrsMaker/include/StTpcElectronics.hh"

// outPut Data--decoder
#include "StTrsMaker/include/StTrsRawDataEvent.hh"
#include "StTrsMaker/include/StTrsDetectorReader.hh"
#include "StTrsMaker/include/StTrsZeroSuppressedReader.hh"
//VP#include "StDaqLib/GENERIC/EventReader.hh"
#include "StSequence.hh"
//#include "StDaqLib/TPC/trans_table.hh"
#include "StDAQMaker/StDAQReader.h"

#include "StDaqLib/TPC/trans_table.hh"

ClassImp(StMixerMaker)

//______________________________________________________________________________
StMixerMaker::StMixerMaker(const char *name,char *kind1,char *kind2):StMaker(name), 
			   mMergeSequences(1),mFirstSector(1),mLastSector(24)  
{ gConfig[0]=kind1; gConfig[1]=kind2; mAllTheDataMixer=0; }


//______________________________________________________________________________
StMixerMaker::~StMixerMaker() { /* nopt */ }


//______________________________________________________________________________
int StMixerMaker::writeFile(char* file, int numEvents)
{
    mOutputFileName = file;
    mNumberOfEvents = numEvents;
    return kStOK;
}

//______________________________________________________________________________
Int_t StMixerMaker::InitRun(int RunId) {

  // The global pointer to the Db is gStTpcDb and it should be created in the macro.  
  mGeometryDb    = StTpcDbGeometry::instance();
  mElectronicsDb = StTpcDbElectronics::instance();
  mSlowControlDb = StTpcDbSlowControl::instance();

  mSector = new StTrsSector(mGeometryDb);
  mDigitalSignalGenerator = new StTrsFastDigitalSignalGenerator(mElectronicsDb, mSector,-1);


  // Output is into an StTpcRawDataEvent* vector<StTrsDigitalSector*>
  // which is accessible via the StTrsUnpacker.  Initialize the pointers!
  
  // Construct constant data sets.  This is what is passed downstream
  mAllTheDataMixer = new StTrsRawDataEvent(mGeometryDb->numberOfSectors());
  TObjectSet *set = new TObjectSet("MixerEvent"  , mAllTheDataMixer, kFALSE);
  AddConst(set);
  return kStOk;
}


//______________________________________________________________________________
Int_t StMixerMaker::Make() {
  
  
static const char *input[2] = {"Input1","Input2"};
  for (int ir=0;ir<2;ir++) {
    TDataSet *dataset = GetDataSet(input[ir]);  assert(dataset);
    if(strcmp(gConfig[ir],"daq")==0) {
      // DAQ
      StDAQReader* daqReader =(StDAQReader*)(dataset->GetObject()); assert(daqReader);
      StTPCReader *tpcReader = daqReader->getTPCReader(); 
      if(!tpcReader) return kStWarn;
      tpcReader->SetSequenceMerging(mMergeSequences);
      mReader[ir].Set(tpcReader);
    } else {
      StTpcRawDataEvent* trsEvent = (StTpcRawDataEvent*)dataset->GetObject(); 
      string version = "TrsDatav1.0"; 
      StTrsDetectorReader *trsDetReader = new StTrsDetectorReader(trsEvent, version); 
      mReader[ir].Set(trsDetReader);
    }
  }
  
  int numberOfRows = mSector->numberOfRows();
  for(int sector=mFirstSector; sector<=mLastSector; sector++) {
    mSector->clear();
    for (int ir=0;ir<2;ir++) {//readers loop
      for(int irow=0; irow<numberOfRows; irow++) { //loop over rows
        unsigned char* padList; 
        int numberOfPads = mReader[ir].getPadList(sector,irow+1,&padList);
	
	for(int ipad=0; ipad<numberOfPads; ipad++) {//loop over pads
	  int pad=padList[ipad];
          assert(pad);
	  int nseq=0;
	  StSequence* listOfSequences=0; 
	  int **listIdTruth =0;
	  mReader[ir].getSequences(sector,irow+1,pad,&nseq,&listOfSequences,&listIdTruth);

	      // Note that ipad is an index, NOT the pad number. 
	      // The pad number comes from padList[ipad] 
	  for(int iseq=0;iseq<nseq;iseq++) { //seq loop
	    int startTimeBin=listOfSequences[iseq].startTimeBin;
	    assert(startTimeBin>=0 && startTimeBin<=511);
	    int seqLen=listOfSequences[iseq].length;
	    unsigned char *pointerToAdc=listOfSequences[iseq].firstAdc;
	    for(int jbin=0;jbin<seqLen;jbin++) { //time loop
              int idt = (listIdTruth)? listIdTruth[iseq][jbin]:0;
	      float conversion=log8to10_table[pointerToAdc[jbin]]; // 10 bit
	      StTrsAnalogSignal padSignal(startTimeBin+jbin,conversion,idt);
	      mSector->addEntry(irow+1,pad,padSignal);
	    } //end time loop
	  }//end  seq loop    
	}//end pad loop
      }//end row loop
    }//end reader loop

    mSector->sort();
    
    // Digitize the Signals
    //
    // First make a sector where the data can go...
    StTrsDigitalSector  *aDigitalSector = new StTrsDigitalSector(mGeometryDb);
    
    // Point to the object you want to fill
    //
    mDigitalSignalGenerator->fillSector(aDigitalSector);
    
    // cout << "sector" << ' ' << sector+1 << endl;
    // ...and digitize it
    // cout << "--->digitizeSignal()..." << endl;
    mDigitalSignalGenerator->digitizeSignal();
    //cout<<"--->digitizeSignal() Finished..." << endl;
    
    // Fill it into the event structure...
    // and you better check the sector number!
    
    mAllTheDataMixer->mSectors[(sector-1)] = aDigitalSector;
    // Clear and reset for next sector:
    
  }// sector loop
    
  return kStOK;
} // Make() 

//______________________________________________________________________________
void StMixerMaker::Clear(Option_t *opt)
{
  if (mAllTheDataMixer) mAllTheDataMixer->clear();
  mReader[0].Clear(); 
  mReader[1].Clear(); 
  StMaker::Clear();
}

//______________________________________________________________________________
Int_t StMixerMaker::Finish()
{
  // Clean up all the pointers that were initialized in StTrsMaker::Init()
  // if (mOutputStreamMixer) delete mOutputStream;
  // mOutputStream = 0;
  delete mSector; 			mSector 		= 0;
  delete mAllTheDataMixer; 		mAllTheDataMixer 	= 0;
  delete mDigitalSignalGenerator; 	mDigitalSignalGenerator = 0;    
  return kStOK;
}

//______________________________________________________________________________
/// See method in St_tpcdaq_Maker and TPCReader. See comments there (FCF related).
char  StMixerMaker::SetSequenceMerging(char mergeSequences)
{
  mMergeSequences=mergeSequences;
  return mMergeSequences;
}


//______________________________________________________________________________
//______________________________________________________________________________
//______________________________________________________________________________
//______________________________________________________________________________
StMixerReader::StMixerReader()
{
 mTpcReader=0;mTrsReader=0;
 mTrsDetectorReader=0; mSector=-1;
}  
//______________________________________________________________________________
void StMixerReader::Set(StTPCReader *r)
{
  mSector=-1; mTrsReader=0;mTpcReader=r;
}
//______________________________________________________________________________
void StMixerReader::Set(StTrsDetectorReader *r)
{
  mSector=-1; mTrsReader=0;mTpcReader=0;mTrsDetectorReader=r;;
}
//______________________________________________________________________________
void StMixerReader::SetSector(int sector)
{
  if (mSector==sector) return;
  mSector=sector;
  mTrsReader = mTrsDetectorReader->getZeroSuppressedReader(sector);
}
//______________________________________________________________________________
int  StMixerReader::getSequences(int sector,int row,int pad,int *nseq,StSequence **listOfSequences, int ***listIdTruth)
{
  if (mTrsDetectorReader) {
    SetSector(sector);
    return mTrsReader->getSequences(row,pad,nseq,listOfSequences,listIdTruth);
  } else {
    TPCSequence **liztOfSequences = (TPCSequence**)listOfSequences;
    if (listIdTruth) *listIdTruth=0;
    return mTpcReader->getSequences(sector,row,pad,*nseq,*liztOfSequences);
  }
}
//______________________________________________________________________________
int StMixerReader::getPadList(int sector,int row, unsigned char **padList)
{
  *padList=0;
  if (mTrsDetectorReader) {
    SetSector(sector);
    if (!mTrsReader) return 0;
    return mTrsReader->getPadList(row,padList);
  } else {
    return mTpcReader->getPadList(sector,row,*padList);
  }
}

//______________________________________________________________________________
void StMixerReader::Clear()
{
  delete mTrsDetectorReader; mTrsDetectorReader=0;
}


/***************************************************************************
 *
 * $Id: StMixerMaker.cxx,v 1.2 2000/02/22 20:25:23 pfachini Exp $
 *
 * Author: Patricia Fachini
 *
 ***************************************************************************
 *
 * Description: Maker for doing the embedding (mixing)
 *              
 ***************************************************************************
 *
 * $Log: StMixerMaker.cxx,v $
 * Revision 1.2  2000/02/22 20:25:23  pfachini
 * *** empty log message ***
 *
 * Revision 1.1  2000/02/16 21:02:09  pfachini
 * First version StMixer
 *
 *
 **************************************************************************/

//////////////////////////////////////////////////////////////////////////
//
// You must select a data base initializer method
//#define TPC_DATABASE_PARAMETERS
#define ROOT_DATABASE_PARAMETERS
//
//////////////////////////////////////////////////////////////////////////

#include <stdio.h>      // For binary file input (the DAQ data file).
#include <string.h>     // For binary file input (the DAQ data file).
#include <sys/types.h>  // For binary file input (the DAQ data file).
#include <sys/stat.h>   // For binary file input (the DAQ data file).
#include <fcntl.h>      // For binary file input (the DAQ data file).

#include <iostream.h>
#include <unistd.h>    // needed for access()/sleep()
#include <fstream.h>
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
#include "StChain.h"
#include "St_DataSetIter.h"
#include "St_ObjectSet.h"

// DataBase Initialization
#ifdef TPC_DATABASE_PARAMETERS
// Dave's Header file
#include "StTpcDb/StTpcDb.h"
#include "StTrsMaker/include/StTpcDbGeometry.hh"
#include "StTrsMaker/include/StTpcDbSlowControl.hh"
#include "StTrsMaker/include/StTpcDbElectronics.hh"
#endif

#ifdef ROOT_DATABASE_PARAMETERS
#include "StTpcROOTGeometry.hh"
#include "StTpcROOTSlowControl.hh"
#include "StTpcROOTElectronics.hh" 
#endif

// processes
#include "StMixerFastDigitalSignalGenerator.hh"
#include "StMixerDigitalSignalGenerator.hh"
#include "StMixerEmbedding.hh"

// containers
#include "StMixerAnalogSignal.hh"
#include "StMixerSector.hh"
#include "StMixerDigitalSector.hh"
#include "StTrsMaker/include/StTpcElectronics.hh"

// outPut Data--decoder
#include "StTrsMaker/include/StTrsDetectorReader.hh"
#include "StTrsMaker/include/StTrsZeroSuppressedReader.hh"
#include "StDaqLib/GENERIC/EventReader.hh"
#include "StSequence.hh"
//#include "StDaqLib/TPC/trans_table.hh"
#include "StMixerDataEvent.hh"
#include "StMixerOstream.hh"

#include "StDAQMaker/StDAQReader.h"

#include "StDaqLib/TPC/trans_table.hh"

StDAQReader *daqr1;
StTPCReader *tpcr1;
StDAQReader *daqr2;
StTPCReader *tpcr2;
StTrsDetectorReader* tdr1;
StTrsDetectorReader* tdr2;
ZeroSuppressedReader* trsZsr;
ZeroSuppressedReader* trsZsr1;
ZeroSuppressedReader* trsZsr2;

ClassImp(StMixerMaker)

StMixerMaker::StMixerMaker(const char *name,char *kind1,char *kind2):
StMaker(name), 
gConfig1(kind1),
gConfig2(kind2),
mFirstSector(1),
mLastSector(24) 
{ /* nopt */ }

StMixerMaker::~StMixerMaker() { /* nopt */ }

int StMixerMaker::getSequences1(int sector,int row,int pad,int *nseq,StSequence **listOfSequences) {
    int nseqPrelim; 
    TPCSequence *listOfSequencesPrelim;
    tpcr1->getSequences(sector,row,pad,nseqPrelim,listOfSequencesPrelim);
    *nseq=nseqPrelim;
    *listOfSequences=(StSequence*)listOfSequencesPrelim;
    return kStOK;
}
int StMixerMaker::getSequences2(int sector,int row,int pad,int *nseq,StSequence **listOfSequences) {
    int nseqPrelim; 
    TPCSequence *listOfSequencesPrelim;
    tpcr2->getSequences(sector,row,pad,nseqPrelim,listOfSequencesPrelim);
    *nseq=nseqPrelim;
    *listOfSequences=(StSequence*)listOfSequencesPrelim;
    return kStOK;
}

int StMixerMaker::writeFile(char* file, int numEvents)
{
    mOutputFileName = file;
    mNumberOfEvents = numEvents;
    return kStOK;
}

Int_t StMixerMaker::Init() {

#ifdef TPC_DATABASE_PARAMETERS
  // The global pointer to the Db is gStTpcDb and it should be created in the macro.  
  mGeometryDb = StTpcDbGeometry::instance(gStTpcDb);
  mElectronicsDb = StTpcDbElectronics::instance(gStTpcDb);
  mSlowControlDb = StTpcDbSlowControl::instance(gStTpcDb);
#endif

#ifdef ROOT_DATABASE_PARAMETERS
  // Set up the DataBase access
  St_DataSet *MixerPars = GetDataBase("params/tpc/trspars");
  assert(MixerPars);
  // should use dynamic_cast when available
  geometryDataSet *Geometry    =
      static_cast<geometryDataSet*>(MixerPars->Find("Trs/Geometry"));
  electronicsDataSet *Electronics =
      static_cast<electronicsDataSet*>(MixerPars->Find("Trs/Electronics"));
  slowcontrolDataSet *SlowControl =
      static_cast<slowcontrolDataSet*>(MixerPars->Find("Trs/SlowControl"));

  mGeometryDb = StTpcROOTGeometry::instance(Geometry);
  mSlowControlDb = StTpcROOTSlowControl::instance(SlowControl);
  mElectronicsDb = StTpcROOTElectronics::instance(Electronics);
#endif

  mSector = new StMixerSector(mGeometryDb);
  mSector1 = new StMixerSector(mGeometryDb);
  mSector2 = new StMixerSector(mGeometryDb);
  mDigitalSignalGenerator =
	    StMixerFastDigitalSignalGenerator::instance(mElectronicsDb, mSector);

  mEmbedding = StMixerEmbedding::instance(mElectronicsDb, mSector, mSector1, mSector2);

  // Output is into an StTpcRawDataEvent* vector<StTrsDigitalSector*>
  // which is accessible via the StTrsUnpacker.  Initialize the pointers!
  mAllTheDataMixer=0;
  
  // Construct constant data sets.  This is what is passed downstream
  mAllTheDataMixer = new StMixerDataEvent(mGeometryDb->numberOfSectors());
  AddConst(new St_ObjectSet("MixerEvent"  , mAllTheDataMixer));

  // Stream Instantiation
  mOutputStreamMixer = new StMixerOstream(mOutputFileName,mNumberOfEvents,mGeometryDb);
  //cout << "StMixerMaker::Init()" << endl;
  return StMaker::Init();
}

Int_t StMixerMaker::Make() {
  
  //ofstream out_file1("file1.dat");
  //ofstream out_file2("file2.dat");
  //ofstream out_file1mixer("file1mixer.dat");
  //ofstream out_file2mixer("file2mixer.dat");
  
  // Make a digital Pad!
#ifndef ST_NO_TEMPLATE_DEF_ARGS
  vector<unsigned char> digitalPadData;
#else
  vector<unsigned char, allocator<unsigned char> > digitalPadData;
#endif
  
  if(!strcmp(GetConfig1(),"daq")) {
    // DAQ
    St_DataSet *dataset1;
    dataset1=GetDataSet("mixer/.make/DaqFirst/.const/StDAQReader");
    assert(dataset1);
    daqr1=(StDAQReader*)(dataset1->GetObject());
    assert(daqr1);
    tpcr1=daqr1->getTPCReader(); 
    assert(tpcr1);
  } else {
    // TRS
    // Get the TRS Event Data Set 
    St_ObjectSet* trsEventDataSet1 = (St_ObjectSet*) GetDataSet("mixer/.make/TrsFirst/.const/Event"); 
    // Get the pointer to the raw data. 
    StTpcRawDataEvent* trsEvent1 = (StTpcRawDataEvent*) trsEventDataSet1->GetObject(); 
    // Instantiate the DetectorReader. Version will be default if not given 
    string version = "TrsDatav1.0"; 
    tdr1 = new StTrsDetectorReader(trsEvent1, version); 
   }
   
  if(!strcmp(GetConfig2(),"daq")) {
    // DAQ
    St_DataSet *dataset2;
    dataset2=GetDataSet("mixer/.make/DaqSecond/.const/StDAQReader");
    assert(dataset2);
    daqr2=(StDAQReader*)(dataset2->GetObject());
    assert(daqr2);
    tpcr2=daqr2->getTPCReader(); 
    assert(tpcr2);
  } else {
    // TRS
    // Get the TRS Event Data Set 
    St_ObjectSet* trsEventDataSet2 = (St_ObjectSet*) GetDataSet("mixer/.make/TrsSecond/.const/Event"); 
    // Get the pointer to the raw data. 
    StTpcRawDataEvent* trsEvent2 = (StTpcRawDataEvent*) trsEventDataSet2->GetObject(); 
    // Instantiate the DetectorReader. Version will be default if not given 
    string version = "TrsDatav1.0"; 
    tdr2 = new StTrsDetectorReader(trsEvent2, version); 
  }
  
  for(int isector=mFirstSector; isector<=mLastSector; isector++) {

    if((!strcmp(GetConfig1(),"daq") && !strcmp(GetConfig2(),"trs")) || (!strcmp(GetConfig1(),"trs") && !strcmp(GetConfig2(),"daq"))){

     if(!strcmp(GetConfig1(),"trs")) {
       trsZsr = tdr1->getZeroSuppressedReader(isector);
     } 
     if(!strcmp(GetConfig2(),"trs")) {
       trsZsr = tdr2->getZeroSuppressedReader(isector);
     }
      unsigned char* padListRaw; 
      unsigned char* padListTrs; 
      // Remember mSector is the "normal" analog sector! 
      for(int irow=0; irow<mSector->numberOfRows(); irow++) { 
	int numberOfPadsRaw;
	if(!strcmp(GetConfig1(),"daq")) {
	  numberOfPadsRaw = tpcr1->getPadList(isector,irow+1,padListRaw);
	}
	if(!strcmp(GetConfig2(),"daq")) {
	  numberOfPadsRaw = tpcr2->getPadList(isector,irow+1,padListRaw);
	}
	if (!trsZsr) {
	  if (numberOfPadsRaw) {
	    int padraw;
	    for(int ipadraw=0; ipadraw<numberOfPadsRaw; ipadraw++) {
	      padraw=padListRaw[ipadraw];
	      int nseqraw;
	      StSequence* listOfSequencesRaw; 
	      getSequences1(isector,irow+1,padraw,&nseqraw,&listOfSequencesRaw); 
	      // Note that ipad is an index, NOT the pad number. 
	      // The pad number comes from padList[ipad] 
	      for(int iseqraw=0;iseqraw<nseqraw;iseqraw++) {
		int startTimeBinRaw=listOfSequencesRaw[iseqraw].startTimeBin;
		if(startTimeBinRaw<0) startTimeBinRaw=0;
		if(startTimeBinRaw>511) startTimeBinRaw=511;
		int seqLenRaw=listOfSequencesRaw[iseqraw].length;
		unsigned char *pointerToAdcRaw=listOfSequencesRaw[iseqraw].firstAdc;
		for(int ibinraw=startTimeBinRaw;ibinraw<(startTimeBinRaw+seqLenRaw);ibinraw++) {
		  float conversionraw=log8to10_table[*(pointerToAdcRaw++)];
		  //if (isector==21) out_file1 << "file1" << ' ' << irow+1 << ' ' << padraw << ' ' << ibinraw << ' ' << conversionraw  << '\n';
		  StMixerAnalogSignal padSignal(ibinraw,conversionraw);
		  mSector->addEntry(irow+1,padraw,padSignal);
		}
	      }// seq loop    
	    }// pad loop, don't confuse padR (table row #) with ipad (loop index)
	  }
	} else {
	  int numberOfPadsTrs = trsZsr->getPadList(irow+1, &padListTrs);
	  if (numberOfPadsRaw) {
	    int padraw;
	    for(int ipadraw=0; ipadraw<numberOfPadsRaw; ipadraw++) {
	      padraw=padListRaw[ipadraw];
	      int nseqraw;
	      StSequence* listOfSequencesRaw; 
	      getSequences1(isector,irow+1,padraw,&nseqraw,&listOfSequencesRaw); 
	      // Note that ipad is an index, NOT the pad number. 
	      // The pad number comes from padList[ipad] 
	      for(int iseqraw=0;iseqraw<nseqraw;iseqraw++) {
		int startTimeBinRaw=listOfSequencesRaw[iseqraw].startTimeBin;
		if(startTimeBinRaw<0) startTimeBinRaw=0;
		if(startTimeBinRaw>511) startTimeBinRaw=511;
		int seqLenRaw=listOfSequencesRaw[iseqraw].length;
		unsigned char *pointerToAdcRaw=listOfSequencesRaw[iseqraw].firstAdc;
		for(int ibinraw=startTimeBinRaw;ibinraw<(startTimeBinRaw+seqLenRaw);ibinraw++) {
		  float conversionraw=log8to10_table[*(pointerToAdcRaw++)];
		  //if (isector==21) out_file1 << "file1" << ' ' << irow+1 << ' ' << padraw << ' ' << ibinraw << ' ' << conversionraw  << '\n';
		  if(numberOfPadsTrs) {
		    StMixerAnalogSignal padSignal(ibinraw,conversionraw);
		    mSector1->addEntry(irow+1,padraw,padSignal);
		    //out_file1mixer << "file1mixer" << ' ' << irow+1 << ' ' << padraw << ' ' << ibinraw << ' ' << conversionraw  << '\n';
		  } else {
		    StMixerAnalogSignal padSignal(ibinraw,conversionraw);
		    mSector->addEntry(irow+1,padraw,padSignal);
		  }
		}
	      }// seq loop    
	    }// pad loop, don't confuse padR (table row #) with ipad (loop index)
	  } 
	  if (numberOfPadsTrs) {
	    int padtrs;
	    for(int ipadtrs = 0; ipadtrs<numberOfPadsTrs; ipadtrs++) { 
	      padtrs=static_cast<int>(padListTrs[ipadtrs]);
	      int nseqtrs; 
	      //cout << irow+1 << ' ' << padtrs << endl;
	      Sequence* listOfSequencesTrs; 
	      trsZsr->getSequences(irow+1,padtrs,&nseqtrs,&listOfSequencesTrs); 	  // Note that ipad is an index, NOT the pad number. 
	      // The pad number comes from padList[ipad] 
	      for(int iseqtrs=0; iseqtrs<nseqtrs; iseqtrs++) { 
		int seqLenTrs=listOfSequencesTrs[iseqtrs].Length;
		int startTimeBinTrs=listOfSequencesTrs[iseqtrs].startTimeBin;
		if(startTimeBinTrs<0) startTimeBinTrs=0;
		if(startTimeBinTrs>511) startTimeBinTrs=511;
		for(int ibintrs=startTimeBinTrs; ibintrs<(startTimeBinTrs+seqLenTrs); ibintrs++) {
		  float conversiontrs=static_cast<float>(*(listOfSequencesTrs[iseqtrs].FirstAdc)); 
		  listOfSequencesTrs[iseqtrs].FirstAdc++; 
		  //out_file2 << "file2" << ' ' << irow+1 << ' ' << padtrs << ' ' << ibintrs << ' ' << conversiontrs  << endl;
		  if(numberOfPadsRaw) {
		    StMixerAnalogSignal padSignal(ibintrs,conversiontrs);
		    mSector2->addEntry(irow+1,padtrs,padSignal);
		    //out_file2mixer << "file2mixer" << ' ' << irow+1 << ' ' << padtrs << ' ' << ibintrs << ' ' << conversiontrs  << endl;
		  } else {
		    //if (isector==21 && (irow+1)==3 && padtrs==54) cout << irow+1 << ' ' << padtrs << ' ' << ibintrs << ' ' << conversiontrs <<endl;  
		    StMixerAnalogSignal padSignal(ibintrs,conversiontrs);
		    mSector->addEntry(irow+1,padtrs,padSignal);
		  }
		}
	      }// seq loop
	    }// pad loop, don't confuse padR (table row #) with ipad (loop index)
	  }
	}// ipadrow loop
      }
    }// if loop

    if((!strcmp(GetConfig1(),"trs") && !strcmp(GetConfig2(),"trs"))) {
      
      trsZsr1 = tdr1->getZeroSuppressedReader(isector); 
      trsZsr2 = tdr2->getZeroSuppressedReader(isector); 
      //cout << trsZsr1 << ' ' << trsZsr2 << ' ' << isector <<endl;
      if (!trsZsr1 && !trsZsr2) continue;
      unsigned char* padListTrs1; 
      unsigned char* padListTrs2; 
      // Remember mSector is the "normal" analog sector! 
      for(int irow=0; irow<mSector->numberOfRows(); irow++) {
	if (!trsZsr1 || !trsZsr2) {
	  if (!trsZsr1) {
	    int numberOfPadsTrs2 = trsZsr2->getPadList(irow+1, &padListTrs2);
	    if (numberOfPadsTrs2) {
	      int padtrs;
	      for(int ipadtrs = 0; ipadtrs<numberOfPadsTrs2; ipadtrs++) { 
		padtrs=static_cast<int>(padListTrs2[ipadtrs]);
		int nseqtrs; 
		Sequence* listOfSequencesTrs; 
		trsZsr2->getSequences(irow+1,padtrs,&nseqtrs,&listOfSequencesTrs);
		// Note that ipad is an index, NOT the pad number. 
		// The pad number comes from padList[ipad] 
		for(int iseqtrs=0; iseqtrs<nseqtrs; iseqtrs++) { 
		  int seqLenTrs=listOfSequencesTrs[iseqtrs].Length;
		  int startTimeBinTrs=listOfSequencesTrs[iseqtrs].startTimeBin;
		  if(startTimeBinTrs<0) startTimeBinTrs=0;
		  if(startTimeBinTrs>511) startTimeBinTrs=511;
		  for(int ibintrs=startTimeBinTrs; ibintrs<(startTimeBinTrs+seqLenTrs); ibintrs++) {
		    float conversiontrs=static_cast<float>(*(listOfSequencesTrs[iseqtrs].FirstAdc)); 
		    listOfSequencesTrs[iseqtrs].FirstAdc++; 
		    //out_file1mixer << "file1mixer" << ' ' << irow+1 << ' ' << padtrs << ' ' << ibintrs << ' ' << conversiontrs  << endl;
		    StMixerAnalogSignal padSignal(ibintrs,conversiontrs);
		    mSector->addEntry(irow+1,padtrs,padSignal);
		  }
		}// seq loop
	      }// pad loop, don't confuse padR (table row #) with ipad (loop index)
	    }
	  } 
	  if (!trsZsr2) {
	    int numberOfPadsTrs1 = trsZsr1->getPadList(irow+1, &padListTrs1);
	    if (numberOfPadsTrs1) {
	      int padtrs;
	      for(int ipadtrs = 0; ipadtrs<numberOfPadsTrs1; ipadtrs++) { 
		padtrs=static_cast<int>(padListTrs1[ipadtrs]);
		int nseqtrs; 
		Sequence* listOfSequencesTrs; 
		trsZsr1->getSequences(irow+1,padtrs,&nseqtrs,&listOfSequencesTrs);
		// Note that ipad is an index, NOT the pad number. 
		// The pad number comes from padList[ipad] 
		for(int iseqtrs=0; iseqtrs<nseqtrs; iseqtrs++) { 
		  int seqLenTrs=listOfSequencesTrs[iseqtrs].Length;
		  int startTimeBinTrs=listOfSequencesTrs[iseqtrs].startTimeBin;
		  if(startTimeBinTrs<0) startTimeBinTrs=0;
		  if(startTimeBinTrs>511) startTimeBinTrs=511;
		  for(int ibintrs=startTimeBinTrs; ibintrs<(startTimeBinTrs+seqLenTrs); ibintrs++) {
		    float conversiontrs=static_cast<float>(*(listOfSequencesTrs[iseqtrs].FirstAdc)); 
		    listOfSequencesTrs[iseqtrs].FirstAdc++; 
		    //out_file2mixer << "file2mixer" << ' ' << irow+1 << ' ' << padtrs << ' ' << ibintrs << ' ' << conversiontrs  << endl;
		    StMixerAnalogSignal padSignal(ibintrs,conversiontrs);
		    mSector->addEntry(irow+1,padtrs,padSignal);
		  }
		}// seq loop
	      }// pad loop, don't confuse padR (table row #) with ipad (loop index)
	    }
	  } 
	}
	if (trsZsr1 && trsZsr2) {
	  int numberOfPadsTrs1 = trsZsr1->getPadList(irow+1, &padListTrs1);
	  int numberOfPadsTrs2 = trsZsr2->getPadList(irow+1, &padListTrs2);
	  //cout << numberOfPadsTrs1 << ' ' << numberOfPadsTrs2 << endl;
	  if (numberOfPadsTrs1) {
	    int padtrs;
	    for(int ipadtrs = 0; ipadtrs<numberOfPadsTrs1; ipadtrs++) { 
	      padtrs=static_cast<int>(padListTrs1[ipadtrs]);
	      int nseqtrs; 
	      Sequence* listOfSequencesTrs;
	      //cout << irow+1 << ' ' << padtrs << endl;
	      trsZsr1->getSequences(irow+1,padtrs,&nseqtrs,&listOfSequencesTrs); 
	      // Note that ipad is an index, NOT the pad number. 
	      // The pad number comes from padList[ipad] 
	      for(int iseqtrs=0; iseqtrs<nseqtrs; iseqtrs++) { 
		int seqLenTrs=listOfSequencesTrs[iseqtrs].Length;
		int startTimeBinTrs=listOfSequencesTrs[iseqtrs].startTimeBin;
		if(startTimeBinTrs<0) startTimeBinTrs=0;
		if(startTimeBinTrs>511) startTimeBinTrs=511;
		for(int ibintrs=startTimeBinTrs; ibintrs<(startTimeBinTrs+seqLenTrs); ibintrs++) {
		  float conversiontrs=static_cast<float>(*(listOfSequencesTrs[iseqtrs].FirstAdc)); 
		  listOfSequencesTrs[iseqtrs].FirstAdc++; 
		  if(numberOfPadsTrs2) {
		    //if (isector==21 && (irow+1)==3 && padtrs==54) cout << irow+1 << ' ' << padtrs << ' ' << ibintrs << ' ' << conversiontrs <<endl;  
		    StMixerAnalogSignal padSignal(ibintrs,conversiontrs);
		    mSector1->addEntry(irow+1,padtrs,padSignal);
		    //if (isector==21) out_file1mixer << "file1mixer" << ' ' << irow+1 << ' ' << padtrs << ' ' << ibintrs << ' ' << conversiontrs  << endl;
		  } else {
		    StMixerAnalogSignal padSignal(ibintrs,conversiontrs);
		    mSector->addEntry(irow+1,padtrs,padSignal);
		  }
		}
	      }// seq loop
	    }// pad loop, don't confuse padR (table row #) with ipad (loop index)
	  }
	  if (numberOfPadsTrs2) {
	    int padtrs;
	    for(int ipadtrs = 0; ipadtrs<numberOfPadsTrs2; ipadtrs++) { 
	      padtrs=static_cast<int>(padListTrs2[ipadtrs]);
	      int nseqtrs; 
	      Sequence* listOfSequencesTrs; 
	      trsZsr2->getSequences(irow+1,padtrs,&nseqtrs,&listOfSequencesTrs); 	  // Note that ipad is an index, NOT the pad number. 
	      // The pad number comes from padList[ipad] 
	      for(int iseqtrs=0; iseqtrs<nseqtrs; iseqtrs++) { 
		int seqLenTrs=listOfSequencesTrs[iseqtrs].Length;
		int startTimeBinTrs=listOfSequencesTrs[iseqtrs].startTimeBin;
		if(startTimeBinTrs<0) startTimeBinTrs=0;
		if(startTimeBinTrs>511) startTimeBinTrs=511;
		for(int ibintrs=startTimeBinTrs; ibintrs<(startTimeBinTrs+seqLenTrs); ibintrs++) {
		  float conversiontrs=static_cast<float>(*(listOfSequencesTrs[iseqtrs].FirstAdc)); 
		  listOfSequencesTrs[iseqtrs].FirstAdc++; 
		  if(numberOfPadsTrs1) {
		    StMixerAnalogSignal padSignal(ibintrs,conversiontrs);
		    mSector2->addEntry(irow+1,padtrs,padSignal);
		    //if (isector==21) out_file1mixer << "file1mixer" << ' ' << irow+1 << ' ' << padtrs << ' ' << ibintrs << ' ' << conversiontrs  << endl;
		  } else {
		    StMixerAnalogSignal padSignal(ibintrs,conversiontrs);
		    mSector->addEntry(irow+1,padtrs,padSignal);
		  }
		}
	      }// seq loop
	    }// pad loop, don't confuse padR (table row #) with ipad (loop index)
	  }
	}
      }// ipadrow loop
    }// if loop
    
    if(!strcmp(GetConfig1(),"daq") && !strcmp(GetConfig2(),"daq")) {
      
      unsigned char* padListRaw1; 
      unsigned char* padListRaw2; 
      // Remember mSector is the "normal" analog sector! 
      for(int irow=0; irow<mSector->numberOfRows(); irow++) { 
	int numberOfPadsRaw1 = tpcr1->getPadList(isector,irow+1,padListRaw1);
	int numberOfPadsRaw2 = tpcr2->getPadList(isector,irow+1,padListRaw2);
	if (!numberOfPadsRaw1 && !numberOfPadsRaw2) continue;
	if (numberOfPadsRaw1) {
	  int padraw;
	  for(int ipadraw=0; ipadraw<numberOfPadsRaw1; ipadraw++) {
	    padraw=padListRaw1[ipadraw];
	    int nseqraw;
	    StSequence* listOfSequencesRaw; 
	    getSequences1(isector,irow+1,padraw,&nseqraw,&listOfSequencesRaw); 
	    // Note that ipad is an index, NOT the pad number. 
	    // The pad number comes from padList[ipad] 
	    for(int iseqraw=0;iseqraw<nseqraw;iseqraw++) {
	      int startTimeBinRaw=listOfSequencesRaw[iseqraw].startTimeBin;
	      if(startTimeBinRaw<0) startTimeBinRaw=0;
	      if(startTimeBinRaw>511) startTimeBinRaw=511;
	      int seqLenRaw=listOfSequencesRaw[iseqraw].length;
	      unsigned char *pointerToAdcRaw=listOfSequencesRaw[iseqraw].firstAdc;
	      for(int ibinraw=startTimeBinRaw;ibinraw<(startTimeBinRaw+seqLenRaw);ibinraw++) {
		float conversionraw=log8to10_table[*(pointerToAdcRaw++)];
		if (numberOfPadsRaw2) {
		  //if (isector==21) out_file1 << "file1" << ' ' << irow+1 << ' ' << padraw << ' ' << ibinraw << ' ' << conversionraw  << '\n';
		  StMixerAnalogSignal padSignal(ibinraw,conversionraw);
		  mSector1->addEntry(irow+1,padraw,padSignal);
		} else {
		  StMixerAnalogSignal padSignal(ibinraw,conversionraw);
		  mSector->addEntry(irow+1,padraw,padSignal);
		}
	      }
	    }// seq loop    
	  }// pad loop, don't confuse padR (table row #) with ipad (loop index)
	}
	if (numberOfPadsRaw2) {
	  int padraw;
	  for(int ipadraw=0; ipadraw<numberOfPadsRaw2; ipadraw++) {
	    padraw=padListRaw2[ipadraw];
	    int nseqraw;
	    StSequence* listOfSequencesRaw; 
	    getSequences2(isector,irow+1,padraw,&nseqraw,&listOfSequencesRaw); 
	    // Note that ipad is an index, NOT the pad number. 
	    // The pad number comes from padList[ipad] 
	    for(int iseqraw=0;iseqraw<nseqraw;iseqraw++) {
	      int startTimeBinRaw=listOfSequencesRaw[iseqraw].startTimeBin;
	      if(startTimeBinRaw<0) startTimeBinRaw=0;
	      if(startTimeBinRaw>511) startTimeBinRaw=511;
	      int seqLenRaw=listOfSequencesRaw[iseqraw].length;
	      unsigned char *pointerToAdcRaw=listOfSequencesRaw[iseqraw].firstAdc;
	      for(int ibinraw=startTimeBinRaw;ibinraw<(startTimeBinRaw+seqLenRaw);ibinraw++) {
		float conversionraw=log8to10_table[*(pointerToAdcRaw++)];
		//cout << isector << ' ' << irow+1 << ' ' << padraw << ' ' << ibinraw << ' ' << conversionraw  << '\n';
		if (numberOfPadsRaw1) {
		  StMixerAnalogSignal padSignal(ibinraw,conversionraw);
		  mSector2->addEntry(irow+1,padraw,padSignal);
		} else {
		  StMixerAnalogSignal padSignal(ibinraw,conversionraw);
		  mSector->addEntry(irow+1,padraw,padSignal);
		}
	      }
	    }// seq loop    
	  }// pad loop, don't confuse padR (table row #) with ipad (loop index)
	}
      }// ipadrow loop
    }// if loop
    
    mEmbedding->doEmbedding();
    
    // Digitize the Signals
    //
    // First make a sector where the data can go...
    StMixerDigitalSector* aDigitalSector = new StMixerDigitalSector(mGeometryDb);
    
    // Point to the object you want to fill
    //
    mDigitalSignalGenerator->fillSector(aDigitalSector);
    
    //cout << "sector" << ' ' << isector+1 << endl;
    // ...and digitize it
    cout << "--->digitizeSignal()..." << endl;
    mDigitalSignalGenerator->digitizeSignal();
    cout<<"--->digitizeSignal() Finished..." << endl;
    
    // Fill it into the event structure...
    // and you better check the sector number!
    
    mAllTheDataMixer->mSectors[(isector-1)] = aDigitalSector;
    // Clear and reset for next sector:
    mSector->clear();
    mSector1->clear();
    mSector2->clear();
    
  }// sector loop
  mOutputStreamMixer->writeMixerEvent((mAllTheDataMixer));
  return kStOK;
} // Make() 

// Make sure the memory is deallocated!
Int_t StMixerMaker::Clear()
{
  //mAllTheDataMixer->clear(); //This deletes all the StTrsDigitalSectors in the StTrsRawDataEvent
  return kStOk;
}
Int_t StMixerMaker::Finish()
{
  //Clean up all the pointers that were initialized in StTrsMaker::Init()
  //if(mOutputStreamMixer) delete mOutputStream;
  return kStOK;
}


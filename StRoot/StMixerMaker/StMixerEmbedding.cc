/***************************************************************************
 *
 * $Id :StMixerEmbedding.cc ,v1.1 2000/01/31 11:55:00 long Exp $$
 *
 * Author: Patricia Fachini 
 *
 ***************************************************************************
 *
 * Description: 
 *
 ***************************************************************************
 *
 **************************************************************************/
#include <unistd.h>

#include <utility>
#include "StMixerEmbedding.hh"

StMixerEmbedding* StMixerEmbedding::mInstance = 0; // static member

StMixerEmbedding::StMixerEmbedding(StTpcElectronics* el, StMixerSector* sec, StMixerSector* sec1, StMixerSector* sec2)
    : mElectronicsDb(el), mSector(sec), mSector1(sec1), mSector2(sec2)
{
    mNumberOfTimeBins = mElectronicsDb->numberOfTimeBins();
}

StMixerEmbedding::~StMixerEmbedding() {/* missing */}

StMixerEmbedding* StMixerEmbedding::instance()
{
    return mInstance;
}

StMixerEmbedding* StMixerEmbedding::instance(StTpcElectronics* el, StMixerSector* sec, StMixerSector* sec1, StMixerSector* sec2)
{
    if(!mInstance) {
	mInstance = new StMixerEmbedding(el, sec, sec1, sec2);
    }
    // else  do nothing
    return mInstance;
}
void StMixerEmbedding::doEmbedding()
{
   
  // Loop over the sector
  
  tpcTimeBins currentPad1; 
  tpcTimeBins currentPad2;   
  tpcTimeBins currentPad;   
   
  // Remember mSector is the "normal" analog sector! 
  cout << "StMixerEmbedding::doEmbedding()" << endl;
  for(int irow=1; irow<=mSector->numberOfRows(); irow++) { 
    for(unsigned int ipad=1; ipad<=mSector->padsOfRow(irow).size(); ipad++) {
      //for (unsigned int i=0; i<mSector1->timeBinsOfRowAndPad(irow,ipad).size(); i++) {
      //cout << mSector1->timeBinsOfRowAndPad(irow,ipad)[i] << endl;
      //}
      currentPad1 = mSector1->timeBinsOfRowAndPad(irow,ipad);
      currentPad2 = mSector2->timeBinsOfRowAndPad(irow,ipad);
      if (!currentPad1.size() && !currentPad2.size()) continue;
      if (currentPad1.size() && !currentPad2.size()) {
	for(mTimeSequenceIterator1  = currentPad1.begin();
	    mTimeSequenceIterator1 != currentPad1.end();
	    mTimeSequenceIterator1++) {
	  bin1 = static_cast<int>(mTimeSequenceIterator1->time());
	  conversion1 = static_cast<float>(mTimeSequenceIterator1->amplitude());
	  //if (irow==3 && ipad==54) cout << irow << ' ' << ipad << ' ' << bin1 << ' ' << conversion1 <<endl;  
	  StMixerAnalogSignal padSignal(bin1,conversion1);
	  mSector->addEntry(irow,ipad,padSignal);
	}
      }
      if (!currentPad1.size() && currentPad2.size()) {
	for(mTimeSequenceIterator2  = currentPad2.begin();
	    mTimeSequenceIterator2 != currentPad2.end();
	    mTimeSequenceIterator2++) {
	  bin2 = static_cast<int>(mTimeSequenceIterator2->time());
	  conversion2 = static_cast<float>(mTimeSequenceIterator2->amplitude());
	  StMixerAnalogSignal padSignal(bin2,conversion2);
	  mSector->addEntry(irow,ipad,padSignal);
	  //if (irow==14) cout << "2nomixer" << ' ' << irow << ' ' << ipad << ' ' << bin2 << ' ' << conversion2  << endl;
	}
      }
      if (currentPad1.size() && currentPad2.size()) {
	for(int bin=0; bin<mTimeBins; bin++) {
	  accum[bin]=0;
	}
	for(mTimeSequenceIterator1  = currentPad1.begin();
	    mTimeSequenceIterator1 != currentPad1.end();
	    mTimeSequenceIterator1++) {
	  bin1 = static_cast<int>(mTimeSequenceIterator1->time());
	  conversion1 = static_cast<float>(mTimeSequenceIterator1->amplitude());
	  accum[bin1]=conversion1;
	}
	for(mTimeSequenceIterator2  = currentPad2.begin();
	    mTimeSequenceIterator2 != currentPad2.end();
	    mTimeSequenceIterator2++) {
	  bin2 = static_cast<int>(mTimeSequenceIterator2->time());
	  conversion2 = static_cast<float>(mTimeSequenceIterator2->amplitude());
	  accum[bin2]+=conversion2;
	  if (accum[bin2]>1015) accum[bin2]=1015; 
	}
	for(int binn=0; binn<mTimeBins; binn++) {
	  if (accum[binn]>0) {
	  //if (irow==3 && ipad==54) cout << irow << ' ' << ipad << ' ' << binn << ' ' << accum[binn] <<endl;  
	  StMixerAnalogSignal padSignal(binn,accum[binn]);
	  mSector->addEntry(irow,ipad,padSignal);
	  }
	}
      }
    }
  }
}

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
#include "StDaqLib/TPC/trans_table.hh"

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
      if (currentPad1.size()>0 && currentPad2.size()==0) {
	size1 = static_cast<int>(currentPad1.size());
	for(mTimeSequenceIterator1  = currentPad1.begin();
	    mTimeSequenceIterator1 != currentPad1.end();
	    mTimeSequenceIterator1++) {
	  bin1 = static_cast<int>(mTimeSequenceIterator1->time());
	  conversion1 = log8to10_table[static_cast<int>(mTimeSequenceIterator1->amplitude())];
	  StMixerAnalogSignal padSignal(bin1,conversion1);
	  mSector->addEntry(irow,ipad,padSignal);
	  //if (irow==14) cout << "1nomixer" << ' ' << irow << ' ' << ipad << ' ' << bin1 << ' ' << conversion1  << endl;
	}
      }
      if (currentPad1.size()==0 && currentPad2.size()>0) {
	size2 = static_cast<int>(currentPad2.size());
	for(mTimeSequenceIterator2  = currentPad2.begin();
	    mTimeSequenceIterator2 != currentPad2.end();
	    mTimeSequenceIterator2++) {
	  bin2 = static_cast<int>(mTimeSequenceIterator2->time());
	  conversion2 = log8to10_table[static_cast<int>(mTimeSequenceIterator2->amplitude())];
	  StMixerAnalogSignal padSignal(bin2,conversion2);
	  mSector->addEntry(irow,ipad,padSignal);
	  //if (irow==14) cout << "2nomixer" << ' ' << irow << ' ' << ipad << ' ' << bin2 << ' ' << conversion2  << endl;
	}
      }
      if (!currentPad1.size() && !currentPad2.size()) continue;
      size2 = static_cast<int>(currentPad2.size());
      size1 = static_cast<int>(currentPad1.size());
      int begin1;
      int begin2;
      bool first=true;
      for(mTimeSequenceIterator1  = currentPad1.begin();
	  mTimeSequenceIterator1 != currentPad1.end();
	  mTimeSequenceIterator1++) {
	for(mTimeSequenceIterator2  = currentPad2.begin();
	    mTimeSequenceIterator2 != currentPad2.end();
	    mTimeSequenceIterator2++) {
	  if (first) {
	    //PR(first);
	    begin1 = static_cast<int>(currentPad1.begin()->time());
	    begin2 = static_cast<int>(currentPad2.begin()->time());
	    first=0;
	  }
	  bin1 = static_cast<int>(mTimeSequenceIterator1->time());
	  bin2 = static_cast<int>(mTimeSequenceIterator2->time());
	  conversion1=log8to10_table[static_cast<int>(mTimeSequenceIterator1->amplitude())];
	  conversion2=log8to10_table[static_cast<int>(mTimeSequenceIterator2->amplitude())];
	  if (bin1==bin2) {
	    conversionmixer=conversion1+conversion2;
	    if (conversionmixer>1015) conversionmixer=1015;
	    //if (irow==14) cout << "2mixer" << ' ' << irow << ' ' << ipad << ' ' << bin2 << ' ' << conversionmixer  << endl;
	    StMixerAnalogSignal padSignal(bin1,conversionmixer);
	    mSector->addEntry(irow,ipad,padSignal);
	    break;
	  }
	  if(bin1<bin2) {
	    //if (irow==14) cout << "1<" << ' ' << irow << ' ' << ipad << ' ' << bin1 << ' ' << conversion1  << endl;
	    StMixerAnalogSignal padSignal(bin1,conversion1);
	    mSector->addEntry(irow,ipad,padSignal);
	    break;
	  }
	  if(((begin1+size1-1)==bin1) && bin2>bin1) {
	    //if (irow==14) cout << "2" << ' ' << irow << ' ' << ipad << ' ' << bin2 << ' ' << conversion2  << endl;
	    StMixerAnalogSignal padSignal(bin2,conversion2);
	    mSector->addEntry(irow,ipad,padSignal);
	  }
	  if((begin1==bin1) && bin2<bin1) {
	    //if (irow==14) cout << "2" << ' ' << irow << ' ' << ipad << ' ' << bin2 << ' ' << conversion2  << endl;
	    StMixerAnalogSignal padSignal(bin2,conversion2);
	    mSector->addEntry(irow,ipad,padSignal);
	  }
	  if(((begin2+size2-1)==bin2) && bin2<bin1) {
	    //if (irow==14) cout << "1" << ' ' << irow << ' ' << ipad << ' ' << bin1 << ' ' << conversion1  << endl;
	    StMixerAnalogSignal padSignal(bin1,conversion1);
	    mSector->addEntry(irow,ipad,padSignal);
	  }
	}
      }
    }
  }
}



/***************************************************************************
 *
 * $Id :StMixerFastDigitalSignalGenerator.cc ,v1.1 1999/10/01 17:15:00 long Exp $$
 *
 * Author: Patricia Fachini - a copy from StTrsFastDigitalSignalGenerator.cc
 *
 ***************************************************************************
 *
 * Description: 10 bit to 8 bit translation  simulation of digitization
 *
 ***************************************************************************
 *
 **************************************************************************/
#include <unistd.h>

#include <utility>
#include "StMixerFastDigitalSignalGenerator.hh"

StMixerDigitalSignalGenerator* StMixerFastDigitalSignalGenerator::mInstance = 0; // static member

StMixerFastDigitalSignalGenerator::StMixerFastDigitalSignalGenerator(StTpcElectronics* el, StMixerSector* sec)
    : StMixerDigitalSignalGenerator(el, sec)
{
}

StMixerFastDigitalSignalGenerator::~StMixerFastDigitalSignalGenerator() {/* missing */}

StMixerDigitalSignalGenerator*
StMixerFastDigitalSignalGenerator::instance()
{
    if(!mInstance) {
#ifndef ST_NO_EXCEPTIONS
	throw range_error("StMixerFastDigitalSignalGenerator::instance() Must Supply a File name");
#else
	cerr << "StMixerFastDigitalSignalGenerator::instance() Must Supply a File name" << endl;
	cerr << "Exitting..." << endl;
	exit(1);
#endif
    }
    return mInstance;
}

StMixerDigitalSignalGenerator*
StMixerFastDigitalSignalGenerator::instance(StTpcElectronics* el, StMixerSector* sec)
{
    if(!mInstance) {
	mInstance = new StMixerFastDigitalSignalGenerator(el, sec);
    }
    // else  do nothing
    
    return mInstance;
}


//unsigned char StMixerFastDigitalSignalGenerator::do10to8Translation(int index)const
//{   
   
//     if(index<-1.0e-30)index=0;
//     if(index>1023)index=1023;
         
//     return log10to8_table[index];
//}            
void StMixerFastDigitalSignalGenerator::digitizeSignal()
{
   
    // Loop over the sector

    tpcTimeBins currentPad; 
  
   
    // Make a digital Pad!
#ifndef ST_NO_TEMPLATE_DEF_ARGS
    vector<unsigned char> digitalPadData;
#else
    vector<unsigned char, allocator<unsigned char> > digitalPadData;
#endif
    // Remember mSector is the "normal" analog sector! 
      cout << "StMixerFastDigitalSignalGenerator::digitizeSignal()" << endl;
      for(int irow=1; irow<=mSector->numberOfRows(); irow++) { 
	for(unsigned int ipad=1; ipad<=mSector->padsOfRow(irow).size(); ipad++) {
           
	    currentPad = mSector->timeBinsOfRowAndPad(irow,ipad); 
           
	    if(!currentPad.size()) continue;
   	    //cout << "dig() r/p " << irow << '/' << ipad << endl;
	    // Make sure the digital Pad is clear!
	    digitalPadData.clear();
	    //cout << irow  << ' ' << "row" << ' ' << ipad << ' ' << "pad" << endl;
         
	    unsigned int currentTimeBin = digitalPadData.size();
	    //cout<<currentTimeBin<<"  should be 0 "<<endl;
 	    //PR(currentTimeBin);
	    unsigned int zeroCounter = 0;
	    for(mTimeSequenceIterator  = currentPad.begin();
		mTimeSequenceIterator != currentPad.end();
		mTimeSequenceIterator++) {

	        //PR(*mTimeSequenceIterator);
		unsigned int timeBinIndex =
		    static_cast<unsigned int>(mTimeSequenceIterator->time());
		if (timeBinIndex > currentTimeBin) {
		    //cout << "Positive time shift" << endl;
		    // remove previous zero if any
		    if (digitalPadData.size() && !digitalPadData.back()) digitalPadData.pop_back(); 
		    // add zeros
		    zeroCounter += timeBinIndex - currentTimeBin; 
		    for (int j=0; j < floor(zeroCounter/255.); j++) {
			digitalPadData.push_back(0);
			digitalPadData.push_back(255);
			zeroCounter -= 255;
		    }
		    if (zeroCounter) digitalPadData.push_back(0);
		    currentTimeBin = timeBinIndex;
		}
		else if (timeBinIndex < currentTimeBin){
		    //cout << "Negative Time Shift" << endl;
		    mTimeSequenceIterator+= currentTimeBin-timeBinIndex;
		}
	        
		int temporary_digitalAmplitude =
		    static_cast<int>(mTimeSequenceIterator->amplitude());
		unsigned char digitalAmplitude = temporary_digitalAmplitude;
		//PR(temporary_digitalAmplitude);
		
		timeBinIndex = static_cast<int>(mTimeSequenceIterator->time());
		// Normal processing without shift
		if(digitalAmplitude>255) digitalAmplitude = 255;
		
		if(digitalAmplitude != 0) {
		    if (zeroCounter) {
			digitalPadData.push_back(static_cast<unsigned char>(zeroCounter));
			zeroCounter = 0;
		    }
		    digitalPadData.push_back(static_cast<unsigned char>(digitalAmplitude));
		    currentTimeBin++;
		    if (currentTimeBin==mNumberOfTimeBins)
			break; //In case the time sequence hasn't finished, because of a time shift.
		}
		// Otherwise there is no signal!
		else {
		    if (!zeroCounter) digitalPadData.push_back(static_cast<unsigned char>(0));
		    else if(zeroCounter==255) {
			digitalPadData.push_back(static_cast<unsigned char>(255));
			digitalPadData.push_back(static_cast<unsigned char>(0));
			zeroCounter=0;
		    }
		    zeroCounter++;
		    currentTimeBin++;
		    if (currentTimeBin==mNumberOfTimeBins){
			digitalPadData.push_back(static_cast<unsigned char>(zeroCounter));
			zeroCounter = 0;
			break;
		    }
		}
	    } // the iterator (mTimeSequence)
	    if (currentTimeBin < mNumberOfTimeBins) {
		//cout << "Adding remaining zeros (should couple with - time shift)" << endl;
		if (zeroCounter == 0) digitalPadData.push_back(0);
		zeroCounter += mNumberOfTimeBins - currentTimeBin;
		for (int k=0; k<floor(zeroCounter/255.); k++) {
		    digitalPadData.push_back(255);
		    digitalPadData.push_back(0);
		    zeroCounter-=255;
		}
		if (zeroCounter) digitalPadData.push_back(zeroCounter);
		else digitalPadData.pop_back();
	    }
 	    //PR(currentTimeBin);
 	    //PR(mNumberOfTimeBins);


	    
	    // print it out:
   	    //PR(digitalPadData.size());
   	    //for(int ii=0; ii<digitalPadData.size(); ii++) {
	    //cout << (ii) << '\t' << dec << (int)(digitalPadData[ii]) << endl;
   	    //}
	    
	    currentPad.clear();
	    mDigitalSector->assignTimeBins(irow,ipad,&digitalPadData);
	    //sleep(2);

  	} // pads
    
    }// rows
        
    

}


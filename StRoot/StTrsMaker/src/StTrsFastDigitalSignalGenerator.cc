/***************************************************************************
 *
 * $Id :StTrsFastDigitalSignalGenerator.cc ,v1.1 1999/10/01 17:15:00 long Exp $$
 *
 * Author: 
 ***************************************************************************
 *
 * Description: 10 bit to 8 bit translation  simulation of digitization
 *
 ***************************************************************************
 *
 * $Log: StTrsFastDigitalSignalGenerator.cc,v $
 * Revision 1.25  2000/06/07 02:03:11  lasiuk
 * exit/abort ultimatum
 *
 * Revision 1.24  2000/02/24 16:29:34  long
 * add 0.5 in digitalization for test
 *
 *  Revision 1.24  2000/02/23  16:14:30  long
 * add 0.5 in digitalization for test 
 * Revision 1.23  2000/02/10 01:21:50  calderon
 * Switch to use StTpcDb.
 * Coordinates checked for consistency.
 * Fixed problems with StTrsIstream & StTrsOstream.
 *
 * Revision 1.22  2000/01/10 23:14:30  lasiuk
 * Include MACROS for compatiblity with SUN CC5
 *
 * Revision 1.21  1999/12/08 02:10:42  calderon
 * Modified to eliminate warnings on Linux.
 *
 * Revision 1.20  1999/11/10 15:46:25  calderon
 * Made changes to reduce timing, including:
 * Made coordinate transfrom a data member of StTrsAnalogSignalGenerator
 * Added upper-lower bound instead of symmetric cut.
 * Revived checking if signal is above threshold.
 *
 * Revision 1.19  1999/11/05 22:18:16  calderon
 *
 * Made private copy constructor and operator= in StTrsDigitalSector.
 * Renamed DigitalSignalGenerators: Fast -> Old, Parameterized -> Fast
 * and use new "Fast" as default.
 * Added StTrsDetectorReader and StTrsZeroSuppressedReader for DAQ type
 * data access.
 * Removed vestigial for loop in sampleAnalogSignal() method.
 * Write version of data format in .trs data file.
 *
 * Revision 1.4  1999/10/22 00:00:14  calderon
 * -added macro to use Erf instead of erf if we have HP and Root together.
 * -constructor with char* for StTrsDedx so solaris doesn't complain
 * -remove mZeros from StTrsDigitalSector.  This causes several files to
 *  be modified to conform to the new data format, so only mData remains,
 *  access functions change and digitization procedure is different.
 *
 * Revision 1.3  1999/10/06 21:53:19  long
 *  comment out one message output
 *
 * Revision 1.2  1999/10/04 15:56:55  long
 * add 10 to 8 bit transformation
 *
 * Revision 1.1  1999/10/04 15:45:27  long
 * add 10 to 8 bit transformation
 *

 *
 **************************************************************************/
#include <unistd.h>
#include <utility>

#include "StTrsFastDigitalSignalGenerator.hh"

StTrsDigitalSignalGenerator* StTrsFastDigitalSignalGenerator::mInstance = 0; // static member

StTrsFastDigitalSignalGenerator::StTrsFastDigitalSignalGenerator(StTpcElectronics* el, StTrsSector* sec)
    : StTrsDigitalSignalGenerator(el, sec)
{
    mSimpleConversion = mElectronicsDb->adcConversion();
   
}

StTrsFastDigitalSignalGenerator::~StTrsFastDigitalSignalGenerator() {/* missing */}

StTrsDigitalSignalGenerator*
StTrsFastDigitalSignalGenerator::instance()
{
    if(!mInstance) {
#ifndef ST_NO_EXCEPTIONS
	throw range_error("StTrsFastDigitalSignalGenerator::instance() Must Supply a File name");
#else
	cerr << "StTrsFastDigitalSignalGenerator::instance() Must Supply a File name" << endl;
	cerr << "Aborting..." << endl;
	abort();
#endif
    }
    return mInstance;
}

StTrsDigitalSignalGenerator*
StTrsFastDigitalSignalGenerator::instance(StTpcElectronics* el, StTrsSector* sec)
{
    if(!mInstance) {
	mInstance = new StTrsFastDigitalSignalGenerator(el, sec);
    }
    // else  do nothing
    
    return mInstance;
}


unsigned char StTrsFastDigitalSignalGenerator::do10to8Translation(int index)const
{   
   
     if(index<-1.0e-30)index=0;
     if(index>1023)index=1023;
    
   
  
     
     
     return log10to8_table[index];
}            
void StTrsFastDigitalSignalGenerator::digitizeSignal()
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
      cout << "StTrsFastDigitalSignalGenerator::digitizeSignal()" << endl;
      for(int irow=1; irow<=mSector->numberOfRows(); irow++) { 
	for(unsigned int ipad=1; ipad<=mSector->padsOfRow(irow).size(); ipad++) {
           
	    currentPad = mSector->timeBinsOfRowAndPad(irow,ipad); 
           
	    if(!currentPad.size()) continue;
   	    //cout << "dig() r/p " << irow << '/' << ipad << endl;
	    // Make sure the digital Pad is clear!
	    digitalPadData.clear();
	    //   cout<<irow<<" row "<<ipad<<" pad"<<endl;
         
	    unsigned int currentTimeBin = digitalPadData.size();
	    //  cout<<currentTimeBin<<"  should be 0 "<<endl;
// 	    PR(currentTimeBin);
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
		  static_cast<int>(mTimeSequenceIterator->amplitude()/mSimpleConversion+0.5);//add 0.5  for test ,by HL
		
		// here we have a 10 bit number!
		// Find in Mike Levine's array from:
		// StDaqLib/TPC/trans_table.hh
		// what the appropriate 8 bit number is!
		
		
		// Conversion
		// Must take into account the 8 <--> 10 bit conversion
		// TRS calculates on a linear scale and then must
		// convert to 8 bit data
		
		
		unsigned char digitalAmplitude = 
		    do10to8Translation(temporary_digitalAmplitude);
		
		
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
// 	    PR(currentTimeBin);
// 	    PR(mNumberOfTimeBins);


	    
	    // print it out:
//   	    PR(digitalPadData.size());
// 	    PR(irow);
// 	    PR(ipad);
//   	    for(int ii=0; ii<digitalPadData.size(); ii++) {
//   		cout << (ii) << '\t' << dec << (int)(digitalPadData[ii]) << endl;
//   	    }
	    
	    currentPad.clear();
	    mDigitalSector->assignTimeBins(irow,ipad,&digitalPadData);
	    //sleep(2);

  	} // pads
    
    }// rows
        
    

}

void StTrsFastDigitalSignalGenerator::addCorrelatedNoise()
{
    cerr << "StTrsFastDigitalSignalGenerator::addCorrelatedNoise()" << endl;
    cerr << "Not Implemented!" << endl;
}

void StTrsFastDigitalSignalGenerator::addWhiteNoise()
{
    cerr << "StTrsFastDigitalSignalGenerator::addWhiteNoise()" << endl;
    cerr << "Not Implemented!" << endl;    
}

/***************************************************************************
 *
 * $Id :StTrsParameterizedDigitalSignalGenerator.cc ,v1.1 1999/10/01 17:15:00 long Exp $$
 *
 * Author: 
 ***************************************************************************
 *
 * Description: 10 bit to 8 bit translation  simulation of digitization
 *
 ***************************************************************************
 *
 * $Log: StTrsParameterizedDigitalSignalGenerator.cc,v $
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
#include "StTrsParameterizedDigitalSignalGenerator.hh"

StTrsDigitalSignalGenerator* StTrsParameterizedDigitalSignalGenerator::mInstance = 0; // static member

StTrsParameterizedDigitalSignalGenerator::StTrsParameterizedDigitalSignalGenerator(StTpcElectronics* el, StTrsSector* sec)
    : StTrsDigitalSignalGenerator(el, sec)
{
    mSimpleConversion = mElectronicsDb->adcConversion();
   
}

StTrsParameterizedDigitalSignalGenerator::~StTrsParameterizedDigitalSignalGenerator() {/* missing */}

StTrsDigitalSignalGenerator*
StTrsParameterizedDigitalSignalGenerator::instance()
{
    if(!mInstance) {
#ifndef ST_NO_EXCEPTIONS
	throw range_error("StTrsParameterizedDigitalSignalGenerator::instance() Must Supply a File name");
#else
	cerr << "StTrsParameterizedDigitalSignalGenerator::instance() Must Supply a File name" << endl;
	cerr << "Exitting..." << endl;
	exit(1);
#endif
    }
    return mInstance;
}

StTrsDigitalSignalGenerator*
StTrsParameterizedDigitalSignalGenerator::instance(StTpcElectronics* el, StTrsSector* sec)
{
    if(!mInstance) {
	mInstance = new StTrsParameterizedDigitalSignalGenerator(el, sec);
    }
    // else  do nothing
    
    return mInstance;
}


unsigned char StTrsParameterizedDigitalSignalGenerator::do10to8Translation(int index)const
{   
   
     if(index<-1.0e-30)index=0;
     if(index>1023)index=1023;
    
   
  
     
     
     return log10to8_table[index];
}            
void StTrsParameterizedDigitalSignalGenerator::digitizeSignal()
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
      cout << "StTrsParameterizedDigitalSignalGenerator::digitizeSignal()" << endl;
      for(int irow=1; irow<=mSector->numberOfRows(); irow++) { 
	for(int ipad=1; ipad<=mSector->padsOfRow(irow).size(); ipad++) {
           
	    currentPad = mSector->timeBinsOfRowAndPad(irow,ipad); 
           
	    if(!currentPad.size()) continue;
   	    //cout << "dig() r/p " << irow << '/' << ipad << endl;
	    // Make sure the digital Pad is clear!
	    digitalPadData.clear();
	    //   cout<<irow<<" row "<<ipad<<" pad"<<endl;
         
	    int currentTimeBin = digitalPadData.size();
	    //  cout<<currentTimeBin<<"  should be 0 "<<endl;
// 	    PR(currentTimeBin);
	    unsigned int zeroCounter = 0;
	    for(mTimeSequenceIterator  = currentPad.begin();
		mTimeSequenceIterator != currentPad.end();
		mTimeSequenceIterator++) {

		//PR(*mTimeSequenceIterator);
	        
		int temporary_digitalAmplitude =
		   (int) (mTimeSequenceIterator->amplitude()/mSimpleConversion);
            
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
			       
		if(digitalAmplitude>255) digitalAmplitude = 255;

		if(digitalAmplitude != 0) {
		    if (zeroCounter) {
			digitalPadData.push_back(static_cast<unsigned char>(zeroCounter));
			zeroCounter = 0;
		    }
		    digitalPadData.push_back(static_cast<unsigned char>(digitalAmplitude));
		    currentTimeBin++;
		}
		// Otherwise there is no signals!
		else if(currentTimeBin!=mNumberOfTimeBins){
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
		    }
		}
	    } // the iterator (mTimeSequence)
// 	    PR(currentTimeBin);
// 	    PR(mNumberOfTimeBins);


	    
	    // print it out:
//   	    PR(digitalPadData.size());
//   	    for(int ii=0; ii<digitalPadData.size(); ii++) {
//   		cout << (ii) << '\t' << dec << (int)(digitalPadData[ii]) << endl;
//   	    }
	    
	    currentPad.clear();
	    mDigitalSector->assignTimeBins(irow,ipad,&digitalPadData);
	    //sleep(2);

  	} // pads
    
    }// rows
        
    

}

void StTrsParameterizedDigitalSignalGenerator::addCorrelatedNoise()
{
    cerr << "StTrsParameterizedDigitalSignalGenerator::addCorrelatedNoise()" << endl;
    cerr << "Not Implemented!" << endl;
}

void StTrsParameterizedDigitalSignalGenerator::addWhiteNoise()
{
    cerr << "StTrsParameterizedDigitalSignalGenerator::addWhiteNoise()" << endl;
    cerr << "Not Implemented!" << endl;    
}

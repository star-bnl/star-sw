/***************************************************************************
 *
 * $Id: StTrsOldDigitalSignalGenerator.cc,v 1.3 2000/06/07 02:03:11 lasiuk Exp $
 *
 * Author: 
 ***************************************************************************
 *
 * Description: fast simulation of digitization
 *
 ***************************************************************************
 *
 * $Log: StTrsOldDigitalSignalGenerator.cc,v $
 * Revision 1.3  2000/06/07 02:03:11  lasiuk
 * exit/abort ultimatum
 *
 * Revision 1.2  1999/12/08 02:10:42  calderon
 * Modified to eliminate warnings on Linux.
 *
 * Revision 1.1  1999/11/05 22:18:16  calderon
 *
 * Made private copy constructor and operator= in StTrsDigitalSector.
 * Renamed DigitalSignalGenerators: Fast -> Old, Parameterized -> Fast
 * and use new "Fast" as default.
 * Added StTrsDetectorReader and StTrsZeroSuppressedReader for DAQ type
 * data access.
 * Removed vestigial for loop in sampleAnalogSignal() method.
 * Write version of data format in .trs data file.
 *
 * Revision 1.18  1999/10/22 00:00:13  calderon
 * -added macro to use Erf instead of erf if we have HP and Root together.
 * -constructor with char* for StTrsDedx so solaris doesn't complain
 * -remove mZeros from StTrsDigitalSector.  This causes several files to
 *  be modified to conform to the new data format, so only mData remains,
 *  access functions change and digitization procedure is different.
 *
 * Revision 1.17  1999/10/11 23:55:22  calderon
 * Version with Database Access and persistent file.
 * Not fully tested due to problems with cons, it
 * doesn't find the local files at compile time.
 * Yuri suggests forcing commit to work directly with
 * files in repository.
 *
 * Revision 1.16  1999/04/11 00:32:54  lasiuk
 * ?reported hang from Herb...check diagnostics.
 * Seems okay.
 *
 * Revision 1.15  1999/03/03 14:20:22  lasiuk
 * set remaining time bins to zero when all full?
 *
 * Revision 1.14  1999/02/28 20:19:30  lasiuk
 * take number of time bins from db
 * not compatible with data compression from the analogSignalGenerator
 *
 * Revision 1.13  1999/02/14 20:46:07  lasiuk
 * debug info
 *
 * Revision 1.12  1999/02/12 01:26:37  lasiuk
 * Limit debug output
 *
 * Revision 1.11  1999/02/10 20:55:16  lasiuk
 * Feb 10,1999
 *
 * Revision 1.10  1999/02/10 04:24:50  lasiuk
 * sleep/unistd
 *
 * Revision 1.9  1999/02/04 18:35:24  lasiuk
 * digital sector removed from constructor;
 * fillSector() added in base class
 *
 * Revision 1.8  1999/01/28 02:52:22  lasiuk
 * printout for SUN
 *
 * Revision 1.7  1999/01/23 02:32:22  lasiuk
 * sun friendly
 *
 * Revision 1.6  1999/01/22 08:08:36  lasiuk
 * unsigned char; use of pair<> for two arrays
 *
 * Revision 1.5  1999/01/18 21:02:49  lasiuk
 * comment diagnostics
 *
 * Revision 1.4  1999/01/18 10:25:23  lasiuk
 * add conversion code for StTrsDigitalSector
 *
 * Revision 1.3  1998/11/16 14:48:45  lasiuk
 * use typedefs from StTrsSector
 *
 * Revision 1.2  1998/11/13 21:31:37  lasiuk
 * diagnostics
 *
 * Revision 1.1  1998/11/10 17:12:25  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.3  1998/11/08 17:31:00  lasiuk
 * allocator for SUN
 *
 * Revision 1.2  1998/11/04 18:51:27  lasiuk
 * initialization in base class
 * incorporate electronics db
 * sector by reference
 *
 * Revision 1.1  1998/06/30 22:46:49  lasiuk
 * Initial Revision
 *
 **************************************************************************/
#include <unistd.h>

#include <utility>
#include "StTrsOldDigitalSignalGenerator.hh"

StTrsDigitalSignalGenerator* StTrsOldDigitalSignalGenerator::mInstance = 0; // static member

StTrsOldDigitalSignalGenerator::StTrsOldDigitalSignalGenerator(StTpcElectronics* el, StTrsSector* sec)
    : StTrsDigitalSignalGenerator(el, sec)
{
    mSimpleConversion = mElectronicsDb->adcConversion();
}

StTrsOldDigitalSignalGenerator::~StTrsOldDigitalSignalGenerator() {/* missing */}

StTrsDigitalSignalGenerator*
StTrsOldDigitalSignalGenerator::instance()
{
    if(!mInstance) {
#ifndef ST_NO_EXCEPTIONS
	throw range_error("StTrsOldDigitalSignalGenerator::instance() Must Supply a File name");
#else
	cerr << "StTrsOldDigitalSignalGenerator::instance() Must Supply a File name" << endl;
	cerr << "Aborting..." << endl;
	abort();
#endif
    }
    return mInstance;
}

StTrsDigitalSignalGenerator*
StTrsOldDigitalSignalGenerator::instance(StTpcElectronics* el, StTrsSector* sec)
{
    if(!mInstance) {
	mInstance = new StTrsOldDigitalSignalGenerator(el, sec);
    }
    // else  do nothing
    
    return mInstance;
}

void StTrsOldDigitalSignalGenerator::digitizeSignal()
{
    //cout << "StTrsOldDigitalSignalGenerator::digitizeSignal()" << endl;
    // Loop over the sector

    tpcTimeBins currentPad;

    // Make a digital Pad!
#ifndef ST_NO_TEMPLATE_DEF_ARGS
    vector<unsigned char> digitalPadData;
#else
    vector<unsigned char, allocator<unsigned char> > digitalPadData;
#endif
    // Remember mSector is the "normal" analog sector!
    for(int irow=1; irow<=mSector->numberOfRows(); irow++) {
	for(unsigned int ipad=1; ipad<=mSector->padsOfRow(irow).size(); ipad++) {

	    currentPad = mSector->timeBinsOfRowAndPad(irow,ipad);
	    if(!currentPad.size()) continue;
   	    //cout << "dig() r/p " << irow << '/' << ipad << endl;

	    // Make sure the digital Pad is clear!
	    digitalPadData.clear();


	    unsigned int currentTimeBin = digitalPadData.size();
// 	    PR(currentTimeBin);
	    unsigned int zeroCounter = 0;
	    for(mTimeSequenceIterator  = currentPad.begin();
		mTimeSequenceIterator != currentPad.end();
		mTimeSequenceIterator++) {
		
		// Conversion
		// Must take into account the 8 <--> 10 bit conversion
		// TRS calculates on a linear scale and then must
		// convert to 8 bit data
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

	       		
		int digitalAmplitude =
		    static_cast<int>(mTimeSequenceIterator->amplitude()/mSimpleConversion);
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
//   	    for(int ii=0; ii<digitalPadData.size(); ii++) {
//   		cout << (ii) << '\t' << dec << (int)(digitalPadData[ii]) << endl;
//   	    }
	    
	    currentPad.clear();
	    mDigitalSector->assignTimeBins(irow,ipad,&digitalPadData);
	    //sleep(2);

	} // pads
    }// rows
    

}

void StTrsOldDigitalSignalGenerator::addCorrelatedNoise()
{
    cerr << "StTrsOldDigitalSignalGenerator::addCorrelatedNoise()" << endl;
    cerr << "Not Implemented!" << endl;
}

void StTrsOldDigitalSignalGenerator::addWhiteNoise()
{
    cerr << "StTrsOldDigitalSignalGenerator::addWhiteNoise()" << endl;
    cerr << "Not Implemented!" << endl;    
}

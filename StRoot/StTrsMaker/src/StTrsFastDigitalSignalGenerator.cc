/***************************************************************************
 *
 * $Id: StTrsFastDigitalSignalGenerator.cc,v 1.15 1999/03/03 14:20:22 lasiuk Exp $
 *
 * Author: 
 ***************************************************************************
 *
 * Description: fast simulation of digitization
 *
 ***************************************************************************
 *
 * $Log: StTrsFastDigitalSignalGenerator.cc,v $
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
	cerr << "Exitting..." << endl;
	exit(1);
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

void StTrsFastDigitalSignalGenerator::digitizeSignal()
{
    //cout << "StTrsFastDigitalSignalGenerator::digitizeSignal()" << endl;
    // Loop over the sector

    tpcTimeBins currentPad;

    // Make a digital Pad!
#ifndef ST_NO_TEMPLATE_DEF_ARGS
    vector<unsigned char> digitalPadData;
    vector<unsigned char> digitalPadZeros;
#else
    vector<unsigned char, allocator<unsigned char> > digitalPadData;
    vector<unsigned char, allocator<unsigned char> > digitalPadZeros;
#endif
    // Remember mSector is the "normal" analog sector!
    for(int irow=1; irow<=mSector->numberOfRows(); irow++) {
	for(int ipad=1; ipad<=mSector->padsOfRow(irow).size(); ipad++) {

	    currentPad = mSector->timeBinsOfRowAndPad(irow,ipad);
	    if(!currentPad.size()) continue;
 //  	    cout << "dig() r/p " << irow << '/' << ipad << endl;
	    // Make sure the digital Pad is clear!
	    digitalPadData.clear();
	    digitalPadZeros.clear();


	    int currentTimeBin = digitalPadData.size();
// 	    PR(currentTimeBin);
	    
	    for(mTimeSequenceIterator  = currentPad.begin();
		mTimeSequenceIterator != currentPad.end();
		mTimeSequenceIterator++) {

		// Conversion
		// Must take into account the 8 <--> 10 bit conversion
		// TRS calculates on a linear scale and then must
		// convert to 8 bit data
		//PR(*mTimeSequenceIterator);

		int digitalAmplitude =
		    mTimeSequenceIterator->amplitude()/mSimpleConversion;
		if(digitalAmplitude>255) digitalAmplitude = 255;

		if(digitalAmplitude != 0) {
		    digitalPadData.push_back(static_cast<unsigned char>(digitalAmplitude));
		    digitalPadZeros.push_back(static_cast<unsigned char>(0));
		    currentTimeBin++;
		}
		// Otherwise there is no signals!
		else if(digitalPadZeros.size() == 0) {
		    digitalPadData.push_back(static_cast<unsigned char>(0));
		    digitalPadZeros.push_back(static_cast<unsigned char>(1));
		    currentTimeBin++;
		}
		else if(digitalPadData.back() == static_cast<unsigned char>(0) ) {
		    
		    if (digitalPadZeros.back() == static_cast<unsigned char>(255)) {
			digitalPadData.push_back(static_cast<unsigned char>(0));
			digitalPadZeros.push_back(static_cast<unsigned char>(1));
			currentTimeBin++;
		    }
		    else {
			digitalPadZeros.back()++;
			currentTimeBin++;
		    }
		}
		else {
		    digitalPadData.push_back(static_cast<unsigned char>(0));
		    digitalPadZeros.push_back(static_cast<unsigned char>(1));
		    currentTimeBin++;
		}
	    } // the iterator (mTimeSequence)

	    if(currentTimeBin<mNumberOfTimeBins) {
		int remainingTimeBins = mNumberOfTimeBins - (currentTimeBin+1);
 // 		PR(currentTimeBin);
		do {
		    if(digitalPadData.back() == 0) {
			if(remainingTimeBins>255) {
			    digitalPadZeros.back() += 254;
			    remainingTimeBins -= 254;
			}
			else {
			    digitalPadZeros.back() += remainingTimeBins;
			    break;
			}
		    }
		    if(remainingTimeBins>255) {
			digitalPadData.push_back(static_cast<unsigned char>(0));
			digitalPadZeros.push_back(static_cast<unsigned char>(255));
			remainingTimeBins -= 255;
		    }
		    else {
			digitalPadData.push_back(static_cast<unsigned char>(0));
			digitalPadZeros.push_back(static_cast<unsigned char>(remainingTimeBins));
			remainingTimeBins = 0;
			break;
		    }
		} while (remainingTimeBins > 0);
	    }

	    
	    // print it out:
//   	    PR(digitalPadData.size());
//   	    for(int ii=0; ii<digitalPadData.size(); ii++) {
//   		cout << (ii) << '\t' << dec << (int)(digitalPadData[ii]) << '\t' << dec << (int)(digitalPadZeros[ii]) << endl;
//   	    }
	    
	    currentPad.clear();
	    pair<digitalTimeBins*, digitalTimeBins*> tmp(&digitalPadData, &digitalPadZeros);
	    mDigitalSector->assignTimeBins(irow,ipad,tmp);
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

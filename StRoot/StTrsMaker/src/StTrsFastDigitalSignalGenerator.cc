/***************************************************************************
 *
 * $Id: StTrsFastDigitalSignalGenerator.cc,v 1.5 1999/01/18 21:02:49 lasiuk Exp $
 *
 * Author: 
 ***************************************************************************
 *
 * Description: fast simulation of digitization
 *
 ***************************************************************************
 *
 * $Log: StTrsFastDigitalSignalGenerator.cc,v $
 * Revision 1.5  1999/01/18 21:02:49  lasiuk
 * comment diagnostics
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
 * Initial Revision
 *
 **************************************************************************/
#include <unistd.h>
StTrsFastDigitalSignalGenerator::StTrsFastDigitalSignalGenerator(StTpcElectronics* el, StTrsSector* sec, StTrsDigitalSector* digiSec)
    : StTrsDigitalSignalGenerator(el, sec, digiSec)
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
StTrsFastDigitalSignalGenerator::instance(StTpcElectronics* el, StTrsSector* sec, StTrsDigitalSector* digiSec)
    }
    return mInstance;
	mInstance = new StTrsFastDigitalSignalGenerator(el, sec, digiSec);

StTrsDigitalSignalGenerator*
StTrsFastDigitalSignalGenerator::instance(StTpcElectronics* el, StTrsSector* sec)
{
    if(!mInstance) {
	mInstance = new StTrsFastDigitalSignalGenerator(el, sec);
    }
    // else  do nothing
    cout << "Got to StTrsFastDigitalSignalGenerator::digitizeSignal()" << endl;
    return mInstance;
}
    cout << "StTrsFastDigitalSignalGenerator::digitizeSignal()" << endl;
    //PR(mSimpleConversion);
void StTrsFastDigitalSignalGenerator::digitizeSignal()
{
    vector<short> digitalPad;

    vector<unsigned char> digitalPadZeros;
#else
	    // should use an STL operation here
    vector<unsigned char, allocator<unsigned char> > digitalPadZeros;
#endif
	    cout << "r/p " << irow << '/' << ipad << endl;
    for(int irow=1; irow<=mSector->numberOfRows(); irow++) {
	    digitalPad.clear();
	    currentPad = mSector->timeBinsOfRowAndPad(irow,ipad);
	    if(!currentPad.size()) continue;
// 	    cout << "dig() r/p " << irow << '/' << ipad << endl;
	    // Make sure the digital Pad is clear!
	    digitalPadData.clear();
	    digitalPadZeros.clear();
		mTimeSequenceIterator++) {
		    static_cast<short>(mTimeSequenceIterator->amplitude()/mSimpleConversion);

		// TRS calculates on a linear scale and then must
		    digitalPad.push_back(static_cast<short>(digitalAmplitude));
		    digitalPadZeros.push_back(static_cast<unsigned char>(0));
		else if(digitalPad.size() == 0) {
		    digitalPad.push_back(static_cast<short>(-1));
			digitalPadData.push_back(static_cast<unsigned char>(0));
		else if(digitalPad.back() > 0) {
		    digitalPad.push_back(static_cast<short>(-1));
			digitalPadZeros.back()++;
		else
		    digitalPad.back()--;
	    }

		}
	    PR(digitalPad.size());
	    for(int ii=0; ii<digitalPad.size(); ii++) {
		cout << (ii) << '\t' << (static_cast<short>(digitalPad[ii])) << endl;
	    }
	    cout << endl;

	    // print it out:
	    mDigitalSector->assignTimeBins(irow,ipad,digitalPad);
	    
	    currentPad.clear();
	    pair<digitalTimeBins*, digitalTimeBins*> tmp(&digitalPadData, &digitalPadZeros);
	    mDigitalSector->assignTimeBins(irow,ipad,tmp);
	    //sleep(2);

	} // pads
    }// rows
    cout << "correlated noise" << endl;
}

void StTrsFastDigitalSignalGenerator::addCorrelatedNoise()
{
}

void StTrsFastDigitalSignalGenerator::addWhiteNoise()
{
    cerr << "StTrsFastDigitalSignalGenerator::addWhiteNoise()" << endl;
    cerr << "Not Implemented!" << endl;    
}

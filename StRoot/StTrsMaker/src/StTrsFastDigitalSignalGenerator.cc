/***************************************************************************
 *
 * $Id: StTrsFastDigitalSignalGenerator.cc,v 1.3 1998/11/16 14:48:45 lasiuk Exp $
 *
 * Author: 
 ***************************************************************************
 *
 * Description: fast simulation of digitization
 *
 ***************************************************************************
 *
 * $Log: StTrsFastDigitalSignalGenerator.cc,v $
 * Revision 1.3  1998/11/16 14:48:45  lasiuk
 * use typedefs from StTrsSector
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
StTrsFastDigitalSignalGenerator::StTrsFastDigitalSignalGenerator(StTpcElectronics* el, StTrsSector* sec)
    : StTrsDigitalSignalGenerator(el, sec)
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
StTrsFastDigitalSignalGenerator::instance(StTpcElectronics* el, StTrsSector* sec)
	throw range_error("StTrsFastDigitalSignalGenerator::instance() Must Supply a File name");
#else
	mInstance = new StTrsFastDigitalSignalGenerator(el, sec);
	cerr << "Exitting..." << endl;
	exit(1);
StTrsFastDigitalSignalGenerator::instance(StTpcElectronics* el, StTrsSector* sec, StTrsDigitalSector* digiSec)
    }
    return mInstance;
	mInstance = new StTrsFastDigitalSignalGenerator(el, sec, digiSec);

StTrsDigitalSignalGenerator*
StTrsFastDigitalSignalGenerator::instance(StTpcElectronics* el, StTrsSector* sec)
    if(!mInstance) {
    cout << "digitize here " << endl;
    PR(mSimpleConversion);
    // Loop over the sector
    // else  do nothing
    for(int irow=0; irow<mSector->numberOfRows(); irow++) {
	for(int ipad=0; ipad<mSector->padsOfRow(irow).size(); ipad++) {
void StTrsFastDigitalSignalGenerator::digitizeSignal()
    for(int irow=25; irow<+mSector->numberOfRows(); irow++) {
#endif
	    PR(digitalPad.size());
	    cout << "r/p " << irow << '/' << ipad << endl;
    for(int irow=1; irow<=mSector->numberOfRows(); irow++) {
		mTimeSequenceIterator->scaleAmplitude(1./mSimpleConversion);
			digitalPadData.push_back(static_cast<unsigned char>(0));
	    //put 'em back
	    mSector->assignTimeBins(irow,ipad,currentPad);
	    PR(digitalPad.size());
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

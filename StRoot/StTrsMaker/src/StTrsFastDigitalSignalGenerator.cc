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
 * Revision 1.31  2008/06/20 15:01:17  fisyak
 * move from StTrsData to StTpcRawData
 *
 * Revision 1.30  2007/07/12 20:25:05  fisyak
 * Use StarLogger, use time of flight, fix cluster shape
 *
 * Revision 1.29  2005/09/09 22:12:49  perev
 * Bug fix + IdTruth added
 *
 * Revision 1.27  2005/07/19 22:23:05  perev
 * Bug fix
 *
 * Revision 1.26  2003/12/24 13:44:53  fisyak
 * Add (GEANT) track Id information in Trs; propagate it via St_tpcdaq_Maker; account interface change in StTrsZeroSuppressedReaded in StMixerMaker
 *
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
 * Added StTrsZeroSuppressedReader and StTrsZeroSuppressedReader for DAQ type
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


StTrsFastDigitalSignalGenerator::StTrsFastDigitalSignalGenerator(StTpcElectronics* el, StTrsSector* sec,double conv)
    : StTrsDigitalSignalGenerator(el, sec)
{
    mSimpleConversion = conv;
    if (!mSimpleConversion) mSimpleConversion = mElectronicsDb->adcConversion();
}
StTrsFastDigitalSignalGenerator::~StTrsFastDigitalSignalGenerator() {/* missing */}


StTrsDigitalSignalGenerator*
StTrsFastDigitalSignalGenerator::instance(StTpcElectronics* el, StTrsSector* sec, double conv)
{
    if(!mInstance && !el) {
	cerr << "StTrsFastDigitalSignalGenerator::instance() Must Supply a File name" << endl;
	cerr << "Aborting..." << endl;
	abort();
    }
    if(!mInstance) {
	mInstance = new StTrsFastDigitalSignalGenerator(el, sec, conv);
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

  
   
    // Make a digital Pad!
      StDigitalTimeBins  digPadData;
    // Remember mSector is the "normal" analog sector! 
      //      cout << "StTrsFastDigitalSignalGenerator::digitizeSignal()" << endl;
      for(int irow=1; irow<=mSector->numberOfRows(); irow++) { 
	for(unsigned int ipad=1; ipad<=mSector->padsOfRow(irow).size(); ipad++) {
           
	  tpcTimeBins &currentPad = mSector->timeBinsOfRowAndPad(irow,ipad); 

	  int currentSize = currentPad.size();
	  if(!currentSize) continue;
   	  //cout << "dig() r/p " << irow << '/' << ipad << endl;
	  // Make sure the digital Pad is clear!
	  digPadData.clear();
	  //   cout<<irow<<" row "<<ipad<<" pad"<<endl;

	  int currentTimeBin = -2005;
	  for (int icur=0;icur<currentSize;icur++) {
            float amp = currentPad[icur].amplitude();
            if (!amp) 				continue;
            if (mSimpleConversion>0) amp = amp/mSimpleConversion;//+0.5;
	    int temporary_digitalAmplitude = int(amp);
            if (!temporary_digitalAmplitude)	continue;

	    int timeBinIndex = int(currentPad[icur].time()+1e-3);
            assert(timeBinIndex > currentTimeBin);
            if (timeBinIndex != currentTimeBin+1) digPadData.push_back(StDigitalPair(timeBinIndex));
            currentTimeBin = timeBinIndex;

	    unsigned char digitalAmplitude = do10to8Translation(temporary_digitalAmplitude);
            int id = currentPad[icur].id();
            digPadData.back().add(digitalAmplitude,id);
	  } // the iterator (mTimeSequence)
//VP	currentPad.clear();
	mDigitalSector->assignTimeBins(irow,ipad,&digPadData);

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

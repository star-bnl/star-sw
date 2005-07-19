/***************************************************************************
 *
 * $Id: StTrsZeroSuppressedReader.cc,v 1.11 2005/07/19 22:23:05 perev Exp $
 *
 * Authors: bl, mcbs
 ***************************************************************************
 *
 * Description: Access to the digital information via the abstract
 *              interface...this is the OLD UNPACKER
 ***************************************************************************
 *
 * $Log: StTrsZeroSuppressedReader.cc,v $
 * Revision 1.11  2005/07/19 22:23:05  perev
 * Bug fix
 *
 * Revision 1.10  2003/12/24 13:44:54  fisyak
 * Add (GEANT) track Id information in Trs; propagate it via St_tpcdaq_Maker; account interface change in StTrsZeroSuppressedReaded in StMixerMaker
 *
 * Revision 1.9  2003/09/19 22:17:50  jecc
 * Fix bug in getSequences intruced during gcc 3.2 updates
 *
 * Revision 1.8  2003/09/02 17:59:19  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.7  2000/06/07 02:03:12  lasiuk
 * exit/abort ultimatum
 *
 * Revision 1.6  2000/03/15 23:33:55  calderon
 * Remove extra messages
 *
 * Revision 1.5  2000/03/15 18:08:56  calderon
 * ZSR is no longer a singleton.  Two will be needed for mixer chain.
 *
 * Revision 1.4  2000/03/14 01:00:24  calderon
 * remove annoying beep
 *
 * Revision 1.3  2000/01/10 23:14:31  lasiuk
 * Include MACROS for compatiblity with SUN CC5
 *
 * Revision 1.2  1999/12/08 02:10:43  calderon
 * Modified to eliminate warnings on Linux.
 *
 * Revision 1.1  1999/11/05 22:18:17  calderon
 *
 * Made private copy constructor and operator= in StTrsDigitalSector.
 * Renamed DigitalSignalGenerators: Fast -> Old, Parameterized -> Fast
 * and use new "Fast" as default.
 * Added StTrsZeroSuppressedReader and StTrsZeroSuppressedReader for DAQ type
 * data access.
 * Removed vestigial for loop in sampleAnalogSignal() method.
 * Write version of data format in .trs data file.
 *
 ***************************************************************************/
#include "StTrsZeroSuppressedReader.hh"
#include <assert.h>
#include <algorithm>
#include <iterator>
#if defined (__SUNPRO_CC) && __SUNPRO_CC >= 0x500
using std::find;
using std::distance;
#endif
#ifndef ST_NO_EXCEPTIONS
#   include <stdexcept>
#   if !defined(ST_NO_NAMESPACES)
        using std::out_of_range;
#   endif
#endif

#include "StGlobals.hh"
#include "StTrsRawDataEvent.hh"
#include "StTrsDigitalSector.hh"
static const int MaxPixels = 512;
static unsigned char ADCs[MaxPixels];
static int  IDs[MaxPixels];
static int  Npixels = 0;


StTrsZeroSuppressedReader::StTrsZeroSuppressedReader()
    :mSector(1), mTheSector(0), mPadList(0), mSequence(0), mTrsEvent(0)
{ 
  assert(sizeof(StSequence) == sizeof(Sequence)); 
}

// StTrsZeroSuppressedReader::StTrsZeroSuppressedReader(int sector, StTpcRawEvent& theEvent) : mSector(sector), mTheEvent(theEvent)
// {  }

StTrsZeroSuppressedReader::StTrsZeroSuppressedReader(StTpcRawDataEvent* ev)
    :mSector(1), mTheSector(0), mPadList(0), mSequence(0)
{
  assert(sizeof(StSequence) == sizeof(Sequence)); 
  
    mTrsEvent = dynamic_cast<StTrsRawDataEvent*>(ev);
    if (!mTrsEvent) {
	cerr << "Error constructing StTrsZeroSuppressedReader" << endl;
	cerr << "Cast Failed! ev not of required type (StTrsRawDataEvent*)\n";
    }
}

int StTrsZeroSuppressedReader::setSector(int sector)
{
    clear();
    // Check if the data is there --> getSector() in the Unpacker!
    // ...you may even want to check the mVersion to return the
    // proper mZSR...
    
    if(checkTheData(sector)) {
	    mSector = sector;
	    mTheSector = mTrsEvent->mSectors[sector-1];
	    return 1;
    }
    else {
	return 0;
    }
}


StTrsZeroSuppressedReader::~StTrsZeroSuppressedReader()
{
    if (mPadList)  delete [] mPadList;
    mPadList  = 0;
}

int StTrsZeroSuppressedReader::checkTheData(unsigned int which)
{
    int status(0);
    
    if(mTrsEvent->mSectors.size() >= which)  // bounds check
	if ( (mTrsEvent->mSectors[(which-1)]) ) status = 1;
    return status;   
}

int StTrsZeroSuppressedReader::getPadList(int padRow, unsigned char **padList)
{
    if(mPadList) {
	delete [] mPadList;
	mPadList = 0;
    }
    //
    // Count the sequences on the pad and store the list
    int numberOfPadsWithSignals = 0;
    //
    // Should be data base derived quatities...
    if(padRow<1 || padRow>45) {
#ifndef ST_NO_EXCEPTIONS
	throw out_of_range("Pad Row out of range");
#else
	cerr << "Pad Row " << padRow << " out of range" << endl;
	cerr << "Normally one would throw an exception here" << endl;
	abort();
#endif
    }
    
//     PR(padRow);
//     PR(mTheSector->numberOfPadsInRow(padRow));

#ifndef ST_NO_TEMPLATE_DEF_ARGS
    vector<unsigned char> tmp;
#else
    vector<unsigned char, allocator<unsigned char> > tmp;
#endif
    tmp.clear();
    // Loop over all the pads:
    for(int ii=1; ii<=mTheSector->numberOfPadsInRow(padRow); ii++) {
	if (mTheSector->numberOfTimeBins(padRow,ii) > 0) {
  	    //cout << " pad " << ii << " " << (mTheSector->numberOfTimeBins(padRow,ii)) << endl;
	    numberOfPadsWithSignals++;
	    tmp.push_back(ii);
	}
    }

    if (tmp.size() == 0) {
	mPadList = 0;
    }
    else {
	// Otherwise fill the pad list
	mPadList = new unsigned char[(tmp.size())];

	for(unsigned int ii=0; ii< tmp.size(); ii++) {
	    mPadList[ii] = tmp[ii];
// 	    PR(static_cast<int>(mPadList[ii]));
	}

	*padList = mPadList;
    }

    return numberOfPadsWithSignals;
}

int getInt3(unsigned char a) { return static_cast<int>(a);}

//int StTrsZeroSuppressedReader::getSequences(int PadRow, int Pad, int *nSeq, StSequence** Seq)
//________________________________________________________________________________
int StTrsZeroSuppressedReader::getSequences(int PadRow, int Pad, int *nSeq, Sequence** Seq) {
  return getSequences(PadRow, Pad, nSeq, (StSequence**) Seq);
}
//________________________________________________________________________________
int StTrsZeroSuppressedReader::getSequences(int PadRow, int Pad, int *nSeq, Sequence** Seq, int ***Ids) {
  return getSequences(PadRow, Pad, nSeq, (StSequence**) Seq, Ids);
}
//________________________________________________________________________________
int StTrsZeroSuppressedReader::getSequences(int PadRow, int Pad, int *nSeq, StSequence** Seq) {
  int **Ids;
  return StTrsZeroSuppressedReader::getSequences(PadRow, Pad,nSeq, Seq, &Ids);
}
//________________________________________________________________________________
int StTrsZeroSuppressedReader::getSequences(int PadRow, int Pad, int *nSeq, StSequence** Seq, int ***Ids) {

  *Seq=0;*Ids=0;*nSeq=0;
  mSequence.clear();
  mIds.clear();
  Npixels = 0;
  memset(ADCs,     0, sizeof(ADCs));
  memset(IDs,      0, sizeof(Ids ));
  digitalTimeBins* TrsPadData = mTheSector->timeBinsOfRowAndPad(PadRow,Pad);
  if (!TrsPadData) return 1;
  digitalTimeBins &trsPadData = *TrsPadData;
  int nTimeBins = trsPadData.size();
  if (!nTimeBins) return 2;

  int currentTimeBin = 0;
  // Construct the sequences:
  StSequence aSequence;aSequence.length=0;

  for (int ibin=0;ibin<nTimeBins;ibin++)  {
    int newTimeBin = trsPadData[ibin].time();
    if (newTimeBin>=0) {
      if (aSequence.length) {
        nSeq[0]++;
        aSequence.startTimeBin = currentTimeBin;
        aSequence.firstAdc = ADCs+Npixels-aSequence.length;
        mSequence.push_back(aSequence);
        mIds.push_back(IDs +Npixels-aSequence.length);
      }
      currentTimeBin=newTimeBin; aSequence.length=0;
      continue;
    }
    aSequence.length++;
    ADCs[Npixels] = trsPadData[ibin].adc(); 
    IDs [Npixels] = trsPadData[ibin].id() ; 
    Npixels++;
  }

  *Seq = &mSequence[0];
  *Ids = &mIds[0];
  
  return 0;
}
//________________________________________________________________________________
void StTrsZeroSuppressedReader::clear()
{
    //cout << "StTrsZeroSuppressedReader::clear()" << endl;
    if (mPadList) delete [] mPadList;
    mPadList = 0;
    mSequence.clear();
}
//________________________________________________________________________________
#if 0
int StTrsZeroSuppressedReader::getSpacePts(int PadRow, int* nSpacePts, SpacePt** SpacePoints)
{
    cout << "StTrsZeroSuppressedReader::getSpacePts() NOT IMPLEMENTED!!" << endl;
    return 0;
}

int StTrsZeroSuppressedReader::MemUsed()
{
    return 0;
}
#endif

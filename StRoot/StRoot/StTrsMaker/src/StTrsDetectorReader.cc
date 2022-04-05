/***************************************************************************
 *
 * $Id: StTrsDetectorReader.cc,v 1.5 2005/09/09 22:12:49 perev Exp $
 *
 * Authors: bl, mcbs
 ***************************************************************************
 *
 * Description: Access to the digital information via the abstract
 *              interface
 ***************************************************************************
 *
 * $Log: StTrsDetectorReader.cc,v $
 * Revision 1.5  2005/09/09 22:12:49  perev
 * Bug fix + IdTruth added
 *
 * Revision 1.4  2003/12/24 13:44:53  fisyak
 * Add (GEANT) track Id information in Trs; propagate it via St_tpcdaq_Maker; account interface change in StTrsZeroSuppressedReaded in StMixerMaker
 *
 * Revision 1.3  2000/03/15 18:08:56  calderon
 * ZSR is no longer a singleton.  Two will be needed for mixer chain.
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
 ***************************************************************************/
#include "StTrsDetectorReader.hh"
#include "StTrsZeroSuppressedReader.hh"

StTrsDetectorReader::StTrsDetectorReader()
    : mVersion("0"), mTheEvent(0), mZSR(0) { /*nopt*/ }

// StTrsDetectorReader::StTrsDetectorReader(string& fd)
//     : mZSR(0)
// {
//     // fd is the file descriptor
//     //mTheEvent =
// }
// StTrsDetectorReader::StTrsDetectorReader(St_DataSet& ev)2
//     : mZSR(0)
// {
//     // ev is the dataset
//     //mTheEvent = 
// }

StTrsDetectorReader::StTrsDetectorReader(StTpcRawDataEvent* ev, std::string ver)
    : mVersion(ver), mTheEvent(ev) 
{
    mZSR = new StTrsZeroSuppressedReader(mTheEvent);
}

StTrsDetectorReader::~StTrsDetectorReader()
{
    delete mZSR;
}

StTrsZeroSuppressedReader* StTrsDetectorReader::getZeroSuppressedReader(int sector)
{
    if(mZSR->setSector(sector))  return mZSR;
    return 0;
	
}
#if 0
ADCRawReader* StTrsDetectorReader::getADCRawReader(int sector)
{
    cerr << "StTrsDetectorReader::getADCRawReader Not Implemented" << endl;
    return 0;
}

PedestalReader* StTrsDetectorReader::getPedestalReader(int sector)
{
    cerr << " Not Implemented" << endl;
    return 0;
}

PedestalRMSReader* StTrsDetectorReader::getPedestalRMSReader(int sector)
{
    cerr << "StTrsDetectorReader::getPedestalRMSReader Not Implemented" << endl;
    return 0;
}

GainReader* StTrsDetectorReader::getGainReader(int sector)
{
    cerr << "StTrsDetectorReader::getGainReader Not Implemented" << endl;
    return 0;
}

CPPReader* StTrsDetectorReader::getCPPReader(int sector)
{
    cerr << "StTrsDetectorReader::getCPPReader Not Implemented" << endl;
    return 0;
}

BadChannelReader* StTrsDetectorReader::getBadChannelReader(int sector)
{
    cerr << "StTrsDetectorReader::getBadChannelReader Not Implemented" << endl;
    return 0;
}


int StTrsDetectorReader::InformBuffers(ZeroSuppressedReader *, int sector) {return 0;}
int StTrsDetectorReader::InformBuffers(ADCRawReader *,int sector) {return 0;}
int StTrsDetectorReader::InformBuffers(PedestalReader *,int sector) {return 0;}
int StTrsDetectorReader::InformBuffers(PedestalRMSReader *,int sector) {return 0;}
int StTrsDetectorReader::InformBuffers(GainReader *,int sector) {return 0;}
int StTrsDetectorReader::InformBuffers(CPPReader *,int sector) {return 0;}
int StTrsDetectorReader::InformBuffers(BadChannelReader *,int sector) {return 0;}
int StTrsDetectorReader::InformBuffers(ConfigReader *,int sector) {return 0;}

int StTrsDetectorReader::AttachBuffers(ZeroSuppressedReader *, int sector) {return 0;}
int StTrsDetectorReader::AttachBuffers(ADCRawReader *, int sector) {return 0;}
int StTrsDetectorReader::AttachBuffers(PedestalReader *, int sector) {return 0;}
int StTrsDetectorReader::AttachBuffers(PedestalRMSReader *, int sector) {return 0;}
int StTrsDetectorReader::AttachBuffers(GainReader *, int sector) {return 0;}
int StTrsDetectorReader::AttachBuffers(CPPReader *, int sector) {return 0;}
int StTrsDetectorReader::AttachBuffers(BadChannelReader *, int sector) {return 0;}
int StTrsDetectorReader::AttachBuffers(ConfigReader *, int sector) {return 0;}

int StTrsDetectorReader::MemUsed() {return 0;}
#endif

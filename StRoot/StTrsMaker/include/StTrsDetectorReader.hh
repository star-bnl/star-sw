/***************************************************************************
 *
 * $Id: StTrsDetectorReader.hh,v 1.3 2003/12/24 13:44:51 fisyak Exp $
 *
 * Authors: bl, mcbs
 ***************************************************************************
 *
 * Description: Access to the digital information via the abstract
 *              interface
 ***************************************************************************
 *
 * $Log: StTrsDetectorReader.hh,v $
 * Revision 1.3  2003/12/24 13:44:51  fisyak
 * Add (GEANT) track Id information in Trs; propagate it via St_tpcdaq_Maker; account interface change in StTrsZeroSuppressedReaded in StMixerMaker
 *
 * Revision 1.2  1999/12/08 02:10:25  calderon
 * Modified to eliminate warnings on Linux.
 *
 * Revision 1.1  1999/11/05 22:17:04  calderon
 * Made private copy constructor and operator= in StTrsDigitalSector.
 * Renamed DigitalSignalGenerators: Fast -> Old, Parameterized -> Fast
 * and use new "Fast" as default.
 * Added StTrsDetectorReader and StTrsZeroSuppressedReader for DAQ type
 * data access.
 *
 ***************************************************************************/
#ifndef ST_TRS_DETECTOR_READER_HH
#define ST_TRS_DETECTOR_READER_HH

#include <string>

#include "StTpcRawDataEvent.hh" 

//#include "StDaqLib/GENERIC/EventReader.hh"
class StTrsZeroSuppressedReader;
//typedef StTrsZeroSuppressedReader ZeroSuppressedReader;
class StTrsDetectorReader {//: public DetectorReader {
public:
    StTrsDetectorReader();
    //    StTrsDetectorReader(string& fd, string& ver = "0");
    //    StTrsDetectorReader(St_DataSet& ev, string& ver = "0");
    StTrsDetectorReader(StTpcRawDataEvent* ev, std::string ver = std::string("TrsDatav1.0"));
    
    ~StTrsDetectorReader();
    
    StTrsZeroSuppressedReader* getZeroSuppressedReader(int sector);
#if 0
    ADCRawReader*         getADCRawReader(int sector);
    PedestalReader*       getPedestalReader(int sector);
    PedestalRMSReader*    getPedestalRMSReader(int sector);
    GainReader*           getGainReader(int sector);
    CPPReader*            getCPPReader(int sector);
    BadChannelReader*     getBadChannelReader(int sector);
    
    int MemUsed();
#endif    
protected:
#if 0    
    // Buffer and index functions for the various readers.
    // Initially these will do nothing.  Add functionality 
    // to increase performance
    int InformBuffers(ZeroSuppressedReader *, int sector);
    int InformBuffers(ADCRawReader *,int sector);
    int InformBuffers(PedestalReader *,int sector);
    int InformBuffers(PedestalRMSReader *,int sector);
    int InformBuffers(GainReader *,int sector);
    int InformBuffers(CPPReader *,int sector);
    int InformBuffers(BadChannelReader *,int sector);
    int InformBuffers(ConfigReader *,int sector);
    
    int AttachBuffers(ZeroSuppressedReader *, int sector);
    int AttachBuffers(ADCRawReader *, int sector);
    int AttachBuffers(PedestalReader *, int sector);
    int AttachBuffers(PedestalRMSReader *, int sector);
    int AttachBuffers(GainReader *, int sector);
    int AttachBuffers(CPPReader *, int sector);
    int AttachBuffers(BadChannelReader *, int sector);
    int AttachBuffers(ConfigReader *, int sector);
    
    int errnum;
    //char errstr0[250];
    
#endif    
private:
    StTrsDetectorReader(const StTrsDetectorReader&);
    StTrsDetectorReader& operator=(const StTrsDetectorReader& rhs);
    std::string                mVersion;
    StTpcRawDataEvent*         mTheEvent;
    StTrsZeroSuppressedReader* mZSR;
};
#endif

 /***************************************************************************
 *
 * $Id: StETofCalibMaker.h,v 1.2 2019/03/08 19:01:01 fseck Exp $
 *
 * Author: Florian Seck, April 2018
 ***************************************************************************
 *
 * Description: StETofCalibMaker - class to read the eTofCollection from
 * StEvent, do calibration, fill the collection with the updated information
 * and write back to StEvent
 *
 ***************************************************************************
 *
 * $Log: StETofCalibMaker.h,v $
 * Revision 1.2  2019/03/08 19:01:01  fseck
 * pick up the right trigger and reset time on event-by-event basis  +  fix to clearing of calibrated tot in afterburner mode  +  flag pulser digis
 *
 * Revision 1.1  2019/02/19 19:52:28  jeromel
 * Reviewed code provided by F.Seck
 *
 *
 ***************************************************************************/
#ifndef STETOFCALIBMAKER_H
#define STETOFCALIBMAKER_H


#include <string>
#include <map>
#include <utility>

#include "StMaker.h"

class TH1F;
class TProfile;

class StEvent;
class StMuDst;

class StETofHeader;
class StETofDigi;
class StMuETofDigi;
class StMuETofHeader;

class StETofHardwareMap;


class StETofCalibMaker: public StMaker {
public:
    /// default constructor
    StETofCalibMaker( const char* name = "etofCalib" );

    ~StETofCalibMaker();


    Int_t  Init();
    Int_t  InitRun(   Int_t );
    Int_t  FinishRun( Int_t );
    Int_t  Finish();
    Int_t  Make();

    void processStEvent();
    void processMuDst();

    /// read calibration parameters from file
    void setFileNameCalibParam(      const char* fileName );
    void setFileNameElectronicsMap(  const char* fileName );
    void setFileNameStatusMap(       const char* fileName );
    void setFileNameTimingWindow(    const char* fileName );
    void setFileNameSignalVelocity(  const char* fileName );
    void setFileNameCalibHistograms( const char* fileName );
    void setFileNameResetTimeCorr(   const char* fileName );

    void setDebug( const bool debug );


private:
    bool isFileExisting( const std::string fileName );

    void resetToRaw      ( StETofDigi* aDigi );
    void applyMapping    ( StETofDigi* aDigi );
    void flagPulserDigis ( StETofDigi* aDigi, unsigned int index );
    void applyCalibration( StETofDigi* aDigi, StETofHeader* etofHeader );

    void resetToRaw      ( StMuETofDigi* aDigi );
    void applyMapping    ( StMuETofDigi* aDigi );
    void flagPulserDigis ( StMuETofDigi* aDigi, unsigned int index );
    void applyCalibration( StMuETofDigi* aDigi, StMuETofHeader* etofHeader );


    double calibTotFactor (    StETofDigi* aDigi );
    double calibTimeOffset(    StETofDigi* aDigi );
    double slewingTimeOffset ( StETofDigi* aDigi );

    double resetTimeCorr() const;

    double triggerTime( StETofHeader* etofHeader );
    double resetTime  ( StETofHeader* etofHeader );

    unsigned int channelToKey(         const unsigned int channelId  );
    unsigned int channelToDetectorKey( const unsigned int channelId  );
    unsigned int detectorToKey(        const unsigned int detectorId );


    StEvent*             mEvent;
    StMuDst*             mMuDst;
    StETofHardwareMap*   mHwMap;         // electronic channel to hardware/geometry map

    std::string   mFileNameCalibParam;      // name of parameter file for calibration parameters
    std::string   mFileNameElectronicsMap;  // name of parameter file for electronics-to-hardware map
    std::string   mFileNameStatusMap;       // name of parameter file for status map
    std::string   mFileNameTimingWindow;    // name of parameter file for timing window
    std::string   mFileNameSignalVelocity;  // name of parameter file for signal velocity
    std::string   mFileNameCalibHistograms; // name of parameter file for calibration histograms (output of QA maker)
    std::string   mFileNameResetTimeCorr;   // name of parameter file for reset time correction

    Int_t         mRunYear;                 // "year" of operation by using roughly October 1st as reference
    Float_t       mGet4TotBinWidthNs;       // conversion factor for Get4 chip TOT bin to nanoseconds
    Int_t         mMinDigisPerSlewBin;      // minimal required statistics per channel and TOT bin to apply slewing corrections
    Float_t       mResetTimeCorr;           // additional offset for the whole system in case reset time was not saved correctly

    Double_t      mTriggerTime;             // trigger time in ns
    Double_t      mResetTime;               // reset time in ns 

    std::map< UInt_t, std::pair< Float_t, Float_t > >  mTimingWindow;    // timing window for each AFCK
    std::map< UInt_t, std::pair< Float_t, Float_t > >  mPulserWindow;    // pulser window for each AFCK
    std::map< UInt_t, UInt_t >   mStatus;          // status of each channel: 0 - not existing/not working, 1 - working
    std::map< UInt_t, Float_t >  mSignalVelocity;  // signal velocities in each detector

    std::map< UInt_t, TH1F* >     mDigiTotCorr;     // factor to calibrate TOT  per channel saved in one histogram (64 bins) per counter accessed by detectorId as key
    std::map< UInt_t, TH1F* >     mDigiTimeCorr;    // offset to calibrate time per channel saved in one histogram (64 bins) per counter accessed by detectorId as key
    std::map< UInt_t, TProfile* > mDigiSlewCorr;    // offset to account for slewing corrections per channel saved in a histogram (~30 TOT bins) accessed by channelId as key

    Bool_t        mDebug;


    virtual const Char_t *GetCVS() const { static const char cvs[]="Tag $Name:  $Id: built " __DATE__ " " __TIME__ ; return cvs; }

    ClassDef( StETofCalibMaker, 0 )
};

inline void StETofCalibMaker::setFileNameCalibParam(      const char* fileName )     { mFileNameCalibParam      = fileName; }
inline void StETofCalibMaker::setFileNameElectronicsMap(  const char* fileName )     { mFileNameElectronicsMap  = fileName; }
inline void StETofCalibMaker::setFileNameStatusMap(       const char* fileName )     { mFileNameStatusMap       = fileName; }
inline void StETofCalibMaker::setFileNameTimingWindow(    const char* fileName )     { mFileNameTimingWindow    = fileName; }
inline void StETofCalibMaker::setFileNameSignalVelocity(  const char* fileName )     { mFileNameSignalVelocity  = fileName; }
inline void StETofCalibMaker::setFileNameCalibHistograms( const char* fileName )     { mFileNameCalibHistograms = fileName; }

inline double StETofCalibMaker::resetTimeCorr() const { return mResetTimeCorr; }

inline void StETofCalibMaker::setDebug( const bool debug ) { mDebug = debug; }

#endif // STETOFCALIBMAKER_H
// $Id: StTrsMaker.h,v 1.13 1999/11/05 22:10:13 calderon Exp $
//
// $Log: StTrsMaker.h,v $
// Revision 1.13  1999/11/05 22:10:13  calderon
// Added Clear() method in StTrsMaker.
// Removed StTrsUnpacker from maker.
// Added StTrsDetectorReader and StTrsZeroSuppressedReader
// for DAQ type interface to event data.
// Made private copy constructor and operator= in classes that needed it.
// Renamed the DigitalSignalGenerators: Fast -> Old, Parameterized -> Fast,
// and the default to use is the new "Fast" which has the 10-8 bit conversion.
// Removed vestigial loop in AnalogSignalGenerator.
// Added Time diagnostics.
// Added trsinterface.pdf in doc/ directory.
// Write version of data format in .trs data file.
//
// Revision 1.12  1999/10/11 23:54:32  calderon
// Version with Database Access and persistent file.
// Not fully tested due to problems with cons, it
// doesn't find the local files at compile time.
// Yuri suggests forcing commit to work directly with
// files in repository.
//
// Revision 1.11  1999/10/04 17:33:43  long
// add mUseParameterizedSignalGenerator
//
// Revision 1.10  1999/07/20 02:16:59  lasiuk
// bring in line with new options (TSS algorithms)
//
// Revision 1.9  1999/07/15 13:57:31  perev
// cleanup
//
// Revision 1.8  1999/03/20 20:07:57  fisyak
// Add access to DataSet with parameters
//
// Revision 1.7  1999/03/20 03:23:58  lasiuk
// setMiniSegmentLength()
// setFirstSectorToProcess()
// setLastSectorToProcess()
//
// Revision 1.6  1999/03/16 02:01:45  lasiuk
// add Finish() which frees the memory allocated in Init()
//
// Revision 1.5  1999/03/15 02:52:26  perev
// new Maker schema
//
// Revision 1.4  1999/02/19 16:28:24  fisyak
// Change given name of Maker
//
// Revision 1.3  1999/02/10 04:30:02  lasiuk
// add unpacker and rawevent as data members/ passed by dataset
//
// Revision 1.2  1999/02/04 18:39:25  lasiuk
// Add private member whichSector() to decode volumeId;
// add multiple sector capabilities
// add unpacker
// runs in LINUX
//
// Revision 1.1  1999/01/22 21:31:46  lasiuk
// Name change
//
// Revision 1.2  1999/01/22 11:43:09  lasiuk
// add output structures
//
//
#ifndef STAR_St_Trs_Maker
#define STAR_St_Trs_Maker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StTrsMaker virtual base class for Maker                            //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif
#ifndef HEP_SYSTEM_OF_UNITS_H
#include "SystemOfUnits.h"
#endif
// Dbs
class StTpcGeometry;
class StTpcSlowControl;
class StMagneticField;
class StTpcElectronics;
class StTrsDeDx;

// Processes 
class StTrsChargeTransporter;
class StTrsAnalogSignalGenerator;
class StTrsDigitalSignalGenerator;

// Containers
class StTrsWireHistogram;
class StTrsSector;
class StTrsDigitalSector;

// Output Data
class StTrsRawDataEvent;
class StTrsIstream;
class StTrsOstream;

class StTrsMaker : public StMaker {
 private:
    StTrsMaker(const StTrsMaker&);
    StTrsMaker& operator=(const StTrsMaker&);
// static Char_t  m_VersionCVS = "$Id: StTrsMaker.h,v 1.13 1999/11/05 22:10:13 calderon Exp $";
// Int_t          m_mode;        // mode 1 = primaries;
// St_stk_stkpar *m_stk_stkpar;  //! pointer to stk parameters


    // DataBases
    StTpcGeometry               *mGeometryDb; //!
    StTpcSlowControl            *mSlowControlDb; //!
    StMagneticField             *mMagneticFieldDb;//!
    StTpcElectronics            *mElectronicsDb; //!
    StTrsDeDx                   *mGasDb;//!
    
    // Processes 
    StTrsChargeTransporter      *mChargeTransporter;//!
    StTrsAnalogSignalGenerator  *mAnalogSignalGenerator;//!
    StTrsDigitalSignalGenerator *mDigitalSignalGenerator;//!
    
    // Container
    StTrsWireHistogram          *mWireHistogram;//!
    StTrsSector                 *mSector;//!
    StTrsDigitalSector          *mDigitalSector;//!

    // Output
    StTrsRawDataEvent           *mAllTheData;//!

    // I/O streams
    char*                        mInputFileName;//!
    char*                        mOutputFileName;//!
    StTrsIstream*                mInputStream;//!
    StTrsOstream*                mOutputStream;//!
    int                          mNumberOfEvents;//!

    // Calculation and Initialization Done Internally in the Maker
    double                       mMiniSegmentLength;
    int                          mFirstSectorToProcess;
    int                          mLastSectorToProcess;
    // should be a boolean
    int                         mProcessPseudoPadRows;
    int                         mWriteToFile;
    int                         mReadFromFile;
    
    // Which Algorithm to be used:
  int                           mUseParameterizedSignalGenerator;

protected:

public: 
    StTrsMaker(const char *name="Trs");
    ~StTrsMaker();
    Int_t  Init();
    Int_t  Make();
    Int_t  Finish();
    Int_t  Clear();
    
    void   setMiniSegmentLength(double len=4.) {mMiniSegmentLength = len*millimeter;} // *MENU*
    void   setFirstSectorToProcess(int first=1){mFirstSectorToProcess = first;}       // *MENU*
    void   setLastSectorToProcess(int last=24) {mLastSectorToProcess = last;}         // *MENU*

    int   readFile(char*);//!
    int   writeFile(char*, int);//!
    
  virtual const char *GetCVS() const
  {
      static const char cvs[]= "Tag $Name:  $ $Id: StTrsMaker.h,v 1.13 1999/11/05 22:10:13 calderon Exp $ built __DATE__ __TIME__" ; return cvs;}

    ClassDef(StTrsMaker, 1)   //StAF chain virtual base class for Makers

private:
    void whichSector(int, int*, int*, int*);
};

#endif

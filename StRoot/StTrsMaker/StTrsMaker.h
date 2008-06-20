// $Id: StTrsMaker.h,v 1.24 2008/06/20 15:00:57 fisyak Exp $
//
// $Log: StTrsMaker.h,v $
// Revision 1.24  2008/06/20 15:00:57  fisyak
// move from StTrsData to StTpcRawData
//
// Revision 1.23  2005/09/09 22:12:48  perev
// Bug fix + IdTruth added
//
// Revision 1.21  2005/07/19 22:21:06  perev
// Bug fix
//
// Revision 1.20  2003/12/24 13:44:47  fisyak
// Add (GEANT) track Id information in Trs; propagate it via St_tpcdaq_Maker; account interface change in StTrsZeroSuppressedReaded in StMixerMaker
//
// Revision 1.19  2003/09/10 19:47:41  perev
// ansi corrs
//
// Revision 1.18  2003/05/02 23:54:19  hardtke
// Allow user to adjust normalFactor (i.e. Fudge Factor)
//
// Revision 1.17  2002/02/05 22:21:28  hardtke
// Move Init code to InitRun
//
// Revision 1.16  2000/08/04 21:03:58  perev
// Leaks + Clear() cleanup
//
// Revision 1.15  2000/01/10 23:11:07  lasiuk
// Include MACROS for compatibility with SUN CC5.0
//
// Revision 1.14  1999/11/11 19:42:25  calderon
// Add #ifdef HISTOGRAM for Ntuple Diagnostics.
// Use ROOT_DATABASE_PARAMETERS.  As soon as Jeff and Dave give Ok,
// we will switch to TPC_DATABASE_PARAMETERS.
//
// Revision 1.13  1999/11/05 22:10:13  calderon
// Added Clear() method in StTrsMaker.
// Removed StTrsUnpacker from maker.
// Added StTrsZeroSuppressedReader and StTrsZeroSuppressedReader
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
#ifndef ST_NO_NAMESPACES
using namespace units;
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
#include  "StTrsDigitalSector.hh"

// Output Data
#include "StTrsRawDataEvent.hh"

class TFile;
class TNtuple;
class g2t_tpc_hit_st;

class StTrsMaker : public StMaker {
 private:
    StTrsMaker(const StTrsMaker&);
    StTrsMaker& operator=(const StTrsMaker&);
// static Char_t  m_VersionCVS = "$Id: StTrsMaker.h,v 1.24 2008/06/20 15:00:57 fisyak Exp $";
// Int_t          m_mode;        // mode 1 = primaries;
// St_stk_stkpar *m_stk_stkpar;  //! pointer to stk parameters


    // DataBases
    char                         mBeg[1];
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

    // Calculation and Initialization Done Internally in the Maker
    double                       mMiniSegmentLength;
    int                          mFirstSectorToProcess;
    int                          mLastSectorToProcess;
    // should be a boolean
    int                          mProcessPseudoPadRows;
    int                          mWriteToFile;
    int                          mReadFromFile;
    
    // Which Algorithm to be used:
    int                          mUseParameterizedSignalGenerator;

   //  Gain normalization Factor
    double 			 mNormalFactor; 

protected:

public:
    TFile*   mTrsNtupleFile; //!
    TNtuple* mWireNtuple; //!
    TNtuple* mContinuousAnalogNtuple;
    TNtuple* mDiscreteAnalogNtuple; //!
    TNtuple* mDigitalNtuple; //!
    char     mEnd[1];

public:
    StTrsMaker(const char *name="Trs");
    ~StTrsMaker();
    Int_t  Init();
    Int_t  InitRun(int runnumber);
    Int_t  Make();
    Int_t  Finish();
    void   Clear(const char *opt="");
    
    void   setMiniSegmentLength(double len=4.) {mMiniSegmentLength = len*millimeter;} // *MENU*
    void   setFirstSectorToProcess(int first=1){mFirstSectorToProcess = first;}       // *MENU*
    void   setLastSectorToProcess(int last=24) {mLastSectorToProcess = last;}         // *MENU*

    int   readFile(char*);//!
    int   writeFile(char*, int);//!
    void  setNormalFactor(double FudgeFactor=1.0);
    
  virtual const char *GetCVS() const
  {
      static const char cvs[]= "Tag $Name:  $ $Id: StTrsMaker.h,v 1.24 2008/06/20 15:00:57 fisyak Exp $ built __DATE__ __TIME__" ; return cvs;}

    ClassDef(StTrsMaker,0)   //StAF chain virtual base class for Makers

private:
    void whichSector(int, int*, int*, int*);
    void CheckTruth(int no_tpc_hits, g2t_tpc_hit_st *tpc_hit);

};

#endif

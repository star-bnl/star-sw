// $Id: StTrsMaker.cxx,v 1.94 2018/12/07 17:59:39 genevb Exp $
//

// $Log: StTrsMaker.cxx,v $
// Revision 1.94  2018/12/07 17:59:39  genevb
// Avoid using 0 as random seed when intent is to fix seed
//
// Revision 1.93  2018/06/21 22:23:08  perev
// TpcGroup fixes
//
// Revision 1.92  2018/02/20 22:45:53  smirnovd
// Revert "Changes from Irakli's directory to make the code compile"
//
// Revision 1.90  2014/12/16 04:09:08  perev
// In Make() SetSeed() for StTrsRand called. Input seed tkane from g2t_event::ge_rndm[2]
// It is made for reproducion event after skip previous ones.
// Simulation of event is independent now.
//
// Revision 1.89  2013/03/26 15:56:00  genevb
// Replace agufld(x,b) with direct call to StarMagField::Instance()->BField(x,b)
//
// Revision 1.88  2013/02/18 16:31:13  fisyak
// gufld => agufld
//
// Revision 1.87  2011/01/18 14:40:15  fisyak
// Clean up TpcDb interfaces and Tpc coordinate transformation
//
// Revision 1.86  2010/01/27 21:33:08  perev
// Account Prompt hits
//
// Revision 1.85  2009/11/03 14:34:19  fisyak
// Remove default in zFromTB
//
// Revision 1.84  2009/07/28 14:40:46  fisyak
// Comment out cut on TPC fiducial volume
//
// Revision 1.83  2008/10/13 19:56:09  fisyak
// Account that Z-offset is sector dependent
//
// Revision 1.82  2008/06/20 15:00:57  fisyak
// move from StTrsData to StTpcRawData
//
// Revision 1.81  2007/07/12 20:25:04  fisyak
// Use StarLogger, use time of flight, fix cluster shape
//
// Revision 1.80  2007/04/28 17:57:27  perev
// Redundant StChain.h removed
//
// Revision 1.79  2005/12/12 21:00:11  perev
// 3 random generators ==> 1
//
// Revision 1.78  2005/09/09 22:12:48  perev
// Bug fix + IdTruth added
//
// Revision 1.76  2005/07/19 22:20:52  perev
// Bug fix
//
// Revision 1.75  2004/03/31 19:45:57  jeromel
// Initialize data member (valgrind report)
//
// Revision 1.74  2003/12/24 13:44:42  fisyak
// Add (GEANT) track Id information in Trs; propagate it via St_tpcdaq_Maker; account interface change in StTrsZeroSuppressedReaded in StMixerMaker
//
// Revision 1.73  2003/09/02 17:59:14  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.72  2003/05/02 23:54:18  hardtke
// Allow user to adjust mNormalFactor (i.e. Fudge Factor)
//
// Revision 1.71  2003/04/30 20:38:56  perev
// Warnings cleanup. Modified lines marked VP
//
// Revision 1.70  2003/04/10 21:30:34  hardtke
// only call Init Run once per job
//
// Revision 1.69  2002/02/05 22:21:27  hardtke
// Move Init code to InitRun
//
// Revision 1.68  2001/11/21 01:49:25  long
// add log message for 3/2001 long;
// adding in Make():
// < //update the gain for very events
// <    mWireHistogram->setGasGainInnerSector(mSlowControlDb->innerSectorGasGain());
// <    mWireHistogram->setGasGainOuterSector(mSlowControlDb->outerSectorGasGain());
//
// Revision 1.67  2001/11/14 22:24:32  jeromel
// mAllTheData deleted in maker. Was in maker AND in Event (destroyed
// twice) causing a crash.
//
// Revision 1.66  2001/03/20 01:44:24  perev
// drift velocity print added
//
// Revision 1.65  2001/03/13 22:09:19  long
// *** empty log message ***
//
// Revision 1.64  2001/02/15 21:34:45  perev
// clear improved
//
// Revision 1.63  2000/08/11 02:24:27  long
// comment out sampleAnalogSignal();
//
// Revision 1.62  2000/08/05 18:32:58  long
// add check for the No.of  input hits. if(no_tpc_hits<1)return kStOK;
//
// Revision 1.61  2000/08/04 21:03:57  perev
// Leaks + Clear() cleanup
//
// Revision 1.60  2000/08/04 03:33:45  long
// mMiniSegmentLength(3.*millimeter)--->mMiniSegmentLength(4.*millimeter)
//
// Revision 1.59  2000/07/30 02:36:35  long
// add dx(d[0]),dy(d[1]),dz calculation instead of just ds
//
// Revision 1.58  2000/07/21 22:31:11  calderon
// Added checks at the beginning of Make() to avoid
// seg faults when dereferencing an invalid pointer.  This
// happens when no g2t tables are present.  For the case of
// embedding this might happen for the very low multiplicity
// events where none of the embedded monte carlo tracks reach
// a sensitive volume.
//
// Revision 1.57  2000/06/25 23:58:28  fisyak
// Remove params
//
// Revision 1.56  2000/03/24 02:43:46  long
// comment out  "hitMomentum.setZ(-(tpc_hit->p[2]*GeV)); when bsectorOfHit>12"
//
// Revision 1.55  2000/03/15 23:33:30  calderon
// Modified Finish() to properly take care of pointers when
// using the mixer chain.
//
// Revision 1.54  2000/02/24 16:30:41  long
// 1) modified  for field on case
// //2) modified for pileup pp events  ---Balewski
//
// Revision 1.54  2000/02/23 01:34:06  long
//1) modified  for field on case
//2) modified for pileup pp events  ---Balewski
// Revision 1.53  2000/02/10 01:21:27  calderon
// Switch to use StTpcDb.
// Coordinates checked for consistency.
// Fixed problems with StTrsIstream & StTrsOstream.
//
// Revision 1.52  2000/01/27 22:56:58  calderon
// add Magnetic field Db instantiation when using StTpcDb.  The
// Magnetic Field is still obtained through gufld, so the call
// is the same as for the StSimpleMagneticField case.  StTpcDb
// is still not used until further tests.
//
// Revision 1.51  2000/01/10 23:11:07  lasiuk
// Include MACROS for compatibility with SUN CC5.0
//
// Revision 1.50  1999/11/12 01:34:06  long
// 1)fix a bug for track with z<0
// 2) turn on the switch on pseudo pad row and set the signal threshold to
//    0 for the time being
//
// Revision 1.49  1999/11/11 19:42:24  calderon
// Add #ifdef HISTOGRAM for Ntuple Diagnostics.
// Use ROOT_DATABASE_PARAMETERS.  As soon as Jeff and Dave give Ok,
// we will switch to TPC_DATABASE_PARAMETERS.
//
// Revision 1.48  1999/11/10 15:45:39  calderon
// Made changes to reduce timing, including:
// Made coordinate transfrom a data member of StTrsAnalogSignalGenerator
// Added upper-lower bound instead of symmetric cut.
// Revived checking if signal is above threshold.
//
// Revision 1.47  1999/11/05 22:10:13  calderon
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
// Revision 1.46  1999/10/21 23:50:32  calderon
// Changes are
// -string for gasDb
// -modified mDeltaRow to 0 if using SlowAnalogSignalGenerator
//
// Revision 1.45  1999/10/13 17:36:53  calderon
// Fixed path to find SimpleDb files.
//
// Revision 1.44  1999/10/12 22:51:17  long
// fix a bug in switching from sector to sector
//
// Revision 1.43  1999/10/11 23:54:31  calderon
// Version with Database Access and persistent file.
// Not fully tested due to problems with cons, it
// doesn't find the local files at compile time.
// Yuri suggests forcing commit to work directly with
// files in repository.
//
// Revision 1.42  1999/10/04 16:13:14  long
// minor change on how to loop over geant hits
//
//
// Revision 1.40  1999/09/24 01:23:29  fisyak
// Reduced Include Path
//
// Revision 1.39  1999/07/20 02:16:58  lasiuk
// bring in line with new options (TSS algorithms)
//
// Revision 1.38  1999/07/15 13:57:30  perev
// cleanup
//
// Revision 1.37  1999/07/09 03:45:50  lasiuk
// set switch for the wireHistogram to determine the use
// of a Gaussian or exponential gas gain based on a collection
// or single electrons
//
// Revision 1.36  1999/06/17 19:04:40  lasiuk
// Rotate the momentum the same way that the position is rotated
//
// Revision 1.35  1999/06/16 14:26:49  fisyak
// Add flags for egcs on Solaris
//
// Revision 1.34  1999/05/28 02:55:44  lasiuk
// change default settings for testing.  Only in the Maker!
// Remove histograms
//
// Revision 1.33  1999/04/29 00:15:10  lasiuk
// make sure to clean up pointers that are store
// in the StTrsRawDataEvent() because they are allocated
// for each event.  This is done with StTrsRawDataEvent::clear()
// which has been added.
//
// Revision 1.32  1999/04/27 19:38:37  lasiuk
// Bfield units in kG from STAR database
//
// Revision 1.31  1999/04/23 19:18:08  lasiuk
// change magnetic field initialization to use gufld from GEANT
//
// Revision 1.30  1999/04/07 01:02:41  lasiuk
// use gas gain from db
//
// Revision 1.29  1999/03/28 02:59:11  perev
// Put interfaces to .const area
//
// Revision 1.28  1999/03/24 22:16:24  lasiuk
// ROOT deletes the dataSet so you have to make
// a new one each time thru Make().  Don't check the
// pointers!
//
// Revision 1.27  1999/03/23 03:37:26  lasiuk
// incorporate ROOT dataSets for DB initialization
// move construction and destruction of "mAllthedata"
//
// Revision 1.26  1999/03/20 20:07:56  fisyak
// Add access to DataSet with parameters
//
// Revision 1.25  1999/03/20 03:23:57  lasiuk
// setMiniSegmentLength()
// setFirstSectorToProcess()
// setLastSectorToProcess()
//
// Revision 1.24  1999/03/19 13:27:11  lasiuk
// change sectors to process 1 ONLY!
//
// Revision 1.23  1999/03/17 17:11:12  lasiuk
// comment out data set deletion for SL99d
//
// Revision 1.22  1999/03/16 02:03:46  lasiuk
// Add Finish(); correct breakNumber calculation; Use C++ casts;
// Change defaults (again) including P10;
// New mechanism for selecting which sectors to process;
// Add flag for processing pseudo-pad rows
//
// Revision 1.21  1999/03/15 02:52:26  perev
// new Maker schema
//
// Revision 1.20  1999/03/02 17:50:39  lasiuk
// for testing/defaults/geantPID
//
// Revision 1.19  1999/02/24 16:59:24  lasiuk
// turn off histos
//
// Revision 1.18  1999/02/24 16:57:02  lasiuk
// x --> -x for sectors>12
//
// Revision 1.17  1999/02/23 19:15:05  lasiuk
// Change in the rotation angle for GEANT global coordinates
//
// Revision 1.16  1999/02/23 01:10:42  lasiuk
// 1st production version
//
// Revision 1.15  1999/02/17 17:02:16  lasiuk
// streamline for production
// remove debug.
// switch for #sectors to be processed
//
// Revision 1.14  1999/02/16 18:15:40  fisyak
// Check in the latest updates to fix them
//
// Revision 1.13  1999/02/15 03:32:09  lasiuk
// coordinate system for input data is global
// deltapad(1)
//
// Revision 1.12  1999/02/12 01:26:51  lasiuk
// Limit debug output
//
// Revision 1.11  1999/02/10 18:01:31  lasiuk
// remove debug/sleep
// ROOT passing
// set defaults
//
// Revision 1.10  1999/02/10 04:30:02  lasiuk
// add unpacker and rawevent as data members/ passed by dataset
//
// Revision 1.9  1999/02/05 23:08:34  fisyak
// Add Valery's update of DataSet
//
// Revision 1.7  1999/01/28 02:46:09  lasiuk
// SUN compile with new GEANT interface
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StTrsMaker class for Makers                                          //
//                                                                      //
//
// You must select a data base initializer method
// When using TPC_DATABASE, change also definition in
// StTrsChargeSegment.cc
// StTrsAnalogSignalGenerator.hh
//////////////////////////////////////////////////////////////////////////

#include "StTrsMaker.h"
#include "TDataSetIter.h"
#include "TObjectSet.h"

#define ST_TRS_RANDOM_SRC
#include "StTrsRandom.hh"

#include <Stiostream.h>
#include <unistd.h>    // needed for access()/sleep()
#include <math.h>
#include <string>
#include <algorithm>
#include <vector>
#include <list>
#include <utility>    // pair
#include <algorithm>  // min() max()
#include <ctime>      // time functions

#if defined (__SUNPRO_CC) && __SUNPRO_CC >= 0x500
using std::list;
using std::vector;
using std::string;
using std::min;
using std::max;
#endif

#include "StMem.h"
// SCL
#include "StGlobals.hh"
#include "Randomize.h"

#include "TNtuple.h"
#include "TFile.h"


// TRS
// DataBase Initialization
// Dave's Header file
#include "StTpcDb/StTpcDb.h"

#include "StTpcDbGeometry.hh"
#include "StTpcDbSlowControl.hh"
#include "StTpcDbElectronics.hh"
//#include "StDbMagneticField.hh" // To be done
#include "StarMagField.h"

#include "StSimpleMagneticField.hh"
#include "StTrsDeDx.hh"

#include "electronicsDataSet.h"
#include "geometryDataSet.h"
#include "slowcontrolDataSet.h"

// processes
#include "StTrsFastChargeTransporter.hh"
#include "StTrsSlowAnalogSignalGenerator.hh"
//***************************** SEE the default options for Analog Signal generator
#include "StTrsParameterizedAnalogSignalGenerator.hh"
//*****************************
#include "StTrsFastDigitalSignalGenerator.hh"
// #include "StTrsOldDigitalSignalGenerator.hh"

// containers
#include "StTrsChargeSegment.hh"
#include "StTrsMiniChargeSegment.hh"
#include "StTrsAnalogSignal.hh"
#include "StTrsWireBinEntry.hh"
#include "StTrsWireHistogram.hh"

#include "StTrsSector.hh"
#include "StTrsDigitalSector.hh"

// outPut Data--decoder
#include "StTrsDetectorReader.hh"
#include "StTrsZeroSuppressedReader.hh"
#include "StDaqLib/GENERIC/EventReader.hh"
#include "StSequence.hh"

// g2t tables
#include "tables/St_g2t_event_Table.h"
#include "tables/St_g2t_tpc_hit_Table.h"
#include "tables/St_g2t_track_Table.h"
#include "tables/St_g2t_vertex_Table.h" 
#include "StMessMgr.h"
#include "TString.h"
#define PILEUP_ON  (m_Mode )

//#define VERBOSE 1
//#define ivb if(VERBOSE)

static const char rcsid[] = "$Id: StTrsMaker.cxx,v 1.94 2018/12/07 17:59:39 genevb Exp $";

ClassImp(electronicsDataSet)
ClassImp(geometryDataSet)
ClassImp(slowcontrolDataSet)
ClassImp(StTrsMaker)




StTrsMaker::StTrsMaker(const char *name):StMaker(name)
{
   memset(mBeg,0,mEnd-mBeg+1); //Zero all

   mMiniSegmentLength			=(4.*millimeter);  // test trial,Hui Long
   mFirstSectorToProcess		=(1);
   mLastSectorToProcess			=(24); 
   mUseParameterizedSignalGenerator	=(1); // test trial,Hui Long
   mNormalFactor 			= 1.25;
}

StTrsMaker::~StTrsMaker() { /* nopt */ }


Int_t StTrsMaker::Init()
{
  mAllTheData = 0;
  int seed = IAttr("trsInitSeed");
  if (!seed) seed = 19460510;
  StTrsRandom::inst().SetSeed(seed);
  return StMaker::Init();
}

Int_t StTrsMaker::InitRun(int runnumber)
{
  if (mAllTheData) {gMessMgr->QAInfo()  << "StTrsMaker::InitRun Already called" << endm; return kStOK;}
    // The global pointer to the Db is gStTpcDb and it should be created in the macro.
    
    if (!gStTpcDb) {
	gMessMgr->QAInfo()  << "DATABASE MISSING!" << endm;
	PR(gStTpcDb);
	gMessMgr->QAInfo()  << "Can't initialize TRS" << endm;
	return kStFatal;
    }
    mGeometryDb =
     StTpcDbGeometry::instance(gStTpcDb);
    if (Debug()) mGeometryDb->print();

    // The print statements are done in Make() because the SlowControl DB is only available then.

  mElectronicsDb =
      StTpcDbElectronics::instance(gStTpcDb);
  if (Debug()) mElectronicsDb->print();

  mSlowControlDb =
       StTpcDbSlowControl::instance(gStTpcDb);
  if (Debug()) mSlowControlDb->print();

//   mMagneticFieldDb =
//       StTpcDbMagneticField::instance();  // default is .5T field in z direction
  
  
   float x[3] = {0,0,0};
   float B[3];
   StarMagField::Instance()->BField(x,B);
   StThreeVector<double> Bfield(B[0],B[1],B[2]);
   Bfield*=kilogauss;
   PR(Bfield/tesla);
   mMagneticFieldDb =
      StSimpleMagneticField::instance(Bfield);  // default is .5T field in z direction


   //
   // Select the gas: Ar, NeCO2, P10 available
   string gas = "P10";
   mGasDb = new StTrsDeDx(gas);
   if (Debug()) mGasDb->print();
   

   //
   // Containers
   //

   // A Wire Plane
   mWireHistogram =
       StTrsWireHistogram::instance(mGeometryDb, mSlowControlDb ,mGasDb,mMagneticFieldDb);
   mWireHistogram->setDoGasGain(true);
   mWireHistogram->setDoGasGainFluctuations(true); // used to be true
   //  mWireHistogram->setDoGasGainFluctuations(false);
   mWireHistogram->setDoSingleElectronMultiplication(false);
   mWireHistogram->setGasGainInnerSector(mSlowControlDb->innerSectorGasGain());
   mWireHistogram->setGasGainOuterSector(mSlowControlDb->outerSectorGasGain());
   mWireHistogram->setDoTimeDelay(false);
   //   mWireHistogram->setRangeOfWiresForChargeDistribution(0);
   //
   // An Analog Sector(for calculation)
   mSector = 
       new StTrsSector(mGeometryDb);

   //
   // Processes
   //
   mChargeTransporter =
       StTrsFastChargeTransporter::instance(mGeometryDb, mSlowControlDb, mGasDb, mMagneticFieldDb);
   // set status:
   mChargeTransporter->setChargeAttachment(false); // used to be true
   mChargeTransporter->setGatingGridTransparency(false);
   mChargeTransporter->setTransverseDiffusion(true);  // used to be true
   mChargeTransporter->setLongitudinalDiffusion(true); // used to be true
   mChargeTransporter->setExB(false);

   mAnalogSignalGenerator = 0;
   if(mUseParameterizedSignalGenerator) {
   //**************for the ParameterizedAnalogSignalGenerator
     mAnalogSignalGenerator = StTrsParameterizedAnalogSignalGenerator::instance(mGeometryDb, mSlowControlDb, mElectronicsDb, mSector);
     StTrsParameterizedAnalogSignalGenerator *myGen = dynamic_cast<StTrsParameterizedAnalogSignalGenerator*>(mAnalogSignalGenerator);

   // Any options that are needed can go here:
   //*******************************************************
     myGen->setDeltaPad(2);
     myGen->setSignalThreshold(.1*millivolt);
     myGen->setSuppressEmptyTimeBins(true);
     myGen->addNoise(true);
     myGen->generateNoiseUnderSignalOnly(true);
     myGen->setNormalFactor(mNormalFactor);
   }
   else {
     StTrsSlowAnalogSignalGenerator *myGen = dynamic_cast<StTrsSlowAnalogSignalGenerator*>
     (StTrsSlowAnalogSignalGenerator::instance(mGeometryDb, mSlowControlDb, mElectronicsDb, mSector));
     mAnalogSignalGenerator = myGen;
   //
   // Set the function for the induced charge on Pad
   // -->StTrsSlowAnalogSignalGenerator::endo
   //-->StTrsSlowAnalogSignalGenerator::gatti
   //-->StTrsSlowAnalogSignalGeneratommetricGaussianApproximation:
// 	    retr::dipole
     myGen->setChargeDistribution(StTrsSlowAnalogSignalGenerator::endo);
   //
   // Set the function for the Analog Electronics signal shape
   //-->StTrsSlowAnalogSignalGenerator::delta
   //-->StTrsSlowAnalogSignalGenerator::symmetricGaussianApproximation
   //-->StTrsSlowAnalogSignalGenerator::symmetricGaussianExact
   //-->asymmetricGaussianApproximation
   //-->StTrsSlowAnalogSignalGenerator::realShaper
     myGen->setElectronicSampler(StTrsSlowAnalogSignalGenerator::symmetricGaussianApproximation);
     //myGen->setDeltaRow(0);
     myGen->setDeltaPad(2);
     myGen->setSignalThreshold(.0*millivolt);
     myGen->setSuppressEmptyTimeBins(true);
     myGen->addNoise(true);
     myGen->generateNoiseUnderSignalOnly(true);
     myGen->setNoiseRMS(900);  // set in  #e
   }

   mDigitalSignalGenerator =
	    StTrsFastDigitalSignalGenerator::instance(mElectronicsDb, mSector);
   
   //
   // Output is into an StTpcRawDataEvent* vector<StTrsDigitalSector*>
   // which is accessible via the StTrsUnpacker.  Initialize the pointers!
   //DH  now in Init mAllTheData=0;

   //
   // Maker Initialization ---now given by default arguments in the constructor
   //    mFirstSectorToProcess = 1;
   //    mLastSectorToProcess  = 1;


   // This should really be a boolean...when ROOT can handle it, change it!
   mProcessPseudoPadRows = 1;  // 0 is no, 1 is yes!

   //
   // Construct constant data sets.  This is what is passed downstream
   if (!mAllTheData) {
    mAllTheData = new StTrsRawDataEvent(mGeometryDb->numberOfSectors());
    AddConst(new TObjectSet("Event"  , mAllTheData, 0));
   }
   if ((Debug()/10)%10 > 0) {
     mTrsNtupleFile          = new TFile("TrsOutput.root","RECREATE","Trs Ntuples");
     mWireNtuple             = new TNtuple("WireNtuple", "Wire Histogram Info.", "electrons:wire:sector:id");
     mContinuousAnalogNtuple = new TNtuple("CAnalogNtuple", "Cont. Analog Sector", "charge:time:pad:row:id");
     mDiscreteAnalogNtuple   = new TNtuple("DAnalogNtuple", "Disc. Analog Sector", "charge:timebin:pad:row:id");
     mDigitalNtuple          = new TNtuple("DigitalSignalNtuple", "Digital Sector", "adc:timebin:pad:row:id");
   }
   return kStOK;
}

//
// My Member Functions
//
void StTrsMaker::whichSector(int volId, int* isDet, int* sector, int* padrow){

    //gMessMgr->QAInfo()  << "StTrsMaker::whichSector()" << endm;
    *isDet  = (volId/100000)%10;

    volId  -= (*isDet)*100000;
    *sector = volId/100;

    volId  -= (*sector)*100;
    *padrow = volId;
	
}
Int_t StTrsMaker::Make(){
    
    //  PrintInfo();
        double d[3],absP[3];
    
    //Do not use this unless you really know what you are
    // doing...This is a histogram diagnostic to compare
    // the GEANT hits to those produced by TRS!
    time_t trsMakeBegin = time(0);
    gMessMgr->QAInfo()  << "\n -- Begin TRS Processing -- " << endm; 
    gMessMgr->QAInfo()  << "Started at: " << ctime(&trsMakeBegin);    
    gMessMgr->QAInfo()  << "========= TRS driftVelocity used = " 
			<< mSlowControlDb->driftVelocity(13) << "(East) " 
			<< mSlowControlDb->driftVelocity(1)  << "(West)" << endm;
    int seed = IAttr("trsMakeSeed");
    if (seed) StTrsRandom::inst().SetSeed(seed);
    gMessMgr->QAInfo()  << "========= TRS Seed  used = " <<  StTrsRandom::inst().GetSeed()<< endm;


    int currentSectorProcessed = mFirstSectorToProcess;
    if (Debug()) {
      gMessMgr->QAInfo()  << "Processing sectors "
	   << mFirstSectorToProcess
	   << "--"
	   << mLastSectorToProcess << endm;
      
      //
      gMessMgr->QAInfo()  << "make sure pointer are clean" << endm;
    } 
    mAllTheData->clear();
    //
    //
    //update the gain for very events  
   mWireHistogram->setGasGainInnerSector(mSlowControlDb->innerSectorGasGain());
   mWireHistogram->setGasGainOuterSector(mSlowControlDb->outerSectorGasGain());
    
    // Normal processing of TRS through GEANT   
    //gMessMgr->QAInfo()  << "Make ofstream" << endm;
    //ofstream ofs("/star/u2b/lasiuk/geantdebug.txt", ios::out);
    //ofstream raw("/star/u2b/lasiuk/event.txt",ios::out);
    //
    // Read the GEANT info
    // these structures/classes are defined in:
    // $STAR/pams/sim/idl/g2t_tpc_hit.idl 
    // $STAR/StRoot/base/TDataSet.h & St_Table.h 
    //
    TDataSetIter geant(GetDataSet("geant"));
    
    St_g2t_event *g2tevent = (St_g2t_event *)(geant("g2t_event"));
    if ( !g2tevent  )  return kStWarn;
    g2t_event_st *event = g2tevent->GetTable();
    seed = event->ge_rndm[0]^event->ge_rndm[1];
    if (seed) StTrsRandom::inst().SetSeed(seed);
    gMessMgr->QAInfo()  << "========= TRS Seed  used = " <<  StTrsRandom::inst().GetSeed()<< endm;

    
    St_g2t_tpc_hit *g2t_tpc_hit =
	static_cast<St_g2t_tpc_hit *>(geant("g2t_tpc_hit"));
    //PR(g2t_tpc_hit);

    if (!g2t_tpc_hit) return kStWarn;
    
    int no_tpc_hits         = g2t_tpc_hit->GetNRows();
    g2t_tpc_hit_st *tpcHit = g2t_tpc_hit->GetTable();

    St_g2t_track *g2t_track =
	static_cast<St_g2t_track *>(geant("g2t_track"));
    if (!g2t_track) return kStWarn;
    
    g2t_track_st *tpc_track = g2t_track->GetTable();
    
    St_g2t_vertex  *g2t_ver=static_cast<St_g2t_vertex *>(geant("g2t_vertex"));
    if (!g2t_ver) return kStWarn;
    
    g2t_vertex_st *gver=g2t_ver->GetTable();
    assert(gver); 
    if(PILEUP_ON) {
       gMessMgr->QAInfo() << Form("\n  TRS(): Pileup is ON (m_Mode=%d)\n",m_Mode) << endm; 
    }
    else {
       gMessMgr->QAInfo() << Form("\n  TRS(): Pileup is OFF (m_Mode=%d)\n",m_Mode) << endm; 
    }
    //int geantPID = tpc_track->ge_pid;
    //PR(geantPID);

    // inRange should be a boolean
    bool inRange,start=true;
    int  bisdet, bsectorOfHit, bpadrow;
    int  numberOfProcessedPointsInCurrentSector = 0;
    // Limit the  processing to a fixed number of segments
    //  no_tpc_hits = 4;
    if(no_tpc_hits<1)return kStOK;
    g2t_tpc_hit_st *tpc_hit = tpcHit;
    for (int i=1; i<=no_tpc_hits; i++,tpc_hit++){
      int id2=tpc_hit->track_p;
      int id3=tpc_track[id2-1].start_vertex_p; //  "-1" is (Fortran-->C++)
      whichSector(tpc_hit->volume_id, &bisdet, &bsectorOfHit, &bpadrow);
      float BunchZoffset=(gver[id3-1].ge_tof+tpc_hit->tof)* mSlowControlDb->driftVelocity(bsectorOfHit);
      mChargeTransporter->setDriftVelocity(mSlowControlDb->driftVelocity(bsectorOfHit));
#if 0      
      float absHitZ=fabs(tpc_hit->x[2]);
	      if(PILEUP_ON)
            {
	    if(absHitZ - tpc_hit->ds + BunchZoffset<0) continue;//crossed central membrane
	    if(absHitZ + tpc_hit->ds + BunchZoffset> mGeometryDb->frischGrid()) 
	   continue;//out of TPC
	     }    //  for piled up events
#endif
// 		gMessMgr->QAInfo()  << "--> tpc_hit:  " << i << endm;
// 		raw << tpc_hit->volume_id   << ' '
// 		    << tpc_hit->de          << ' '
// 		    << tpc_hit->ds          << ' '
// 		    << tpc_hit->x[0]        << ' '
// 		    << tpc_hit->x[1]        << ' '
// 		    << tpc_hit->x[2]        << ' '
// 		    << tpc_hit->p[0]        << ' '
// 		    << tpc_hit->p[1]        << ' '
// 		    << tpc_hit->p[2]        << ' '  << endm;
        
// 	PR(bsectorOfHit);
// 	gMessMgr->QAInfo() << mFirstSectorToProcess<<" "<< mLastSectorToProcess<<endm;
	if(bsectorOfHit >= mFirstSectorToProcess &&
	   bsectorOfHit <= mLastSectorToProcess)
	    inRange = true;
	else
	    inRange = false;

	// Save time initially  - by not processing pseudo padrows
	if(bisdet && !mProcessPseudoPadRows) {
	  //   gMessMgr->QAInfo()  << "Segment in a pseudo-padRow. Skipping..." << endm;
	    //  tpc_hit++;
	     if(i != no_tpc_hits) continue;
             inRange=false;
	}
	//sleep(2);

	//
	// If not in range AND there are no points processed, skip to next point
	if(!inRange && !numberOfProcessedPointsInCurrentSector) {
	    //gMessMgr->QAInfo()  << "out of range and no points" << endm;
	    //  tpc_hit++;
	    continue;
	}
	if((inRange)  &&                                 //HL
	   (bsectorOfHit != currentSectorProcessed&&start==false) &&//HL
	   (i            <= no_tpc_hits           )) {//HL
      
          
          tpc_hit--;
          i--;
	}  //HL    ,continue to another sector....
	   else	if((inRange)  &&
	 (bsectorOfHit == currentSectorProcessed||start==true) &&
	(i            <= no_tpc_hits           )) {
                  currentSectorProcessed=bsectorOfHit;//HL
		  start=false;//HL,9/9/99
	    // CAREFUL:
	    // GEANT uses: (which is not correct!)
	    //double GEANTDriftLength = 208.55119*centimeter;
	    //double GEANTOffSet      = mGeometryDb->frischGrid() - GEANTDriftLength;
	    
	    // Now relative to this, we get the zPosition in coordinates where :
	    // 0 is the membrane, 208+/-dz is the wire grid
	    //double zPosition =
	    //GEANTDriftLength/2. + tpc_hit->x[2]*centimeter + GEANTOffSet;
	    //--->double zPosition = tpc_hit->x[2]*centimeter;
	    //PR(tpc_hit->x[2]*centimeter);
	    //PR(zPosition);
	    
		  //	    StThreeVector<double> hitPosition(tpc_hit->x[0]*centimeter,
		  //	    				      tpc_hit->x[1]*centimeter,
		  //	    				      tpc_hit->x[2]*centimeter); 
// 	    PR(hitPosition);

// 	    // Drift Length is calculated with respect to the FG!
// 	    double fgOffSet = (bpadrow <= mGeometryDb->numberOfInnerRows()) ?
// 		mGeometryDb->innerSectorFrischGridPadPlaneSeparation() :
// 		mGeometryDb->innerSectorFrischGridPadPlaneSeparation();

	    // In GEANT Global Coordinates we have to rotate
	    // to the sector 12 position
	    // Not using StTpcCoordinateTransform
	    // because it is slower, so this requires that we make sure
	    // the code below follows exactly what is in StTpcCoordinateTransform
	    // It is also in StTrsChargeSegment::rotate()
	    double beta = (bsectorOfHit>12) ?
		-bsectorOfHit*M_PI/6. :
		bsectorOfHit*M_PI/6. ;   //(30 degrees)
	    double cb   = cos(beta);
	    double sb   = sin(beta);
	    //    double xp = hitPosition.x()*cb - hitPosition.y()*sb;
	    //   double yp = hitPosition.x()*sb + hitPosition.y()*cb;
                 double xp = tpc_hit->x[0]*cb -tpc_hit->x[1]*sb;
	         double yp = tpc_hit->x[0]*sb +tpc_hit->x[1]*cb;
		 
	    StThreeVector<double>
		sector12Coordinate(xp,yp,(tpc_hit->x[2]));

	   
	    // Must rotate the momentum as well,  BUT you should
	    // only incur this calculational penalty if you split
	    // the segment into more than 1 mini segement
	    double pxPrime = tpc_hit->p[0]*cb - tpc_hit->p[1]*sb;
	    double pyPrime = tpc_hit->p[0]*sb + tpc_hit->p[1]*cb;
	    absP[0]=fabs(pxPrime);
            absP[1]=fabs(pyPrime); 
            absP[2]=fabs(tpc_hit->p[2]); 
	    StThreeVector<double> hitMomentum(pxPrime*GeV,
					      pyPrime*GeV,
					      tpc_hit->p[2]*GeV);

	    //added by Hui Long ,8/24/99
	    //Modified by M. Calderon 2/5/00
	    
             if(bsectorOfHit>12) 
                   {
                    sector12Coordinate.setZ((tpc_hit->x[2])+mGeometryDb->driftDistance());
	       
		 
                   } 
             else
		  { 
		    sector12Coordinate.setX(-xp);
		    sector12Coordinate.setZ(-(tpc_hit->x[2])+mGeometryDb->driftDistance());
                    hitMomentum.setX(-pxPrime*GeV);
		    hitMomentum.setZ(-(tpc_hit->p[2]*GeV));
                  }
                    //

// 	    PR(tpc_hit->p[0]*GeV);
// 	    PR(pxPrime*GeV);
// 	    PR(hitMomentum);

	    
	    // I need PID info here, for the ionization splitting (beta gamma)!
	   // int geantPID = tpc_track[tpc_hit->track_p].ge_pid;
             int geantPID = tpc_track[tpc_hit->track_p-1].ge_pid;
	   
	    // WARNING:  cannot use "abs" (not overloaded (double) for LINUX!
	     StTrsChargeSegment aSegment(sector12Coordinate,
					 hitMomentum,id2,
					(fabs(tpc_hit->de*GeV)),
					tpc_hit->ds*centimeter,
					geantPID);
// 	    PR(hitPosition);
// 	    PR(sector12Coordinate);
// 	    PR(hitMomentum.mag());

// 	    ofs << " " << aSegment << endl;
//   	    PR(aSegment);
	    
	    
#ifndef ST_NO_TEMPLATE_DEF_ARGS
	    list<StTrsMiniChargeSegment> comp;
	    list<StTrsMiniChargeSegment>::iterator iter;
#else
	    list<StTrsMiniChargeSegment,allocator<StTrsMiniChargeSegment> > comp;
	    list<StTrsMiniChargeSegment,allocator<StTrsMiniChargeSegment> >::iterator iter;
#endif
	    //
	    // Break the segment for diffusion reproduction.
	    // Fast Simulation can use breakNumber = 1 
         double ptot=::sqrt(absP[0]*absP[0]+absP[1]*absP[1]+absP[2]*absP[2]);
                             
       	     d[0] =tpc_hit->ds*absP[0]/ptot;
             d[1] =tpc_hit->ds*absP[1]/ptot; 
             d[2] =tpc_hit->ds*absP[2]/ptot;// approximation 
	     //   int breakNumber = (int)max(d[0]/mMiniSegmentLength,d[2]/mMiniSegmentLength);
            int breakNumber = (int)max(aSegment.ds()/mMiniSegmentLength,1.);
	      if(breakNumber<1)   breakNumber = 1;
	     breakNumber = min(breakNumber,16);  // set limit
            int numberOfLevels =
	    static_cast<int>(::log(static_cast<double>(breakNumber))/M_LN2 + .999);
	    d[0]/=::pow(2.,numberOfLevels);
            d[1]/=::pow(2.,numberOfLevels); 
            d[2]/=::pow(2.,numberOfLevels);// approximation
            numberOfLevels++;  // take care of the zero!  in aSegment.tssSplit
// 	    PR(aSegment.ds()/millimeter);
// 	    PR(breakNumber); 
          
	    //
	 //    int breakNumber = (int)max(aSegment.ds()/mMiniSegmentLength,1.);
	 //   breakNumber = min(breakNumber,16);  // set limit
// 	    PR(aSegment.ds()/millimeter);
// 	    PR(breakNumber);

// 	    aSegment.split(mGasDb, mMagneticFieldDb, breakNumber, &comp);
           
	    //   aSegment.tssSplit(mGasDb, mMagneticFieldDb, breakNumber, &comp);
 	    aSegment.tssSplit(mGasDb, mMagneticFieldDb, numberOfLevels , &comp);
	    
#ifndef ST_NO_TEMPLATE_DEF_ARGS
// 	    copy(comp.begin(), comp.end(), ostream_iterator<StTrsMiniChargeSegment>(cout,"\n"));
#endif

	    double SigmaL,SigmaT;//HL,added for field on case,02/08/00
	    
	    // Loop over the miniSegments
	    for(iter = comp.begin();
		iter != comp.end();
		iter++) {
	            
		//
	        // TRANSPORT HERE
	        //
	      //	mChargeTransporter->transportToWire(*iter); 
               mChargeTransporter->transportToWire(*iter,SigmaL,SigmaT);//HL,08/02/00   for field on 
              
	            if(PILEUP_ON)
	      { 
	      StTrsMiniChargeSegment *seg1=&(*iter);
	      float Znew=seg1->position().z()-BunchZoffset;
	      seg1->position().setZ(Znew);
	        }

              
//   		PR(*iter);
		
		//
		// CHARGE COLLECTION AND AMPLIFICATION
	        //
		
		//#if  defined(__sun) && !defined(__GNUG__)   
// Bug in the sun iterators.  Must Explicitly dereference!
		//		StTrsWireBinEntry anEntry(iter->position(), iter->charge());
// 		PR(anEntry);
		//#else
              
		//  	StTrsWireBinEntry anEntry((*iter).position(), (*iter).charge());
               
		//#endif
	       //	mWireHistogram->addEntry(anEntry); 
             
               StTrsWireBinEntry anEntry((*iter).position(), (*iter).charge(),SigmaL,SigmaT,d,(*iter).id());//HL,for field on ,08/02/00
	        mWireHistogram->addEntry(anEntry,bsectorOfHit);	
	    } // Loop over the list of iterators

	    //  tpc_hit++;  // increase the pointer to the next hit
	    numberOfProcessedPointsInCurrentSector++;
	   
	   if(i<no_tpc_hits)continue;   // don't digitize, you still have data in the same sector to process
	} // if (currentSector == bsectorOfHit)
	// Otherwise, do the digitization...
	
	if (mWireNtuple) {
	if (mWireHistogram->minWire() >= 0) {
	float wireValues[4];
	// Loop over Wire Histogram
	for(int jj=mWireHistogram->minWire(); jj<=mWireHistogram->maxWire(); jj++) {
	    aTpcWire currentWire = mWireHistogram->getWire(jj);
	    aTpcWire::iterator iter;
	    for(iter  = currentWire.begin();
		iter != currentWire.end();
		iter++) {
		wireValues[0] = iter->numberOfElectrons();
		wireValues[1] = jj;
		wireValues[2] = currentSectorProcessed;
		wireValues[3] = iter->id();
		mWireNtuple->Fill(wireValues);
	    }
	}
	mWireNtuple->Write();
	}
	}
	if (Debug()) PR(currentSectorProcessed);
     
	//
	// Generate the ANALOG Signals on pads
	//
	time_t inducedChargeBegin = time(0);
	if (Debug()) {gMessMgr->QAInfo()  << "--->inducedChargeOnPad()..." << endm;}
	mAnalogSignalGenerator->inducedChargeOnPad(mWireHistogram,currentSectorProcessed);
	if (Debug()) {
	time_t inducedChargeEnd= time(0);
	double inducedChargeTime = difftime(inducedChargeEnd,inducedChargeBegin);
	gMessMgr->QAInfo()  << "Time to process induced Charge: " << inducedChargeTime << " sec\n" << endm;
	}
	if (mContinuousAnalogNtuple) {
	tpcTimeBins continuousAnalogTimeSequence;
	timeBinIterator timeSeqIter;
	float cAnalogValues[5];
	// Loop over Continuous Analog Sector
	for(int jrow=1; jrow<=mGeometryDb->numberOfRows(); jrow++) {
	    for (int jpad=1; jpad<=mGeometryDb->numberOfPadsAtRow(jrow); jpad++){
		continuousAnalogTimeSequence = mSector->timeBinsOfRowAndPad(jrow,jpad);
		if(!continuousAnalogTimeSequence.size()) continue; 
		for(timeSeqIter  = continuousAnalogTimeSequence.begin();
		    timeSeqIter != continuousAnalogTimeSequence.end();
		    timeSeqIter++) {
		    cAnalogValues[0] = timeSeqIter->amplitude();
		    cAnalogValues[1] = timeSeqIter->time();
		    cAnalogValues[2] = jpad;
		    cAnalogValues[3] = jrow;
		    cAnalogValues[4] = timeSeqIter->id();
		    mContinuousAnalogNtuple->Fill(cAnalogValues);
		}
	    }
	}
	mContinuousAnalogNtuple->Write();
	}

	time_t sampleAnalogSignalBegin = time(0);
	if (Debug()) {
	gMessMgr->QAInfo()  << "--->sampleAnalogSignal()..." << endm;
	//	mAnalogSignalGenerator->sampleAnalogSignal();
	time_t sampleAnalogSignalEnd= time(0);
	double sampleAnalogSignalTime = difftime(sampleAnalogSignalEnd,sampleAnalogSignalBegin);
	gMessMgr->QAInfo()  << "Time to sample Analog Signal: " << sampleAnalogSignalTime << " sec\n" << endm;
	}
	if (mDiscreteAnalogNtuple) {
	tpcTimeBins discreteAnalogTimeSequence;
	timeBinIterator timeBinIter;
	float dAnalogValues[5];
	// Loop over Discrete Analog Sector
	for(int drow=1; drow<=mGeometryDb->numberOfRows(); drow++) {
	    for (int dpad=1; dpad<=mGeometryDb->numberOfPadsAtRow(drow); dpad++){
		discreteAnalogTimeSequence = mSector->timeBinsOfRowAndPad(drow,dpad);
		if(!discreteAnalogTimeSequence.size()) continue; 
		for(timeBinIter  = discreteAnalogTimeSequence.begin();
		    timeBinIter != discreteAnalogTimeSequence.end();
		    timeBinIter++) {
		    dAnalogValues[0] = timeBinIter->amplitude();
		    dAnalogValues[1] = timeBinIter->time();
		    dAnalogValues[2] = dpad;
		    dAnalogValues[3] = drow;
		    dAnalogValues[4] = timeBinIter->id();
		    mDiscreteAnalogNtuple->Fill(dAnalogValues);
		}
	    }
	}
	mDiscreteAnalogNtuple->Write();
	}
	
	//
	// Digitize the Signals
	//
	// First make a sector where the data can go...
	StTrsDigitalSector* aDigitalSector =
	  new StTrsDigitalSector(20);
	aDigitalSector->setSector(currentSectorProcessed);
	//
	// Point to the object you want to fill
	//
	mDigitalSignalGenerator->fillSector(aDigitalSector);
	mDigitalSignalGenerator->SetSectorNo(currentSectorProcessed);
	//
	// ...and digitize it
	time_t digitizeSignalBegin = time(0);
	if (Debug()) {gMessMgr->QAInfo()  << "--->digitizeSignal()..." << endm;}
      	mDigitalSignalGenerator->digitizeSignal();
	if (Debug()) {
	gMessMgr->QAInfo() <<"--->digitizeSignal() Finished..." << endm;
	time_t digitizeSignalEnd= time(0);
	double digitizeSignalTime = difftime(digitizeSignalEnd,digitizeSignalBegin);
	gMessMgr->QAInfo()  << "Time to digitize Signal: " << digitizeSignalTime << " sec\n" << endm;
	}
	//
	// Fill it into the event structure...
	// and you better check the sector number!
	
	mAllTheData->setSector(currentSectorProcessed,aDigitalSector);
	// Clear and reset for next sector:
	mWireHistogram->clear();
	mSector->clear();
	if (Debug()) gMessMgr->QAInfo()  << endm;

	//
	// Go to the next sector --> should be identical to a simple increment
	currentSectorProcessed = bsectorOfHit;
        numberOfProcessedPointsInCurrentSector = 0;
// you can skip out here if you only want to process a single sector...
// 	if(currentSectorProcessed>3)
// 	    break;  // Finish here
        
	//
    } // loop over all segments: for(int i...
  
  // The access stuff:
    if (Debug() > 2) {
  //
  // Access the data with
  //   *mDetectorReader 

  string version = "TrsDRv1.0";
  StTrsDetectorReader mDetectorReader(mAllTheData, version);
    //
    // Loop around the sectors: (should be from db, or size of the structure!)
    //
  //  for(int isector=1; isector<=mGeometryDb->numberOfSectors(); isector++) {
  for(int isector=mFirstSectorToProcess; isector<=mLastSectorToProcess; isector++) {
      StTrsZeroSuppressedReader* zsr = mDetectorReader.getZeroSuppressedReader(isector);
      if(!zsr) continue; 
// 	PR(isector);
      // otherwise, let's decode it
      unsigned char* padList;
      if (mDigitalNtuple) {
      float digitalValues[5];
      for (int irow=1; irow<=mGeometryDb->numberOfRows();irow++) {
	  int numberOfPads = zsr->getPadList(irow, &padList);
	  // If there are no pads, go to the next row...
	  if(!numberOfPads) continue;
	  for(int ipad = 0; ipad<numberOfPads; ipad++) {
	      //PR(static_cast<int>(padList[ipad]));
	      int nseq;
	      
	      StSequence* listOfSequences;
	      //Sequence* listOfSequences;
	      UShort_t      ** listOfIds = 0;
	      zsr->getSequences(irow,
				static_cast<int>(padList[ipad]),
				&nseq,
				&listOfSequences, &listOfIds);
	      //PR(getSequencesStatus);
	      for(int kk=0; kk<nseq; kk++) {
		  for(int zz=0; zz<listOfSequences[kk].length; zz++) {
		    if (Debug()%10 > 2 && kk < Debug()) {
		    gMessMgr->QAInfo()  << "sector\t" << isector << "\trow\t" << irow 
		     << "\tpad\t" << ipad  << "\tseq\t" << kk 
		     << "\tz\t" << zz + (int)(listOfSequences[kk].startTimeBin)
		     << "\tADC\t" << (int)(*(listOfSequences[kk].firstAdc)) 
		     << "\tid\t" << (int)(*(listOfIds[kk])) 
		     << endm;
		    }
		    digitalValues[0] = (int)(*(listOfSequences[kk].firstAdc)) ;
		    digitalValues[1] = zz + (int)(listOfSequences[kk].startTimeBin);
		    digitalValues[2] = ipad;
		    digitalValues[3] = irow;
		    digitalValues[4] = (int)(*(listOfIds[kk]));
		    mDigitalNtuple->Fill(digitalValues);
		      
		    listOfSequences[kk].firstAdc++;
		    listOfIds[kk]++;
		  } // zz
	      } // Loop kk
	  } // loop over pads
	  //
	  // One would do the data manipulation here!
	  // Then deallocate the memory
	  // 	    dynamic_cast<StTrsZeroSuppressedReader*>(zsr);
	  //  	    zsr->clear();
      } // Loop over rows!
      mDigitalNtuple->Write();
      }
  } // Loop over sectors
    }
    
    //gMessMgr->QAInfo()  << "Got to the end of the maker" << endm;
    // CAUTION: ROOT is resposible for the memory at this point
    // ROOT deletes m_DataSet in the chain after every event.
    if (Debug()) {
    time_t trsMakeEnd = time(0);
    gMessMgr->QAInfo()  << "\nFinished at: " << ctime(&trsMakeEnd);
    double trsMakeTotal = difftime(trsMakeEnd,trsMakeBegin);
    gMessMgr->QAInfo()  << "Total StTrsMaker::Make() Processing Time: " << trsMakeTotal << " sec" << endm; 
    }
#if 0
    CheckTruth(no_tpc_hits, tpcHit);
#endif
    return kStOK;
}

// *****************************************************************
// Make sure the memory is deallocated!
//
void  StTrsMaker::Clear(const char *)
{
    if (mAllTheData)   mAllTheData   ->clear(); //This deletes all the StTpcDigitalSectors in the StTrsRawDataEvent
    if (mWireHistogram)mWireHistogram->clear();

    StMaker::Clear();
}
Int_t StTrsMaker::Finish()
{
    //Clean up all the pointers that were initialized in StTrsMaker::Init()
    // Don't delete pointers to databases, they're singletons.
//     if (mGeometryDb) delete mGeometryDb;
//     if (mSlowControlDb) delete mSlowControlDb;
//     if (mMagneticFieldDb) delete mMagneticFieldDb;
//     if (mElectronicsDb) delete mElectronicsDb;
//     if (mGasDb) delete mGasDb;

    StTrsWireHistogram::dropit();
    mWireHistogram = 0;
    if (mSector) 		delete mSector;
    mSector = 0;
    if (mAllTheData) 		delete mAllTheData;
    mAllTheData = 0;
    if (mChargeTransporter) 	delete mChargeTransporter;
    mChargeTransporter = 0;
    if (mAnalogSignalGenerator) delete mAnalogSignalGenerator;
    mAnalogSignalGenerator = 0;
    if (mDigitalSignalGenerator)delete mDigitalSignalGenerator;
    mDigitalSignalGenerator = 0;    
    return kStOK;
}

void StTrsMaker::setNormalFactor(double FudgeFactor) {
  mNormalFactor = FudgeFactor;
//VP  if (mUseParameterizedSignalGenerator&&mAnalogSignalGenerator) {
//VP    mAnalogSignalGenerator->setNormalFactor(mNormalFactor);
//VP  }
}
#include "StDbUtilities/StTpcCoordinateTransform.hh"
#include "StarClassLibrary/StSequence.hh"

void StTrsMaker::CheckTruth(int no_tpc_hits, g2t_tpc_hit_st *tpc_hit)
{
   StSequence *seq; int nSeq;
   UShort_t **ids;
   int sector, row,pad,tim;
   TObjectSet *trsEvent=(TObjectSet*)GetDataSet("Event"); 			if(!trsEvent)  	return;
   StTrsZeroSuppressedReader *mZsr=0;
   StTpcRawDataEvent   *mEvent=(StTpcRawDataEvent*)(trsEvent->GetObject());	if(!mEvent) 	return;
   StTrsDetectorReader mTdr(mEvent);
   StTpcCoordinateTransform transform(gStTpcDb);

  int noData=0,lowTruth=0;
  for (int ih=0;ih<no_tpc_hits;ih++) {
    UShort_t idtru = (UShort_t) tpc_hit[ih].track_p;
    StGlobalCoordinate global(tpc_hit[ih].x[0],tpc_hit[ih].x[1],tpc_hit[ih].x[2]);
    sector = (tpc_hit[ih].volume_id/100)%100;
    row    =  tpc_hit[ih].volume_id   %  100;
    StTpcPadCoordinate Pad;      transform(global,Pad,sector,row);
    sector = Pad.sector();
    pad = (Int_t) Pad.pad();
    row = Pad.row();
    tim = (Int_t) Pad.timeBucket();
    mZsr=mTdr.getZeroSuppressedReader(sector);
    if (!mZsr) {
       noData++;
//       Warning("CheckTruth","%d - No Data in %d.%d.%d",noData,sector,row,pad);   
       continue;
    }
    nSeq=0;
    mZsr->getSequences(row, pad, &nSeq, &seq, &ids);
    if (!nSeq) {
       noData++;
//       Warning("CheckTruth","%d - No Data in %d.%d.%d",noData,sector,row,pad);   
       continue;
    }
    
    int nAdc=0,nIds=0;
    
    for (int jSeq=0;jSeq<nSeq;jSeq++) {
      int nnAdc = seq[jSeq].length;
      int startTimeBin =  seq[jSeq].startTimeBin;
      if (tim < startTimeBin      ) continue;
      if (tim > startTimeBin+nnAdc) continue;
      for (int j=0;j<nnAdc;j++) {
        if(abs(startTimeBin+j-tim)>5) continue;
        nAdc++;
	if(ids[jSeq][j]==idtru) nIds++;
      }
    }
    if (!nAdc) nAdc=1;
    double pct = double(nIds)/nAdc*100; if (pct){};
    if (!nIds) {
       lowTruth++;
//       Warning("CheckTruth","%d - Low contrib %2.1f%% in %d.%d.%d",lowTruth,pct,sector,row,pad);   
    }
  }
  if (lowTruth) 
    Warning("CheckTruth","nHits=%d lowTruth=%d pct=%2.1f%%",
    no_tpc_hits,lowTruth,(lowTruth*100.)/no_tpc_hits);

}    

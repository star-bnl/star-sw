// $Id: StTrsMaker.cxx,v 1.7 1999/01/28 02:46:09 lasiuk Exp $
//
// $Log: StTrsMaker.cxx,v $
// Revision 1.7  1999/01/28 02:46:09  lasiuk
// SUN compile with new GEANT interface
//
// Revision 1.6  1999/01/23 18:47:21  fisyak
// Cleanup for SL98l
//
// Revision 1.5  1999/01/23 05:03:18  lasiuk
// ready for test
//
// Revision 1.4  1999/01/23 02:33:03  lasiuk
// sun compatible
//
// Revision 1.3  1999/01/22 23:37:50  lasiuk
// root does not eat dynamic_cast<>
//
// Revision 1.2  1999/01/22 22:05:08  fisyak
// Rename
//
// Revision 1.1  1999/01/22 21:31:57  lasiuk
// name change
//
// Revision 1.3  1999/01/22 08:45:14  lasiuk
// example
//
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StTrsMaker class for Makers                                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "StTrsMaker.h"
#include "StChain.h"
#include "St_DataSetIter.h"

#include <iostream.h>
#include <unistd.h>    // needed for access()

#include <string>
#include <vector>
#include <list>
#include <utility>    // pair
#include <algorithm>  // min() max()

// SCL
#include "StGlobals.hh"
#include "Randomize.h"

// General TRS
#include "StCoordinates.hh"
#include "StTpcCoordinateTransform.hh"

// TRS
// db
#include "StTpcSimpleGeometry.hh"
#include "StTpcSimpleSlowControl.hh"
#include "StTpcSimpleElectronics.hh"
#include "StSimpleMagneticField.hh"
#include "StTrsDeDx.hh"

// processes
#include "StTrsFastChargeTransporter.hh"
#include "StTrsSlowAnalogSignalGenerator.hh"
#include "StTrsFastDigitalSignalGenerator.hh"

// containers
#include "StTrsChargeSegment.hh"
#include "StTrsMiniChargeSegment.hh"
#include "StTrsAnalogSignal.hh"
#include "StTrsWireBinEntry.hh"
#include "StTrsWireHistogram.hh"

#include "StTrsSector.hh"
#include "StTrsDigitalSector.hh"

// g2t tables
#include "St_g2t_tpc_hit_Table.h"
#include "St_g2t_track_Table.h"

//#define VERBOSE 1
//#define ivb if(VERBOSE)

// #define sINGLE
// #ifdef SINGLE
// const int ROWS = 1;
// const int PADS[ROWS] = {1};
// const int SIGNALS    = 1;
// const int TBINS      = 10;
// #endif

static const char rcsid[] = "$Id: StTrsMaker.cxx,v 1.7 1999/01/28 02:46:09 lasiuk Exp $";

ClassImp(StTrsMaker)

StTrsMaker::StTrsMaker(const char *name, const char *title):StMaker(name,title)
{
   drawinit=kFALSE;
}

StTrsMaker::~StTrsMaker() { /* nopt */ }

Int_t StTrsMaker::Init()
{
//     // Create tables
//     St_DataSetIter       local(gStChain->DataSet("params"));

    //
    // Make the DataBase
    //
    // Check File access
    //
    cout << "*********************************************" << endl;
    cout << "Try Initialize" << endl;
    string geoFile("../run/TPCgeo.conf");
    if (access(geoFile.c_str(),R_OK)) {
	cerr << "ERROR:\n" << geoFile.c_str() << " cannot be opened" << endl;
	//shell(pwd);
	cerr << "Exitting..." << endl;
	exit(1);
    }

    string scFile = "../run/sc.conf";
    if (access(scFile.c_str(),R_OK)) {
     cerr << "ERROR:\n" << scFile.c_str() << " cannot be opened" << endl;
     cerr << "Exitting..." << endl;
     exit(1);
    }

    string electronicsFile = "../run/electronics.conf";
    if (access(electronicsFile.c_str(),R_OK)) {
	cerr << "ERROR:\n" << electronicsFile.c_str() << " cannot be opened" << endl;
	cerr << "Exitting..." << endl;
	exit(1);
    }

    string magFile = "../run/example.conf";         // contains B field
    if (access(magFile.c_str(),R_OK)) {
	cerr << "ERROR:\n" << magFile.c_str() << " cannot be opened" << endl;
	cerr << "Exitting..." << endl;
	exit(1);
    }

   //
   // The DataBases
   //
   mGeometryDb =
     StTpcSimpleGeometry::instance(geoFile.c_str());
   mGeometryDb->print();

   mSlowControlDb =
       StTpcSimpleSlowControl::instance(scFile.c_str());

   mMagneticFieldDb =
       StSimpleMagneticField::instance(magFile.c_str());

   mElectronicsDb =
       StTpcSimpleElectronics::instance(electronicsFile.c_str());

   string gas =("Ar");
   mGasDb = new StTrsDeDx(gas);
   mGasDb->print();

    //
    // Containers
    //

    // create a Sector:
    // Analog (for calculation)
    mSector = 
      new StTrsSector(mGeometryDb);

    // Digital (for output)
    mDigitalSector =
	new StTrsDigitalSector(mGeometryDb);

    mWireHistogram =
      StTrsWireHistogram::instance(mGeometryDb, mSlowControlDb);
    mWireHistogram->setDoGasGain(true);  // True by default
    mWireHistogram->setDoGasGainFluctuations(false);
    mWireHistogram->setDoTimeDelay(false);

    
    //
    // Processes
    //
    mChargeTransporter =
      StTrsFastChargeTransporter::instance(mGeometryDb, mSlowControlDb, mGasDb, mMagneticFieldDb);
    // set status:
    mChargeTransporter->setChargeAttachment(true);
    mChargeTransporter->setGatingGridTransparency(true);
    mChargeTransporter->setTransverseDiffusion(true);
    mChargeTransporter->setLongitudinalDiffusion(true);
    mChargeTransporter->setExB(true);


    mAnalogSignalGenerator =
	StTrsSlowAnalogSignalGenerator::instance(mGeometryDb, mSlowControlDb, mElectronicsDb, mSector);
    //
    // Set the function for the induced charge on Pad
    //
    //dynamic_cast<StTrsSlowAnalogSignalGenerator*>(mAnalogSignalGenerator)->
        //setChargeDistribution(StTrsSlowAnalogSignalGenerator::endo);
	//setChargeDistribution(StTrsSlowAnalogSignalGenerator::gatti);
	//setChargeDistribution(StTrsSlowAnalogSignalGenerator::dipole);
    //
    // Set the function for the Analog Electronics signal shape
    //
    //dynamic_cast<StTrsSlowAnalogSignalGenerator*>(mAnalogSignalGenerator)->
	//setElectronicSampler(StTrsSlowAnalogSignalGenerator::delta);
        //setElectronicSampler(StTrsSlowAnalogSignalGenerator::symmetricGaussianApproximation);
	//setElectronicSampler(StTrsSlowAnalogSignalGenerator::symmetricGaussianExact);
	//setElectronicSampler(StTrsSlowAnalogSignalGenerator::asymmetricGaussianApproximation);
	//setElectronicSampler(StTrsSlowAnalogSignalGenerator::realShaper); 
    mAnalogSignalGenerator->setDeltaRow(0);
    mAnalogSignalGenerator->setDeltaPad(0);
    mAnalogSignalGenerator->setSignalThreshold(.0001*(.001*volt));
    mAnalogSignalGenerator->setSuppressEmptyTimeBins(true);
	

    mDigitalSignalGenerator =
	StTrsFastDigitalSignalGenerator::instance(mElectronicsDb, mSector, mDigitalSector);


    return StMaker::Init();
}


Int_t StTrsMaker::Make(){
//  PrintInfo();
  if (!m_DataSet->GetList())  {//if DataSet is empty fill it
    //
    // Read the Ionization
    //
    St_DataSetIter geant(gStChain->DataSet("geant"));
    // $STAR/pams/sim/idl/g2t_tpc_hit.idl 

      St_g2t_tpc_hit *g2t_tpc_hit = (St_g2t_tpc_hit *) geant("g2t_tpc_hit");
      Int_t no_tpc_hits =  g2t_tpc_hit->GetNRows(); // $STAR/StRoot/base/St_DataSet.h & St_Table.h 
      g2t_tpc_hit_st *tpc_hit =  g2t_tpc_hit->GetTable();

      StTpcCoordinateTransform transformer(mGeometryDb, mSlowControlDb);
      //for (int i=0; i< no_tpc_hits; i++, tpc_hit++){
      cout << "Loop over: " << no_tpc_hits << endl;
      for (int i=0; i< 1; i++, tpc_hit++){
	cout << tpc_hit->id                << ' '
	     << (tpc_hit->de/eV)           << ' '
	     << (tpc_hit->ds/centimeter)   << ' '
	     << tpc_hit->volume_id         << endl;

	int pID = 1;  // I need to know the PID for ionization calculations
	
	StThreeVector<double> hitPosition(tpc_hit->x[0]*centimeter,
					  tpc_hit->x[1]*centimeter,
					  tpc_hit->x[2]*centimeter); 
	PR(hitPosition);
	
	int decodedNumber = tpc_hit->volume_id;
	cout << decodedNumber << endl;
	int isdet = (decodedNumber/100000);
	decodedNumber -= isdet*100000;
	int sectorOfHit = decodedNumber/100;
	decodedNumber -= sectorOfHit*100;
	int padrow = decodedNumber;
	
	PR(isdet);
	PR(sectorOfHit);
	PR(padrow);

	double fgOffSet = (padrow <= mGeometryDb->numberOfInnerRows()) ?
	    mGeometryDb->innerSectorFrischGridPadPlaneSeparation() :
	    mGeometryDb->innerSectorFrischGridPadPlaneSeparation();

	StThreeVector<double>
	    sector12Coordinate(tpc_hit->x[0]*centimeter,
			       tpc_hit->x[1]*centimeter + mGeometryDb->radialDistanceAtRow(padrow),
			       tpc_hit->x[2]*centimeter + mGeometryDb->frischGrid() + fgOffSet);

	StThreeVector<double> hitMomentum(tpc_hit->p[0]*GeV,
					  tpc_hit->p[1]*GeV,
					  tpc_hit->p[2]*GeV);

	StTrsChargeSegment aSegment(sector12Coordinate,
				    hitMomentum,
				    tpc_hit->de*GeV,
				    tpc_hit->ds*centimeter,
				    pID);

	PR(hitMomentum.mag());

	PR(aSegment);
	sleep(2);
	cout << "*****************" << '\n' << endl;
	
#ifndef ST_NO_TEMPLATE_DEF_ARGS
	vector<int> all[3];
#else
	vector<int,allocator<int> > all[3];
#endif

#ifndef ST_NO_TEMPLATE_DEF_ARGS
	list<StTrsMiniChargeSegment> comp;
	list<StTrsMiniChargeSegment>::iterator iter;
#else
	list<StTrsMiniChargeSegment,allocator<StTrsMiniChargeSegment> > comp;
	list<StTrsMiniChargeSegment,allocator<StTrsMiniChargeSegment> >::iterator iter;
#endif
	int breakNumber = 1;
	cout << "StTrsSegment::split() into " << breakNumber << " segments." << endl;
	aSegment.split(mGasDb, mMagneticFieldDb, breakNumber, &comp);

#ifndef ST_NO_TEMPLATE_DEF_ARGS
	copy(comp.begin(), comp.end(), ostream_iterator<StTrsMiniChargeSegment>(cout,"\n"));
#endif
	cout << endl;
    
	cout << "Number of \"miniSegments\": " << (comp.size()) << endl;

    // Loop over the miniSegments
	for(iter = comp.begin();
	iter != comp.end();
	    iter++) {

	    cout << endl;
	    cout << " *iter " << (*iter) << endl;
	    PR(*iter);
	//
	// TRANSPORT HERE
	//
	    mChargeTransporter->transportToWire(*iter);
	    PR(*iter);

	//
	// CHARGE COLLECTION AND AMPLIFICATION
	//

#ifndef __sun
	    StTrsWireBinEntry anEntry(iter->position(), iter->charge());
	    PR(anEntry);
#else
	    StTrsWireBinEntry anEntry((*iter).position(), (*iter).charge());
#endif
	    mWireHistogram->addEntry(anEntry);
	
	} // Loop over the list of iterators

	
      } // Loop over the g2t_tpc_hits in the chamber
      
      cout << "\a***************************\a\n" << endl;

    //
    // Generate the ANALOG Signals on pads
    //
      mAnalogSignalGenerator->inducedChargeOnPad(mWireHistogram);

      mAnalogSignalGenerator->sampleAnalogSignal();

    //
    // Digitize the Signals
    //
      mDigitalSignalGenerator->digitizeSignal();


    //
    // Write it out!
    //
    
  }
  return kStOK;
}

void StTrsMaker::PrintInfo(){
  printf("**************************************************************\n");
  printf("* $Id: StTrsMaker.cxx,v 1.7 1999/01/28 02:46:09 lasiuk Exp $\n");
//  printf("* %s    *\n",m_VersionCVS);
  printf("**************************************************************\n");
  if (gStChain->Debug()) StMaker::PrintInfo();
}








// $Id: StTrsMaker.cxx,v 1.3 1999/01/22 23:37:50 lasiuk Exp $
//
// $Log: StTrsMaker.cxx,v $
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
//#include "St_g2t_tpc_hit_Table.h"
//#include "St_g2t_track_Table.h"

//#define VERBOSE 1
//#define ivb if(VERBOSE)

// #define sINGLE
// #ifdef SINGLE
// const int ROWS = 1;
// const int PADS[ROWS] = {1};
// const int SIGNALS    = 1;
// const int TBINS      = 10;
// #endif


ClassImp(StTrsMaker)

StTrsMaker::StTrsMaker(const char *name, const char *title):StMaker(name,title)
{
   drawinit=kFALSE;
}

StTrsMaker::~StTrsMaker() { /* nopt */ }

Int_t StTrsMaker::Init()
{
    // Create tables
    St_DataSetIter       local(gStChain->DataSet("params"));

    //
    // Make the DataBase
    //
    // Check File access
    //
    string geoFile("../run/TPCgeo.conf");
    if (access(geoFile.c_str(),R_OK)) {
	cerr << "ERROR:\n" << geoFile << " cannot be opened" << endl;
	//shell(pwd);
	cerr << "Exitting..." << endl;
	exit(1);
    }
    
    string scFile("../run/sc.conf");         // contains B field
    if (access(scFile.c_str(),R_OK)) {
	cerr << "ERROR:\n" << scFile << " cannot be opened" << endl;
     cerr << "Exitting..." << endl;
     exit(1);
    }
    
    string electronicsFile("../run/electronics.conf");
    if (access(electronicsFile.c_str(),R_OK)) {
	cerr << "ERROR:\n" << electronicsFile << " cannot be opened" << endl;
	cerr << "Exitting..." << endl;
	exit(1);
    }
    
    string magFile("../run/example.conf");         // contains B field
    if (access(magFile.c_str(),R_OK)) {
	cerr << "ERROR:\n" << magFile << " cannot be opened" << endl;
	cerr << "Exitting..." << endl;
	exit(1);
    }

   //
   // The DataBases
   //
   mGeometryDb =
     StTpcSimpleGeometry::instance(geoFile.c_str());

    mSlowControlDb =
	StTpcSimpleSlowControl::instance(scFile.c_str());

    mMagneticFieldDb =
	StSimpleMagneticField::instance(magFile.c_str());

    mElectronicsDb =
	StTpcSimpleElectronics::instance(electronicsFile.c_str());

    
    string gas("Ar");
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
//     mWireHistogram->setDoGasGain(true);  // True by default
//     mWireHistogram->setDoGasGainFluctuations(false);
//     mWireHistogram->setDoTimeDelay(false);

    
    //
    // Processes
    //
    mChargeTransporter =
      StTrsFastChargeTransporter::instance(mGeometryDb, mSlowControlDb, mGasDb, mMagneticFieldDb);
    // set status:
//     mChargeTransporter->setChargeAttachment(true);
//     mChargeTransporter->setGatingGridTransparency(true);
//     mChargeTransporter->setTransverseDiffusion(true);
//     mChargeTransporter->setLongitudinalDiffusion(true);
//     mChargeTransporter->setExB(true);


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
//     mAnalogSignalGenerator->setDeltaRow(0);
//     mAnalogSignalGenerator->setDeltaPad(0);
//     mAnalogSignalGenerator->setSignalThreshold(.0001*(.001*volt));
//     mAnalogSignalGenerator->setSuppressEmptyTimeBins(true);
	

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

//      St_g2t_tpc_hit *g2t_tpc_hit = (St_g2t_tpc_hit *) geant("g2t_tpc_hit");
//      Int_t no_tpc_hits =  g2t_tpc_hit->GetNRows(); // $STAR/StRoot/base/St_DataSet.h & St_Table.h 
//      g2t_tpc_hit_st *tpc_hit =  g2t_tpc_hit->GetTable();

//     for (int i=0; i< no_tpc_hits; i++){
// 	printf("id =%f  de%f  tof=%f \n",tpc_hit->id,tpc_hit->de,tpc_hit->tof);
// 	tpc_hit++;
//     }
//     St_g2t_track   *g2t_track   = (St_g2t_track *)   geant("g2t_track");
    
    // Read a charge Segment (g2t) from GEANT:
    double bg = 3; // minimum ionizing particle
    
    // Energy deposited per centimeter:
    float dE = 2.444*keV;  // deposited per cm of Ar
    float dS = 1.*centimeter;
    vector<int> all[3];

    StThreeVector<double> position(0.,1500.*millimeter,200.*millimeter);
    StThreeVector<double> momentum(1.*GeV,0.,0.);

    StTrsChargeSegment aSegment(position,
				momentum,
				dE,
				dS);

    PR(aSegment);
    list<StTrsMiniChargeSegment> comp;
    list<StTrsMiniChargeSegment>::iterator iter;

    int breakNumber = 1;
    aSegment.split(mGasDb, mMagneticFieldDb, breakNumber, &comp);

    copy(comp.begin(), comp.end(), ostream_iterator<StTrsMiniChargeSegment>(cout,"\n"));
    cout << endl;
    
    cout << "comp.size() " << (comp.size()) << endl;

    // Loop over the miniSegments
    for(iter = comp.begin();
	iter != comp.end();
	iter++) {

	cout << endl;
	cout << "*iter " << (*iter) << endl;
	
	//
	// TRANSPORT HERE
	//
	mChargeTransporter->transportToWire(*iter);
	PR(*iter);

	//
	// CHARGE COLLECTION AND AMPLIFICATION
	//
	StTrsWireBinEntry anEntry(iter->position(), iter->charge());
 	PR(anEntry);
	mWireHistogram->addEntry(anEntry);
	
    } // Loop over the list of iterators

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
  printf("* $Id: StTrsMaker.cxx,v 1.3 1999/01/22 23:37:50 lasiuk Exp $\n");
//  printf("* %s    *\n",m_VersionCVS);
  printf("**************************************************************\n");
  if (gStChain->Debug()) StMaker::PrintInfo();
}


//*******************************************************/
// $Id: trsExample.cc,v 1.4 2003/09/02 17:59:15 perev Exp $
//
// Author: brian, October, 1998
//
// Purpose: Prototypes the operation of TRS and provides diagnostic
//          at several levels
//
// :: To produce a histogram file, define DIAGNOSTICS
#define DIAGNOSTICS
//
// $Log: trsExample.cc,v $
// Revision 1.4  2003/09/02 17:59:15  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.3  1999/01/22 23:36:47  lasiuk
// macro TRUE
//
// Revision 1.2  1999/01/18 20:59:06  lasiuk
// function selection
//
// Revision 1.1  1999/01/18 10:54:48  lasiuk
// Initial revision
/********************************************************/
#include <Stiostream.h>
#include <unistd.h>    // needed for access()
#include "Stiostream.h"

#include <string>
#include <vector>
#include <utility>    // pair
#include <algorithm>  // min() max()

// SCL
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

#define VERBOSE 1
#define ivb if(VERBOSE)

/* -------------------------------------------------------------------- */
/*                         Main Program                                 */
/* -------------------------------------------------------------------- */
int main (int argc,char* argv[])
{
    int   breakNumber = 1;

    int opt = 1;
    int c;
    bool usage = (argc > 1 ? false : true);
    while ((c = getopt(argc, argv,"b:")) != EOF)
    switch (c) {
    case 'b':
        if (argc < ++opt +1)
            usage = true;
        breakNumber = atoi(argv[opt++]);
        break;
    default:
	break;
    }

    PR(breakNumber);

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
    StTpcGeometry *geomDb =
	StTpcSimpleGeometry::instance(geoFile.c_str());

    StTpcSlowControl *scDb =
	StTpcSimpleSlowControl::instance(scFile.c_str());
    //scDb->print();

    StMagneticField *magDb =
	StSimpleMagneticField::instance(magFile.c_str());

    StTpcElectronics *electronicsDb =
	StTpcSimpleElectronics::instance(electronicsFile.c_str());
    
    string gas("Ar");
    StTrsDeDx myEloss(gas);
    myEloss.print();
    
    //
    // create a Sector: (for analog Siganals)
    //
    StTrsSector *sector = new StTrsSector(geomDb);

    // digital signals (chars)
    StTrsDigitalSector *digitalSector = new StTrsDigitalSector(geomDb);

    //
    // Processes
    //
    StTrsChargeTransporter *trsTransporter =
	StTrsFastChargeTransporter::instance(geomDb, scDb, &myEloss, magDb);
//     trsTransporter->setChargeAttachment(true);
//     trsTransporter->setGatingGridTransparency(true);
//     trsTransporter->setTransverseDiffusion(true);
//     trsTransporter->setLongitudinalDiffusion(true);
//     trsTransporter->setExB(false);

    StTrsWireHistogram *theWirePlane =
	StTrsWireHistogram::instance(geomDb, scDb);
//     theWirePlane->setDoGasGain(true);  // True by default
//     theWirePlane->setDoGasGainFluctuations(true);
//     theWirePlane->setDoTimeDelay(true);

    StTrsAnalogSignalGenerator *trsAnalogSignalGenerator =
	StTrsSlowAnalogSignalGenerator::instance(geomDb, scDb, electronicsDb, sector);
    dynamic_cast<StTrsSlowAnalogSignalGenerator*>(trsAnalogSignalGenerator)->
	setChargeDistribution(StTrsSlowAnalogSignalGenerator::endo);
	//setChargeDistribution(StTrsSlowAnalogSignalGenerator::gatti);
	//setChargeDistribution(StTrsSlowAnalogSignalGenerator::dipole);
    dynamic_cast<StTrsSlowAnalogSignalGenerator*>(trsAnalogSignalGenerator)->
	//setElectronicSampler(StTrsSlowAnalogSignalGenerator::delta);
	setElectronicSampler(StTrsSlowAnalogSignalGenerator::symmetricGaussianApproximation);
	//setElectronicSampler(StTrsSlowAnalogSignalGenerator::symmetricGaussianExact);
	//setElectronicSampler(StTrsSlowAnalogSignalGenerator::asymmetricGaussianApproximation);
	//setElectronicSampler(StTrsSlowAnalogSignalGenerator::realShaper); 
//     trsAnalogSignalGenerator->setDeltaRow(0);
    trsAnalogSignalGenerator->setDeltaPad(2);
//     trsAnalogSignalGenerator->setSignalThreshold(.0001);
//     trsAnalogSignalGenerator->setSuppressEmptyTimeBins(true);
    //     ??should the type of function be an option ???
	
    StTrsDigitalSignalGenerator *trsDigitalSignalGenerator =
	StTrsFastDigitalSignalGenerator::instance(electronicsDb, sector, digitalSector);


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
    
    aSegment.split(&myEloss, magDb, breakNumber, &comp);

    copy(comp.begin(), comp.end(), ostream_iterator<StTrsMiniChargeSegment>(cout,"\n"));
    cout << endl;
    
    PR(comp.size());

    // Loop over the miniSegments
    for(iter = comp.begin();
	iter != comp.end();
	iter++) {

	cout << endl;
	PR(*iter);
	//
	// TRANSPORT HERE
	//
	trsTransporter->transportToWire(*iter);

    
 	//
 	// CHARGE COLLECTION AND AMPLIFICATION
 	//
	//PR(*iter);
	StTrsWireBinEntry anEntry(iter->position(), iter->charge());
 	PR(anEntry);
	theWirePlane->addEntry(anEntry);
	
    }

    cout << "\a***************************\a\n" << endl;


    
    //
    // Generate the ANALOG Signals on pads
    //
    // -- calculate the induced charge on pads
    trsAnalogSignalGenerator->inducedChargeOnPad(theWirePlane);
    //
    // -- sample the ANALOG signals that were induced on pads
    cout << "sampleAnalogSiganl() " << endl;
    trsAnalogSignalGenerator->sampleAnalogSignal();
    
    
     //
     // Digitize the Signals
     //
    trsDigitalSignalGenerator->digitizeSignal();

    
    //
    // Write it out!
    //
    
    return 0;
}

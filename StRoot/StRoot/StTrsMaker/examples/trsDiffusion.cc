//**************************************************************/
// $Id: trsDiffusion.cc,v 1.3 2003/09/02 17:59:15 perev Exp $
//
// Author: brian, Oct 1998
//
// Purpose: Evaluation of the diffusion characteristics of TRS
//
// $Log: trsDiffusion.cc,v $
// Revision 1.3  2003/09/02 17:59:15  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.2  1998/11/13 00:24:36  lasiuk
// TRUE/FALSE, pntrs in Db
//
// Revision 1.1  1998/11/10 17:12:00  fisyak
// Put Brian trs versin into StRoot
//
// Revision 1.1  1998/11/08 17:44:56  lasiuk
// Initial Revision
//
//**************************************************************/
#include <Stiostream.h>
#include <unistd.h>    // needed for access()

#include <string>
#include <utility>    // pair
#include <algorithm>  // min() max()

// SCL
#include "Randomize.h"
#include "StHbook.hh"

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
#include "StTrsAnalogSignal.hh"
#include "StTrsWireBinEntry.hh"
#include "StTrsWireHistogram.hh"

#include "StTrsSector.hh"


#define VERBOSE 1
#define ivb if(VERBOSE)

void printPad(tpcSector& a)
{
    ostream_iterator<StTrsAnalogSignal> out(cout,",");
    
    for(int irow=0; irow<a.size(); irow++)
	cout << irow << " " <<  "<" << a[irow].size() << "> ";

// 	for(int ipad=0; ipad<a[irow].size(); ipad++) {
// 	    cout << irow << " " << ipad << "<" << a[irow][ipad].size() << "> ";
// 	    //copy(a[irow][ipad].begin(),a[irow][ipad].end(),out);
// 	    cout << endl;
// 	}
}

// Sort the "analogSignal"s on a pad according to the time
// void sortTime(sector& a)
// {
//     for(int irow=0; irow<a.size(); irow++)
// 	for(int ipad=0; ipad<PADS[irow]; ipad++) {
// 	    sort(a[irow][ipad].begin(),a[irow][ipad].end(),comp_last());
// 	}
// }


/* -------------------------------------------------------------------- */
/*                         Main Program                                 */
/* -------------------------------------------------------------------- */
int main (int argc,char* argv[])
{
    const int tupleSize = 4;
    StHbookFile hbookFile("hbook");
    StHbookTuple theTuple("Transport", tupleSize);
    float tuple[tupleSize];
    theTuple << "x" << "y" << "z" << "amp" << book;

    const int tupleSize2 = 5;
    StHbookTuple secTuple("WireHistogram", tupleSize2);
    float tuple2[tupleSize2];
    secTuple << "x" << "y" << "z" << "wire" << "amp" << book;

    int irow, ipad, itbin;   // ctrs

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

    string scFile("../run/sc.conf");
    if (access(scFile.c_str(),R_OK)) {
	cerr << "ERROR:\n" << scFile << " cannot be opened" << endl;
	cerr << "Exitting..." << endl;
	exit(1);
    }

    string magFile("../run/example.conf");         // contains B field
    if (access(magFile.c_str(),R_OK)) {
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

    //
    // The DataBases
    //
    StTpcGeometry *geomDb =
	StTpcSimpleGeometry::instance(geoFile.c_str());

    StTpcSlowControl *scDb =
	StTpcSimpleSlowControl::instance(scFile.c_str());

    StMagneticField *magDb =
	StSimpleMagneticField::instance(magFile.c_str());

    StTpcElectronics *electronicsDb =
	StTpcSimpleElectronics::instance(electronicsFile.c_str());

    
    string gas("Ar");
    StTrsDeDx myEloss(gas);


    //
    // create a Sector:
    //
    StTrsSector *sector = new StTrsSector(geomDb);
    
    //
    // Processes
    //
    StTrsChargeTransporter *trsTransporter =
	StTrsFastChargeTransporter::instance(geomDb, scDb, &myEloss, magDb);
    // set status:
//     trsTransporter->setChargeAttachment(true);
//     trsTransporter->setGatingGridTransparency(true);
     trsTransporter->setTransverseDiffusion(true);
     trsTransporter->setLongitudinalDiffusion(true);
//     trsTransporter->setExB(true);

    StTrsWireHistogram *myWireHistogram =
	StTrsWireHistogram::instance(geomDb, scDb);
//     myWireHistogram->setDoGasGain(true);  // True by default
//     myWireHistogram->setDoGasGainFluctuations(false);
//     myWireHistogram->setDoTimeDelay(false);

    StTrsAnalogSignalGenerator *trsSignalGenerator =
	StTrsSlowAnalogSignalGenerator::instance(geomDb, scDb, electronicsDb, sector);
//     trsSignalGenerator->setDeltaRow(0);
//     trsSignalGenerator->setDeltaPad(0);
//     trsSignalGenerator->setSignalThreshold(.0001);
//     trsSignalGenerator->setSuppressEmptyTimeBins(true);
    //     ??select the type of function?
	
    StTrsDigitalSignalGenerator *trsDigitalGenerator =
	StTrsFastDigitalSignalGenerator::instance(electronicsDb, sector);

    //
    // Generate the Ionization
    //
    
    float maxDistance = geomDb->lastOuterSectorAnodeWire();
    PR(maxDistance);
    float zPosition = 1.*meter;
    float position = 52.*centimeter;
    float dS;
    do {
	dS = 5.*myEloss.nextInteraction();
// 	PR(dS);
	position += dS;

	if(position>maxDistance) break;

	double primaryEnergyDistribution;
	int totalElectrons = myEloss.secondary(&primaryEnergyDistribution) + 1;
 	PR(totalElectrons);

	// Make a StTrsMiniChargeSegment (the thing that must be transported)
	StTrsMiniChargeSegment
	    aMiniSegment(StThreeVector<double>(0, position, zPosition),
			 totalElectrons,  // q
			 0);              // dl
	PR(aMiniSegment);

	//
	// TRANSPORT HERE
	//
	trsTransporter->transportToWire(aMiniSegment);
	PR(aMiniSegment);

	tuple[0] = static_cast<float>(aMiniSegment.position().x());
	tuple[1] = static_cast<float>(aMiniSegment.position().y());
	tuple[2] = static_cast<float>(aMiniSegment.position().z());
	tuple[3] = static_cast<float>(aMiniSegment.charge());

	theTuple.fill(tuple);
	
	//
	// CHARGE COLLECTION AND AMPLIFICATION
	//
	StTrsWireBinEntry anEntry(aMiniSegment.position(), aMiniSegment.charge());
 	PR(anEntry);
	myWireHistogram->addEntry(anEntry);

	cout << "okay..." << endl;
	cout << myWireHistogram->lastEntry() << endl;
	
	if(myWireHistogram->lastEntry() != 0) {
	    tuple2[0] = static_cast<float>(myWireHistogram->lastEntry()->position().x());
	    tuple2[1] = static_cast<float>(myWireHistogram->lastEntry()->position().y());
	    tuple2[2] = static_cast<float>(myWireHistogram->lastEntry()->position().z());
	    tuple2[3] = static_cast<float>(myWireHistogram->lastWire());
	    tuple2[4] = static_cast<float>(myWireHistogram->lastEntry()->numberOfElectrons());
	
	    secTuple.fill(tuple2);
	}

	cout << "*********" << endl;
    }while(true);

    hbookFile.saveAndClose();
        
    return 0;
}

/***************************************************************************
 *
 * $Id: run1.cc,v 1.1 1998/11/10 23:40:26 lasiuk Exp $
 *
 * Author: brian May 18, 1998
 *
 ***************************************************************************
 *
 * Description: main program
 *
 ***************************************************************************
 *
 * $Log: run1.cc,v $
 * Revision 1.1  1998/11/10 23:40:26  lasiuk
 * BNL initialization
 *
 * Revision 1.5  1998/08/07 19:49:53  lasiuk
 * change path for config files after STAR_SYS
 *
 * Revision 1.4  1998/06/30 23:00:41  lasiuk
 * full framework
 *
 * Revision 1.3  1998/06/23 22:13:55  ullrich
 * Modified event loop to cope with modified manager.
 *
 * Revision 1.2  1998/06/04 23:27:48  lasiuk
 * incorporate addDb() functions; Upto filling the wireHistogram
 *
 **************************************************************************/
#include <unistd.h>
#include <iostream.h>
#include <fstream.h>
#include <string>
#include <math.h>
#include <vector>

// SCL/CLHEP
#include "StGlobals.hh"
#include "SystemOfUnits.h"
#include "PhysicalConstants.h"
#include "StThreeVector.hh"

// TRS
#include "StTrsManager.hh"
#include "StTrsReader.hh"
#include "StTrsChargeSegment.hh"
#include "StTrsMiniChargeSegment.hh"
#include "StTrsWriter.hh"

// DataBases and Information
#include "StTpcSimpleSlowControl.hh"
#include "StTpcSimpleGeometry.hh"
#include "StSimpleMagneticField.hh"
#include "StTrsDeDx.hh"

// Tools
#include "StTrsFastChargeTransporter.hh"
#include "StTrsSlowAnalogSignalGenerator.hh"
#include "StTrsFastDigitalSignalGenerator.hh"

/************************************************************/

StTrsManager      *trsManager; 
StTrsReader       *trsReader;

int main(int argc, char* argv[]) {

    //
    // Parse the command line
    //
    bool usage = FALSE;

    //
    //  default arguments
    //
    
    int numberOfSubSegments = 1;

    int c, opt=1;
    while ((c = getopt(argc, argv,"s:h")) != EOF) {
	switch (c)
	    {
	    case 'h':           // help
		usage = TRUE;
		break;
	    case 's':           // numberOfSubSegments
		if (argc < ++opt +1) {
		    usage = FALSE;
		}
		numberOfSubSegments = atoi(argv[opt++]);
		break;
	    default:
		cerr << "Unknown option" << endl;
		cerr << "Exitting..." << endl;
		exit(1);
	    }
    }

    PR(usage);
    if (usage) {
	cerr << "Usage: " << argv[0] << " [-s # of subSegments (1)]" << endl;
	exit(1);
    }

    // DEBUG
    PR(numberOfSubSegments);
    
    //
    // The following are input files or input data that is needed
    // for some part of the simulation.  It may eventually go into
    // a single configuration file and/or be stored by the manager.
    // A good idea is that the info is all specified in the main run
    // program so there is no confusion as to which "run cards" have
    // been specified.
    
    // Input data
    string dataFile("/data/g2texample2.txt");

    // DataBase files (for simple databases)
    string geoFile("../TPCgeo.conf");
    string scFile("../example.conf");         // contains B field
    //string elecFile("./electronics.conf");
    
    if (access(dataFile.c_str(),R_OK)) {
	cerr << "ERROR:\n" << dataFile << " cannot be opened" << endl;
	cerr << "Exitting..." << endl;
	exit(1);
    }
    cout << "Access to:\ndataFile=" << dataFile << endl;

    // Make sure databases are accessible

    if (access(geoFile.c_str(),R_OK)) {
	cerr << "ERROR:\n" << geoFile << " cannot be opened" << endl;
	cerr << "Exitting..." << endl;
	exit(1);
    }
    cout << "Access to:\ngeoFile=" << geoFile << endl;
    StTpcGeometry *geomdb =
	StTpcSimpleGeometry::instance(geoFile.c_str());

    if (access(scFile.c_str(),R_OK)) {
	cerr << "ERROR:\n" << scFile << " cannot be opened" << endl;
	cerr << "Exitting..." << endl;
	exit(1);
    }
    cout << "Access to:\nscFile=" << scFile << endl;
    StTpcSlowControl *scdb =
	StTpcSimpleSlowControl::instance(scFile.c_str());

    StMagneticField *magdb =
	StSimpleMagneticField::instance(scFile.c_str());

    trsManager = new StTrsManager(numberOfSubSegments);

    //
    // add the Singleton databases to the manager
    //
    trsManager->addSlowControlDb(scdb);
    trsManager->addTpcGeometryDb(geomdb);
    trsManager->addMagneticFieldDb(magdb);

    //
    // Add Gas/Ionization utility class
    StTrsDeDx    *trsGas;
    trsGas = new StTrsDeDx("Ar");
    trsManager->addGasDb(trsGas);
    //trsManager->addGasDb(new StTrsDeDx("Ar"));
    
    //
    // add the reader
    //
    trsReader = new StTrsReader(dataFile);
    trsManager->addReader(trsReader);

    //
    // BOOK-KEEPING
    // add the Wire Histogram (where the electrons drift to)
    //
     StTrsWireHistogram *wireHisto =
	StTrsWireHistogram::instance(geomdb);
    trsManager->addWireHistogram(wireHisto);


    // add the Sector:
    StTrsSector* trsSector = new StTrsSector(geomdb);
    trsManager->addSector(trsSector);
    
    //
    // TOOLS
    // add the charge transporter
    //    
    StTrsChargeTransporter *chargeTransporter =
	StTrsFastChargeTransporter::instance(geomdb);
    trsManager->addChargeTransporter(chargeTransporter);
    
    
    // The Analog signal generator should
    // require access to the calib db in the
    // future (constructor argument?)
    StTrsAnalogSignalGenerator *analogGenerator =
	StTrsSlowAnalogSignalGenerator::instance();
    trsManager->addAnalogSignalGenerator(analogGenerator);

    // The Digital Signal Generator should also require
    // access to the electronic (maybe calib db)
    // in the future
    StTrsDigitalSignalGenerator *digitalGenerator =
	StTrsFastDigitalSignalGenerator::instance(); // require a db
    trsManager->addDigitalSignalGenerator(digitalGenerator);

    //
    // add the writer
    //
    StTrsWriter *trsWriter = new StTrsWriter();
    trsManager->addWriter(trsWriter);
    
    //
    // Setup finished, process events
    //
    
    while(trsManager->processEvent());


    cout << "okay clean up memory... " << endl;

    delete trsWriter;
    delete trsSector;
    delete trsReader;
    delete trsGas; 
    delete trsManager;

    cout << "Done... " << endl;
    
    return 0;
}

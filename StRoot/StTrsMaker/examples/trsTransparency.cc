//*************************************************************
// $Id: trsTransparency.cc,v 1.3 2003/09/02 17:59:15 perev Exp $
//
// Author: brian, October 1998
//
// Purpose: Calculation of the transparency for a monostable
//          switched Gating Grid from the ChargeTransporter
//          in TRS.  Requires the initialization of several
//          Data Bases.
//
// $Log: trsTransparency.cc,v $
// Revision 1.3  2003/09/02 17:59:15  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.2  1998/11/13 00:24:37  lasiuk
// TRUE/FALSE, pntrs in Db
//
// Revision 1.1  1998/11/10 17:12:01  fisyak
// Put Brian trs versin into StRoot
//
// Revision 1.1  1998/11/08 17:44:57  lasiuk
// Initial Revision
//
/********************************************************/
#include <Stiostream.h>
#include <unistd.h>    // needed for access()

#include <string>

#include "SystemOfUnits.h"
#include "StHbook.hh"

#include "StTpcSimpleGeometry.hh"
#include "StTpcSimpleSlowControl.hh"
#include "StTpcSimpleElectronics.hh"
#include "StSimpleMagneticField.hh"
#include "StTrsDeDx.hh"

#include "StTrsFastChargeTransporter.hh"

int main ()
{
    // HBOOK
    const int tupleSize = 2;
    StHbookFile hbookFile("hbook");
    StHbookTuple theTuple("gating grid", tupleSize);
    float tuple[tupleSize];
    theTuple << "v" << "t" << book;
    
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
	//shell(pwd);
	cerr << "Exitting..." << endl;
	exit(1);
    }

    string magFile("../run/example.conf");
    if (access(magFile.c_str(),R_OK)) {
	cerr << "ERROR:\n" << magFile << " cannot be opened" << endl;
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
    
    StTpcElectronics *electronicsDb =
	StTpcSimpleElectronics::instance(electronicsFile.c_str());
    
    StTpcSlowControl *scDb =
	StTpcSimpleSlowControl::instance(scFile.c_str());
    
    StMagneticField *magDb =
	StSimpleMagneticField::instance(scFile.c_str());
    
    string gas("Ar");
    StTrsDeDx gasDb(gas);


    //
    // Make a Transporter!

    StTrsChargeTransporter* trsTransporter =
	StTrsFastChargeTransporter::instance(geomDb, scDb, &gasDb, magDb);

    trsTransporter->setGatingGridTransparency(true);

#ifndef ST_NO_NAMESPACES
    using namespace units;
#endif
    float voltage = -140.*volt;
    do {
	voltage += (1.*volt);
	trsTransporter->setGatingGridVoltage(voltage);
	double value = trsTransporter->transparencyCalculation();

	tuple[0] = static_cast<float>(voltage/volt);
	tuple[1] = static_cast<float>(value);

	theTuple.fill(tuple);

    } while (voltage <= (10.*volt));

    hbookFile.saveAndClose();

    return 0;
}

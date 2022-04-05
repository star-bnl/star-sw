//*******************************************************/
// $Id: trsDataBase.cc,v 1.3 2003/09/02 17:59:15 perev Exp $
//
// Author: brian, October 1998
//
// Purpose: Illustrates the proper way to call
//          a data base and reference its components.
//
// $Log: trsDataBase.cc,v $
// Revision 1.3  2003/09/02 17:59:15  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.2  1999/01/26 20:40:08  lasiuk
// magnetic field db
//
// Revision 1.1  1998/11/10 17:11:59  fisyak
// Put Brian trs versin into StRoot
//
// Revision 1.1  1998/11/08 17:44:56  lasiuk
// Initial Revision
//
/********************************************************/
#include <Stiostream.h>
#include <unistd.h>    // needed for access()

#include <string>

#include "StTpcSimpleGeometry.hh"
#include "StTpcSimpleSlowControl.hh"
#include "StTpcSimpleElectronics.hh"
#include "StSimpleMagneticField.hh"
#include "StTrsDeDx.hh"

int main (int argc,char* argv[])
{
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
    geomDb->print();
    
    StTpcElectronics *electronicsDb =
	StTpcSimpleElectronics::instance(electronicsFile.c_str());
    electronicsDb->print();
    
    StTpcSlowControl *scDb =
	StTpcSimpleSlowControl::instance(scFile.c_str());
    scDb->print();
    
    StMagneticField *magDb =
	StSimpleMagneticField::instance(magFile.c_str());
    
//     string gas("Ar");
//     StTrsDeDx myEloss(gas);


//    
    
    
    return 0;
}

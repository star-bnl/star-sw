//*******************************************************/
// $Id: trsDataBase.cc,v 1.1 1998/11/10 17:11:59 fisyak Exp $
//
// Author: brian, October 1998
//
// Purpose: Illustrates the proper way to call
//          a data base and reference its components.
//
// $Log: trsDataBase.cc,v $
// Revision 1.1  1998/11/10 17:11:59  fisyak
// Put Brian trs versin into StRoot
//
// Revision 1.1  1998/11/08 17:44:56  lasiuk
// Initial Revision
//
/********************************************************/
#include <iostream.h>
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
	StSimpleMagneticField::instance(scFile.c_str());
    
//     string gas("Ar");
//     StTrsDeDx myEloss(gas);


//    
    
    
    return 0;
}

//*********************************************************************
//   $Id: trsLandau.cc,v 1.1 1998/11/10 17:12:00 fisyak Exp $
//
//   Author: brian, October 1998
//
//   Purpose: Used to determine and evaluate the functional form
//            of the ionization process:
//            -- Landau Distribution
//            -- Primary Energy Distribution
//
//   $Log: trsLandau.cc,v $
//   Revision 1.1  1998/11/10 17:12:00  fisyak
//   Put Brian trs versin into StRoot
//
//   Revision 1.1  1998/11/08 17:44:57  lasiuk
//   Initial Revision
//
//*********************************************************************
#include <unistd.h>
#include <iostream.h>
#include <fstream.h>
#include <string>

// SCL
#include "SystemOfUnits.h"
#ifndef ST_NO_NAMESPACES
using namespace units;
#endif
#include "StHbook.hh"
#include "Randomize.h"

// TRS
#include "StTrsDeDx.hh"

//         --------------------- MAIN --------------------------       //
int main()
{
    const int tupleSize = 3;
    StHbookFile hbookFile("hbook");
    StHbookTuple theTuple("dedx", tupleSize);
    float tuple[tupleSize];
    theTuple << "pri" << "tot" << "pnt" << book; // position points
    
    const int tupleSize2 = 2;
    StHbookTuple secTuple("distribution", tupleSize2);
    float tuple2[tupleSize2];
    
    secTuple << "energy" << "sec" << book;

    string gas("Ar");
    StTrsDeDx myEloss(gas);  // default pad Length
    
    int    primary, secondary;
    int    totalSecondary;
    int    total;               // number Of Electrons
    double xysec;               // energy distribution

    myEloss.print();
    for(int pnt=0; pnt<10000; pnt++) {

	// primaries are given by a Poissonian distribution with a mean
	primary = myEloss.primary(); // bg = 1

	// loop over all primaries
	totalSecondary = 0;
	for(int ii=0; ii<primary; ii++) {

	    secondary = myEloss.secondary(&xysec);
	    totalSecondary += secondary;
	    tuple2[0] = static_cast<float>(xysec/eV);
	    tuple2[1] = static_cast<float>(secondary);
	    secTuple.fill(tuple2);

	    totalSecondary +=secondary;
	}

	// Total number of electrons in the ionization interaction
	total = primary+totalSecondary;

	tuple[0] = static_cast<float>(primary);
	tuple[1] = static_cast<float>(total);
	tuple[2] = static_cast<float>(pnt);
	
	theTuple.fill(tuple);		// write the tuple
    }
    
    cout <<"Done: " << endl;

    hbookFile.saveAndClose();

    return 0;
}

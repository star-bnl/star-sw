//*********************************************************************
//   $Id: trsLandau.cc,v 1.3 2003/09/02 17:59:15 perev Exp $
//
//   Author: brian, October 1998
//
//   Purpose: Used to determine and evaluate the functional form
//            of the ionization process:
//            -- Landau Distribution
//            -- Primary Energy Distribution
//
//   $Log: trsLandau.cc,v $
//   Revision 1.3  2003/09/02 17:59:15  perev
//   gcc 3.2 updates + WarnOff
//
//   Revision 1.2  1998/11/12 22:39:57  lasiuk
//   compatibility at BNL
//
//   Revision 1.1  1998/11/10 17:12:00  fisyak
//   Put Brian trs versin into StRoot
//
//   Revision 1.1  1998/11/08 17:44:57  lasiuk
//   Initial Revision
//
//*********************************************************************
#include <unistd.h>
#include <Stiostream.h>
#include "Stiostream.h"
#include <string>

// SCL
#include "SystemOfUnits.h"
#ifndef ST_NO_NAMESPACES
using namespace units;
#endif

#define HBOOK
#ifdef HBOOK
#include "StHbook.hh"
#endif
#include "Randomize.h"

// TRS
#include "StTrsDeDx.hh"


//         --------------------- MAIN --------------------------       //
int main()
{
#ifdef HBOOK
    const int tupleSize = 3;
    StHbookFile hbookFile("hbook");
    StHbookTuple theTuple("dedx", tupleSize);
    float tuple[tupleSize];
    theTuple << "pri" << "tot" << "pnt" << book; // position points
    
    const int tupleSize2 = 2;
    StHbookTuple secTuple("distribution", tupleSize2);
    float tuple2[tupleSize2];
    
    secTuple << "energy" << "sec" << book;
#endif

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
#ifdef HBOOK
	    tuple2[0] = static_cast<float>(xysec/eV);
	    tuple2[1] = static_cast<float>(secondary);
	    secTuple.fill(tuple2);
#endif
	    totalSecondary +=secondary;
	}

	// Total number of electrons in the ionization interaction
	total = primary+totalSecondary;

#ifdef HBOOK
	tuple[0] = static_cast<float>(primary);
	tuple[1] = static_cast<float>(total);
	tuple[2] = static_cast<float>(pnt);
	
	theTuple.fill(tuple);		// write the tuple
#endif
    }
    
    cout <<"Done: " << endl;

#ifdef HBOOK
    hbookFile.saveAndClose();
#endif
    return 0;
}

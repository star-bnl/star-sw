/*************************************************************************
 *
 *  $Id: hbookTest1.cc,v 1.1 1999/02/17 12:43:57 ullrich Exp $
 *
 *  Author: brian
 *
 *************************************************************************
 *
 *  Description: Illustrate the use of the hbook package
 *
 *************************************************************************
 *
 * $Log: hbookTest1.cc,v $
 * Revision 1.1  1999/02/17 12:43:57  ullrich
 * New Revision
 *
 * Revision 1.1  1999/02/17 12:43:57  ullrich
 * New Revision
 *
 * Revision 1.1  1999/01/23 00:26:42  ullrich
 * Initial Revision
 *
 *************************************************************************/
#include <iostream.h>
#include <unistd.h>
#include <string>
#include "Random.h"
#include "JamesRandom.h"
#include "RandGauss.h"
#include "StHbook.hh"

int main(int argc, char* argv[])
{
    // Define name of output hbook filename
    // Can be read in from command line
    string filename("hbook.ntp");

    cout << "HBOOK file is: " << filename << endl;
    
    if (!access(filename.c_str(),F_OK|R_OK)) {
	cerr << "Hbook File: `" << filename << "' already exists." << endl;
	exit(1);
    }

    // Define the filename
    StHbookFile hbookFile(filename.c_str());

    // ntuple
    const int tupleSize1 = 2;
    StHbookTuple theTuple("random",tupleSize1);

    // label the rows
    theTuple.setTag("index").setTag("random").book();

    // or alternatively
    //theTuple << "index" << "random" << book;




    // histogram
    StHbookHisto theHisto("gauss distribution", 100, -10, 10);

    
    float tuple[tupleSize1];  // array to be filled

    
    HepJamesRandom engine;
    RandGauss gaussDistribution(engine);
    
    for(int ii=0; ii<100; ii++) {
	int k=0;
	tuple[k++] = static_cast<float>(ii);
	double randomNumber = gaussDistribution.shoot();
 	tuple[k++] = static_cast<float>(randomNumber);
	
	theTuple.fill(tuple);

	theHisto.fill(static_cast<float>(randomNumber),1);
    }

    cout << "\nAvailable information regarding the histogram" << endl;
    cout << "---------------------------------------------" << endl;
    cout << "Histogram ID:      " << theHisto.id()      << endl;
    cout << "Histogram entries: " << theHisto.entries() << endl;
    cout << "Histogram max:     " << theHisto.max()     << endl;
    cout << "Histogram min:     " << theHisto.min()     << endl;
    cout << "Histogram sum:     " << theHisto.sum()     << endl;
    cout << "Histogram mean:    " << theHisto.mean()    << endl;
    cout << "Histogram sigma:   " << theHisto.sigma()   << endl;

    // print ASCII representation of the Histogram to std out
    theHisto.print();

    theHisto.setOpt("show");

    cout << "\nAvailable information regarding the N-tuple" << endl;
    cout << "-------------------------------------------" << endl;
    cout << "N-tuple ID:      " << theTuple.id()      << endl;
    cout << "N-tuple entries: " << theTuple.entries() << endl;
    cout << "N-tuple length:  " << theTuple.length()  << endl;

    cout << "\nFile information" << endl;
    cout << "hbookFile status: (1) is good " << hbookFile.isGood() << endl;
    hbookFile.list();
    
    //
    //  Terminate program
    //
    hbookFile.saveAndClose();

    return 0;
}

/*************************************************************************
 *
 *  $Id: hbookTest2.cc,v 1.3 1999/12/21 15:14:46 ullrich Exp $
 *
 *  Author: brian
 *
 *************************************************************************
 *
 *  Description: Illustrate the use of the hbook package
 *
 *************************************************************************
 *
 * $Log: hbookTest2.cc,v $
 * Revision 1.3  1999/12/21 15:14:46  ullrich
 * Modified to cope with new compiler version on Sun (CC5.0).
 *
 * Revision 1.2  1999/05/19 21:12:48  ullrich
 * Change printout
 *
 * Revision 1.1  1999/02/17 12:43:58  ullrich
 * New Revision
 *
 * Revision 1.1  1999/01/23 00:26:43  ullrich
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
#if !defined(ST_NO_NAMESPACES)
using std::string;
#endif

//
// This books the histograms/ntuples by pointer
//
StHbookFile *hbookFile;

// ntuple
const int tupleSize1 = 2;
float tuple[tupleSize1];
StHbookTuple *theTuple;

// histogram
StHbookHisto *theHisto;

// Functions:
void init_files(string name)
{
    cout << "HBOOK file is: " << name.c_str() << endl;    
    if (!access(name.c_str(),F_OK|R_OK)) {
	cerr << "Hbook File: `" << name.c_str() << "' already exists." << endl;
	exit(1);
    }
    
    //
    //  Open histogram file and book tuple
    //
    // ntuple
    hbookFile = new StHbookFile(name.c_str());
    theTuple  = new StHbookTuple("Random Numbers", tupleSize1);

    *theTuple << "index" << "random" << book;

    // histogram
    theHisto = new StHbookHisto("gauss distribution", 100, -10, 10);
}

void cleanUp()
{
    cout << "Saving files and cleaning up ... ";
    hbookFile->saveAndClose();

    delete hbookFile;
    delete theTuple;
    cout << "done" << endl;
}

//         --------------------- MAIN --------------------------       //
int main(int argc, char* argv[])
{
    // Can be read in from command line
    string filename("hbook.ntp");

    //
    // HBook Initialization
    //
    
    init_files(filename);

    HepJamesRandom engine;
    RandGauss gaussDistribution(engine);
    
    for(int ii=0; ii<100; ii++) {
	int k=0;
	tuple[k++] = static_cast<float>(ii);
	double randomNumber = gaussDistribution.shoot();
 	tuple[k++] = static_cast<float>(randomNumber);
	
	theTuple->fill(tuple);

	theHisto->fill(static_cast<float>(randomNumber),1);
    }

    cout << "\nAvailable information regarding the histogram" << endl;
    cout << "---------------------------------------------" << endl;
    cout << "Histogram ID:      " << theHisto->id()      << endl;
    cout << "Histogram entries: " << theHisto->entries() << endl;
    cout << "Histogram max:     " << theHisto->max()     << endl;
    cout << "Histogram min:     " << theHisto->min()     << endl;
    cout << "Histogram sum:     " << theHisto->sum()     << endl;
    cout << "Histogram mean:    " << theHisto->mean()    << endl;
    cout << "Histogram sigma:   " << theHisto->sigma()   << endl;

    // print ASCII representation of the Histogram to std out
    //theHisto->print();

    theHisto->setOpt("show");

    cout << "\nAvailable information regarding the N-tuple" << endl;
    cout << "-------------------------------------------" << endl;
    cout << "N-tuple ID:      " << theTuple->id()      << endl;
    cout << "N-tuple entries: " << theTuple->entries() << endl;
    cout << "N-tuple length:  " << theTuple->length()  << endl;

    cout << "\nFile information" << endl;
    cout << "hbookFile status: (1) is good " << hbookFile->isGood() << endl;
    
    //
    //  clean up
    //
    cleanUp();
        
    return 0;
}

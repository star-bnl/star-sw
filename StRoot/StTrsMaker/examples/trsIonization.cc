//*******************************************************************
// 
// $Id: trsIonization.cc,v 1.3 2003/09/02 17:59:15 perev Exp $
//
//*******************************************************************
//
// Description:  This programs illustrates the use of the
//               StTrsDeDx class.  It demonstrates the bethe-bloch
//               parameterization, the primary and secondary electron
//               as well as the sub segment splitting.
//
// Usage: trsIonization [-s # of samples][-n # of subsegments][-t tracks]
//             [-f forceOverWrite] hbookfile
//
// Example: trsIonization -s 45 -n 1 -t 10 -f hbook
//*******************************************************************
//
// $Log: trsIonization.cc,v $
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
//*******************************************************************
								     
#include "Stiostream.h"
#include <unistd.h>
#include <vector>
#include <string>

#include "StGlobals.hh"
#include "SystemOfUnits.h"
#include "StThreeVector.hh"

#ifndef ST_NO_NAMESPACES
using namespace units;
#endif

#include "StTrsDeDx.hh"

#define HBOOK__DIAGNOSTIC
#ifdef HBOOK__DIAGNOSTIC
#include "StHbook.hh"
#endif

#ifdef HBOOK__DIAGNOSTIC
    StHbookFile *hbookFile;
    const int tupleSize1 = 2;
    const int tupleSize2 = 5;

    float tuple[tupleSize1];
    float tuple2[tupleSize2];

    StHbookTuple *theTuple;
    StHbookTuple *secTuple;
#endif

// Functions:
void init_files(int overWrite, char* fname)
{
    cout << "HBOOK file is " << fname << endl;
#ifdef HBOOK__DIAGNOSTIC
    cout << "ForceOverwrite: " << overWrite << endl;
    
    if (!overWrite && !access(fname,F_OK|R_OK)) {
	cerr << "ERROR: output file '"  << fname << "' already exists." << endl;
	exit(1);
    }
    
    //
    //  Open histogram file and book tuple
    //
    hbookFile = new StHbookFile(fname);

    theTuple  = new StHbookTuple("ionization", tupleSize1);
    *theTuple << "bg" << "i" << book;

    secTuple  = new StHbookTuple("dedx", tupleSize2);
    *secTuple << "event" << "pri" << "sec" << "tot" << "sub" << book;

//     *thirdTuple  = new StHbookTuple("segments", tupleSize3);
//     *thirdTuple << "sub" << "pri" << "tot" << book;
    
#endif
}

void cleanup()
{
#ifdef HBOOK__DIAGNOSTIC
    cout << "Save and close " << endl;
    hbookFile->saveAndClose();
#endif
}

int main(int argc, char* argv[])
{
    // Parse the command line

    // default arguments
    bool   usage = false;
    bool   forceOverwrite;
    int    numberOfTracks  = 1000;
    int    numberOfSamples = 45;    // number of pads
    float  subSegments     = 1.;    // number of pieces to break a sample into
    char*  filename = "hbook";

    int c, opt=1;
    while ((c = getopt(argc, argv,"s:n:t:fh")) != EOF) {
        switch (c)
            {
	    case 's':           // numberOfSamples
                if (argc < ++opt +1) {
                    usage = false;
                }
                numberOfSamples = atoi(argv[opt++]);
                break;
	    case 'n': // number of 'subSegments'
                if (argc < ++opt +1) {
                    usage = false;
                }
                subSegments = atof(argv[opt++]);
		break;
	    case 't': // numberOfTracks
		if (argc < ++opt +1) {
                    usage = false;
                }
		numberOfTracks = atoi(argv[opt++]);
	    case 'h':
		usage = true;
		break;
	    case 'f':
		forceOverwrite = true;
                break;
	    case 'o':
		filename = (char *)strdup (argv[opt++]);\
		break;
            default:
                cerr << "Unknown option" << endl;
                cerr << "Exitting..." << endl;
                exit(1);
            }
    }

    if (argc > 1)
	usage = false;
  
    PR(usage);
    if (usage) {
        cerr << "Usage: " << argv[0] << " [-s # of samples (1)]\n";
	cerr << " [-n # of subsegments][-t tracks][-f forceOverWrite]\n";
	cerr << " [-h help] [-o hbookfile]" << endl;
        exit(1);
    }

    // DEBUG
    PR(subSegments);
    PR(numberOfSamples);
    PR(numberOfTracks);
    PR(filename);
    
// Hbook Initialization
#ifdef HBOOK__DIAGNOSTIC
    init_files(forceOverwrite, filename);
#endif

    double padLength = 1.95*centimeter;
    cout << "subSegments= " << subSegments << endl;
    
    //string gas("Ne");
    string gas("Ar");
    StTrsDeDx myELoss(gas,padLength);
    StTrsDeDx subELoss(gas,(padLength/subSegments));
 
    myELoss.print();

    int ii,jj,kk;
#ifndef ST_NO_TEMPLATE_DEF_ARGS
    vector<int> sum;
#else
    vector<int, allocator<int> > sum;
#endif    
    
    double bg = .1;

    for(jj=1; jj<=5; jj++) {
	double increment = bg;
	for(kk=1; kk<10; kk++) {
	    bg+=increment;

	    tuple[0] = static_cast<float>(bg);
	    tuple[1] = static_cast<float>(myELoss.betheBloch(bg));

	    theTuple->fill(tuple);
	}
	bg+=increment;
    }

    
    //Create tracks:
    for(int itrack=0; itrack<numberOfTracks; itrack++) {
	for (int isample=0; isample<numberOfSamples; isample++) {

	    sum.resize((StTrsDeDx::numberOfElectrons),0);
	    //myELoss.electrons(sum, bg);
	    myELoss.electrons(sum);
	    tuple2[0] = static_cast<float>(itrack);
	    tuple2[1] = static_cast<float>(sum[StTrsDeDx::primaries]);
	    tuple2[2] = static_cast<float>(sum[StTrsDeDx::secondaries]);
	    tuple2[3] = static_cast<float>(sum[StTrsDeDx::total]);

	    int totalInSubsegment = 0;
	    for(int isubsample=0; isubsample<subSegments; isubsample++) {
		sum.resize((StTrsDeDx::numberOfElectrons),0);
		subELoss.electrons(sum);
		totalInSubsegment += sum[StTrsDeDx::total];


		tuple2[4] = static_cast<float>(totalInSubsegment);
	    }
	    secTuple->fill(tuple2);
	} // isample
    }     //  itrack

	    
    cleanup();
    
    return 0;
}

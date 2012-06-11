//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StGridCollector                                                      //
//                                                                      //
// Loads the appropriate shared libraries                               //
// for the StGridCollector interface to the Grid Collector services     //
//                                                                      //
// John Wu <John.Wu@nersc.gov>                                          //
// Based on StChallenger by                                             //
// Alexandre V. Vaniachine <AVVaniachine@lbl.gov>                       //
//////////////////////////////////////////////////////////////////////////
#include "Riostream.h"
#include "TROOT.h"
#include "TClass.h"
#include "StGridCollector.h"

ClassImp(StGridCollector)
//___________________________________________________________________________
    StGridCollector* StGridCollector::Create(const char* sel) {
    StGridCollector  *serv = 0;

    //if (gROOT->LoadClass("libpthread","libpthread")) return 0;
    //if (gROOT->LoadClass("libJTC","libJTC")) return 0;
    //if (gROOT->LoadClass("libOB","libOB")) return 0;
    //if (gROOT->LoadClass("libCosNaming","libCosNaming")) return 0;
    if (gROOT->LoadClass("libGridCollector","libGridCollector")) return 0;

    TClass *c = gROOT->GetClass("Collector");
    if (!c) {
	std::cerr << "ERROR <StGridCollector::Create> failed to GetClass"
	     << std::endl;      
	return 0;
    }

    serv = (StGridCollector *) c->New();
    if(!serv)
	std::cerr << "ERROR <StGridCollector::Create> failed to make a new "
	     << "GridCollector" << std::endl;
    if (sel) {
	if (*sel) {
	    int ierr = serv->Init(sel);
	    if (ierr) {
		std::cerr << "ERROR <StGridCollector::Create(" << sel
		     << ")> failed to with error code " << ierr << std::endl;
		delete serv;
		serv = 0;
	    }
	}
    }

    return serv;
}
//___________________________________________________________________________

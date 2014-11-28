#ifndef __CINT__
#include "TSystem.h"
#include "TBrowser.h"
#include "TBenchmark.h"
#include "TClassTable.h"
#include "StBFChain.h"
#include "St_tcl_Maker/St_tcl_Maker.h"
#include "St_tpt_Maker/St_tpt_Maker.h"
#include "StEvent.h"
#include "St_geant_Maker/St_geant_Maker.h"
#include "StIOMaker/StIOMaker.h"
#include "StEventDisplayMaker/StEventDisplayMaker.h"
#include "StEventMaker/StEventMaker.h"
#include "StAssociationMaker/StMcParameterDB.h"
#include "St_dst_Maker/StV0Maker.h"
#include "xdf2root/St_XDFFile.h"
#include "StTpcT0Maker/StTpcT0Maker.h"

gSystem->Load("StDetectorDbMaker");

void Usage();
void Load();

#else

class StMaker;
class StBFChain;
class StEvent;
class St_geant_Maker;
class StIOMaker;
class St_XDFFile;
class StEventDisplayMaker;
class StEventMaker;
class StTpcT0Maker;
class StChain;
class StAssociationMaker;
class StMcAnalysisMaker;

#endif 

#include <string>
class St_DataSet;
St_DataSet *Event;
StChain *chain;
TBrowser *brow=0;


// The acual file to be used is passed as an argument to the macro, or a default can be set

void simM2Maker (
		const char *MainFile="/star/institutions/rice/geurts/sim/tof/pythia_0.geant.root",
		Int_t nevents=10,
		const char *outFileName="test.root"
		)
{
	// Load all the System libraries
	gSystem->Load("StarRoot");
	gSystem->Load("St_base");
	gSystem->Load("StChain");
	gSystem->Load("libglobal_Tables");
	gSystem->Load("libgen_Tables");
	gSystem->Load("libsim_Tables");
	gSystem->Load("StUtilities");
	gSystem->Load("StIOMaker");
	gSystem->Load("StarClassLibrary");
	gSystem->Load("StDbLib.so");   
	gSystem->Load("StDbBroker.so");
	gSystem->Load("libStDb_Tables.so");
	gSystem->Load("St_db_Maker.so");
	gROOT->Macro("loadMuDst.C");
	gSystem->Load("StTpcDb");
	gSystem->Load("StDetectorDbMaker");
	gSystem->Load("StDbUtilities");
	gSystem->Load("StBFChain");
	gSystem->Load("StChallenger");
	gSystem->Load("StEvent");
	gSystem->Load("StEventMaker");
	gSystem->Load("StIOMaker");
	gSystem->Load("StBTofUtil");
	gSystem->Load("St_Tables");
	gSystem->Load("StEmcUtil");
	gSystem->Load("StAssociationMaker");
	gSystem->Load("StMcAnalysisMaker");
	gSystem->Load("StMcEvent");
	gSystem->Load("StMcEventMaker");
	gSystem->Load("St_g2t");  // is a part od St_Tables
	gSystem->Load("geometry");
	gSystem->Load("St_geant_Maker");
	gSystem->Load("StTableUtilities");
	gSystem->Load("StBTofSimMaker");
	gSystem->Load("StBTofMatchMaker");
	gSystem->Load("StTofCalibMaker");

	chain = new StChain("BTofSim"); 
	//chain->SetDebug();

	St_geant_Maker *geantMk = new St_geant_Maker();
	geantMk->LoadGeometry("detp geometry y2009");
	//geantMk->LoadGeometry("detp geometry y2008");
	geantMk->SetActive(kFALSE);

	StIOMaker *IOMk = new StIOMaker("bfc");
	IOMk->SetFile(MainFile);
	IOMk->SetIOMode("r");
	IOMk->SetBranch("*",0,"0");                 //deactivate all branches
	IOMk->SetBranch("geantBranch",0,"r");
	IOMk->SetBranch("eventBranch",0,"r"); 

	StMcEventMaker  *mcEventReader  = new StMcEventMaker; // Make an instance...
	mcEventReader->doPrintEventInfo = false;
	mcEventReader->doPrintMemoryInfo = false;
	mcEventReader->doUseTpc = true;
	mcEventReader->doUseBemc = false;
	mcEventReader->doUseBsmd = false;
	mcEventReader->doUseFtpc = false;
	mcEventReader->doUseSvt = false;
	mcEventReader->doUseRich = false;
	mcEventReader->doUseEemc = false;
	mcEventReader->doUseTof  = true;
	mcEventReader->SetDebug(1);


	St_db_Maker *dbMk = new St_db_Maker("StarDb", "MySQL:StarDb", "MySQL:StarDb", "$STAR/StarDb");
	dbMk->SetDateTime(20090401,214129); // for simulation
	cout << " DbMaker loading done " << endl;


	//geant to StEvent convertion code
	string filename=outFileName;
	StBTofSimMaker *simMaker = new StBTofSimMaker;
	simMaker->setBookHist(kTRUE);
	simMaker->setHistFileName(filename);
	simMaker->writeStEvent(kTRUE);

	StBTofMatchMaker *matchMaker = new StBTofMatchMaker;
        matchMaker->setIdealGeometry();
	matchMaker->SetDebug(1);

	//StEvent to MuDst conversion
	StMuDstMaker* makeMu=new StMuDstMaker("makeMu");

	//user analysis makers here
	//M2Maker *mk = new M2Maker(makeMu);
	//mk->SetRootFile(newOutName);

	int totalProcessedTofPoints=0;

	chain->Init();
	chain->PrintInfo();
	int total=0;
	for (Int_t iev=0;iev<nevents; iev++) {
		cout << "****************************************** " << endl;
		cout << "Working on eventNumber " << iev << endl;
		cout << "****************************************** " << endl;
		chain->Clear();
		int iret = chain->Make(iev); 

		if (iret) { cout << "Bad return code!" << endl; break;}

		total++;

		StMuDst* temp=makeMu->muDst();	
		int nTofs=temp->numberOfBTofHit();
		cout<<"MUDSTOUTPUT-- BTofHits "<<nTofs<<endl;


	} 


	cout << "****************************************** " << endl;
	cout << "Work done... now its time to close up shop!"<< endl;
	cout << "****************************************** " << endl;
	chain->Finish(); 
	cout << "****************************************** " << endl;
	cout << "total number of events  " << total << endl;
	cout << "****************************************** " << endl;      

	cout<<endl<<endl;
	cout<<"Total Tof Hits Processed in MuDst: "<<totalProcessedTofPoints<<endl;
//	gApplication->Terminate();

}

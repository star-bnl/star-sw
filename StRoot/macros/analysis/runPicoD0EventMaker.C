/* **************************************************
 *  A macro to run StPicoD0EventMaker
 *
 *  Authors:  **Mustafa Mustafa (mmustafa@lbl.gov)
 *
 *  **Code Maintainer
 *
 * **************************************************
 */


#include <TSystem>

class StMaker;
class StChain;
class StPicoDstMaker;


StChain *chain;
void runPicoD0EventMaker(const Char_t *inputFile=, const Char_t *outputFile="test.root")
{ 
  //Check STAR Library. Please set SL_version to the original star library used in the production from http://www.star.bnl.gov/devcgi/dbProdOptionRetrv.pl
#if 0
  string SL_version = "SL15c";
  string env_SL = getenv ("STAR");
  if(env_SL.find(SL_version)==string::npos)
  {
      cout<<"Environment Star Library does not match the requested library in runPicoD0EventMaker.C. Exiting..."<<endl;
      exit(1);
  }
#endif
  Int_t nEvents = 10000000;
	
  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
	loadSharedLibraries();

	gSystem->Load("StPicoDstMaker");
  gSystem->Load("StPicoPrescales");

  // KFVertexFitter dependancies
  gSystem->Load("StTpcDb");
  gSystem->Load("StDbUtilities");
  gSystem->Load("Sti");
  gSystem->Load("StiUtilities");
  gSystem->Load("StSsdDbMaker");
  gSystem->Load("StSvtDbMaker");
  gSystem->Load("StTMVARank");
  gSystem->Load("StiMaker");
  gSystem->Load("StPicoKFVertexFitter");
  // ---

  gSystem->Load("StPicoD0EventMaker");

	chain = new StChain();

	StPicoDstMaker* picoDstMaker = new StPicoDstMaker(0,inputFile,"picoDstMaker");
  StPicoD0EventMaker* picoD0Maker = new StPicoD0EventMaker("picoD0Maker",picoDstMaker,outputFile);

	chain->Init();
	cout<<"chain->Init();"<<endl;
	int total = picoDstMaker->chain()->GetEntries();
  cout << " Total entries = " << total << endl;
  if(nEvents>total) nEvents = total;

	for (Int_t i=0; i<nEvents; i++)
  {
	  if(i%10000==0)
		cout << "Working on eventNumber " << i << endl;
		
	  chain->Clear();
	  int iret = chain->Make(i);
		
	  if (iret) { cout << "Bad return code!" << iret << endl; break;}

	  total++;
	}
	
	cout << "****************************************** " << endl;
	cout << "Work done... now its time to close up shop!"<< endl;
	cout << "****************************************** " << endl;
	chain->Finish();
	cout << "****************************************** " << endl;
	cout << "total number of events  " << nEvents << endl;
	cout << "****************************************** " << endl;
	
	delete chain;
}

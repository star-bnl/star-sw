#include <iostream.h>
#include <stdexcept>
#include <math.h>
using namespace std;
#include "StChain/StChain.h"
#include "StMiniMcMaker/StMiniMcMaker.h"
#include "StiMaker/StiMaker.h"
#include "StiMaker/MiniChain.h"
#include "StiMaker/MiniChainParameters.h"
#include "StiMaker/MainFrame.h"
#include "St_db_Maker/St_db_Maker.h"
#include "StDetectorDbMaker/StDetectorDbMaker.h"
#include "StTpcDb/StTpcDb.h"
#include "StTpcDb/StTpcDbMaker.h"
#include "StSvtDbMaker/StSvtDbMaker.h"
#include "StMcEventMaker/StMcEventMaker.h"
#include "StAssociationMaker/StAssociationMaker.h"
#include "St_geant_Maker/St_geant_Maker.h"
#include "StIOMaker/StIOMaker.h"
#include "StTreeMaker/StTreeMaker.h"
#include "StiMaker/StiDefaultToolkit.h"

ClassImp(MiniChain)

MiniChain::MiniChain()
	: _pars( new MiniChainParameters() ),
		_chain(0),
		_stiMaker(0),
		_ioMaker(0),
		_toolkit(new StiDefaultToolkit())
{ 
  StiToolkit::setToolkit(_toolkit);
  /*  _guiIoroker->setUnMarkedHitSize(.3);
  _guiIoroker->setUnMarkedHitColor(4);
  _guiIoroker->setUnMarkedHitStyle(8);
  _guiIoroker->setUpdateEachTrack(false);
  _guiIoroker->setMarkedHitSize(.3);
  _guiIoroker->setMarkedHitColor(2);
  _guiIoroker->setMarkedHitStyle(3);*/
}

MiniChain::~MiniChain()
{}

MiniChainParameters * MiniChain::getParameters()
{
	return _pars;
}

void MiniChain::run(int first, 
		    int nEvents, 
		    const char *  filePrefix,
		    const char ** fileList )
{
  int iList=0;
  cout << "MiniChain::Run(...) - INFO - Starting"<< endl
       << "     First Event Index:" << first      << endl 
       << "     #Events Requested:" << nEvents    << endl
       << "           File Prefix:" << filePrefix << endl
       << *_pars<<endl
       << "             File List:" << endl;
  while(fileList[iList]) 
    cout << "               "<<iList<<" :"<< fileList[iList++] << endl;
  _chain  = new StChain("StChain");
  try
    {
      setupInput(fileList);
      setupDatabase();
      StMcEventMaker * mcEventMaker;
      StAssociationMaker * assocMaker=0;
      if (_pars->doSimulation)
    	{
	    mcEventMaker = new StMcEventMaker();
	    assocMaker   = new StAssociationMaker("EgrStAssociationMaker");
	    }
      _stiMaker = StiMaker::instance();
      if (_pars->doSimulation) 
	    {
				_toolkit->setMcEnabled(true);
	      _stiMaker->setMcEventMaker(mcEventMaker);
	      _stiMaker->setAssociationMaker(assocMaker);
	    }
      if (_pars->useGui)      
				{
					_toolkit->setGuiEnabled(true);
					setupGui();
				}
      setupOutput(filePrefix,fileList[0]);
      eventLoop(first, first+nEvents);
    }
  catch (runtime_error & rte)
    {
      cout<< "MiniChain::eventLoop() - Run Time Error :" << rte.what() << endl;
    }
  catch (logic_error & logicError)
    {
      cout<< "MiniChain::eventLoop() - Logic Error :" << logicError.what() << endl;
    }
  catch (exception & e)
    {
      cout << "MiniChain::eventLoop() - Exception :"<< e.what()<<endl;
    }
}

void MiniChain::setupDatabase()
{  
  cout <<"MiniChain::setupDatabase() - INFO - Started" <<endl;
  St_db_Maker * dbaseMk  = new St_db_Maker("db","MySQL:StarDb","$STAR/StarDb");
  if (_pars->doSimulation) dbaseMk-> SetDateTime(20010801,000000);  // << trouble here
  if (_pars->useTpc) new StTpcDbMaker("tpcDb");
  if (_pars->useSvt) new StSvtDbMaker("svtDb");
  new StDetectorDbMaker("detDb");
  cout <<"MiniChain::setupDatabase() - INFO - Done" <<endl;
}

void MiniChain::setupSimulation()
{}


void MiniChain::eventLoop(int first, int last)
{
  cout <<"MiniChain::eventLoop() - INFO - Started" <<endl;
  _chain->PrintInfo();

  int status = _chain->Init();
  cout <<"MiniChain::eventLoop() - INFO - chain->Init() completed with status:"<<status <<endl;
  int theRunNumber = 100000;
  status = _chain->InitRun(theRunNumber);
  cout <<"MiniChain::eventLoop() - INFO - chain->InitRun() completed with status:"<<status <<endl;
  int iEvent=first;
  while (iEvent < last && status!=2) 
    {
      cout << "MiniChain::eventLoop() - INFO - Starting Event# " <<iEvent<<endl;
      _chain->Clear();
      status = _chain->Make(iEvent++); 
      if (status>1) 
	cout << " MiniChain::eventLoop() - WARNING - Event completed with Status:"
	     <<status<<endl;
    }
}


void MiniChain::setupInput(const char ** fileList)
{
  StFileI *setFiles =0;
  if (fileList) 
    {	//Normal case
      setFiles= new StFile(fileList);
    }
  //Instantiate IO Maker to read in StEvent
  _ioMaker = new StIOMaker("IO","r",setFiles); 
  //deactivate all branches
  _ioMaker->SetBranch("*",0,"0");	
  //_ioMaker->SetBranch("dstBranch",0,"r");
  //_ioMaker->SetBranch("runcoBranch",0,"r");
  // Select the event branch (StEvent)
  _ioMaker->SetBranch("eventBranch",0,"r");
  // In a simulation mode, throw in the Geant Branch
  if (_pars->doSimulation) _ioMaker->SetBranch("geantBranch",0,"r");
  // We need all the help we can get...
  _ioMaker->SetDebug();
  // Apparently wee need this
  St_geant_Maker *geantMaker  = new St_geant_Maker("geant");
  geantMaker->SetActive(kFALSE);
 
  //Maker to read events from file or database into StEvent
  //StEventMaker *readerMaker =  new StEventMaker("events","title");
}

/*!
  Setup the graphical interface used to visualize the reconstruction of tracks.
  This is done via the instantiation of a ROOT based class called MainFrame.
  MainFrame must be made "aware" of the chain and the ioMaker so it can read
  and process events on its own.
*/
void MiniChain::setupGui()
{  
  cout <<"MiniChain::setupGui - INFO - Started"<<endl;
  MainFrame * mainFrame=0;
  cout << "     Interactive mode selected."<<endl
       << "          ITTF Run in Gui Mode."<<endl;
  mainFrame = new MainFrame(gClient->GetRoot(), 400, 220);
  mainFrame->setStChain(_chain);
  mainFrame->setIoMaker(_ioMaker);
  cout <<"MiniChain::setupGui - INFO - Done"<<endl;
}

/*!
  Setup the desired output. In principle, one should be
  able to request StEvent, StMiniMcEvent, as well as  
  MiniDst.
 */
void MiniChain::setupOutput(const char * filePrefix,
			    const char * fileTemplate)
{
  cout <<"MiniChain::setupOutput - INFO - Started"<<endl;
  TString outFilePrefix = "MiniChain_";
  TString stEventFileSuffix = "event.root";
  TString MuDstSuffix = "MuDst.root";
  TString miniMcSuffix = "stiminimc.root";

  //Now clip prefix and 'event.root' from filename
  TString templateFile = fileTemplate;
  int fileBeginIndex = templateFile.Index(filePrefix,0);
  templateFile.Remove(0,fileBeginIndex);
  int fileEndIndex = templateFile.Index(stEventFileSuffix,0);
  templateFile.Remove(fileEndIndex, templateFile.Length());

  if (_pars->doStEventOutput) 
    {
      TString stEventFileName = outFilePrefix;
      stEventFileName.Append(templateFile);
      stEventFileName.Append(stEventFileSuffix);
      
      cout << "    Events written out to " <<stEventFileName << endl;
      StTreeMaker * outMaker = new StTreeMaker("EvOut","","bfcTree");
      outMaker->SetIOMode("w");
      outMaker->SetBranch("eventBranch",stEventFileName,"w");
      outMaker->IntoBranch("eventBranch","StEvent");
    }
  else
    cout << "    Events not written out"<<endl;

  if (_pars->doMiniMcEvent)
    {
      TString miniMcFileName = outFilePrefix;
      miniMcFileName.Append(templateFile);
      miniMcFileName.Append(miniMcSuffix);
      cout << "    MiniMcEvent will be called."<<endl
	   << "    MiniMcEvent output to file : " << miniMcFileName << endl;

      StAssociationMaker  * assocMakerIt = new StAssociationMaker();
      assocMakerIt->useInTracker();
      assocMakerIt->SetDebug();

      StMiniMcMaker * minimcMaker = new StMiniMcMaker;
      minimcMaker->setDebug();
      minimcMaker->setOutDir("./");
      minimcMaker->setFileName(miniMcFileName);
    }
  else
    {
      cout << "    MiniMcEvent will NOT be called."<<endl;
    }
  cout <<"MiniChain::setupOutput - INFO - Done"<<endl;
}


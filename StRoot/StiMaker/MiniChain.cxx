#include <Stiostream.h>
#include <stdexcept>
#include <math.h>
using namespace std;
#include "StChain/StChain.h"
#include "StMiniMcMaker/StMiniMcMaker.h"
#include "StiMaker/StiDefaultToolkit.h"
#include "StiMaker/StiMaker.h"
#include "StiMaker/MiniChain.h"
#include "StiMaker/StiMakerParameters.h"
#include "St_db_Maker/St_db_Maker.h"
#include "StDetectorDbMaker/StDetectorDbMaker.h"
#include "StTpcDb/StTpcDb.h"
#include "StTpcDb/StTpcDbMaker.h"
#include "StSvtDbMaker/StSvtDbMaker.h"
#include "StMcEventMaker/StMcEventMaker.h"
#include "StAssociationMaker/StAssociationMaker.h"
#include "St_geant_Maker/St_geant_Maker.h"
#include "StGenericVertexMaker/StGenericVertexMaker.h"
#include "StIOMaker/StIOMaker.h"
#include "StTreeMaker/StTreeMaker.h"
#include "StiGui/EventDisplay.h"

#include "Sti/StiKalmanTrackFinderParameters.h"
#include "TDataSet.h"


ClassImp(MiniChain)

MiniChain::MiniChain()
	: _pars( new StiMakerParameters() ),
	  _chain(0),
	  _stiMaker(0),
	  _ioMaker(0),
	  _toolkit(new StiDefaultToolkit())
{ 
  StiToolkit::setToolkit(_toolkit);


}

MiniChain::~MiniChain()
{}

StiMakerParameters * MiniChain::getParameters()
{
  return _pars;
}

void MiniChain::run(int first, 
		    int nEvents, 
		    const char *  filePrefix,
		    const char ** fileList,
		    const char * outfile)
{
  int iList=0;
  cout << "MiniChain::Run(...) -I- Starting"<< endl
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
      StAssociationMaker * assocMaker=0;		if(assocMaker){}
      if (_pars->doSimulation)
    	{
	  mcEventMaker = new StMcEventMaker();
	  //assocMaker   = new StAssociationMaker("EgrStAssociationMaker");
	}

      
      StGenericVertexMaker *genVert =0;
      genVert = new StGenericVertexMaker("GenericVertex");
      

      _stiMaker = new StiMaker("StiMaker");
      _stiMaker->setParameters(_pars);
      if (_pars->doSimulation) 
	{
	  //assocMaker   = new StAssociationMaker("EgrStAssociationMaker");
	  _toolkit->setMcEnabled(true);
	  _stiMaker->setMcEventMaker(mcEventMaker);
	  //_stiMaker->setAssociationMaker(assocMaker);
	}
      if (_pars->useGui)      
	{
	  _toolkit->setGuiEnabled(true);
	  setupGui();
	}
      setupOutput(filePrefix,fileList[0],outfile);
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
  cout <<"MiniChain::setupDatabase() -I- Started" <<endl;
  St_db_Maker * dbaseMk  = new St_db_Maker("db","MySQL:StarDb","$STAR/StarDb");
  if (_pars->doSimulation) 
		dbaseMk->SetDateTime("year2003"); //20010102,000000);  // << trouble here
  cout <<"MiniChain: I - Date Set: "
       <<dbaseMk->GetDateTime().AsString()
       <<endl;
  dbaseMk->SetDebug(1);

  if (_pars->useTpc) new StTpcDbMaker("tpcDb");
  if (_pars->useSvt) new StSvtDbMaker("svtDb");
  new StDetectorDbMaker("detDb");
  cout <<"MiniChain::setupDatabase() -I- Done" <<endl;
}

void MiniChain::setupSimulation()
{}


void MiniChain::eventLoop(int first, int last)
{
  cout <<"MiniChain::eventLoop() -I- Started" <<endl;
  _chain->PrintInfo();

  int status = _chain->Init();
  _chain->SetDebug();
  cout <<"MiniChain::eventLoop() -I- chain->Init() completed with status:"<<status <<endl;
  int theRunNumber = 100000;
  status = _chain->InitRun(theRunNumber);
  cout <<"MiniChain::eventLoop() -I- chain->InitRun() completed with status:"<<status <<endl;
  int iEvent=first;
  while (iEvent < last && status!=2) 
    {
      cout << "MiniChain::eventLoop() -I- Starting Event# " <<iEvent<<endl;
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
  This is done via the instantiation of a ROOT based class called EventDisplay
  EventDisplay must be made "aware" of the chain and the ioMaker so it can read
  and process events on its own.
*/
void MiniChain::setupGui()
{  
  cout <<"MiniChain::setupGui -I- Started"<<endl;
  if (!gClient)
    {
      cout <<"MiniChain::setupGui -E- gClient==0"<<endl;
      return;
    }
  EventDisplay * display = new EventDisplay("STAREventDisplay","STAR Event Display",_toolkit,gClient->GetRoot(), 400, 220);
  cout <<"MiniChain::setupGui -I- Display instantiated"<<endl;
  display->setStChain(_chain);
  display->setIoMaker(_ioMaker);
  if (_stiMaker)
    _stiMaker->setEventDisplay(display);
  else
    cout << "MiniChain::setupGui -E- _stiMaker==0"<<endl;
  cout <<"MiniChain::setupGui -I- Done"<<endl;
}



/*!
  Setup the desired output. In principle, one should be
  able to request StEvent, StMiniMcEvent, as well as  
  MiniDst.
 */
void MiniChain::setupOutput(const char * filePrefix,
			    const char * infile,
			    const char * fileTemplate)
{
  cout <<"MiniChain::setupOutput -I- Started"<<endl;

  bool useExplicitName=true;
  
  //make local copies
  TString inputfile = infile;
  TString outdir = fileTemplate;
  TString templateFile=fileTemplate;

  //get only the dir part
  if(fileTemplate==inputfile)
    {
      //no outfile specified, so now make output from input file names
      outdir = "./";
      //remove inputfile.Last('/')+1 characters, starting at 0....
      templateFile=inputfile.Remove(0,inputfile.Last('/')+1);
      useExplicitName=false;
    }


  if(outdir.Last('/')==-1)
    {
      outdir="./";
    }
  else
    {
      outdir.Remove(outdir.Last('/')+1,outdir.Capacity()-outdir.Last('/'));
      cout <<"Directory for output: "<<outdir<<endl;
    }

  //make minimc file
  //Since miniMcMaker redefines names, pass it what it expects....
  // a simple prefix + input file name
  TString miniMcName= infile;
  //get rid of any directory part...
  if(miniMcName.Last('/')!=-1) miniMcName.Remove(0,miniMcName.Last('/')+1);
  //miniMcName.Prepend("Sti_");

  TString eventName=templateFile;
  if(!useExplicitName)
    {
     //Now make output file name for StEvent file
     eventName.ReplaceAll("geant.root","event.root"); //replace "geant" if it's there
     eventName.ReplaceAll(".event.root","_sti.event.root");
     //.event.root now replaced with _sti.event.root
    }

  
  if (_pars->doStEventOutput) 
    {
      cout << "    Events written out to " <<eventName << endl;
      StTreeMaker * outMaker = new StTreeMaker("EvOut","","bfcTree");
      outMaker->SetIOMode("w");
      outMaker->SetBranch("eventBranch",eventName,"w");
      outMaker->IntoBranch("eventBranch","StEvent");
    }
  else
    cout << "    Events not written out"<<endl;

  if (_pars->doMiniMcEvent)
    {
      StAssociationMaker  * assocMakerIt = new StAssociationMaker();
      assocMakerIt->useInTracker();
      assocMakerIt->SetDebug();

      StMiniMcMaker * minimcMaker = new StMiniMcMaker;
      minimcMaker->SetDebug();

      minimcMaker->setOutDir(outdir);
      minimcMaker->setFileName(miniMcName);
    }
  else
    {
      cout << "    MiniMcEvent will NOT be called."<<endl;
    }
  cout <<"MiniChain::setupOutput -I- Done"<<endl;
}


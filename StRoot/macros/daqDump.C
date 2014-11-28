// $Id: daqDump.C,v 1.1 2010/01/22 21:09:43 fine Exp $
// *-- Author :    Valery Fine(fine@bnl.gov)   22/01/2010

/*! \brief  daqDump dumps the "Event Header" of the first \a maxEvents DAQ events from the input \a daqFile file 
 */
/// \author Valery Fine (fine@bnl.gov)
/// \date 22/01/2010
void daqDump(const char* daqFile="/star/rcf/test/daq/2009/115/st_physics_10115020_raw_5020001.daq", int maxEvents=-1)) 
{
  // Dump the "Event Header" for each DAQ Event from the input  daqFile
  // daqFile   - the DAQ file name 
  // maxEvents -  the max number  of events to  dump
  //            -1 process ALL events from the DAQ file
   
  // Load the shared libs
  gROOT->Macro("Load.C");  
  Info(__FILE__, "Enabling Logger...");
  gROOT->LoadMacro("LoadLogger.C");
  LoadLogger();
  gSystem->Load("RTS");
  gSystem->Load("StDaqLib"); 
  gSystem->Load("StDAQMaker"); 
  gSystem->Load("StIOMaker");
  chain = new StBFChain(); 
  cout << "Create chain " << chain->GetName() << endl;
  chain->SetFlags("in");
  StMaker *ioMk = new StIOMaker("IO","r",daqFile);
  ioMk->SetDebug(0);
  // Execute the chain
  chain->Init();    // start the chain
  while ( maxEvents &&( chain->MakeEvent() == kStOK ) )   { 
     chain->GetEvtHddr()->Print();
     if (maxEvents >=0 ) --maxEvents;
  }
  chain->Finish();  // close chain
  delete chain;
}

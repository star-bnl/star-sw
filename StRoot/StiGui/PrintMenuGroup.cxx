#include "StMemoryInfo.hh"
#include "Sti/Base/EditableFilter.h"
#include "Sti/Base/Factory.h"
#include "Sti/StiDetectorContainer.h"
#include "Sti/StiHitContainer.h"
#include "Sti/StiTrackContainer.h"
#include "Sti/StiHit.h"
#include "Sti/StiDetector.h"
#include "Sti/StiTrack.h"
#include "StiGui/PrintMenuGroup.h"
#include "StiGui/EventDisplay.h"

PrintMenuGroup::PrintMenuGroup(const string & name, 
			       const string & description, 
			       EventDisplay * display, 
			       int offset)
  : MenuGroup(name,description,display,offset)
{}

PrintMenuGroup::~PrintMenuGroup()
{}

void PrintMenuGroup::create(TGMenuBar *menuBar, TGLayoutHints *itemLayout)
{  
  cout<<"PrintMenuGroup::create() -I- Started"<<endl;
  TGPopupMenu * menu = new TGPopupMenu(getClient()->GetRoot());
  menu->AddLabel("Print");	
  menu->AddEntry("Options",       _offset+_cmdPrintOptions);
  menu->AddEntry("Detectors",     _offset+_cmdPrintDetectors);
  menu->AddEntry("Hits",          _offset+_cmdPrintHits); 
  menu->AddEntry("Tracks",        _offset+_cmdPrintTracks);
  menu->AddEntry("McTracks",      _offset+_cmdPrintMcTracks);
  menu->AddEntry("HitFilter",     _offset+_cmdPrintHitFilter);
  menu->AddEntry("TrackFilter",   _offset+_cmdPrintTrackFilter);
  menu->AddEntry("McTrackFilter", _offset+_cmdPrintMcTrackFilter);
  menu->AddEntry("MemoryInfo",    _offset+_cmdPrintMemoryInfo);
  menu->AddEntry("FactoryInfo",   _offset+_cmdPrintFactoryInfo);
  menu->Associate(getDisplay());
  menuBar->AddPopup("&Print", menu, itemLayout);
  cout<<"PrintMenuGroup::create() -I- Done"<<endl;
}

void PrintMenuGroup::dispatch(int option)
{
  switch (option-_offset)
    {
    case _cmdPrintOptions      : printOptions();       break;
    case _cmdPrintDetectors    : printDetectors();     break;
    case _cmdPrintHits         : printHits();          break;
    case _cmdPrintTracks       : printTracks();        break;
    case _cmdPrintMcTracks     : printMcTracks();      break;
    case _cmdPrintHitFilter    : printHitFilter();     break;
    case _cmdPrintTrackFilter  : printTrackFilter();   break;
    case _cmdPrintMcTrackFilter: printMcTrackFilter(); break;
    case _cmdPrintMemoryInfo   : printMemoryInfo();    break;
    case _cmdPrintFactoryInfo  : printFactoryInfo();   break;
    }
}

void PrintMenuGroup::printDetectors()
{
  cout << "PrintMenuGroup::printDetectors() -I- Started"<<endl;
  StiDetectorContainer * detectorContainer = getToolkit()->getDetectorContainer();
  if (detectorContainer)
    ;//cout << *detectorContainer;
  else
    cout << "PrintMenuGroup::printDetectors() -E- _detectorContainer==null"<<endl;
  cout << "PrintMenuGroup::printDetectors() -I- Done"<<endl;
}


void PrintMenuGroup::printHits()
{
  cout << "PrintMenuGroup::printHits() -I- Started"<<endl;
  StiHitContainer * hitContainer = getToolkit()->getHitContainer();
  if (hitContainer)
    cout << "HitContainer - track count:" << hitContainer->size() << endl;
  else
    cout << "PrintMenuGroup::printHits() -E- hitContainer==null"<<endl;
  cout << "PrintMenuGroup::printHits() -I- Done"<<endl;
}

void PrintMenuGroup::printTracks()
{
  cout << "PrintMenuGroup::printTracks() -I- Started"<<endl;
  StiTrackContainer * trackContainer = getToolkit()->getTrackContainer();
  if (trackContainer)
    cout << "TrackContainer - track count:" << trackContainer->getTrackCount(0) << endl;
  else
    cout << "PrintMenuGroup::printTracks() -E- trackContainer==null"<<endl;
  cout << "PrintMenuGroup::printTracks() -I- Done"<<endl;
}

void PrintMenuGroup::printMcTracks()
{
  cout << "PrintMenuGroup::printMcTracks() -I- Started"<<endl; 
  StiTrackContainer * mcTrackContainer = getToolkit()->getMcTrackContainer();
  if (mcTrackContainer)
    cout << "mcTrackContainer - track count:" << mcTrackContainer->getTrackCount(0) << endl;
  else
    cout << "PrintMenuGroup::printMcTracks() -E- mcTrackContainer==null"<<endl;
  cout << "PrintMenuGroup::printMcTracks() -I- Done"<<endl;
}

void PrintMenuGroup::printHitFilter()
{
  cout << "PrintMenuGroup::printHitFilter() -I- Started"<<endl;
  EditableFilter<StiHit>* hitFilter = getDisplay()->getHitFilter();
  if (hitFilter)
    ;//cout << *hitFilter << endl;
  else
    cout << "PrintMenuGroup::printMcTracks() -E- hitFilter==null"<<endl;
  cout << "PrintMenuGroup::printHitFilter() -I- Done"<<endl;
}

void PrintMenuGroup::printTrackFilter()
{
  cout << "PrintMenuGroup::printTrackFilter() -I- Started"<<endl;
  EditableFilter<StiTrack>* trackFilter = getDisplay()->getTrackFilter();
  if (trackFilter)
    cout << *trackFilter << endl;
  else
    cout << "PrintMenuGroup::printTrackFilter() -E- _trackFilter==null"<<endl;
  cout << "PrintMenuGroup::printTrackFilter() -I- Done"<<endl;
}

void PrintMenuGroup::printMcTrackFilter()
{
  cout << "PrintMenuGroup::printMcTrackFilter() -I- Started"<<endl;
  EditableFilter<StiTrack>* mcTrackFilter = getDisplay()->getMcTrackFilter();
  if (mcTrackFilter)
    cout << *mcTrackFilter << endl;
  else
    cout << "PrintMenuGroup::printMcTrackFilter() -E- mcTrackFilter==null"<<endl;
  cout << "PrintMenuGroup::printMcTrackFilter() -I- Done"<<endl;
}

void PrintMenuGroup::printMemoryInfo()
{
  cout << "PrintMenuGroup::printMemoryInfo() -I- Started"<<endl;
  StMemoryInfo::instance()->snapshot();
  StMemoryInfo::instance()->print();
  cout << "PrintMenuGroup::printMemoryInfo() -I- Done"<<endl;
}

void PrintMenuGroup::printFactoryInfo()
{
  cout << "PrintMenuGroup::printFactoryInfo() -I- Started"<<endl;
  if (getToolkit()->getDetectorFactory())
    {
      cout << "PrintMenuGroup::printFactoryInfo() -I- Detector Factory Information"<<endl
	   << "-----------------------------------------------------------------------------"<<endl
	//<< *getToolkit()->getDetectorFactory()
		 << "-----------------------------------------------------------------------------"<<endl;
    }
  else
    cout << "PrintMenuGroup::printFactoryInfo() -E- Detector factory not available" << endl;
  if (getToolkit()->getHitFactory())
    {
      cout << "PrintMenuGroup::printFactoryInfo() -I- Hit Factory Information"<<endl
		 << "-----------------------------------------------------------------------------"<<endl
	// << *getToolkit()->getHitFactory()
		 << "-----------------------------------------------------------------------------"<<endl;
    }
  else
    cout << "PrintMenuGroup::printFactoryInfo() -E- Hit factory not available" << endl;
  if (getToolkit()->getTrackNodeFactory())
    {
      cout << "PrintMenuGroup::printFactoryInfo() -I- Track Node Factory Information"<<endl
		 << "-----------------------------------------------------------------------------"<<endl
	// << *getToolkit()->getTrackNodeFactory()
		 << "-----------------------------------------------------------------------------"<<endl;
    }
  else
    cout << "PrintMenuGroup::printFactoryInfo() -E- Track Node factory not available" << endl;
  if (getToolkit()->getTrackFactory())
    {
      cout << "PrintMenuGroup::printFactoryInfo() -I- Track Factory Information"<<endl
		 << "-----------------------------------------------------------------------------"<<endl
	/// << *getToolkit()->getTrackFactory()
		 << "-----------------------------------------------------------------------------"<<endl;
    }
  else
    cout << "PrintMenuGroup::printFactoryInfo() -E- Track factory not available" << endl;
  if (getToolkit()->getMcTrackFactory())
    {
      cout << "PrintMenuGroup::printFactoryInfo() -I- MC Track Factory Information"<<endl
		 << "-----------------------------------------------------------------------------"<<endl
	// << *getToolkit()->getMcTrackFactory()
		 << "-----------------------------------------------------------------------------"<<endl;
    }
  else
    cout << "PrintMenuGroup::printFactoryInfo() -E- MC Track factory not available" << endl;
  cout << "PrintMenuGroup::printFactoryInfo() -I- Done"<<endl;
}

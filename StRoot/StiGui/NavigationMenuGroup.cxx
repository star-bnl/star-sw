#include "StiGui/NavigationMenuGroup.h"
#include "Sti/StiDetectorContainer.h"
#include "StiGui/EventDisplay.h"

NavigationMenuGroup::NavigationMenuGroup(const string& name, 
					 const string & description, 
					 EventDisplay * display, 
					 int offset)
  : MenuGroup(name,description,display,offset)
{}

NavigationMenuGroup::~NavigationMenuGroup()
{}

void NavigationMenuGroup::create(TGMenuBar *menuBar, TGLayoutHints *itemLayout)
{ 
  cout<<"NavigationMenuGroup::create() -I- Started"<<endl;
  TGPopupMenu * menu = new TGPopupMenu(getClient()->GetRoot());
  //menu->AddEntry("Navigator",          _offset+_cmdNavigator);
  //menu->AddSeparator();  
  menu->AddEntry("Move &In",           _offset+_cmdMoveIn);
  menu->AddEntry("Move &Out",          _offset+_cmdMoveOut);
  menu->AddEntry("Move &Plus Phi",     _offset+_cmdMovePlusPhi);
  menu->AddEntry("Move &Minus Phi",    _offset+_cmdMoveMinusPhi);
  menu->AddEntry("Set Layer",          _offset+_cmdSetLayer);
  menu->AddEntry("Set Layer and Angle",_offset+_cmdSetLayerAndAngle);
  menu->Associate(getDisplay());
  menuBar->AddPopup("&Navigation", menu, itemLayout);
  cout<<"NavigationMenuGroup::create() -I- Done"<<endl;
}

void NavigationMenuGroup::dispatch(int option)
{
  switch (option-_offset)
    {
    case _cmdMoveIn:           moveIn(); break;
    case _cmdMoveOut:          moveOut(); break;
    case _cmdMovePlusPhi:      movePlusPhi(); break;
    case _cmdMoveMinusPhi:     moveMinuPhi(); break;
    case _cmdSetLayer:         setLayer(); break;
    case _cmdSetLayerAndAngle: setLayerAndAngle(); break;
    }
}

void NavigationMenuGroup::moveIn()
{
  cout <<"NavigationMenuGroup::moveOut() -I- Started"<<endl;
  StiDetectorContainer* detectorContainer = getToolkit()->getDetectorContainer();
  cout <<"NavigationMenuGroup::moveOut() -I- DetectorContainer returned: "<<detectorContainer->moveIn()<<endl;
  _display->showCurrentDetector();
  cout <<"NavigationMenuGroup::moveOut() -I- Done"<<endl;
}

void NavigationMenuGroup::moveOut()
{
  cout <<"NavigationMenuGroup::moveOut() -I- Started"<<endl;
  StiDetectorContainer* detectorContainer = getToolkit()->getDetectorContainer();
  cout <<"NavigationMenuGroup::moveOut() -I- DetectorContainer returned: "<<detectorContainer->moveOut()<<endl;
  _display->showCurrentDetector();
  cout <<"NavigationMenuGroup::moveOut() -I- Done"<<endl;
}

void NavigationMenuGroup::movePlusPhi()
{
}

void NavigationMenuGroup::moveMinuPhi()
{
}

void NavigationMenuGroup::setLayer()
{
  /*
  cout <<"Function Not Currently Implemented"<<endl;
  setCurrentDetectorToDefault();
  cout <<"\nEnter position: (double)"<<endl;
  double position;
  cin >>position;
  cout <<"Setting to  position:\t"<<position<<endl;
  StiDetectorContainer*rdet = getToolkit()->getDetectorContainer();
  rdet->setToDetector(position);
  StiDetector* layer = **rdet;
  if (!layer) {
    cout <<"Error in setSectorAndPadrow"<<endl;
    return;
  }
  cout <<"Detector Set To: "<<layer->getName()<<endl;
  showCurrentDetector();*/
}

void NavigationMenuGroup::setLayerAndAngle()
{	
  cout <<"Function Not Currently Implemented"<<endl;
  /*setCurrentDetectorToDefault();
	cout <<"\nEnter position: (double)"<<endl;
	double position;
	cin >>position;

	cout <<"\nEnter angle: (double)"<<endl;
	double angle;
	cin >> angle;
	cout <<"Setting to  position:\t"<<position<<"\tangle:\t"<<angle<<endl;
	StiDetectorContainer*rdet = getToolkit()->getDetectorContainer();
	rdet->setToDetector(position, angle);
	StiDetector* layer = **rdet;
	if (!layer) {
		cout <<"Error in setSectorAndPadrow"<<endl;
		return;
	}
	cout <<"Detector Set To: "<<layer->getName()<<endl;
	showCurrentDetector();*/
}

#include "StiGui/NavigationMenuGroup.h"
#include "Sti/StiDetectorContainer.h"
#include "StiGui/EventDisplay.h"
#include "StiGui/StiRootDrawableDetector.h"

NavigationMenuGroup::NavigationMenuGroup(const string& name, 
					 const string & description, 
					 EventDisplay * display, 
					 int offset)
  : MenuGroup(name,description,display,offset)
{
  _colorSave = -1;
  _visibleSave = true;
}

NavigationMenuGroup::~NavigationMenuGroup()
{}

void NavigationMenuGroup::create(TGMenuBar *menuBar, TGLayoutHints *itemLayout)
{ 
  //cout<<"NavigationMenuGroup::create() -I- Started"<<endl;
  TGPopupMenu * menu = new TGPopupMenu(getClient()->GetRoot());
  menu->AddEntry("Navigator",          _offset+_cmdNavigator);
  menu->AddSeparator();
  menu->AddEntry("&Next Region",       _offset+_cmdMoveToNextRegion);
  menu->AddEntry("&Previous Region",   _offset+_cmdMoveToPreviousRegion);
  menu->AddEntry("Move &In",           _offset+_cmdMoveIn);
  menu->AddEntry("Move &Out",          _offset+_cmdMoveOut);
  menu->AddEntry("Move &Plus Phi",     _offset+_cmdMovePlusPhi);
  menu->AddEntry("Move &Minus Phi",    _offset+_cmdMoveMinusPhi);
  menu->AddEntry("Set Layer",          _offset+_cmdSetLayer);
  menu->AddEntry("Set Layer and Angle",_offset+_cmdSetLayerAndAngle);
  menu->Associate(getDisplay());
  menuBar->AddPopup("&Navigation", menu, itemLayout);
  //cout<<"NavigationMenuGroup::create() -I- Done"<<endl;
}


void NavigationMenuGroup::dispatch(int option)
{
  switch (option-_offset)
    {
    case _cmdNavigator:        launchNavigator(); break;
    case _cmdMoveToNextRegion:  moveToNextRegion(); break;
    case _cmdMoveToPreviousRegion: moveToPreviousRegion(); break;	
    case _cmdMoveIn:           moveIn(); break;
    case _cmdMoveOut:          moveOut(); break;
    case _cmdMovePlusPhi:      movePlusPhi(); break;
    case _cmdMoveMinusPhi:     moveMinusPhi(); break;
    case _cmdSetLayer:         setLayer(); break;
    case _cmdSetLayerAndAngle: setLayerAndAngle(); break;
    }
}

void NavigationMenuGroup::moveToNextRegion()
{
  hideCurrentDetector();
  StiDetectorContainer* detectorContainer = getToolkit()->getDetectorContainer();
  if (!detectorContainer->moveToNextRegion())
    cout <<"Navigation::moveToNextRegion() -I- Reached inner most volume."<<endl;
  showCurrentDetector();
}

void NavigationMenuGroup::moveToPreviousRegion()
{
  hideCurrentDetector();
  StiDetectorContainer* detectorContainer = getToolkit()->getDetectorContainer();
  if (!detectorContainer->moveToPreviousRegion())
    cout <<"Navigation::moveToPreviousRegion() -I- Reached inner most volume."<<endl;
  showCurrentDetector();
}

void NavigationMenuGroup::moveIn()
{
  hideCurrentDetector();
  StiDetectorContainer* detectorContainer = getToolkit()->getDetectorContainer();
  if (!detectorContainer->moveIn())
    cout <<"Navigation::moveIn() -I- Reached inner most volume."<<endl;
  showCurrentDetector();
}

void NavigationMenuGroup::moveOut()
{
  hideCurrentDetector();
  StiDetectorContainer* detectorContainer = getToolkit()->getDetectorContainer();
  if (!detectorContainer->moveOut())
    cout <<"Navigation::moveOut() -I- Reached outer most volume."<<endl;
  showCurrentDetector();
}

void NavigationMenuGroup::movePlusPhi()
{
  hideCurrentDetector();
  StiDetectorContainer* detectorContainer = getToolkit()->getDetectorContainer();
  detectorContainer->movePlusPhi();
  showCurrentDetector();
}

void NavigationMenuGroup::moveMinusPhi()
{
  hideCurrentDetector();
  StiDetectorContainer* detectorContainer = getToolkit()->getDetectorContainer();
  detectorContainer->moveMinusPhi();
  showCurrentDetector();
}

void NavigationMenuGroup::setLayer()
{
  hideCurrentDetector();
  //setCurrentDetectorToDefault();
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
  showCurrentDetector();
}

void NavigationMenuGroup::setLayerAndAngle()
{	
  //setCurrentDetectorToDefault();
  cout <<"\nEnter position: (double)"<<endl;
  hideCurrentDetector();
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
  showCurrentDetector();
}

void NavigationMenuGroup::launchNavigator()
{
  TGCompositeFrame * compositeFrame = getCompositeFrame();
  TGLayoutHints * layout = new TGLayoutHints(kLHintsBottom | kLHintsLeft,  0,0,1,0);
  getDisplay()->AddFrame(compositeFrame, layout); 
  getDisplay()->MapSubwindows();
  getDisplay()->Resize(getDisplay()->GetDefaultSize()); 
  getDisplay()->MapWindow(); 
}

TGCompositeFrame * NavigationMenuGroup::getCompositeFrame()
{  
  TGCompositeFrame * frame = new TGCompositeFrame(getDisplay(), 60, 20, kHorizontalFrame | kSunkenFrame);
  TGTextButton * button;
  TGLayoutHints * layout = new TGLayoutHints(kLHintsTop | kLHintsLeft, 2, 0, 2, 2);

  button = new TGTextButton(frame, "Next Region", _offset+_cmdMoveToNextRegion);
  button->Associate(getDisplay()); 
  frame->AddFrame(button,layout);
  
  button = new TGTextButton(frame, "Previous Region", _offset+_cmdMoveToPreviousRegion);
  button->Associate(getDisplay()); 
  frame->AddFrame(button,layout);
  
  button = new TGTextButton(frame, "Move In", _offset+_cmdMoveIn);
  button->Associate(getDisplay()); 
  frame->AddFrame(button,layout);
  
  button = new TGTextButton(frame, "Move Out", _offset+_cmdMoveOut);
  button->Associate(getDisplay()); 
  frame->AddFrame(button,layout);
  
  button = new TGTextButton(frame, "Move Plus Phi", _offset+_cmdMovePlusPhi);
  button->Associate(getDisplay()); 
  frame->AddFrame(button,layout);
  
  button = new TGTextButton(frame, "Move Minus Phi", _offset+_cmdMoveMinusPhi);
  button->Associate(getDisplay()); 
  frame->AddFrame(button,layout);
  //frame->AddFrame(button,layout);
  //frame->Resize(button->GetDefaultWidth()+40, frame->GetDefaultHeight());
  return frame;
}

///Highlight the current detector
void NavigationMenuGroup::showCurrentDetector()
{
  StiDetector * currentDetector = **(getToolkit()->getDetectorContainer());
  if (!currentDetector)
    {
      cout << "EventDisplay::showCurrentDetector() -W- currentDetector==0" <<endl;
      return;
    }
  StiRootDrawableDetector * det = dynamic_cast<StiRootDrawableDetector *>(currentDetector);
  cout <<"Current Detector:\t"<<det->getName()<<endl;
  //store current visibility and color state
  _colorSave = det->getColor();
  _visibleSave = det->isVisible();
  // make red and visible
  det->setVisible(true);
  det->setColor(2);
  getDisplay()->draw();
}

///Restore the normal display state of the current detector
void NavigationMenuGroup::hideCurrentDetector()
{
  StiDetector * currentDetector = **(getToolkit()->getDetectorContainer());
  if (!currentDetector)
    {
      cout << "EventDisplay::showCurrentDetector() -W- currentDetector==0" <<endl;
      return;
    }
  if (_colorSave<0) return;
  StiRootDrawableDetector * det = dynamic_cast<StiRootDrawableDetector *>(currentDetector);
  //restore original color and visibility state
  det->setVisible(_visibleSave);
  det->setColor(_colorSave);
  getDisplay()->draw();
}

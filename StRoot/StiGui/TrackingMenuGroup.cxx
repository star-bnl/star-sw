#include "StiGui/TrackingMenuGroup.h"
#include "StiGui/EventDisplay.h"
#include "Sti/StiTrackFinder.h"
#include "TGFrame.h"

TrackingMenuGroup::TrackingMenuGroup(const string & name, 
				     const string & description, 
				     EventDisplay * display, 
				     int offset)
  : MenuGroup(name,description,display,offset)
{}

TrackingMenuGroup::~TrackingMenuGroup()
{}

void TrackingMenuGroup::create(TGMenuBar *menuBar, TGLayoutHints *itemLayout)
{ 
  cout<<"TrackingMenuGroup::create() -I- Started"<<endl;
  TGPopupMenu * menu = new TGPopupMenu(getClient()->GetRoot());
  menu->AddLabel("Access to tracking functions");
  menu->AddEntry("Finish Track", _offset+_cmdFinishTrack);
  menu->AddEntry("Finish Event", _offset+_cmdFinishEvent);
  menu->AddEntry("Next Event",   _offset+_cmdNextEvent);
  menu->AddEntry("N-Event Step", _offset+_cmdNEvent);
  menu->AddEntry("Reset Event",  _offset+_cmdResetEvent);
  menu->Associate(getDisplay());
  menuBar->AddPopup("&Tracking", menu, itemLayout);  
  cout<<"TrackingMenuGroup::create() -I- Done"<<endl;
}

void TrackingMenuGroup::dispatch(int option)
{
  switch (option-_offset)
    {
    case _cmdResetEvent:   resetEvent();  break;
    case _cmdFinishTrack:  finishTrack(); break;
    case _cmdFinishEvent:  finishEvent(); break;
    case _cmdNextEvent:    nextEvent();   break;
    case _cmdNEvent:       nEvent();      break;
    }
}

void TrackingMenuGroup::finishTrack()
{ 
  cout<<"TrackingMenuGroup::finishTrack() -I- Started"<<endl;
  getToolkit()->getTrackFinder()->findNextTrack(); 
  getDisplay()->draw();
  cout<<"TrackingMenuGroup::finishTrack() -I- Done"<<endl;
}

void TrackingMenuGroup::finishEvent()
{ 	
  cout<<"TrackingMenuGroup::finishEvent() -I- Started"<<endl;
  getToolkit()->getTrackFinder()->findTracks();
  getDisplay()->draw();
  cout<<"TrackingMenuGroup::finishEvent() -I- Done"<<endl;
}

void TrackingMenuGroup::nextEvent()
{ 
  cout <<"TrackingMenuGroup::nextEvent() -I- Started"<<endl;
  //setCurrentDetectorToDefault();    
  StChain * chain = getDisplay()->getChain();
  if (!chain) 
    {
      cout << " TrackingMenuGroup::nextEvent() -F- chain==0"<<endl;
      throw runtime_error("TrackingMenuGroupstepToNextEvent() - FATAL - _chain==0");
    }
  chain->Clear();
  chain->Make();
  getDisplay()->draw();
  cout <<"TrackingMenuGroup:nextEvent() -I- Done"<<endl;
}

void TrackingMenuGroup::nEvent()
{  
  //setCurrentDetectorToDefault();    
  cout <<"\nEnter number of events to process (int) "<<endl;
  int nevents;
  cin >> nevents;
  StChain * chain = getDisplay()->getChain();
  if (!chain) 
    {
      cout << " TrackingMenuGroup::nEvent() -F- chain==0"<<endl;
      throw runtime_error("TrackingMenuGroupstepToNextEvent() -F- _chain==0");
    }
  for (int i=0; i<nevents; ++i) 
    { 
      chain->Clear();
      chain->Make();
    }
  getDisplay()->draw();
}


void TrackingMenuGroup::resetEvent()
{  
  cout <<"TrackingMenuGroup::resetEvent() -I- Started"<<endl;
  getToolkit()->getTrackFinder()->reset(); 
  getDisplay()->reset();
  getDisplay()->draw();
  cout <<"TrackingMenuGroup::resetEvent() -I- Done"<<endl;
}

TGCompositeFrame * TrackingMenuGroup::getCompositeFrame()
{  
  TGCompositeFrame * frame = new TGCompositeFrame(getDisplay(), 60, 20, kHorizontalFrame | kSunkenFrame);
  TGTextButton * button;

  button = new TGTextButton(frame,"Finish &Track", _offset+_cmdFinishTrack);
  button->Associate(getDisplay());
  button->SetToolTipText("Finish The Current Track");
  frame->AddFrame(button, new TGLayoutHints(kLHintsTop | kLHintsLeft, 2, 0, 2, 2));
  
  button = new TGTextButton(frame,"Finish &Event", _offset+_cmdFinishEvent);
  button->Associate(getDisplay());
  button->SetToolTipText("Finish The Current Event");
  frame->AddFrame(button, new TGLayoutHints(kLHintsTop | kLHintsLeft, 2, 0, 2, 2));
  
  button = new TGTextButton(frame,"Reset Event", _offset+_cmdResetEvent);
  button->Associate(getDisplay());
  button->SetToolTipText("Reset The Current Event");
  frame->AddFrame(button, new TGLayoutHints(kLHintsTop | kLHintsLeft, 2, 0, 2, 2));
    
  button = new TGTextButton(frame, "&Next Event", _offset+_cmdNextEvent);
  button->Associate(getDisplay());
  button->SetToolTipText("Step To Next Event");
  frame->AddFrame(button, new TGLayoutHints(kLHintsTop | kLHintsRight, 2, 0, 2, 2));

  return frame;
}

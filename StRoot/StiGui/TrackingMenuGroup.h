#ifndef TrackingMenuGroup_H_INCLUDED
#define TrackingMenuGroup_H_INCLUDED
#include "MenuGroup.h"

class TrackingMenuGroup : public MenuGroup
{
 public:

  enum cmds
    {
      _cmdFinishTrack=1,_cmdFinishEvent,_cmdNextEvent,_cmdNEvent,_cmdResetEvent
    };
  
  TrackingMenuGroup(const string& name, 
		    const string & description, 
		    EventDisplay * display, 
		    int offset);
  ~TrackingMenuGroup();
  void create(TGMenuBar *menuBar, TGLayoutHints *itemLayout);
  void dispatch(int); 
  virtual TGCompositeFrame * getCompositeFrame();
  
 protected:
  void finishTrack();
  void finishEvent();
  void nextEvent();
  void nEvent();
  void resetEvent();
};

#endif

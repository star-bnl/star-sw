#ifndef ViewMenuGroup_H_INCLUDED
#define ViewMenuGroup_H_INCLUDED
#include "MenuGroup.h"

class ViewMenuGroup : public MenuGroup
{
 public:	
  enum _cmds
    {
      _cmdSetDefaultView=1
    };
  
  ViewMenuGroup(const string& name, 
		  const string & description, 
		  EventDisplay * display, 
		  int offset);
  ~ViewMenuGroup();
  void create(TGMenuBar *menuBar, TGLayoutHints *itemLayout);
  void dispatch(int);  

  void setDefaultView();
  void setSelectedView(int option);

};

#endif


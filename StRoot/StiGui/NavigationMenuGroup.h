#ifndef NavigationMenuGroup_H_INCLUDED
#define NavigationMenuGroup_H_INCLUDED
#include "MenuGroup.h"

class NavigationMenuGroup : public MenuGroup
{
 public:

  enum cmds
    {
      _cmdMoveIn=1, _cmdMoveOut, _cmdMovePlusPhi, _cmdMoveMinusPhi, _cmdSetLayer, _cmdSetLayerAndAngle
    };

  NavigationMenuGroup(const string & name, 
		      const string & description, 
		      EventDisplay * display, 
		      int offset);
  ~NavigationMenuGroup();
  void create(TGMenuBar *menuBar, TGLayoutHints *itemLayout);
  void dispatch(int);

  
 protected:

  void moveIn();
  void moveOut();
  void movePlusPhi();
  void moveMinuPhi();
  void setLayer();
  void setLayerAndAngle();

};

#endif


#ifndef NavigationMenuGroup_H_INCLUDED
#define NavigationMenuGroup_H_INCLUDED
#include "MenuGroup.h"

class NavigationMenuGroup : public MenuGroup
{
 public:

  enum cmds
    {
      _cmdNavigator=1, _cmdMoveIn, _cmdMoveOut, _cmdMovePlusPhi, 
      _cmdMoveMinusPhi, _cmdSetLayer, _cmdSetLayerAndAngle, 
      _cmdMoveToNextRegion, _cmdMoveToPreviousRegion
    };
  
  NavigationMenuGroup(const string & name, 
		      const string & description, 
		      EventDisplay * display, 
		      int offset);
  ~NavigationMenuGroup();
  void create(TGMenuBar *menuBar, TGLayoutHints *itemLayout);
  void dispatch(int);
  
  
 protected:
  void showCurrentDetector();
  void hideCurrentDetector();
  void moveToNextRegion();
  void moveToPreviousRegion();
  void moveIn();
  void moveOut();
  void movePlusPhi();
  void moveMinusPhi();
  void setLayer();
  void setLayerAndAngle();
  void launchNavigator();
  TGCompositeFrame * getCompositeFrame();

  bool _visibleSave;
  int  _colorSave;
  bool _launched;
};

#endif


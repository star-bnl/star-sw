#ifndef HelpMenuGroup_H_INCLUDED
#define HelpMenuGroup_H_INCLUDED
#include "MenuGroup.h"

class HelpMenuGroup : public MenuGroup
{
 public:
  enum cmds
    {
      _cmdHelpContents, _cmdHelpSearch, _cmdHelpAbout
    };

  HelpMenuGroup(const string& name, const string & description, EventDisplay * display, int offset);
  ~HelpMenuGroup();
  void create(TGMenuBar *menuBar, TGLayoutHints *itemLayout);
  void dispatch(int);

  void helpContents();
  void helpSearch();
  void helpAbout();
};

#endif


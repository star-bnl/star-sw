#ifndef OptionMenuGroup_H_INCLUDED
#define OptionMenuGroup_H_INCLUDED
#include "MenuGroup.h"

class OptionMenuGroup : public MenuGroup
{
 public:	
  enum _cmds
    {
      _cmdPrintOptions=1,
      _cmdDisplayOptions,
      _cmdMessengerOptions,
      _cmdSeedFinderOptions,
      _cmdTrackFinderOptions,
      _cmdHitFilterOptions,
      _cmdMcHitFilterOptions,
      _cmdMcTrackLoaderFilterOptions,
      _cmdGuiTrackFilterOptions,
      _cmdGuiMcTrackFilterOptions,
      _cmdFinderTrackFilterOptions
    };
  
  OptionMenuGroup(const string& name, 
		  const string & description, 
		  EventDisplay * display, 
		  int offset);
  ~OptionMenuGroup();
  void create(TGMenuBar *menuBar, TGLayoutHints *itemLayout);
  void dispatch(int);  

  void setPrintOptions();
  void setDisplayOptions();
  void setMessengerOptions();
  void setSeedFinderOptions();
  void setTrackFinderOptions();
  void setHitFilterOptions();
  void setMcHitFilterOptions();
  void setMcTrackLoaderFilterOptions();
  void setGuiTrackFilterOptions();
  void setGuiMcTrackFilterOptions();
  void setFinderTrackFilterOptions();
};

#endif


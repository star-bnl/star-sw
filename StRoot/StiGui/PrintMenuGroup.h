#ifndef PrintMenuGroup_H_INCLUDED
#define PrintMenuGroup_H_INCLUDED
#include "StiGui/MenuGroup.h"

class PrintMenuGroup : public MenuGroup
{
 public:	
  enum _cmds 
    {
      _cmdPrintOptions=1,
      _cmdPrintDetectors,
      _cmdPrintHits, 
      _cmdPrintTracks,
      _cmdPrintEff,
      _cmdPrintMcTracks,
      _cmdPrintHitFilter,
      _cmdPrintTrackFilter,
      _cmdPrintMcTrackFilter,
      _cmdPrintMemoryInfo,
      _cmdPrintFactoryInfo
    };
  
  PrintMenuGroup(const string& name, const string & description, EventDisplay * display, int offset);
  ~PrintMenuGroup();
  void create(TGMenuBar *menuBar, TGLayoutHints *itemLayout);
  void dispatch(int);  
  void printOptions();
  void printDetectors();
  void printHits(); 
  void printEff();
  void printTracks();
  void printMcTracks();
  void printHitFilter();
  void printTrackFilter();
  void printMcTrackFilter();
  void printMemoryInfo();
  void printFactoryInfo();
};

#endif


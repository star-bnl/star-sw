#ifndef FileMenuGroup_H_INCLUDED
#define FileMenuGroup_H_INCLUDED
#include "MenuGroup.h"

class FileMenuGroup : public MenuGroup
{
 public:

  enum cmds
    {
      _cmdOpenFile=1,_cmdSave,_cmdSaveAs,_cmdExit
    };

  FileMenuGroup(const string& name, const string & description, EventDisplay * display, int offset);
  ~FileMenuGroup();
  void create(TGMenuBar *menuBar, TGLayoutHints *itemLayout);
  void dispatch(int);

 protected:

  void openFile();
  void saveAs();
  void save();
  void exit();

};

#endif


#include "StiGui/FileMenuGroup.h"
#include "StiGui/EventDisplay.h"

FileMenuGroup::FileMenuGroup(const string& name, 
			     const string & description, 
			     EventDisplay * display, 
			     int offset)
  : MenuGroup(name,description,display,offset)
{}

FileMenuGroup::~FileMenuGroup()
{}

void FileMenuGroup::create(TGMenuBar *menuBar, TGLayoutHints *itemLayout)
{
  cout << "FileMenuGroup::create() -I- Started"<<endl;
  TGPopupMenu * menu = new TGPopupMenu(getClient()->GetRoot());
  menu->AddEntry("&Open...",    _offset+_cmdOpenFile);
  menu->AddEntry("&Save",       _offset+_cmdSave);
  menu->AddEntry("S&ave as...", _offset+_cmdSaveAs);
  menu->AddEntry("&Close",      -1);
  menu->AddSeparator();
  menu->AddEntry("&Print",      -1);
  menu->AddEntry("P&rint setup...", -1);
  menu->AddSeparator();
  menu->AddEntry("E&xit",       _offset+_cmdExit);
  menu->DisableEntry(_offset+_cmdSave);  
  menu->DisableEntry(_offset+_cmdSaveAs);  
  cout << "FileMenuGroup::create() -I- Associate"<<endl;
  menu->Associate(getDisplay());
  menuBar->AddPopup("&File", menu, itemLayout);
  cout << "FileMenuGroup::create() -I- Done"<<endl;
}

void FileMenuGroup::dispatch(int option)
{
  switch (option-_offset)
    {
    case _cmdOpenFile: openFile(); break;
    case _cmdSave:     save(); break;
    case _cmdSaveAs:   saveAs(); break;
    case _cmdExit:     exit(); break;
    }
}

void FileMenuGroup::openFile()
{
  /*
  static TString dir("/star/data22/ITTF/");
  TGFileInfo fi;
  fi.fFileTypes = filetypes;
  fi.fIniDir    = StrDup(dir);
  new TGFileDialog(getClient()->GetRoot(), this, kFDOpen, &fi);
  printf("Open file: %s (dir: %s)\n", fi.fFilename,
	 fi.fIniDir);
  dir = fi.fIniDir;
  _ioMaker->Close();
  _ioMaker->SetFile(fi.fFilename);
  stepToNextEvent();
  */
}

void FileMenuGroup::saveAs()
{}

void FileMenuGroup::save()
{}

void FileMenuGroup::exit()
{
  gApplication->Terminate(0);
}

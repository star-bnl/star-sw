#include "HelpMenuGroup.h"
#include "StiGui/EventDisplay.h"

HelpMenuGroup::HelpMenuGroup(const string& name, 
			     const string & description, 
			     EventDisplay * display, 
			     int offset)
  : MenuGroup(name,description,display,offset)
{}

HelpMenuGroup::~HelpMenuGroup()
{}

void HelpMenuGroup::create(TGMenuBar *menuBar, TGLayoutHints *itemLayout)
{
  cout<<"HelpMenuGroup::create() -I- Started"<<endl;
  TGPopupMenu * menu = new TGPopupMenu(getClient()->GetRoot());
  menu->AddEntry("&Contents",  _offset+_cmdHelpContents);
  menu->AddEntry("&Search...", _offset+_cmdHelpSearch);
  menu->AddSeparator();
  menu->AddEntry("&About",     _offset+_cmdHelpAbout);
  menu->Associate(getDisplay());
  menuBar->AddPopup("&Help", menu, itemLayout); 
  cout<<"HelpMenuGroup::create() -I- Done"<<endl;
}

void HelpMenuGroup::dispatch(int option)
{
  switch (option-_offset)
    {
    case _cmdHelpContents      : break;
    case _cmdHelpSearch        : break;
    case _cmdHelpAbout         : helpAbout(); break;
    }
}

void HelpMenuGroup::helpAbout()
{
  cout << "EventDisplay::helpAbout() -I- Started/Done"<<endl;
  cout << "STI Event Display" 
       << "written by C. Pruneau and M. Miller"
       << "2001-2003."
       <<endl;
}

void HelpMenuGroup::helpSearch()
{}
  
void HelpMenuGroup::helpContents()
{}

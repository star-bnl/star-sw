#include "Sti/Base/EditableParameters.h"
#include "Sti/StiTrackSeedFinder.h"
#include "Sti/StiKalmanTrackFinder.h"
#include "Sti/StiKalmanTrackFitter.h"
#include "StiGui/OptionMenuGroup.h"
#include "StiGui/EventDisplay.h"
#include "StiGui/EventDisplayParameters.h"
#include "StiMaker/MessengerOptionsDialog.h"
#include "StiMaker/StiOptionFrame.h"

OptionMenuGroup::OptionMenuGroup(const string & name, 
				 const string & description, 
				 EventDisplay * display, 
				 int offset)
  : MenuGroup(name,description,display,offset)
{}

OptionMenuGroup::~OptionMenuGroup()
{}

void OptionMenuGroup::create(TGMenuBar *menuBar, TGLayoutHints *itemLayout)
{ 
  cout << "OptionMenuGroup::create() -I- Started"<<endl;
  TGPopupMenu * menu = new TGPopupMenu(getClient()->GetRoot());
  menu->AddLabel("Options");
  menu->AddEntry("Print",                _offset+_cmdPrintOptions);
  menu->AddEntry("Display",              _offset+_cmdDisplayOptions);
  menu->AddEntry("Messenger",            _offset+_cmdMessengerOptions);
  menu->AddEntry("Seed Finder",          _offset+_cmdSeedFinderOptions);
  menu->AddEntry("Kalman Finder",        _offset+_cmdTrackFinderOptions);
  menu->AddEntry("Kalman Fitter",        _offset+_cmdTrackFitterOptions);
  menu->AddEntry("Hit Filter",           _offset+_cmdHitFilterOptions);
  menu->AddEntry("MC Hit Filter",        _offset+_cmdMcHitFilterOptions);
  menu->AddEntry("McLoader Track Filter",_offset+_cmdMcTrackLoaderFilterOptions);
  menu->AddEntry("Gui Track Filter",     _offset+_cmdGuiTrackFilterOptions);
  menu->AddEntry("Gui MC Track Filter",  _offset+_cmdGuiMcTrackFilterOptions);
  menu->AddEntry("Finder Track Filter",  _offset+_cmdFinderTrackFilterOptions);
  menu->Associate(getDisplay());
  menuBar->AddPopup("&Option", menu, itemLayout);
  cout << "OptionMenuGroup::create() -I- Done"<<endl;
}

void OptionMenuGroup::dispatch(int option)
{
  switch (option-_offset)
    {
    case _cmdPrintOptions               : setPrintOptions();         break;
    case _cmdDisplayOptions             : setDisplayOptions();       break;
    case _cmdMessengerOptions           : setMessengerOptions();     break;
    case _cmdSeedFinderOptions          : setSeedFinderOptions();    break;
    case _cmdTrackFinderOptions         : setTrackFinderOptions();   break;
    case _cmdTrackFitterOptions         : setTrackFitterOptions();   break;
    case _cmdMcHitFilterOptions         : setMcHitFilterOptions();   break;
    case _cmdHitFilterOptions           : setHitFilterOptions();     break;
    case _cmdMcTrackLoaderFilterOptions : setMcTrackLoaderFilterOptions();   break;
    case _cmdGuiTrackFilterOptions      : setGuiTrackFilterOptions();        break;
    case _cmdGuiMcTrackFilterOptions    : setGuiMcTrackFilterOptions();      break;
    case _cmdFinderTrackFilterOptions   : setFinderTrackFilterOptions();     break;
    }
}

void OptionMenuGroup::setPrintOptions()
{
  /* no ops */
}
 
void OptionMenuGroup::setDisplayOptions()
{
  cout << "OptionMenuGroup::setDisplayOptions() -I- Started" <<endl;
  EditableParameters * pars = dynamic_cast<EditableParameters *>(getDisplay()->getOptions());
  if (pars)
    new StiOptionFrame(getClient()->GetRoot(), getDisplay(), pars);
  else
    cout << "OptionMenuGroup::setDisplayOptions() -E- event display options not available"<<endl;
  cout << "OptionMenuGroup::setDisplayOptions() -I- Done" <<endl;

}
 
void OptionMenuGroup::setMessengerOptions()
{ 
  cout << "OptionMenuGroup::setMessengerOptions() -I- Started" <<endl;
  new MessengerOptionsDialog(getClient()->GetRoot(), getDisplay(), 400, 200);
  cout << "OptionMenuGroup::setMessengerOptions() -I- Done" <<endl;
}

void OptionMenuGroup::setSeedFinderOptions()
{	
  cout << "OptionMenuGroup::setSeedFinderOptions() -I- Started" <<endl;
  new StiOptionFrame(getClient()->GetRoot(), getDisplay(), &getToolkit()->getTrackSeedFinder()->getParameters() );
  cout << "OptionMenuGroup::setSeedFinderOptions() -I- Done" <<endl;
}

void OptionMenuGroup::setTrackFinderOptions()
{
  cout << "OptionMenuGroup::setTrackFinderOptions() -I- Started" <<endl;
  new StiOptionFrame(getClient()->GetRoot(), getDisplay(), &getToolkit()->getTrackFinder()->getParameters() );
  cout << "OptionMenuGroup::setTrackFinderOptions() -I- Done" <<endl;
}

void OptionMenuGroup::setTrackFitterOptions()
{
  cout << "OptionMenuGroup::setTrackFitterOptions() -I- Started" <<endl;
  new StiOptionFrame(getClient()->GetRoot(), getDisplay(), &getToolkit()->getTrackFitter()->getParameters() );
  cout << "OptionMenuGroup::setTrackFitterOptions() -I- Done" <<endl;
}

void OptionMenuGroup::setHitFilterOptions()
{	
  cout << "OptionMenuGroup::setHitFilterOptions() -I- Started" <<endl;
  EditableParameters * pars = dynamic_cast<EditableParameters *>(getDisplay()->getHitFilter());
  if (pars)
    {
      new StiOptionFrame(getClient()->GetRoot(), getDisplay(), pars); 
      getDisplay()->draw();
    }
  else
    cout << "OptionMenuGroup::setHitFinderOptions() -E- Hit Finder options not available"<<endl;
  cout << "OptionMenuGroup::setHitFinderOptions() -I- Done" <<endl;
}

void OptionMenuGroup::setMcHitFilterOptions()
{	
  cout << "OptionMenuGroup::setMcHitFilterOptions() -I- Started" <<endl;
  EditableParameters * pars = dynamic_cast<EditableParameters *>(getDisplay()->getMcHitFilter());
  if (pars)
    {
      new StiOptionFrame(getClient()->GetRoot(), getDisplay(), pars);
      getDisplay()->draw();
    }
  else
    cout << "OptionMenuGroup::setMcHitFinderOptions() -E- Mc Hit Finder options not available"<<endl;
  cout << "OptionMenuGroup::setMcHitFinderOptions() -I- Done" <<endl;
}

void OptionMenuGroup::setGuiTrackFilterOptions()
{
  cout << "OptionMenuGroup::setTrackFilterOptions() -I- Started" <<endl;
  EditableParameters * pars = dynamic_cast<EditableParameters *>(getDisplay()->getTrackFilter());
  if (pars)
    {
      new StiOptionFrame(getClient()->GetRoot(), getDisplay(), pars);
      getDisplay()->draw();
    }
  else
    cout << "OptionMenuGroup::setTrackFilterOptions() -E- Track Filter options not available"<<endl;
  cout << "OptionMenuGroup::setTrackFilterOptions() -I- Done" <<endl;
}
   

void OptionMenuGroup::setGuiMcTrackFilterOptions()
{
  cout << "OptionMenuGroup::setMcTrackFilterOptions() -I- Started" <<endl;
  EditableParameters * pars = dynamic_cast<EditableParameters *>(getDisplay()->getMcTrackFilter());
  if (pars)
    {
      new StiOptionFrame(getClient()->GetRoot(), getDisplay(), pars);
       getDisplay()->draw();
    }
  else
    cout << "OptionMenuGroup::setMcTrackFilterOptions() -E- MC Track Filter options not available"<<endl;
  cout << "OptionMenuGroup::setMcTrackFilterOptions() -I- Done" <<endl;
}
   

void OptionMenuGroup::setFinderTrackFilterOptions()
{
  cout << "OptionMenuGroup::setFinderTrackFilterOptions() -I- Started" <<endl;
  EditableParameters * pars = dynamic_cast<EditableParameters *>(getToolkit()->getFinderTrackFilter());
  if (pars)
    new StiOptionFrame(getClient()->GetRoot(), getDisplay(), pars);
  else
    cout << "OptionMenuGroup::setFinderTrackFilterOptions() -E- Finder Track Filter options not available"<<endl;
  cout << "OptionMenuGroup::setFinderTrackFilterOptions() -I- Done" <<endl;
}
   


void OptionMenuGroup::setMcTrackLoaderFilterOptions()
{
  cout << "OptionMenuGroup::setFinderTrackFilterOptions() -I- Started" <<endl;
  EditableParameters * pars = dynamic_cast<EditableParameters *>(getToolkit()->getLoaderTrackFilter());
  if (pars)
    new StiOptionFrame(getClient()->GetRoot(), getDisplay(), pars);
  else
    cout << "OptionMenuGroup::setFinderTrackFilterOptions() -E- Finder Track Filter options not available"<<endl;
  cout << "OptionMenuGroup::setFinderTrackFilterOptions() -I- Done" <<endl;
}
   


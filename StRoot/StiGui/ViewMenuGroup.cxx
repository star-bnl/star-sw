#include "StiGui/ViewMenuGroup.h"
#include "StiGui/EventDisplay.h"
#include "StiGui/StiDetectorViews.h"
#include "StiGui/StiDetectorView.h"
#include "Sti/StiDetectorGroups.h"

ViewMenuGroup::ViewMenuGroup(const string & name, 
			     const string & description, 
			     EventDisplay * display, 
			     int offset)
  : MenuGroup(name,description,display,offset)
{}

ViewMenuGroup::~ViewMenuGroup()
{}

void ViewMenuGroup::create(TGMenuBar *menuBar, TGLayoutHints *itemLayout)
{ 
  cout << "ViewMenuGroup::create() -I- Started"<<endl;
  TGPopupMenu * menu = new TGPopupMenu(getClient()->GetRoot());
  menu->AddEntry("SetDefault",  _offset+_cmdSetDefaultView);
  cout << "ViewMenuGroup::create() -I- Get Views"<<endl;
  StiDetectorViews * views = getDisplay()->getDetectorViews();
  if (!views)
    {
      cout << "ViewMenuGroup::create() -E- views==0"<<endl;
      throw runtime_error("ViewMenuGroup::create() -E- views==0");
    }
  views->clear();
  StiDetectorGroups<StEvent,StMcEvent> * groups = getToolkit()->getDetectorGroups();
  if (!groups)
    {
      cout << "ViewMenuGroup::create() -E- groups==0"<<endl;
      throw runtime_error("ViewMenuGroup::create() -E- groups==0");
    }
  vector<StiDetectorGroup<StEvent,StMcEvent>*>::const_iterator groupIter;
  vector<StiDetectorView*>::const_iterator viewIter;
  int group=0; 
  TGPopupMenu * subMenu;
  for (groupIter=groups->begin();
       groupIter!=groups->end();
       ++groupIter)
    {
      cout << "ViewMenuGroup::create() -I- Setup for group:"<<(*groupIter)->getName()<<endl;
      int view = 0;
      StiDetectorViews *groupViews = (*groupIter)->getDetectorViews();
      if (!groupViews)
	{
	  cout << "ViewMenuGroup::create() -E- groupViews==0"<<endl;
	  throw runtime_error("ViewMenuGroup::create() -E- groupViews==0");
	}
      subMenu = new TGPopupMenu(getClient()->GetRoot());
      menu->AddPopup(groupViews->getName().c_str(),subMenu);
      for (viewIter=groupViews->begin();
	   viewIter!=groupViews->end();
	   ++viewIter)
	{
	  StiDetectorView * aView = *viewIter;
	  cout <<"ViewMenuGroup::create() -I- DetectorView:"<<aView->getName()<<endl;
	  subMenu->AddEntry(aView->getName().c_str(),_offset+10+10*group+view);
	  ++view;
	}
      ++group;
      subMenu->Associate(getDisplay());
    }
  menu->Associate(getDisplay());
  menuBar->AddPopup("&View", menu, itemLayout);

  setDefaultView();
  cout << "ViewMenuGroup::create() -I- Done"<<endl;
}

void ViewMenuGroup::dispatch(int option)
{
  cout << "ViewMenuGroup::dispatch(int option) -I- Started with option=="<<option<<endl;
  switch (option-_offset)
    {
    case _cmdSetDefaultView : 
      cout << "setDefault"<<endl;
setDefaultView(); break;
    default: 
      int viewOption = option-_offset-10;
      cout << "setSelectedView:"<<viewOption<<endl;
      if (viewOption<1000 && viewOption>=0) setSelectedView(viewOption);
    }  
  getDisplay()->draw();
}

void ViewMenuGroup::setDefaultView()
{
  StiDetectorViews * views = getDisplay()->getDetectorViews();
  views->clear();
  StiDetectorGroups<StEvent,StMcEvent> * groups = getToolkit()->getDetectorGroups();
  vector<StiDetectorGroup<StEvent,StMcEvent>*>::const_iterator iter;
  for (iter=groups->begin();iter!=groups->end();iter++)
    {
      views->add( (*iter)->getDetectorViews()->getDefaultView() );
      (*iter)->getDetectorViews()->getDefaultView()->activate();
    }
}

void ViewMenuGroup::setSelectedView(int option)
{   
  cout << "setSelectedView():"<<option<<endl;
  StiDetectorViews * views = getDisplay()->getDetectorViews();
  StiDetectorGroups<StEvent,StMcEvent> * groups = getToolkit()->getDetectorGroups();
  int group = option/10;
  int view  = option - 10*group;
  StiDetectorViews * groupViews = (*groups)[group]->getDetectorViews();
  (*views)[group] = (*groupViews)[view];
  (*groupViews)[view]->activate(); 
}
 

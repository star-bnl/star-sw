#include "StiGui/MenuGroup.h"
#include "StiGui/EventDisplay.h"
#include "Sti/StiToolkit.h"


MenuGroup::MenuGroup(const string& name, 
		     const string & description, 
		     EventDisplay * display,
		     int offset)
  : Named(name),
    Described(description),
    _display(display),
    _offset(offset)
{}

MenuGroup::~MenuGroup()
{}

StiToolkit   * MenuGroup::getToolkit()
{
  return _display->getToolkit();
}

TGClient     * MenuGroup::getClient()
{
  return _display->getClient();
}

///Returns a null pointer to a composite frame
///Derived classes should build a real object.
TGCompositeFrame *  MenuGroup::getCompositeFrame()
{
  return 0;
}

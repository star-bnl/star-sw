#ifndef MenuGroup_H_INCLUDED
#define MenuGroup_H_INCLUDED
#include "Sti/Base/Named.h"
#include "Sti/Base/Described.h"
#include <TROOT.h>
#include <TApplication.h>
#include <TVirtualX.h>

#include <TGListBox.h>
#include <TGClient.h>
#include <TGFrame.h>
#include <TGIcon.h>
#include <TGLabel.h>
#include <TGButton.h>
#include <TGTextEntry.h>
#include <TGMsgBox.h>
#include <TGMenu.h>
#include <TGCanvas.h>
#include <TGComboBox.h>
#include <TGTab.h>
#include <TGSlider.h>
#include <TGDoubleSlider.h>
#include <TGFileDialog.h>
#include <TGTextEdit.h>
#include <TGShutter.h>
#include <TGProgressBar.h>
#include <TGNumberEntry.h>
#include <TRootEmbeddedCanvas.h>

class EventDisplay;
class StiToolkit;
class TGCompositeFrame;

class MenuGroup : public Named, public Described
{
 public:
  MenuGroup(const string& name, const string & description, EventDisplay * display, int offset);
  virtual ~MenuGroup();
  virtual void create(TGMenuBar *menuBar, TGLayoutHints *itemLayout)=0;
  virtual void dispatch(int)=0;
  EventDisplay * getDisplay();
  int getOffset() const;
  StiToolkit * getToolkit();
  TGClient   * getClient();
  virtual TGCompositeFrame * getCompositeFrame();

 protected:

  EventDisplay * _display;
  int            _offset;
};

inline EventDisplay * MenuGroup::getDisplay()
{
  return _display;
}

inline int MenuGroup::getOffset() const
{
  return _offset;
}



#endif


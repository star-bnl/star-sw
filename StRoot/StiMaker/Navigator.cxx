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
#include "Navigator.h"
#include "MainFrame.h"

Navigator::Navigator(const TGWindow *p, const TGWindow *main,
		     UInt_t w, UInt_t h, UInt_t options) :
  TGTransientFrame(p, main, w, h, options)
{
  int ax, ay;
  
  ChangeOptions((GetOptions() & ~kVerticalFrame) | kHorizontalFrame);
  
  f1 = new TGCompositeFrame(this, 60, 20, kVerticalFrame | kFixedWidth);
  
  mMoveIn = new TGTextButton(f1, "Move In", 1);
  mMoveOut = new TGTextButton(f1, "Move Out", 2);
  mMovePlusPhi = new TGTextButton(f1, "Move Plus Phi", 3);
  mMoveMinusPhi = new TGTextButton(f1, "Move Minus Phi", 4);
  mClose = new TGTextButton(f1, "Close", 5);
  
  f1->Resize(mMoveMinusPhi->GetDefaultWidth()+40, GetDefaultHeight());
  
  mMoveIn->Associate(this);
  mMoveOut->Associate(this);
  mMovePlusPhi->Associate(this);
  mMoveMinusPhi->Associate(this);
  mClose->Associate(this);
  
  fL1 = new TGLayoutHints(kLHintsTop | kLHintsExpandX, 2, 2, 3, 0);
  fL21 = new TGLayoutHints(kLHintsTop | kLHintsRight,  2, 5, 10, 0);
  
  f1->AddFrame(mMoveIn, fL1);
  f1->AddFrame(mMoveOut, fL1);
  f1->AddFrame(mMovePlusPhi, fL1);
  f1->AddFrame(mMoveMinusPhi, fL1);
  f1->AddFrame(mClose, fL1);
  
  AddFrame(f1, fL21);
  
  MapSubwindows();
  Resize(GetDefaultSize());
  Window_t wdum;
  gVirtualX->TranslateCoordinates(main->GetId(), GetParent()->GetId(),
				  (((TGFrame *) main)->GetWidth() - fWidth) >> 1,
				  (((TGFrame *) main)->GetHeight() - fHeight) >> 1,
				  ax, ay, wdum);
  Move(ax, ay);
  SetWindowName("ITTF Detector Navigator");
  MapWindow();
}

Navigator::~Navigator()
{
  delete mMoveIn;
  delete mMoveOut;
  delete mMovePlusPhi;
  delete mMoveMinusPhi;
  delete mClose;
  delete f1;
  delete fL1;
  delete fL21;
}

void Navigator::CloseWindow()
{
  delete this;
}

/// Process messages sent to this dialog.
Bool_t Navigator::ProcessMessage(Long_t msg, Long_t parm1, Long_t)
{
  switch(GET_MSG(msg)) 
    {
    case kC_COMMAND:
      switch(GET_SUBMSG(msg)) 
	{
	case kCM_BUTTON:
	  switch(parm1) 
	    {
	    case 1:  MainFrame::instance()->moveIn(); break;
	    case 2:  MainFrame::instance()->moveOut(); break;
	    case 3:  MainFrame::instance()->movePlusPhi();  break;
	    case 4:  MainFrame::instance()->moveMinusPhi(); break;
	    case 5:  CloseWindow(); break;
	    }
	  break;
	case kCM_RADIOBUTTON:
	case kCM_CHECKBUTTON:	    break;
	default:    break;
	}
      break;
    default: break;
    }
  return kTRUE;
}


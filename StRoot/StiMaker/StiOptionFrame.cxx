#include "StiOptionFrame.h"
#include "Sti/Parameter.h"
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
#include "Sti/EditableParameters.h"
#include "StiMaker/RootEditableParameter.h"

StiOptionFrame::StiOptionFrame(const TGWindow * p, 
			       const TGWindow * main, 
			       EditableParameters * parameters)
  : TGTransientFrame(p, main, 10, 10, kHorizontalFrame)
{
    // build widgets
    frame1 = new TGVerticalFrame(this, 200, 300);
    layout1 = new TGLayoutHints(kLHintsTop | kLHintsLeft, 2, 2, 2, 2);
    AddFrame(frame1, layout1);

    layout2 = new TGLayoutHints(kLHintsCenterY | kLHintsRight, 2, 2, 2, 2);
    initialize();
    frame2 = new TGVerticalFrame(this, 200, 500);
    layout3 = new TGLayoutHints(kLHintsTop | kLHintsLeft, 2, 2, 2, 2);
    AddFrame(frame2, layout3);
    applyButton = new TGTextButton(frame2, " Apply ", 2);
    applyButton->Associate(this);
    frame2->AddFrame(applyButton, layout3);
    
    closeButton = new TGTextButton(frame2, " Close ", 1);
    closeButton->Associate(this);
    frame2->AddFrame(closeButton, layout3);
    
    // set dialog box title
    SetWindowName(parameters->getName().c_str());
    SetIconName(parameters->getName().c_str());
    SetClassHints("Options", "Options");
    // resize & move to center
    MapSubwindows();
    UInt_t width = GetDefaultWidth();
    UInt_t height = GetDefaultHeight();
    Resize(width, height);

    Int_t ax;
    Int_t ay;
    if (main) 
      {
	Window_t wdum;
	gVirtualX->TranslateCoordinates(main->GetId(), GetParent()->GetId(),
					(((TGFrame *) main)->GetWidth() -
					 fWidth) >> 1,
					(((TGFrame *) main)->GetHeight() -
					 fHeight) >> 1, ax, ay, wdum);
      } 
    else 
      {
	UInt_t root_w, root_h;
	gVirtualX->GetWindowSize(fClient->GetRoot()->GetId(), ax, ay,
				 root_w, root_h);
	ax = (root_w - fWidth) >> 1;
	ay = (root_h - fHeight) >> 1;
      }
    Move(ax, ay);
    SetWMPosition(ax, ay);
    // make the message box non-resizable
    SetWMSize(width, height);
    SetWMSizeHints(width, height, width, height, 0, 0);
    SetMWMHints(kMWMDecorAll | kMWMDecorResizeH | kMWMDecorMaximize |
		kMWMDecorMinimize | kMWMDecorMenu,
		kMWMFuncAll | kMWMFuncResize | kMWMFuncMaximize |
		kMWMFuncMinimize, kMWMInputModeless);
    
    MapWindow();
}

void StiOptionFrame::initialize()
{
  ParameterIterator it;
  for (it=parameters->begin();it!=parameters->end();it++)
    {
      RootEditableParameter * par = static_cast<RootEditableParameter *>(*it);
      if (!par)
	{
	  cout << "StiOptionFrame::initialize() - ERROR - static_cast failed."<<endl;
	  break;
	}
      //fNumericEntries.back().second->SetFormat(TGNumberFormat::kNESRealTwo);
      //TGNumberFormat::kNESRealFour,TGNumberFormat::kNESInteger
      
      frame.push_back( new TGHorizontalFrame(frame1, 200, 30) );
      frame1->AddFrame(frame.back(), layout2);
      if (par->getType()==Parameter::Boolean)
	{
	  TGCheckButton* tempButton = new TGCheckButton(frame.back(),
							new TGHotString(par->getDescription().c_str()), -1);
	  par->setCheckButton(tempButton);
	  if (par->getValue()==true)
	    tempButton->SetState(kButtonDown);
	  frame.back()->AddFrame(tempButton, layout2);
	}
      else
	{
	  TGNumberEntry * numberEntry = new TGNumberEntry( frame.back() );
	  par->setNumberEntry(numberEntry);
	  numberEntry->SetNumber( par->getValue() );
	  numberEntry->SetFormat(TGNumberFormat::kNESRealOne, 
				 TGNumberFormat::kNEAPositive);
	  numberEntry->SetLimits(TGNumberFormat::kNELLimitMinMax, 
				 par->getMinimum(),
				 par->getMaximum() );
	  numberEntry->Associate(this);

	  frame.back()->AddFrame(numberEntry, layout2);
	  fLabel.push_back( new TGLabel(frame.back(), par->getDescription().c_str() ));
	  frame.back()->AddFrame(fLabel.back(), layout2);
	}
    }
}

StiOptionFrame::~StiOptionFrame()
{
  /*  const Parameter * it;
  for (it=parameters->begin();it!=parameters->end();it++)
    {
      const RootEditableParameter * iter = static_cast<RootEditableParameter *>(it);
      if (it->getType()==Parameter::Boolean)
	delete iter->getCheckButton();
      else
	delete iter->getNumberEntry();
    }
  */
  delete applyButton;
  delete closeButton;
  delete frame1;
  delete frame2;
  delete layout1;
  delete layout2;
  delete layout3;
}

void StiOptionFrame::closeWindow()
{
  delete this;
}

void StiOptionFrame::apply()
{
  ParameterIterator it;
  for (it=parameters->begin();it!=parameters->end();it++)
    {
      RootEditableParameter * par = static_cast<RootEditableParameter *>(*it);
      if (par->getType()==Parameter::Boolean)
	par->setValue(par->getCheckButton()->GetState()==kButtonDown );
      else
	par->setValue(par->getNumberEntry()->GetNumber());
    }
}

Bool_t StiOptionFrame::ProcessMessage(Long_t msg, Long_t parm1, Long_t parm2)
{
  switch (GET_MSG(msg)) 
    {
    case kC_COMMAND:
      {
	switch (GET_SUBMSG(msg)) 
	  {
	  case kCM_BUTTON:
	    {
	      switch (parm1) 
		{
		case 1:  closeWindow();  break;
		case 2:  apply();    break;
		}
	    }
	    break;
	  }
      }
      break;
    }
  return kTRUE;
}


///\File StiFilterOptionFrame.cxx
///\Author Claude A Pruneau (Wayne State U) 
///\Date 2004

#include "StiFilterOptionFrame.h"
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
#include "Sti/Base/EditableParameters.h"
#include "StiMaker/RootEditableParameter.h"

StiFilterOptionFrame::StiFilterOptionFrame(const TGWindow * p, 
															 const TGWindow * main, 
															 EditableParameters * params)
  : TGTransientFrame(p, main, 10, 10, kHorizontalFrame)
{
  parameters = params;
  //cout << "StiFilterOptionFrame() -I- Starting" << endl;
	// build widgets
	frame1 = new TGVerticalFrame(this, 200, 300);
	layout1 = new TGLayoutHints(kLHintsTop | kLHintsLeft, 2, 2, 2, 2);
	AddFrame(frame1, layout1);
	//cout << "StiFilterOptionFrame() -I- frame1 added" << endl;

	layout2 = new TGLayoutHints(kLHintsCenterY | kLHintsRight, 2, 2, 2, 2);
	initialize();
	frame2 = new TGVerticalFrame(this, 200, 500);
	layout3 = new TGLayoutHints(kLHintsTop | kLHintsLeft, 2, 2, 2, 2);
	AddFrame(frame2, layout3);
	applyButton = new TGTextButton(frame2, " Apply ", 2);
	applyButton->Associate(this);
	frame2->AddFrame(applyButton, layout3);
    
	//cout << "StiFilterOptionFrame() -I- frame2 added" << endl;

	closeButton = new TGTextButton(frame2, " Close ", 1);
	closeButton->Associate(this);
	frame2->AddFrame(closeButton, layout3);
	//cout << "StiFilterOptionFrame() -I- closeButton added" << endl;

	// set dialog box title
	SetWindowName(parameters->getName().c_str());
	SetIconName(parameters->getName().c_str());
	SetClassHints("Options", "Options");
	//cout << "StiFilterOptionFrame() -I- Window/Icon/Class hints added" << endl;

	// resize & move to center
	MapSubwindows();
	UInt_t width = GetDefaultWidth();
	UInt_t height = GetDefaultHeight();
	Resize(width, height);
	//cout << "StiFilterOptionFrame() -I- Resize done" << endl;

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

void StiFilterOptionFrame::initialize()
{
  cout << "StiFilterOptionFrame::initialize() -I- Starting" << endl;
  ParameterIterator it = parameters->begin();
	while (it!=parameters->end())
    {
      TGHorizontalFrame * hFrame = new TGHorizontalFrame(frame1, 200, 30);
      frame1->AddFrame(hFrame, layout2);
			for (int i=0;i<3;++i)
				{
					RootEditableParameter * par = static_cast<RootEditableParameter *>(*it);
					if (!par) throw runtime_error("StiFilterOptionFrame::initialize() -E- par==0");
					if (par->getType()==Parameter::Boolean)
						{
							TGCheckButton* tempButton = new TGCheckButton(hFrame,new TGHotString(par->getDescription().c_str()), -1);
							par->setCheckButton(tempButton);
							if (par->getBoolValue()==true)	tempButton->SetState(kButtonDown);
							hFrame->AddFrame(tempButton, layout2);
						}
					else
						{
							TGNumberEntry * numberEntry = new TGNumberEntry( hFrame );
							par->setNumberEntry(numberEntry);
							if (par->getType()==Parameter::Integer)
								{	// Integer
									numberEntry->SetNumber( par->getIntValue() );
									numberEntry->SetFormat(TGNumberFormat::kNESInteger,TGNumberFormat::kNEAAnyNumber);
								}
							else
								{ // Double 
									numberEntry->SetNumber( par->getDoubleValue() );
									if (par->getIncrement()==2)	// two significant figures
										numberEntry->SetFormat(TGNumberFormat::kNESRealTwo,TGNumberFormat::kNEAAnyNumber);
									else // one only
										numberEntry->SetFormat(TGNumberFormat::kNESRealOne,TGNumberFormat::kNEAAnyNumber);
								}
							numberEntry->SetLimits(TGNumberFormat::kNELLimitMinMax, par->getMinimum(),par->getMaximum() );
							numberEntry->Associate(this);
							hFrame->AddFrame(numberEntry, layout2);
							TGLabel * label = new TGLabel(hFrame,par->getDescription().c_str());
							label->SetSize(TGDimension(50,20));
							hFrame->AddFrame( label, layout2);
						}
					++it;
				}
		}
}

StiFilterOptionFrame::~StiFilterOptionFrame()
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

void StiFilterOptionFrame::closeWindow()
{
  apply();
  delete this;
}

void StiFilterOptionFrame::apply()
{
  cout << "StiFilterOptionFrame::apply() -I-  Started" << endl;
  ParameterIterator it;
  for (it=parameters->begin();it!=parameters->end();it++)
    {
      RootEditableParameter * par = static_cast<RootEditableParameter *>(*it);
      if (par->getType()==Parameter::Boolean)
				par->setValue(par->getCheckButton()->GetState()==kButtonDown );
      else
					par->setValue(par->getNumberEntry()->GetNumber());
    }
	cout << "PARAMETERS NEW VALUES:" << endl
			 << "===============================================================" << endl
			 << *parameters;	
  parameters->notify();
  cout << "StiFilterOptionFrame::apply() -I- Done" << endl;  
}

Bool_t StiFilterOptionFrame::ProcessMessage(Long_t msg, Long_t parm1, Long_t parm2)
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


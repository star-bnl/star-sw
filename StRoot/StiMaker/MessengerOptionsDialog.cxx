#include "MessengerOptionsDialog.h"


MessengerOptionsDialog::MessengerOptionsDialog(const TGWindow *p, const TGWindow *main,
					       UInt_t w, UInt_t h, UInt_t options) :
  TGTransientFrame(p, main, w, h, options),
  fRedTextGC(TGButton::GetDefaultGC())
{
  ULong_t red;
  fClient->GetColorByName("red", red);
  fRedTextGC.SetForeground(red);
  int ax, ay;
  ChangeOptions((GetOptions() & ~kVerticalFrame) | kHorizontalFrame);
  _frame1 = new TGCompositeFrame(this, 60, 20, kVerticalFrame | kFixedWidth);
  _frame2 = new TGCompositeFrame(this, 60, 20, kVerticalFrame);
  _frame3 = new TGCompositeFrame(_frame2, 60, 20, kHorizontalFrame);
  _applyButton = new TGTextButton(_frame1, "&Apply", 1, fRedTextGC());
  // Change background of _applyButton to green
  ULong_t green;
  fClient->GetColorByName("green", green);
  _applyButton->ChangeBackground(green);
  _closeButton = new TGTextButton(_frame1, "&Close", 2);
  _frame1->Resize(_applyButton->GetDefaultWidth()+40, GetDefaultHeight());
  _applyButton->Associate(this);
  _closeButton->Associate(this);
  _layout1 = new TGLayoutHints(kLHintsTop | kLHintsExpandX, 2, 2, 3, 0);
  _layout2 = new TGLayoutHints(kLHintsTop | kLHintsRight | kLHintsExpandX, 2, 5, 0, 2);
  _layout21 = new TGLayoutHints(kLHintsTop | kLHintsRight,  2, 5, 10, 0);
  _frame1->AddFrame(_applyButton, _layout1);
  _frame1->AddFrame(_closeButton, _layout1);    
  AddFrame(_frame1, _layout21);
  //--------- create check and radio buttons groups
  fG1 = new TGGroupFrame(_frame3, new TGString("Message Streams"));
  _layout3 = new TGLayoutHints(kLHintsTop | kLHintsLeft |
			  kLHintsExpandX | kLHintsExpandY,
			  2, 2, 2, 2);
  _layout4 = new TGLayoutHints(kLHintsTop | kLHintsLeft,
			  0, 0, 5, 0);

  for(unsigned int iMessageType=0; iMessageType<MessageType::getNtypes();
      iMessageType++){
    MessageType *pType = MessageType::getTypeByIndex(iMessageType);

    _options.push_back( MessengerPair( pType->getCode(),
				       new TGCheckButton(fG1, new TGHotString(pType->getName().c_str()), -1) ));
  }

  for (unsigned int i=0; i<_options.size(); ++i) 
    {
      //cout <<"Adding Frame: "<<i<<endl;
      fG1->AddFrame(_options[i].second, _layout4);
    }

  Messenger* msgr = Messenger::instance();
  //Set current state here!
  for (unsigned int i=0; i<_options.size(); ++i) {
    unsigned int theBit = msgr->getRoutingBits(_options[i].first);
    cout <<"Bit for message: "<<_options[i].first<<" = "<<theBit<<endl;
    if (theBit) {
      _options[i].second->SetState(kButtonDown);
    }
  }

  _frame3->AddFrame(fG1, _layout3);
  _frame2->AddFrame(_frame3, _layout1);
  AddFrame(_frame2, _layout2);
  MapSubwindows();
  Resize(GetDefaultSize());

  // position relative to the parent's window
  Window_t wdum;
  gVirtualX->TranslateCoordinates(main->GetId(), GetParent()->GetId(),
				  (((TGFrame *) main)->GetWidth() - fWidth) >> 1,
				  (((TGFrame *) main)->GetHeight() - fHeight) >> 1,
				  ax, ay, wdum);
  Move(ax, ay);
  SetWindowName("Activate ITTF Message Streams");
  MapWindow();
}

// Order is important when deleting frames. Delete children first,
// parents last.
MessengerOptionsDialog::~MessengerOptionsDialog()
{
  // Delete widgets created by dialog.

  delete _applyButton; delete _closeButton;
  for (unsigned int i=0; i<_options.size(); ++i) {
    delete _options[i].second;
  }
  delete _frame3; 
  delete _frame2; 
  delete _frame1;
  delete _layout1; 
  delete _layout2; 
  delete _layout3; 
  delete _layout4;
  delete _layout21;
}

/// Close dialog in response to window manager close.
void MessengerOptionsDialog::CloseWindow()
{
  delete this;
}

/// Process messages sent to this dialog.
Bool_t MessengerOptionsDialog::ProcessMessage(Long_t msg, Long_t parm1, Long_t)
{
  switch(GET_MSG(msg)) 
    {
    case kC_COMMAND:
      switch(GET_SUBMSG(msg)) 
	{
	case kCM_BUTTON:
	  switch(parm1) 
	    {
	    case 1:
	      updateMessenger();		
	      break;
	    case 2:
	      CloseWindow();
	      break;
	  }
	  break;
	}
    case kCM_RADIOBUTTON:
    case kCM_CHECKBUTTON:
    default:      break;
    }
  return kTRUE;
}

void MessengerOptionsDialog::updateMessenger()
{
  Messenger* msgr = Messenger::instance();
  for (unsigned int j=0; j<_options.size(); ++j) 
    {
      if (_options[j].second->GetState() == kButtonDown) 
	{
	  cout <<"Button "<<j<<" is checked with enum: "<<_options[j].first<<endl;
	  msgr->setRoutingBits( _options[j].first );
	}
      else 
	{
	  msgr->clearRoutingBits( _options[j].first );
	}
    }
}

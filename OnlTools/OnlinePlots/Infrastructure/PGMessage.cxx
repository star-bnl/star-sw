//Author: Dariusz Mi$kowiec
//Date:   1999

///////////////////////////////////////////////////////////////////////////////
// PGMessage is a help text viewer with a "close" button. Text comes from a file.
// It's a know off of DGHelp class
///////////////////////////////////////////////////////////////////////////////
//
//
//

# include "PGMessage.h"


#define ENTHEI 20
#define ENTWID 10

ClassImp(PGMessage) ;

//-----------------------------------------------------------------------------
PGMessage::PGMessage(const char* message, bool exitOnClose, Pixel_t color ) :
        TGTransientFrame(gClient->GetRoot(), 0, 0, 0,kVerticalFrame)
{

    fMemList = new TObjArray();
    fL0 = new TGLayoutHints(kLHintsTop | kLHintsExpandX, 2, 2, 2, 2);
    fL1 = new TGLayoutHints(kLHintsTop | kLHintsExpandX | kLHintsExpandY,
                            2, 2, 2, 2);
    fTextView = new TGTextView(this, 550, 240, kChildFrame, 3);
    fCloseButton = new TGTextButton(this,"Close",71);

    AddFrame(fTextView,fL1);
    AddFrame(fCloseButton,fL0);

    fMemList->Add(fL0);
    fMemList->Add(fL1);
    fMemList->Add(fTextView);
    fMemList->Add(fCloseButton);

    //  fTextView->LoadFile(message);
    fTextView->AddLine(message);
    fTextView->SetBackgroundColor(color);
    MapSubwindows();
    this->Resize(this->GetDefaultSize());
    MapWindow();
    SetWindowName("EVP Client help");


    if ( exitOnClose ) {
      fCloseButton->Connect("Clicked()","PGMessage", (PGMessage*)this, "Exit()");    
    }

}
//-----------------------------------------------------------------------------
void PGMessage::AddLine(const char* line) {
  fTextView->AddLine(line);
  Resize();
}
//-----------------------------------------------------------------------------
PGMessage::~PGMessage()
{
    fMemList->Delete();
}
//-----------------------------------------------------------------------------
Bool_t PGMessage::ProcessMessage(Long_t msg, Long_t parm1, Long_t parm2)
{

    fTextView->ProcessMessage(msg, parm1, parm2);

    switch(GET_MSG(msg))
    {
    case kC_COMMAND:
        switch(GET_SUBMSG(msg))
        {
        case kCM_BUTTON:
            if (parm1 == 71)
                delete this;
            break;
        default:
            break;
        }
        break;
    default:
        break;
    }
    return kTRUE;
}
//-----------------------------------------------------------------------------
void  PGMessage::Exit() { 
  std::exit(0); 
}







/***************************************************************************
 *
 * $Id: PGMessage.cxx,v 1.1 2009/01/23 16:11:06 jeromel Exp $
 *
 * Author: Frank Laue, laue@bnl.gov
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: PGMessage.cxx,v $
 * Revision 1.1  2009/01/23 16:11:06  jeromel
 * Import from online/RTS/src/
 *
 * Revision 1.1  2007/02/27 15:23:38  laue
 * Initial version
 *
 * Revision 1.1  2006/10/04 20:31:16  laue
 * Initial Version
 *
 *
 ***************************************************************************/


/***************************************************************************
 *
 * $Id: InputDialog2.cc,v 1.1 2004/02/06 02:30:33 munhoz Exp $
 *
 * Author: Marcelo Munhoz (adapted from ROOT)
 ***************************************************************************
 *
 * Description: Input Dialogs
 *
 ***************************************************************************
 *
 * $Log: InputDialog2.cc,v $
 * Revision 1.1  2004/02/06 02:30:33  munhoz
 * inserting SVT online monitor
 *
 **************************************************************************/
//

#include <stdlib.h> 

#include <TROOT.h>
#include <TGFrame.h>
#include <TGLabel.h>
#include <TGTextBuffer.h>
#include <TGTextEntry.h>
#include <TGButton.h>

#include "InputDialog2.hh"

ClassImp(InputDialog2)
 
InputDialog2::InputDialog2(const char *prompt, const char *prompt2, const char *defval, const char *defval2, char *retstr, char *retstr2)
{
   // Create simple input dialog.
 
   fWidgets = new TList;
 
   TGWindow *main = (TGWindow*)gClient->GetRoot();
   fDialog = new TGTransientFrame(main, main, 10, 10);
 
   // command to be executed by buttons and text entry widget
   char cmd[128];
   sprintf(cmd, "{long r__ptr=0x%x; ((InputDialog2*)r__ptr)->ProcessMessage($MSG,$PARM1,$PARM2);}", this);
 
   // create prompt label and textentry widget
   TGLabel *label = new TGLabel(fDialog, prompt);
   fWidgets->Add(label);

   TGLabel *label2 = new TGLabel(fDialog, prompt2);
   fWidgets->Add(label2);
 
   TGTextBuffer *tbuf = new TGTextBuffer(256);  //will be deleted by TGtextEntry
   tbuf->AddText(0, defval);

   TGTextBuffer *tbuf2 = new TGTextBuffer(256);  //will be deleted by TGtextEntry
   tbuf2->AddText(0, defval2);
 
   fTE = new TGTextEntry(fDialog, tbuf,1);
   fTE->Resize(260, fTE->GetDefaultHeight());
   fTE->SetCommand(cmd);
 
   fTE2 = new TGTextEntry(fDialog, tbuf2,2);
   fTE2->Resize(260, fTE2->GetDefaultHeight());
   fTE2->SetCommand(cmd);

   TGLayoutHints *l1 = new TGLayoutHints(kLHintsTop | kLHintsLeft, 5, 5, 5, 0);
   TGLayoutHints *l2 = new TGLayoutHints(kLHintsTop | kLHintsLeft, 5, 5, 5, 5);
   TGLayoutHints *l12 = new TGLayoutHints(kLHintsTop | kLHintsLeft, 5, 5, 5, 0);
   TGLayoutHints *l22 = new TGLayoutHints(kLHintsTop | kLHintsLeft, 5, 5, 5, 5);

   fWidgets->Add(l1);
   fWidgets->Add(l2);
   fWidgets->Add(l12);
   fWidgets->Add(l22);
 
   fDialog->AddFrame(label, l1);
   fDialog->AddFrame(fTE, l2);
   fDialog->AddFrame(label2, l12);
   fDialog->AddFrame(fTE2, l22);
 
   // create frame and layout hints for Ok and Cancel buttons
   TGHorizontalFrame *hf = new TGHorizontalFrame(fDialog, 60, 20, kFixedWidth);
   TGLayoutHints     *l3 = new TGLayoutHints(kLHintsCenterY | kLHintsExpandX, 5, 5, 0, 0);
 
   // put hf as last in list to be deleted
   fWidgets->Add(l3);
 
   // create OK and Cancel buttons in their own frame (hf)
   UInt_t  nb = 0, width = 0, height = 0;
   TGTextButton *b;
 
   b = new TGTextButton(hf, "&Ok", cmd, 1);
   fWidgets->Add(b);
   b->Associate(fDialog);
   hf->AddFrame(b, l3);
   height = b->GetDefaultHeight();
   width  = TMath::Max(width, b->GetDefaultWidth()); ++nb;
 
   b = new TGTextButton(hf, "&Cancel", cmd, 2);
   fWidgets->Add(b);
   b->Associate(fDialog);
   hf->AddFrame(b, l3);
   height = b->GetDefaultHeight();
   width  = TMath::Max(width, b->GetDefaultWidth()); ++nb;
 
   // place button frame (hf) at the bottom
   TGLayoutHints *l4 = new TGLayoutHints(kLHintsBottom | kLHintsCenterX, 0, 0, 5, 5);
   fWidgets->Add(l4);
   fWidgets->Add(hf);
 
   fDialog->AddFrame(hf, l4);
 
   // keep buttons centered and with the same width
   hf->Resize((width + 20) * nb, height);
 
   // set dialog title
   fDialog->SetWindowName("Get Input");
 
   // map all widgets and calculate size of dialog
   fDialog->MapSubwindows();
 
   width  = fDialog->GetDefaultWidth();
   height = fDialog->GetDefaultHeight();
 
   fDialog->Resize(width, height);
 
   // position relative to the parent window (which is the root window)
   Window_t wdum;
   int      ax, ay;
 
   gVirtualX->TranslateCoordinates(main->GetId(), main->GetId(),
                          (((TGFrame *) main)->GetWidth() - width) >> 1,
                          (((TGFrame *) main)->GetHeight() - height) >> 1,
                          ax, ay, wdum);
   fDialog->Move(ax, ay);
   fDialog->SetWMPosition(ax, ay);
 
   // make the message box non-resizable
   fDialog->SetWMSize(width, height);
   fDialog->SetWMSizeHints(width, height, width, height, 0, 0);
 
   fDialog->SetMWMHints(kMWMDecorAll | kMWMDecorResizeH  | kMWMDecorMaximize |
                                       kMWMDecorMinimize | kMWMDecorMenu,
                        kMWMFuncAll  | kMWMFuncResize    | kMWMFuncMaximize |
                                       kMWMFuncMinimize,
                        kMWMInputModeless);
 
   // popup dialog and wait till user replies
   fDialog->MapWindow();
 
   fRetStr = retstr;
   fRetStr2 = retstr2;
 
   gClient->WaitFor(fDialog);
}
 
InputDialog2::~InputDialog2()
{
   // Cleanup dialog.
 
   fWidgets->Delete();
   delete fWidgets;
 
   delete fTE;
   delete fTE2;
   delete fDialog;
}
 
void InputDialog2::ProcessMessage(Long_t msg, Long_t parm1, Long_t parm2)
{
   // Handle button and text enter events

   switch (GET_MSG(msg)) {
      case kC_COMMAND:
         switch (GET_SUBMSG(msg)) {
             case kCM_BUTTON:
                switch (parm1) {
                   case 1:
                      // here copy the string from text buffer to return variable
                      strcpy(fRetStr, fTE->GetBuffer()->GetString());
		      strcpy(fRetStr2, fTE2->GetBuffer()->GetString());
                      delete this;
                      break;
 
                   case 2:
                      fRetStr[0] = 0;
                      fRetStr2[0] = 0;
                      delete this;
                      break;
                 }
              default:
                 break;
          }
          break;
 
       case kC_TEXTENTRY:
         switch (GET_SUBMSG(msg)) {
             case kTE_ENTER:
                // here copy the string from text buffer to return variable
                strcpy(fRetStr, fTE->GetBuffer()->GetString());
		strcpy(fRetStr2, fTE2->GetBuffer()->GetString());
                delete this;
                break;
             default:
                break;
          }
          break;
 
       default:
          break;
   }
}

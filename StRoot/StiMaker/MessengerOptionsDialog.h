#ifndef MessengerOptionsDialog_H_INCLUDED
#define MessengerOptionsDialog_H_INCLUDED
#include <vector>
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
#include "Sti/Base/Messenger.h"

#ifndef __CINT__
typedef pair<unsigned int, TGCheckButton*> MessengerPair;
#else
class MessengerPair;
#endif

#ifndef __CINT__
typedef vector<MessengerPair> MsgPairVec;
#else
class MsgPairVec;
#endif

class MessengerOptionsDialog : public TGTransientFrame
{
private:
    
    TGCompositeFrame     *_frame1, *_frame2, *_frame3;
    TGButton             *_applyButton, *_closeButton;
    
    MsgPairVec _options;
    
    TGGroupFrame         *fG1;
    TGLayoutHints        *_layout1, *_layout2, *_layout3, *_layout4, *_layout21;
    TGGC                  fRedTextGC;

    void updateMessenger();
    
public:
    MessengerOptionsDialog(const TGWindow *p, const TGWindow *main, UInt_t w, UInt_t h,
			   UInt_t options = kVerticalFrame);
    virtual ~MessengerOptionsDialog();
    virtual void CloseWindow();
    virtual Bool_t ProcessMessage(Long_t msg, Long_t parm1, Long_t parm2);
};

#endif

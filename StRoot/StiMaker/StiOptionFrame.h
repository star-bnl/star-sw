#ifndef StiOptionFrame_H
#define StiOptionFrame_H 
#include <vector>
using std::vector;
#include <utility>
using std::pair;
#include <string>
using std::string;

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
#include <TCanvas.h>
#include <TH1.h>
#include <TH2.h>
#include <TRandom.h>
#include <TSystem.h>
#include <TEnv.h>
class EditableParameters;
class TGWindow;
#ifndef __CINT__
typedef vector<TGHorizontalFrame*> HorizontalFrameVec;
typedef pair<string, TGNumberEntry*> NamedNumberEntry;
typedef vector<NamedNumberEntry> NumberEntryVec;
//typedef vector<TGNumberEntry*> NumberEntryVec;
typedef vector<TGLabel*> LabelVec;
#else
class HorizontalFrameVec;
class NamedNumberEntry;
class NumberEntryVec;
class LabelVec;
#endif

class  StiOptionFrame : public TGTransientFrame
{    
 public:
    StiOptionFrame(const TGWindow *p, const TGWindow *main, EditableParameters * params);
    virtual ~StiOptionFrame();
    virtual void closeWindow();
    virtual void apply();
    virtual Bool_t ProcessMessage(Long_t msg, Long_t parm1, Long_t);
    
 protected:

    void initialize();

    EditableParameters * parameters;

    TGVerticalFrame      *frame1;
    TGVerticalFrame      *frame2;
    HorizontalFrameVec    frame;
    TGLayoutHints        *layout1;
    TGLayoutHints        *layout2;
    TGLayoutHints        *layout3;
    LabelVec             fLabel;
    TGButton             *applyButton;
    TGButton             *closeButton;
};


#endif

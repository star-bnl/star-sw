#include <stdlib.h>
#include <vector>
using std::vector;
#include <utility>
using std::pair;

#include <string>
using std::string;

#include "TObject.h"

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
#include "Sti/StiDetector.h"
#include "Sti/StiDetectorContainer.h"
#include "Sti/StiCompositeTreeNode.h"
#include "StiGui/StiDrawable.h"
#include "StiGui/StiRootDisplayManager.h"

#ifndef __CINT__
typedef pair<string, TGCheckButton*> DetectorActivatePair;
#else
class DetectorActivatePair;
#endif

#ifndef __CINT__
typedef vector<DetectorActivatePair> DetActivatePairVec;
#else
class DetActivatePairVec;
#endif

class DetectorActivator : public TGTransientFrame
{
private:
    
    TGCompositeFrame     *f1, *f2, *f3;

    TGButton             *fTestButton, *fCloseButton;
    
    DetActivatePairVec fC;
    
    TGGroupFrame         *fG1;
    TGLayoutHints        *fL1, *fL2, *fL3, *fL4, *fL42, *fL21;
    TGGC                  fRedTextGC;

    void updateDetectors();
    
public:
    DetectorActivator(const TGWindow *p, const TGWindow *main, UInt_t w, UInt_t h,
	       UInt_t options = kVerticalFrame);
    virtual ~DetectorActivator();
    
    virtual void CloseWindow();
    virtual void activateLayer(const string& name, bool on);
    virtual Bool_t ProcessMessage(Long_t msg, Long_t parm1, Long_t parm2);
};

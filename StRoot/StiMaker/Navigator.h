#ifndef Navigator_H_INCLUDED
#define Navigator_H_INCLUDED
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

#include "StChain.h"


class Navigator : public TGTransientFrame
{
 public:
  Navigator(const TGWindow *p, const TGWindow *main, UInt_t w, UInt_t h,
	    UInt_t options = kVerticalFrame);
  virtual ~Navigator();
  virtual void CloseWindow();
  virtual Bool_t ProcessMessage(Long_t msg, Long_t parm1, Long_t parm2);
  
 private:
  
  TGCompositeFrame *f1;
  TGButton* mMoveIn;
  TGButton* mMoveOut;
  TGButton* mMovePlusPhi;
  TGButton* mMoveMinusPhi;
  TGButton* mClose;
  TGLayoutHints *fL1, *fL21;
  
  void moveIn();
  void moveOut();
  void movePlusPhi();
  void moveMinusPhi();
  
};

#endif

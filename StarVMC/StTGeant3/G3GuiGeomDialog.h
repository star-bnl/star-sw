#ifndef G3GUIGEOMDIALOG_H
#define G3GUIGEOMDIALOG_H
/* Copyright(c) 1998-1999, ALICE Experiment at CERN, All rights reserved. *
 * See cxx source for full Copyright notice                               */

/* $Id: G3GuiGeomDialog.h,v 1.1.1.1 2004/07/17 20:02:55 perev Exp $ */

//
// Dialog Panels for the G3 GUI
//

#include "TGFrame.h"

class G3GUISliders;
class TGButton;
class TGComboBox;
class TGLabel;
class TGTab;
class TGTextBuffer;
class TGTextEntry;
class TGDoubleHSlider;
class G3Volume;

class G3GuiGeomDialog : public TGTransientFrame {
public:
   G3GuiGeomDialog(const TGWindow *p, const TGWindow *main, UInt_t w, UInt_t h,
               UInt_t options = kMainFrame | kVerticalFrame);
   virtual ~G3GuiGeomDialog();
// Destroy this window
   virtual void CloseWindow();
// Process messages from this window    
   virtual Bool_t ProcessMessage(Long_t msg, Long_t parm1, Long_t parm2);
// Update widgets   
   virtual void Update();
private:
    G3GUISliders       *fF1;                                          // Slider for Draw Control
    TGCompositeFrame    *fFrame1, *fF2, *fF3, *fF4;                            // Outer frames
    TGButton            *fOkButton, *fCancelButton;                            // Buttons
    TGButton            *fChk1, *fChk2, *fChk3;                                // Buttons
    TGComboBox          *fCombo, *fCombo2;                                     // Combo Boxes
    TGLabel             *fLabel1, *fLabel2;                                    // Labels
    TGTab               *fTab;                                                 // Tab Entries
    TGLayoutHints       *fL1, *fL2, *fL3, *fL4, *fBly, *fBfly1;                // Layout hints
    TGHorizontalFrame   *fHSframe1, *fHSframe2, *fHSframe3;                    // Horizontal frames
    TGTextBuffer        *fTbh11, *fTbh12, *fTbh21, *fTbh22, *fTbh31, *fTbh32;  // Text buffers
    TGTextEntry         *fTeh11, *fTeh12, *fTeh21, *fTeh22, *fTeh31, *fTeh32;  // Text Entries
    TGDoubleHSlider     *fDslider1, *fDslider2, *fDslider3;                    // Sliders for clip box
    TGLabel             *fSLabel1,  *fSLabel2,  *fSLabel3;                     // Labels

private:
  G3GuiGeomDialog(const G3GuiGeomDialog& gd):
    TGTransientFrame((const TGTransientFrame&)gd) {}
  virtual G3GuiGeomDialog & operator=(const G3GuiGeomDialog &) 
  {return *this;}

  ClassDef(G3GuiGeomDialog,1)

};

R__EXTERN G3Volume  *gCurrentVolume;

#endif

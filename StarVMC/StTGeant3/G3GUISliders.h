#ifndef G3GUISLIDERS_H
#define G3GUISLIDERS_H
/* Copyright(c) 1998-1999, ALICE Experiment at CERN, All rights reserved. *
 * See cxx source for full Copyright notice                               */

/* $Id: G3GUISliders.h,v 1.1.1.1 2004/07/17 20:02:55 perev Exp $ */

#include "TGFrame.h"
#include "TGSlider.h"
#include "TGTextEntry.h"
#include "TGTextBuffer.h"
#include "TGLabel.h"

class G3Volume;

class G3GUISliders : public  TGCompositeFrame {
public:
   G3GUISliders(const TGWindow *p, const TGWindow *main, UInt_t w, UInt_t h);
   virtual ~G3GUISliders();
   virtual void CloseWindow();
   virtual Bool_t ProcessMessage(Long_t msg, Long_t parm1, Long_t parm2);
   virtual void Update();
private:
//
    TGHorizontalFrame *fHframe[8];       // 8 Horizontal frames for sliders
    TGLayoutHints     *fBly, *fBfly1;    // Lay-out hints
    TGHSlider         *fHslider[8];      // 8 Sliders
    TGTextEntry       *fTeh[8];          // Text entries for slider position
    TGTextBuffer      *fTbh[8];          // Text buffer  
    TGLabel           *fLabel[8];        // Slider labels
    Text_t            fLabelText[8];     // Label text 

  G3GUISliders(const G3GUISliders &gs) :
    TGCompositeFrame((const TGCompositeFrame&) gs) {}
  G3GUISliders & operator=(const G3GUISliders &) {return *this;}
  
      
  ClassDef(G3GUISliders,1)  // Sliders for the G3 GUI
};

R__EXTERN G3Volume  *gCurrentVolume;

#endif

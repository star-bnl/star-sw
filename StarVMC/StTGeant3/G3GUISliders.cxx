/* *************************************************************************
 * Copyright(c) 1998-1999, ALICE Experiment at CERN, All rights reserved. *
 *                                                                        *
 * Author: The ALICE Off-line Project.                                    *
 * Contributors are mentioned in the code where appropriate.              *
 *                                                                        *
 * Permission to use, copy, modify and distribute this software and its   *
 * documentation strictly for non-commercial purposes is hereby granted   *
 * without fee, provided that the above copyright notice appears in all   *
 * copies and that both the copyright notice and this permission notice   *
 * appear in the supporting documentation. The authors make no claims     *
 * about the suitability of this software for any purpose. It is          *
 * provided "as is" without express or implied warranty.                  *
 **************************************************************************/

/*
$Log: G3GUISliders.cxx,v $
Revision 1.1.1.1  2004/07/17 20:02:55  perev
STAR version of Geant321 TGeant3 etc

Revision 1.3  2004/01/28 08:17:52  brun
Reintroduce the Geant3 graphics classes (thanks Andreas Morsch)

Revision 1.1.1.1  2002/07/24 15:56:26  rdm
initial import into CVS

*/

//
// Sliders for the G3 GUI
// Author: Andreas Morsch
// andreas.morsch@cern.ch
//

#include <stdlib.h>

#include "G3GUISliders.h"
#include "G3Volume.h"

static Text_t* kLabelText[7]  = 
{"Theta ", "Phi   ", "Psi   ", "U     ", "V     ", "UScale", "VScale"};
static Int_t   IRangeMin[7]  = {    0,     0,     0,    0,    0,   0,   0};
static Int_t   IRangeMax[7]  = {36000, 36000, 36000, 2000, 2000, 10, 10};
static Int_t   DefaultPos[7] = { 3000,  4000,     0, 1000, 1000,   1,   1};

ClassImp(G3GUISliders)

G3GUISliders::G3GUISliders(const TGWindow *p, const TGWindow *,
                         UInt_t w, UInt_t h) :
    TGCompositeFrame(p, w, h,kVerticalFrame)
{
// Constructor
    ChangeOptions((GetOptions() & ~kHorizontalFrame) | kVerticalFrame);
   //--- layout for buttons: top g3gn, equally expand horizontally
    fBly = new TGLayoutHints(kLHintsTop | kLHintsExpandY, 5, 5, 5, 5);

   //--- layout for the frame: place at bottom, right g3gned
    fBfly1 = new TGLayoutHints(kLHintsLeft);
//
// Frames

   for (Int_t i=0; i<7; i++) {
       Int_t idT=i+1;
       Int_t idS=i+8;       
       fHframe[i] = new TGHorizontalFrame(this, 400, 100, kFixedWidth);
       fTbh[i] = new TGTextBuffer(10);
       fTeh[i] = new TGTextEntry(fHframe[i], fTbh[i],idT);

       
       char buf[10];
       sprintf(buf, "%6.2f", Float_t(DefaultPos[i])/100);
       fTbh[i]->AddText(0, buf);
       fTeh[i]->SetWidth(80);
       fTeh[i]->Associate(this);
       
       fHslider[i] = new TGHSlider(fHframe[i], 200, kSlider1 | kScaleBoth, idS);
       fHslider[i]->Associate(this);
       fHslider[i]->SetRange(IRangeMin[i], IRangeMax[i]);
       fHslider[i]->SetPosition(DefaultPos[i]);

       fLabel[i] = new TGLabel(fHframe[i], kLabelText[i]);
       fLabel[i]->SetWidth(100);
       
       
       fHframe[i]->AddFrame(fLabel[i], fBfly1);
       fHframe[i]->AddFrame(fTeh[i], fBfly1);
       fHframe[i]->AddFrame(fHslider[i], fBfly1);
       AddFrame(fHframe[i], fBly);
   }
}

G3GUISliders::~G3GUISliders()
{
// Destructor
    delete fBfly1; delete fBly;
   // Delete dialog.
    for (Int_t i=0; i<7; i++) {
	delete fHframe[i];
	delete fHslider[i];
	delete fTbh[i]; 
    }
}

void G3GUISliders::Update()
{
// Update sliders
    char buf[10];
    
    for (Int_t i=0; i<7; i++) {
	Float_t param = gCurrentVolume->GetParam(i);
//
	fHslider[i]->SetPosition(Int_t(param*100.));
	gClient->NeedRedraw(fHslider[i]);
//
	sprintf(buf, "%6.2f", param);
	fTbh[i]->Clear();
	fTbh[i]->AddText(0, buf);
	gClient->NeedRedraw(fTeh[i]);
//
    }

    
}

void G3GUISliders::CloseWindow()
{
   // Called when window is closed via the window manager.

   delete this;
}

Bool_t G3GUISliders::ProcessMessage(Long_t msg, Long_t parm1, Long_t parm2)
{
   // Process slider messages.

   char buf[10];

   switch (GET_MSG(msg)) {
   case kC_TEXTENTRY:
       switch (GET_SUBMSG(msg)) {
       case kTE_TEXTCHANGED:
	   Int_t idT=Int_t(parm1)-1;
	   fHslider[idT]->SetPosition((Int_t)atof(fTbh[idT]->GetString())*100);
	   gClient->NeedRedraw(fHslider[idT]);
	   gCurrentVolume->SetParam(idT,atof(fTbh[idT]->GetString()));
	   gCurrentVolume->Draw();
       }
       break;
   case kC_HSLIDER:
       switch (GET_SUBMSG(msg)) {
       case kSL_POS:
	   sprintf(buf, "%6.2f", Float_t(parm2)/100);
	   Int_t idS=Int_t(parm1)-8;
	   fTbh[idS]->Clear();
	   fTbh[idS]->AddText(0, buf);
	   gClient->NeedRedraw(fTeh[idS]);
	   gCurrentVolume->SetParam(idS, Float_t(parm2)/100.);
	   gCurrentVolume->Draw();
       }
       break;
   }
   return kTRUE;
}


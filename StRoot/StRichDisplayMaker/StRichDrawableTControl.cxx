/***************************************************************
 * $Id: StRichDrawableTControl.cxx,v 2.3 2003/09/02 17:58:52 perev Exp $
 *
 * Description: A Clickabe Control Button
 *
 ***************************************************************/

#ifdef __ROOT__
#include "StRichDrawableTControl.h"
#include <Stiostream.h>
#include "TBox.h"
#include "TPaveText.h"
#include "TCanvas.h"
#include "StRichPadMonitor.h"

ClassImp(StRichDrawableTControl)

StRichDrawableTControl::StRichDrawableTControl(){/*no op*/};

StRichDrawableTControl::StRichDrawableTControl(Double_t x1,Double_t y1,Double_t x2,Double_t y2,StRichTControlType type, StRichPadMonitor * mon)
    : TPaveText(x1,y1,x2,y2)
{

    mPadMon = mon;
    mControlType = type;
    
    this->SetLineWidth(2);
    this->SetLineColor(1);

    
    if(mControlType == eZoomIn){
	this->AddText("Zoom");
	this->AddText("In");
    }
    if(mControlType == eZoomOut){
	this->AddText("Zoom");
	this->AddText("Out");
    }
	
    
    this->Draw();
        
}
StRichDrawableTControl::~StRichDrawableTControl() {}


void StRichDrawableTControl::ExecuteEvent(Int_t event, Int_t px, Int_t py)
{
    if (!gPad->IsEditable()) return;
    
    
    if (!gPad) return;
    if (!gPad->IsEditable() && event != kMouseEnter) return;
    
    switch(event) {
	
    case kButton1Down:
	if(mControlType == eZoomIn){
	    TCanvas * tempCanvas = mPadMon->getRichCanvas();
	    unsigned int ww = tempCanvas->GetWw();
	    unsigned int wh = tempCanvas->GetWh();
	    tempCanvas->SetCanvasSize((int)1.25*ww,(int)1.25*wh);
	    tempCanvas->ForceUpdate();
	}
	if(mControlType == eZoomOut){
	    TCanvas * tempCanvas = mPadMon->getRichCanvas();
	    unsigned int ww = tempCanvas->GetWw();
	    unsigned int wh = tempCanvas->GetWh();
	    tempCanvas->SetCanvasSize((int).8*ww,(int).8*wh);
	    tempCanvas->ForceUpdate();
	}
	
	
	break;
    
    case kMouseLeave:
	
    default:
	break;
    }
}
#endif /* ROOT */

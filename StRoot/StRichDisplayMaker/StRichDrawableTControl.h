//***********************************
// StRichDrawableTControl.h
//
// First attempt at a Control
//
/* Enumerated Types for ROOT Actions are in: TButton.h
 * enum EEventType {
 *   kNoEvent       =  0,
 *  kButton1Down   =  1, kButton2Down   =  2, kButton3Down   =  3, kKeyDown  =  4,
 *  kButton1Up     = 11, kButton2Up     = 12, kButton3Up     = 13, kKeyUp    = 14,
 *  kButton1Motion = 21, kButton2Motion = 22, kButton3Motion = 23, kKeyPress = 24,
 *  kButton1Locate = 41, kButton2Locate = 42, kButton3Locate = 43,
 *  kMouseMotion   = 51, kMouseEnter    = 52, kMouseLeave    = 53,
 *  kButton1Double = 61, kButton2Double = 62, kButton3Double = 63
 *  };
 *
 * enum EEditMode {
 *  kPolyLine  = 1,  kSPolyLine = 2,  kPolyGone  = 3,
 *  kSPolyGone = 4,  kBox       = 5,  kDelete    = 6,
 *  kPad       = 7,  kText      = 8,  kEditor    = 9,
 *  kExit      = 10, kPave      = 11, kPaveLabel = 12,
 *  kPaveText  = 13, kPavesText = 14, kEllipse   = 15,
 *  kArc       = 16, kLine      = 17, kArrow     = 18,
 *  kGraph     = 19, kMarker    = 20, kPolyMarker= 21,
 *  kPolyLine3D= 22, kWbox      = 23, kGaxis     = 24,
 *  kF1        = 25, kF2        = 26, kF3        = 27,
 *  kDiamond   = 28, kPolyMarker3D = 29
 *  };
 ****************************************************************/



// **********************************
#ifdef __ROOT__
#ifndef ST_RICH_DRAWABLE_TCONTROL_H
#define ST_RICH_DRAWABLE_TCONTROL_H


enum StRichTControlType {eZoomIn,eZoomOut};
class StRichPadMonitor;
#include "TBox.h"
#include "TCanvas.h"
#include "TPaveText.h"

class StRichDrawableTControl : public TPaveText{
public:
    
    virtual ~StRichDrawableTControl();
    StRichDrawableTControl();
    StRichDrawableTControl(Double_t x1,Double_t y1,Double_t x2,Double_t y2,StRichTControlType type, StRichPadMonitor * mon);
	
    virtual void ExecuteEvent(Int_t event, Int_t px, Int_t py);
    
    
protected:
    
    StRichPadMonitor * mPadMon;//!
    StRichTControlType mControlType;
    
    ClassDef(StRichDrawableTControl,1)
	};


#endif
#endif

/******************************************************
 * $Id: StRichPadMonitor.h,v 1.1 2000/02/12 21:57:57 lasiuk Exp $
 * Description:
 *  First aTtempt at a simple Pad Monitor.
 *  Runs only in ROOT
 *
 * Auxiliary classes are kept here for now as well
 *
 * $Log: StRichPadMonitor.h,v $
 * Revision 1.1  2000/02/12 21:57:57  lasiuk
 * Initial revision
 *
 ******************************************************/
 * Revision 1.1  2000/02/12 21:57:57  lasiuk
 * Initial revision
 ***************************************************************/
#ifdef __ROOT__

#include "TCanvas.h"
#include "TBox.h"

#include "StRichGeometryDb.h"
#include "StRichCoordinateTransform.h"
///////////////////////////////////////////////////////////////////////
// Object which is produced by the Slow simulator
//
class StRichSinglePixel  {
public:
    StRichSinglePixel();
    StRichSinglePixel(int p, int r, int adc);
    
    ~StRichSinglePixel();

    //StRichSinglePixel(const StRichSinglePixel&) {/*use default*/|
    //StRichSinglePixel& operator=(const StRichSinglePixel&) {/*use default*/|
    
public:
    double mLength;
    double mWidth;
    double mPad;
    double mRow;
    int    mAdc;
};

////////////////////////////////////////////////////////////////////////////
// Object which we want ROOT to draw
//
class StRichDrawableTPad : public TBox {
public:
    StRichDrawableTPad();
    StRichDrawableTPad(double xl, double yl, double xu, double yu, int adc);
    
    virtual ~StRichDrawableTPad();
class StRichCoordinateTransform;
    //StRichDrawableTPad(const StRichDrawableTPad&) {/*use default*/|
    //StRichDrawableTPad& operator=(const StRichDrawableTPad&) {/*use default*/}
    
    void draw();
    void setPosition(int p, int r);

public:
    int mAdc;
    double mLength;
    double mWidth;
    double mPad;
    double mRow;
};

///////////////////////////////////////////////////////////////////////////////////////
// the pad monitor which is a canvas with some boundaries drawn
//
class StRichSinglePixel;
class StRichDrawableTPad;

class StRichPadMonitor : public TObject {
public:
    static StRichPadMonitor* getInstance(StRichGeometryDb*);
    //StRichPadMonitor(const StRichPadMonitor&) {/*nopt*/}
    //StRichPadMonitor& operator=(const StRichPadMonitor&) {/*nopt*/}

    void clearPads();
    void addPad(StRichSinglePixel* pad);
    void update();
    void addOuterRingPoint(double x, double y);
    void drawRing();

    //This better be private lateer!
public:
    Color_t GetColorAttribute(double amp);
    
    vector<double>     mXOPoints;

    StRichPadMonitorText*      mTextWindow;
    
    StRichCoordinateTransform* mTransform;
    StRichGeometryDb*          mGeometryDb;
    
    double mRowPitch;
    double mPadPitch;
    double mPadLength;

    short mColorMode;
private:
    static StRichPadMonitor* mInstance;
};

#endif  // ST_RICH_PADMONITOR_H
#endif  // __ROOT__

/***************************************************************
 * $Id: StRichPadMonitor.h,v 1.2 2000/02/29 18:19:38 lasiuk Exp $
 * Description:
 *  First aTtempt at a simple Pad Monitor.
 *  Runs only in ROOT
 *
 ***************************************************************
 *
 * $Log: StRichPadMonitor.h,v $
 * Revision 1.2  2000/02/29 18:19:38  lasiuk
 * Split needed classes into individual files
 *
 * Revision 1.2  2000/02/29 18:19:38  lasiuk
 * Split needed classes into individual files
 *
 * Revision 1.1  2000/02/12 21:57:57  lasiuk
 * Initial revision
 ***************************************************************/
#ifdef __ROOT__
//#include <vector>
using std::vector;
#endif

#include "TObject.h"
#include "TCanvas.h"
#include "TObjArray.h"
//#include "St_Node.h"
#include "TText.h"
#include "TPaveLabel.h"

#include "TColor.h"

class StRichPadMonitorText;
class StRichGeometryDb;
class StRichCoordinateTransform;
class StRichSinglePixel;
class StRichDrawableTPad;

class StRichPadMonitor : public TObject {
public:
    static StRichPadMonitor* getInstance(StRichGeometryDb*);
    
    ~StRichPadMonitor();

    //StRichPadMonitor(const StRichPadMonitor&) {/*nopt*/}
    //StRichPadMonitor& operator=(const StRichPadMonitor&) {/*nopt*/}

    void clearPads();
    void addPad(StRichSinglePixel* pad);
    void update();
    void addOuterRingPoint(double x, double y);
    void drawRing();
    
    StRichPadMonitor(StRichGeometryDb*);

    void    drawColorBox();
    Color_t GetColorAttribute(double amp);
    
// #ifndef ST_NO_DEF_TEMPLATE_ARGS
//     vector<StRichDrawableTPad*>   mAllFilledPads;//!
// #else
//     vector<StRichDrawableTPad*, allocator<StRichDrawableTPad*> >   mAllFilledPads;//!
// #endif
    vector<double>     mYPoints;
    vector<double>     mXOPoints;
    vector<double>     mYOPoints;
    //St_Node *mHall;

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

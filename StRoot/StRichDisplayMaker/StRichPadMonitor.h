/***************************************************************
 * $Id: StRichPadMonitor.h,v 2.2 2000/08/13 01:26:50 gans Exp $
 * Description:
 *  First aTtempt at a simple Pad Monitor.
 *  Runs only in ROOT
 *
 ***************************************************************
 *
 * $Log: StRichPadMonitor.h,v $
 * Revision 2.2  2000/08/13 01:26:50  gans
 * Added directory changing for pidMaker->printCanvas("directory/")
 *
 * Revision 2.1  2000/08/11 20:20:44  gans
 * Added use of StRichDrawableTControl
 *
 * Revision 2.3  2000/09/29 17:36:58  gans
 * Modified addHit(), StThreeVector<double> -> StThreeVectorF,other minor stuff
 *
 * Revision 2.0  2000/08/09 16:28:04  gans
 * Created New Maker for all drawable objects.
 *
 * Revision 1.6  2000/06/16 02:00:51  lasiuk
 * add MC drawing diagnositics
 * add vector for storage
 * cosmetics for display
 *
 * Revision 1.5  2000/05/17 22:20:40  lasiuk
 * charge from the pixel
 *
 * Revision 1.4  2000/04/05 16:02:13  lasiuk
 * GEANT info now drawable
 *
 * Revision 1.3  2000/03/13 21:50:35  lasiuk
 * coordinates
 *
 * Revision 1.2  2000/02/29 18:19:38  lasiuk
 * Split needed classes into individual files
 *
 * Revision 1.1  2000/02/12 21:57:57  lasiuk
 * Initial revision
 ***************************************************************/
#ifdef __ROOT__
#ifndef ST_RICH_PADMONITOR_H
#define ST_RICH_PADMONITOR_H

#include <vector>
#ifndef ST_NO_NAMESPACES
using std::vector;
#endif

#include "TObject.h"
#include "TCanvas.h"
#include "TObjArray.h"
//#include "St_Node.h"
#include "TText.h"
#include "TPaveLabel.h"

#include "TFile.h"
#include "TNtuple.h"

#include "TColor.h"
#include "StParticleTypes.hh"


#include "StRrsMaker/StRichG2TInfo.h"
#include "StRichPIDMaker/StRichRings.h"
class TCanvas;
#include "StRichPIDMaker/StRichTrack.h"
#include "StRichDrawableTTrack.h"

class StRichPadMonitorText;
class StRichGeometryDb;
class StRichCoordinateTransform;
class StRichSinglePixel;
class StRichSimpleHit;
class StRichSingleMCPixel;
class StRichDrawableTPad;
class StRichTrack;
class StRichDrawableTTrack;

class StRichPadMonitor : public TObject {
public:
    static StRichPadMonitor* getInstance(StRichGeometryDb*);
    
    ~StRichPadMonitor();

    //StRichPadMonitor(const StRichPadMonitor&) {/*nopt*/}
    //StRichPadMonitor& operator=(const StRichPadMonitor&) {/*nopt*/}

    void clearAll();
    void clearPads();
    void clearG2T();
    void clearHits();

    void drawPads();
    void addPad(StRichSinglePixel*);

    void drawPad(const StRichSingleMCPixel&);
    void drawPad(const StRichSinglePixel&);
    void drawG2T(const StRichG2TInfo&);
    void drawGeantGroup(int track, int color=1);
    void drawHit(StRichSimpleHit*);

    // Controls
    //void removeGeantPoints();
    //void drawGeantPoints();
    
    void update();
    
    void calculatePadPosition(const StRichSinglePixel* pad,
			      double* xl, double* yl, double* xu, double* yu);

    void addTrack(StRichTrack*);
    StRichDrawableTTrack* getTrack(StRichTrack*);
    void drawRings();          
    void clearTracks();         

    void drawZVertex(double,int,int);
    
    void drawLegend();
    void printCanvas(char*,char*,int);
    TCanvas * getRichCanvas();
    
    void printCanvas(char*,char*, int);
   
   
    void doResiduals(double,long[],int,int);
    inline TCanvas* getRichCanvas(){ return mRichCanvas;};
protected:
    StRichPadMonitor(StRichGeometryDb*);

    Color_t GetColorAttribute(double amp);
private:
    
private:
    TCanvas*    mRichCanvas;
    TObjArray   mAllFilledPads;
    TObjArray   mColorBoxes;
    TObjArray   mTextLabels;
    TObjArray   mControls;
    TObjArray   mG2TSegments;
    TObjArray   mHits;

    vector<StRichDrawableTTrack*> mVectorTracks; // Vector of rings to be drawn
    
    //St_Node *mHall;

    StRichPadMonitorText*      mTextWindow;
    
    StRichCoordinateTransform* mTransform;
    StRichGeometryDb*          mGeometryDb;
    
    double mRowPitch;
    double mPadPitch;
    double mPadWidth;
    double mPadLength;

    short mColorMode;

    TText* mLegendE;
    TText* mLegendPi;
    TText* mLegendK;
    TText* mLegendP;
    TText* mFileEventNum;
    TText* mFileName;

    TMarker* mZVertex;
    TText* mNumTracks;
    //StRichDrawableTControl * mControlPanel;
    
    char mFileTextEventNum[50]; // Should be big Enough to
                                 // Hold Any Event Number

    // Residual Stuff
   
    StPionMinus* pionminus;
    StKaonMinus* kaonminus;
    StAntiProton* antiproton;
    
    StPionPlus* pionplus;
    StKaonPlus* kaonplus;
    StProton* proton;
    

private:
    static StRichPadMonitor* mInstance;
};

#endif  // ST_RICH_PADMONITOR_H
#endif  // __ROOT__

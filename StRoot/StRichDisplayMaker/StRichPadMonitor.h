/***************************************************************
 * $Id: StRichPadMonitor.h,v 2.7 2001/11/21 20:57:18 lasiuk Exp $
 * Description:
 *  First aTtempt at a simple Pad Monitor.
 *  Runs only in ROOT
 *
 ***************************************************************
 *
 * $Log: StRichPadMonitor.h,v $
 * Revision 2.7  2001/11/21 20:57:18  lasiuk
 * add quick rings, text drawing
 *
 * Revision 2.6  2000/12/08 04:59:13  lasiuk
 * unambiguate hiliteHits(flag) by requiring user
 * to specify which ring is of interest
 *
 * Revision 2.5  2000/11/01 16:59:20  lasiuk
 * MAJOR.  add ringInfo().  Clear() of TObjArray containers is simplified
 * (Valery suggestion)  Utilities added to draw lines and markers.
 * hilite() members based on StEvent hit flags now implemented.  This
 * makes use of the DrawableHits contained in the DrawableRings.
 *
 * Revision 2.4  2000/10/01 00:24:57  gans
 * Fixed Stomped getRichCanvas()
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
#include "StRichPIDMaker/StRichTrack.h"

#include "StEvent/StEnumerations.h"

#include "StRichDrawableTTrack.h"

class StParticleDefinition;
class StRichGeometryDb;
class StRichSinglePixel;
class StRichCoordinateTransform;

class StRichPadMonitorText;
class StRichSingleMCPixel;
class StRichDrawableTPad;
class StRichDrawableTTrack;

class StRichTrack;
class StRichSimpleHit;

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
    void clearTracks();         
    void clearRingInfo();
    void clearMisc();
    void clearQuickRings();
    void clearText();
    
    //
    // Event info
    //
    void drawZVertex(double,int,int);

    //
    // Pads and Pixels
    //
    void drawPads();
    void addPad(StRichSinglePixel*);
    void drawPad(const StRichSingleMCPixel&);
    void drawPad(const StRichSinglePixel&);

    //
    // Geant info
    void drawG2T(const StRichG2TInfo&);
    void drawGeantGroup(int track, int color=1);

    //
    // Hits
    //
    void drawHit(StRichSimpleHit*);
    void hiLiteHits();
    void hiLiteHits(const StRichHitFlag&,StParticleDefinition*);
    
    void update();
    //
    // Tracks
    //
    void addTrack(StRichTrack*);
    StRichDrawableTTrack* getTrack(StRichTrack*);

    //
    // PID
    //
    void drawRings();          
    void drawRingInfo();

    // Misc
    void drawMarker(StThreeVectorF&,int type=26, float size=1.2, int color=1);
    void drawLine(StThreeVectorF&, StThreeVectorF&);
    void drawText(double,double,char*);
    
    // Controls
    //void removeGeantPoints();
    //void drawGeantPoints();    
    void drawLegend();
    void drawFileName(char*);
    void drawEventInfo(Long_t,Long_t);
    void printCanvas(char*,char*, int);

    //
    // Rings
    void drawQuickRing(vector<StThreeVectorF>&, vector<StThreeVectorF>&,
		       vector<StThreeVectorF>&, vector<StThreeVectorF>&,
		       vector<StThreeVectorF>&, vector<StThreeVectorF>&);
   
    void doResiduals(double,long[],int,int);
    inline TCanvas* getRichCanvas(){ return mRichCanvas;};

private:
    void calculatePadPosition(const StRichSinglePixel* pad,
			      double* xl, double* yl, double* xu, double* yu);

protected:
    StRichPadMonitor(StRichGeometryDb*);

    void    drawColorBox();
    Color_t GetColorAttribute(double amp);
    
private:
    TCanvas*    mRichCanvas;

    //
    // changing EBE
    //
    TObjArray   mAllFilledPads;
    TObjArray   mG2TSegments;
    TObjArray   mHits;
    TObjArray   mRingInfo;
    TObjArray   mMisc;
    TObjArray   mText;
    
    TObjArray   mColorBoxes;
    TObjArray   mTextLabels;
    TObjArray   mControls;
    TObjArray   mQuickRings;
    
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

    vector<StParticleDefinition*> mListOfParticles;
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

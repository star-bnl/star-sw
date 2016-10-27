//-*- Mode: C++ -*-
// @(#) $Id: AliHLTTPCCADisplay.h,v 1.3 2013/11/21 13:07:28 mzyzak Exp $
//  *************************************************************************
//  This file is property of and copyright by the ALICE HLT Project         *
//  ALICE Experiment at CERN, All rights reserved.                          *
//  See cxx source for full Copyright notice                                *
//                                                                          *
//  AliHLTTPCCADisplay class is a debug utility.                            *
//  It is not used in the normal data processing.                           *
//                                                                          *
//***************************************************************************

#ifndef ALIHLTTPCCADISPLAY_H
#define ALIHLTTPCCADISPLAY_H

#include "AliHLTTPCCAParameters.h"
#include "AliHLTArray.h"

class AliHLTTPCCAGBTracker;
class AliHLTTPCCATracker;
class AliHLTTPCCATrack;
class AliHLTTPCCATrackParam;
class AliHLTTPCCAParam;
class AliHLTTPCCAPerformance;
class AliHLTTPCCAHitLabel;
class AliHLTTPCCAMCTrack;
class AliHLTTPCCALocalMCPoint;
class TCanvas;
class TPad;
#include "TArc.h"
#include "TLine.h"
#include "TPolyLine.h"
#include "TBox.h"
#include "TCrown.h"
#include "TMarker.h"
#include "TLatex.h"
#include "TArrow.h"

#include <vector>
using std::vector;


/**
 * @class AliHLTTPCCADisplay
 */
class AliHLTTPCCADisplay
{

  public:

    class AliHLTTPCCADisplayTmpHit;

    static AliHLTTPCCADisplay &Instance();

    AliHLTTPCCADisplay();

    virtual ~AliHLTTPCCADisplay();

    void Init();
    void Update();
    void ClearView();
    void Ask();
    void SetSliceView();
    void SetTPCView();
    void SetTPC( const AliHLTTPCCAParam& tpcParam);  // iklm
    void SetCurrentSlice( AliHLTTPCCATracker *slice );
    void SetGB( const AliHLTTPCCAGBTracker * GBTracker );
    void Set2Slices( AliHLTTPCCATracker * slice );

    const AliHLTTPCCAGBTracker * GetGB(){return fGB;};
    int GetColor( int i ) const;
    int GetColorZ( double z ) const ;
    int GetColorY( double y ) const ;
    int GetColorK( double k ) const ;
    void Global2View( double x, double y, double *xv, double *yv ) const ;
    void Slice2View( double x, double y, double *xv, double *yv ) const ;
    int GetTrackMC( const AliHLTTPCCADisplayTmpHit *vHits, int NHits );
  

    void DrawTPC();
    void DrawSlice( AliHLTTPCCATracker *slice, bool DrawRows = 0, bool DrawGrid = 1 );
///mvz start 20.01.2010
    void DrawPoint(float x, float y, float z, int Start = 1, Size_t width = 1 );
    void DrawGBPoint(const AliHLTTPCCAGBTracker &tracker, int iSlice, float x, float y, float z, int Start = 1, Size_t width = 1 ); ///mvz 27.01.2010
    void DrawGBPoint( float x, float y, float z, int Start = 1, Size_t width = 1 ); ///mvz 27.01.2010
    void DrawGBPoint(float p[6], int Start, int color=kBlack, Size_t width = 1 );
    void DrawGBPoint(AliHLTTPCCATracker &slice, float x, float y, float z, int Start=1, Size_t width=1 ); ///mvz 29.01.2010
#ifdef DO_TPCCATRACKER_EFF_PERFORMANCE
    void DrawSliceOutTrackParam( int itr, int color, Size_t width ); ///mvz 03.02.2010
    void DrawSliceOutTrack1( int itr, int color, Size_t width ); ///mvz 03.02.2010
#endif
    void DrawHelix(float p0, float c, float z, float zStart, float z0, float xc, float yc, float r, float b, int color, Size_t width); ///mvz 10.02.2010
    void DrawParticleGlobal(float *param, float q, float tStart, float tEnd, float b, int color=kOrange, Size_t width = 1);
///mvz end 20.01.2010
    void DrawSliceOutTrack( int itr, int color = -1, Size_t width = -1  );
    void DrawSliceOutTrack( AliHLTTPCCATrackParam &t, double Alpha, int itr, int color = -1, Size_t width = -1  );
    //void DrawSliceTrack( int itr, int color = -1 );
    bool DrawTrack( AliHLTTPCCATrackParam t, double Alpha, const AliHLTTPCCADisplayTmpHit *vHits,
                    int NHits, int color = -1, Size_t width = -1, bool pPoint = 0 );

    void DrawGBTrack( int itr, int color = -1, int width = -1 );
    void DrawGBTrackFast( const AliHLTTPCCAGBTracker &tracker, int itr, int color = -1 );
    bool DrawTracklet( AliHLTTPCCATrackParam &track, const int *hitstore, int color = -1, int width = -1, bool pPoint = 0 );

//     void DrawGBLinks( const AliHLTTPCCAGBTracker &tracker, int color = -1, Size_t width = -1 );
    
    void DrawGBHit( const AliHLTTPCCAGBTracker &tracker, int iHit, int color = -1, Size_t width = -1 );
    void DrawGBHits( const AliHLTTPCCAGBTracker &tracker, int color = -1, Size_t width = -1, int hitsType = -1 );

    void DrawSliceHit( int iRow, int iHit, int color = -1, Size_t width = -1 );
    void DrawSliceHits( int color = -1, Size_t width = -1 );
    void DrawSliceLinks( int colorUp = -1, int colorDn = -1, int width = -1 );
    void DrawSliceLink( int iRow, int iHit, int colorUp = -1, int colorDn = -1, int width = -1 );

#ifdef XXXX

    void DrawMergedHit( int iRow, int iHit, int color = -1 );

    void DrawTrack( const AliHLTTPCCATrack &track, int color = -1, bool DrawCells = 1 );
#endif // XXXX
    void DrawTrackletPoint( const AliHLTTPCCATrackParam &t, int color = -1 );
    void DrawTrackParam( AliHLTTPCCATrackParam t, int color = 1 );

    void SetSliceTransform( double alpha );
    void SetSliceTransform( AliHLTTPCCATracker *slice );
    void SaveCanvasToFile( TString fileName);
        
    TPad *CanvasYX() { return fYX; }
    TPad *CanvasZX() { return fZX; }


/// IKu some new functional. Supposed to be usefull.
  struct TDrawObject {
    TDrawObject();

    virtual void DrawLocal(int iSlice){iSlice++;}; // draw in local coor
    virtual void DrawGlobal(){};// draw in global coor
    
    void SetColor(int c);  // set color for this track
    void SetWidth(float w);

         // please add here transformation, if you use some ....
    static void Global2View( double x, double y, double *xv, double *yv);
    static void Slice2View( double x, double y, double *xv, double *yv, float alpha);
    static void Slice2View( double x, double y, double *xv, double *yv, int iSlice);
    static float GetAlpha(const AliHLTTPCCAGBTracker *gbTracker, int iSlice);
   protected:
    enum{ NON = -1000000000 };

    static AliHLTTPCCADisplay& fDisplay; // display instance
    int fColor; // line color
    float fWidth; // width or size    
  };
  
  struct TDrawHit: public TDrawObject {
    TDrawHit();
    bool CheckAlpha(); // check if alpha was set. true - was set, false - wasn't
    bool CheckLocal();
    bool CheckGlobal();
    void FillGBCoor(); // take local cooridates and fill global

    virtual void DrawLocal(int iSlice); // draw in local coor
    virtual void DrawGlobal();// draw in global coor
    
      // can be used one of coor. set:
    float x, y, z;    // sector coor.
    float alpha; 
    float gx, gy, gz; // global coor.
    
   private:
    TMarker& GetMarker();
  };
  
  struct TDrawTrack: public TDrawObject {
    TDrawTrack();
    void FillGBHitsCoor(); // take local cooridates and fill global of hits

    vector<TDrawHit> hits;

    virtual void DrawLocal(int iSlice); // draw in local coor
    virtual void DrawGlobal();// draw in global coor
    void DrawHitsLocal(int iSlice);
    void DrawHitsGlobal();
    
      // draw adjustment
    void SetHitsColor(int c);  // set color for all hits
    void SetHitsWidth(float w);
    void SetStatColor(int c); // set color for line for all tracks
    void SetStatWidth(float w);

   private:
    TPolyLine& GetPolyLine();
    
    static int fStatColor;  // used for all tracks if nonstatic members are not set
    static float fStatWidth;
  };


  void SpecDrawMCTrackPointsGlobal(AliHLTTPCCAMCTrack& mcTrack, AliHLTResizableArray<AliHLTTPCCALocalMCPoint>* mcPointsArray, int color = kRed, float width = 0.5);
  void SpecDrawMCTrackHitsGlobal(AliHLTTPCCAMCTrack& mcTrack, AliHLTResizableArray<AliHLTTPCCALocalMCPoint>* mcPointsArray, AliHLTResizableArray<AliHLTTPCCAHitLabel>* hitLabels, int color = kGreen, float width = 0.5);
  void SpecDrawMCTrackLocal(AliHLTTPCCAMCTrack& mcTrack, AliHLTResizableArray<AliHLTTPCCALocalMCPoint>* mcPointsArray, int iSlice);

  void SpecDrawHitsFromMCTrackLocal(int iMC, const AliHLTResizableArray<AliHLTTPCCAHitLabel>* hitLabels, int iSlice);

  void SpecDrawRecoTrackGlobal( int iTr, int color = kBlue, float width = 0.5);
 public:
/// ENDOF IKu

  void SetDrawType( int t ) { fDrawType = t; }
  int DrawType() const { return fDrawType; }
  
  protected:

  TCanvas *fCanvas;              // the canvas
  TPad *fYX, *fZX;               // two views
  bool fAsk;                      // flag to ask for the pressing key
  bool fSliceView;               // switch between slice/TPC zoom
  AliHLTTPCCATracker *fSlice;      // current CA tracker, includes slice geometry
  const AliHLTTPCCAGBTracker *fGB;      // the Aliglobal tracker
  AliHLTTPCCAPerformance *fPerf; // Performance class (mc labels etc)
  double fCos, fSin, fZMin, fZMax, fYMin, fYMax;// view parameters
  double fSliceCos, fSliceSin;        // current slice angle
  double fRInnerMin, fRInnerMax, fROuterMin, fROuterMax, fTPCZMin, fTPCZMax; // view parameters

  TArc fArc;       // parameters of drawing objects are copied from this members
  TLine fLine;     //!
  TPolyLine fPLine;//!
  TMarker fMarker; //!
  TBox fBox;       //!
  TCrown fCrown;   //!
  TLatex fLatex;   //!
  TArrow fArrow;

  bool fDrawOnlyRef; // draw only clusters from ref. mc tracks

  int fDrawType; // what to draw, used from tracker. Values 1,2,3 is used.

  private:
    AliHLTTPCCADisplay( const AliHLTTPCCADisplay& );
    AliHLTTPCCADisplay& operator=( const AliHLTTPCCADisplay& );
};

#endif

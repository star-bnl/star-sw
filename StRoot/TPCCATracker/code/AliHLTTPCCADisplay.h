//-*- Mode: C++ -*-
// @(#) $Id: AliHLTTPCCADisplay.h,v 1.1.1.1 2010/07/26 20:55:38 ikulakov Exp $
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


class AliHLTTPCCATracker;
class AliHLTTPCCAGBTracker;
class AliHLTTPCCATrack;
class AliHLTTPCCATrackParam;
class AliHLTTPCCAParam;
class AliHLTTPCCAPerformance;
class TCanvas;
class TPad;
#include "TArc.h"
#include "TLine.h"
#include "TPolyLine.h"
#include "TBox.h"
#include "TCrown.h"
#include "TMarker.h"
#include "TLatex.h"



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
    void SetGB( AliHLTTPCCAGBTracker * const GBTracker );
    void Set2Slices( AliHLTTPCCATracker * const slice );

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
    void DrawGBPoint(AliHLTTPCCAGBTracker &tracker, int iSlice, float x, float y, float z, int Start = 1, Size_t width = 1 ); ///mvz 27.01.2010
    void DrawGBPoint(AliHLTTPCCATracker &slice, float x, float y, float z, int Start=1, Size_t width=1 ); ///mvz 29.01.2010
    void DrawSliceOutTrackParam( int itr, int color, Size_t width ); ///mvz 03.02.2010
    void DrawSliceOutTrack1( int itr, int color, Size_t width ); ///mvz 03.02.2010
    void DrawHelix(float p0, float c, float z, float zStart, float z0, float xc, float yc, float r, float b, int color, Size_t width); ///mvz 10.02.2010
///mvz end 20.01.2010
    void DrawSliceOutTrack( int itr, int color = -1, Size_t width = -1  );
    void DrawSliceOutTrack( AliHLTTPCCATrackParam &t, double Alpha, int itr, int color = -1, Size_t width = -1  );
    //void DrawSliceTrack( int itr, int color = -1 );
    bool DrawTrack( AliHLTTPCCATrackParam t, double Alpha, const AliHLTTPCCADisplayTmpHit *vHits,
                    int NHits, int color = -1, Size_t width = -1, bool pPoint = 0 );

    void DrawGBTrack( int itr, int color = -1, int width = -1 );
    void DrawGBTrackFast( AliHLTTPCCAGBTracker &tracker, int itr, int color = -1 );
    bool DrawTracklet( AliHLTTPCCATrackParam &track, const int *hitstore, int color = -1, int width = -1, bool pPoint = 0 );

//     void DrawGBLinks( AliHLTTPCCAGBTracker &tracker, int color = -1, Size_t width = -1 );
    
    void DrawGBHit( AliHLTTPCCAGBTracker &tracker, int iHit, int color = -1, Size_t width = -1 );
    void DrawGBHits( AliHLTTPCCAGBTracker &tracker, int color = -1, Size_t width = -1, int hitsType = -1 );

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

  protected:

    TCanvas *fCanvas;              // the canvas
    TPad *fYX, *fZX;               // two views
    bool fAsk;                      // flag to ask for the pressing key
    bool fSliceView;               // switch between slice/TPC zoom
    AliHLTTPCCATracker *fSlice;      // current CA tracker, includes slice geometry
    AliHLTTPCCAGBTracker *fGB;      // the global tracker
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

    bool fDrawOnlyRef; // draw only clusters from ref. mc tracks


  private:
    AliHLTTPCCADisplay( const AliHLTTPCCADisplay& );
    AliHLTTPCCADisplay& operator=( const AliHLTTPCCADisplay& );
};

#endif

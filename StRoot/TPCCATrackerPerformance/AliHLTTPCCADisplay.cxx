// $Id: AliHLTTPCCADisplay.cxx,v 1.5 2012/08/13 19:35:05 fisyak Exp $
// **************************************************************************
// This file is property of and copyright by the ALICE HLT Project          *
// ALICE Experiment at CERN, All rights reserved.                           *
//                                                                          *
// Primary Authors: Sergey Gorbunov <sergey.gorbunov@kip.uni-heidelberg.de> *
//                  Ivan Kisel <kisel@kip.uni-heidelberg.de>                *
//                  for The ALICE HLT Project.                              *
//                                                                          *
// Developed by:   Igor Kulakov <I.Kulakov@gsi.de>                          *
//                 Maksym Zyzak <M.Zyzak@gsi.de>                            *
//                                                                          *
// Permission to use, copy, modify and distribute this software and its     *
// documentation strictly for non-commercial purposes is hereby granted     *
// without fee, provided that the above copyright notice appears in all     *
// copies and that both the copyright notice and this permission notice     *
// appear in the supporting documentation. The authors make no claims       *
// about the suitability of this software for any purpose. It is            *
// provided "as is" without express or implied warranty.                    *
//                                                                          *
//***************************************************************************

// TODO now we use right CS, so z change sign, but it wasn't changed everywhere in the Display-code

#include "AliHLTTPCCADisplay.h"

//#define DRAW_3D // infrequent use for drawing 3D pictures

#include "AliHLTTPCCATracker.h"
#include "AliHLTTPCCAGBTracker.h"
#include "AliHLTTPCCARow.h"
#include "AliHLTTPCCATrack.h"
#include "AliHLTTPCCAGBTrack.h"
#include "AliHLTTPCCAGBHit.h"
#include "AliHLTTPCCAPerformance.h"
#include "AliHLTTPCCAMCTrack.h"
#include "AliHLTTPCCAOutTrack.h"
#include "AliHLTTPCCAParam.h"
#include "AliHLTTPCCASliceOutput.h"
#include "AliHLTTPCCAClusterData.h"
#include "AliHLTTPCCATrack.h"

#include "TString.h"
#include "Riostream.h"
#include "TMath.h"
#include "TStyle.h"
#include "TCanvas.h"
#include "TApplication.h"
#include "TLatex.h"

//#define DRAWDZDSQPT
#define ROWS5

#ifdef DRAW_3D
  #include "TVector3.h"
#endif // DRAW_3D



#define RZ
using namespace std;
class AliHLTTPCCADisplay::AliHLTTPCCADisplayTmpHit
{

  public:

    int ID() const { return fHitID; }
    double S() const { return fS; }
    double Z() const { return fZ; }

    void SetID( int v ) { fHitID = v; }
    void SetS( double v ) { fS = v; }
    void SetZ( double v ) { fZ = v; }

    static bool CompareHitDS( const AliHLTTPCCADisplayTmpHit &a,
                                const AliHLTTPCCADisplayTmpHit  &b ) {
      return ( a.fS < b.fS );
    }

    static bool CompareHitZ( const AliHLTTPCCADisplayTmpHit &a,
                               const AliHLTTPCCADisplayTmpHit  &b ) {
      return ( a.fZ < b.fZ );
    }

  private:

    int fHitID; // hit ID
    double fS;  // hit position on the XY track curve
    double fZ;  // hit Z position

};



AliHLTTPCCADisplay &AliHLTTPCCADisplay::Instance()
{
  // reference to static object
  static AliHLTTPCCADisplay gAliHLTTPCCADisplay;

  return gAliHLTTPCCADisplay;
}

AliHLTTPCCADisplay::AliHLTTPCCADisplay() : fYX( 0 ), fZX( 0 ), fAsk( 1 ), fSliceView( 1 ), fSlice( 0 ), fGB( 0 ), fPerf( 0 ),
    fCos( 1 ), fSin( 0 ), fZMin( -210 ), fZMax( 210 ), fYMin( -210 ), fYMax( 210 ), fSliceCos( 1 ), fSliceSin( 0 ),
    fRInnerMin( 83.65 ), fRInnerMax( 133.3 ), fROuterMin( 133.5 ), fROuterMax( 247.7 ),
    fTPCZMin( -210. ), fTPCZMax( 210 ), fArc(), fLine(), fPLine(), fMarker(), fBox(), fCrown(), fLatex(), fDrawOnlyRef( 0 ) // iklm. This is just default. If they are not correct SetTPC(...) can and should be used!
{
  fPerf = &( AliHLTTPCCAPerformance::Instance() );
  // constructor
}

AliHLTTPCCADisplay::~AliHLTTPCCADisplay()
{
  // destructor
//  if (fYX) delete fYX;
//  if (fZX) delete fZX;
}

//#define CANVASPT
void AliHLTTPCCADisplay::Init()
{
  static bool firstCall = 1;
  if ( firstCall ) {
    if ( !gApplication ) new TApplication( "myapp", 0, 0 );

      // initialization
    gStyle->SetCanvasBorderMode( 0 );
    gStyle->SetCanvasBorderSize( 1 );
    gStyle->SetCanvasColor( 0 );
//    fCanvas = new TCanvas( "CA", "CA Display", 2000, 1000 );
    fCanvas = new TCanvas( "CA", "CA Display", 1500, 750 );
    fCanvas->Divide( 2, 1 );
    fYX = static_cast<TPad *>( fCanvas->GetPrimitive( "CA_1" ) ); // ("YX", "YX window", -1, 0, 600, 600);
    fZX = static_cast<TPad *>( fCanvas->GetPrimitive( "CA_2" ) ); // ("ZX", "ZX window", -610, 0, 590, 600);
    fYX->SetCanvas( fCanvas );
    fYX->SetTitle( "YX" );
    fZX->SetCanvas( fCanvas );
    fZX->SetTitle( "ZX" );
    fMarker = TMarker( 0.0, 0.0, 20 );//6);
    fDrawOnlyRef = 0;
#ifdef CANVASPT
    // --- PT ---
    fCanvasPT = new TCanvas( "CA PT", "CA PT test Display", 1500, 750 );
    fCanvasPT->Divide( 1, 1 );
    fPT = static_cast<TPad *>( fCanvasPT->GetPrimitive( "CA PT_1" ) );
    fPT->SetCanvas( fCanvasPT );
    fPT->SetTitle( "PhiTeta" );
    fPT->cd();
//    fPT->Range( -1.5, -0.7, 1.5, 1.2 );
//    fPT->Range( -1.5, -1.5, 1.5, 1.5 );
//    fPT->Range( -1.5, 0, 1.5, 1.5 );
    fPT->Range( -1.7, 0, 1.7, 25 );
//    fPT->Range( -1.5, 0, 1.5, 3 );
//    fPT->Range( -1.5, -0.15, 1.5, 0.15 );
//    fPT->Range( -1.5, -1, 1.5, 46 );
//    fPT->Range( -1.5, 50, 1.5, 190 );
//    fPT->Range( -2.5, 50, 2.5, 190 );
//    fPT->Range( 0, 50, 2.5, 190 );
//    fPT->Range( -0.5, 50, 0.5, 190 );
//    fPT->Range( 0, 50, 10, 190 );
//    fPT->Range( 0, 50, 3, 190 );
//    fPT->Range( -0.15, 50, 0.15, 190 );
//    fPT->Range( -1, -0.7, 46, 1.2 );
#ifdef DRAWDZDSQPT
    fPT->Range( -3.2, -3, 3.3, 33 );
#endif
#ifdef ROWS5
//    fPT->Range( -0.5, -55, 2.5, 55 );	// X - Teta
//    fPT->Range( -5, -55, 250, 55 );	// Z - Z
//    fPT->Range( -0, -0.35, 2, 0.35 );	// Phi - Teta
//    fPT->Range( -0.005, 0, 0.005, 0.02 );	// U - V
    fPT->Range( 0, -0.005, 0.02, 0.005 );	// U - V
#endif
    // ---
#endif

    firstCall = 0;
  }
}

void AliHLTTPCCADisplay::Update()
{
  // update windows
  if ( !fAsk ) return;
  fYX->Update();
  fZX->Update();
#ifdef CANVASPT
  fPT->Update();
#endif
//  fCanvasPT->Update();
//X   fYX->Print( "YX.pdf" );
//X   fZX->Print( "ZX.pdf" );

}

void AliHLTTPCCADisplay::ClearView()
{
  // clear windows
  fYX->Clear();
  fZX->Clear();
}

void AliHLTTPCCADisplay::ClearViewPT()
{
  // clear windows
//  fCanvasPT->Clear();
#ifdef CANVASPT
  fPT->Clear();
#endif
}

void AliHLTTPCCADisplay::Ask()
{
  // wait for the pressed key, when "r" pressed, don't ask anymore
  char symbol;
  if ( fAsk ) {
    Update();
    std::cout << "ask> ";
    do {
      std::cin.get( symbol );
      if ( symbol == 'r' )
        fAsk = false;
    } while ( symbol != '\n' );
  }
}


void AliHLTTPCCADisplay::SetSliceView()
{
  // switch to slice view
  fSliceView = 1;
}

void AliHLTTPCCADisplay::SetTPCView()
{
  // switch to full TPC view
  fSliceView = 0;
  fCos = 1;
  fSin = 0;
  fZMin = fTPCZMin;
  fZMax = fTPCZMax;
  fYMin = -fROuterMax;
  fYMax = fROuterMax;
}


void AliHLTTPCCADisplay::SetGB( const AliHLTTPCCAGBTracker * GBTracker )
{
  fGB = GBTracker;
}

void AliHLTTPCCADisplay::SetCurrentSlice( AliHLTTPCCATracker *slice )
{
  // set reference to the current CA tracker, and read the current slice geometry
  fSlice = slice;
  SetSliceTransform( slice );
  if ( fSliceView ) {
    fCos = slice->Param().SinAlpha();
    fSin = slice->Param().CosAlpha();
    fZMin = slice->Param().ZMin();
    fZMax = slice->Param().ZMax();
    ClearView();
    double r0 = .5 * ( slice->Param().RMax() + slice->Param().RMin() );
    double dr = .5 * ( slice->Param().RMax() - slice->Param().RMin() );
    fYMin = -dr;
    fYMax = dr;
    double cx = 0;
    double cy = r0;
    double cz = .5 * ( slice->Param().ZMax() + slice->Param().ZMin() );
    double dz = .5 * ( slice->Param().ZMax() - slice->Param().ZMin() ) * 1.2;
//    fYX->Range( cx - dr, cy - dr*1.05, cx + dr, cy + dr );
//    fYX->Range( cx - dr, -3, cx + dr, cy + dr );
    // --- norm ---
    fYX->Range( cx - dr, -25, cx + dr, cy + dr );
    fZX->Range( -10, -25, cz + dz, cy + dr );
    // ---
    // --- for tof test ---
    fYX->Range( cx - dr, -5, cx + dr, cy + dr +20 );
    fZX->Range( -10, -5, cz + dz, cy + dr +20 );
    // ---
//    fZX->Range( -0.005, -0.001, 0.005, 0.02 );
    //fYX->Range(cx-dr*.3, cy-dr*1.05, cx+dr*.3, cy-dr*.35);
    //fZX->Range(cz-dz, cy-dr*1.05, cz+dz, cy-dr*.3);

    //fYX->Range(cx-dr*.3, cy-dr*.8, cx-dr*.1, cy-dr*.75);
    //fZX->Range(cz-dz*0, cy-dr*.8, cz+dz, cy-dr*.75);

    //fYX->Range(cx-dr*.08, cy-dr*1., cx-dr*.02, cy-dr*0.7);
    //fZX->Range(cz-dz*.2, cy-dr*1., cz-dz*.05, cy-dr*0.7);

    //double x0 = cx-dr*.1, x1 = cx-dr*.05;
    //double y0 = cy-dr*1.05, y1 = cy-dr*0.7;
    //double z0 = cz-dz*.3, z1 = cz;
    //double xc = (x0+x1)/2, yc= (y0+y1)/2, zc=(z0+z1)/2;
    //double d = TMath::Max((x1-x0)/2,TMath::Max((y1-y0)/2,(z1-z0)/2));
    //fYX->Range(xc-d, yc-d, xc+d, yc+d);
    //fZX->Range(zc-d, yc-d, zc+d, yc+d);

  }
}

void AliHLTTPCCADisplay::SetSliceTransform( double alpha )
{
  fSliceCos = TMath::Cos( alpha );
  fSliceSin = TMath::Sin( alpha );
}

void AliHLTTPCCADisplay::SetSliceTransform( AliHLTTPCCATracker *slice )
{
  SetSliceTransform( slice->Param().Alpha() );
}


void AliHLTTPCCADisplay::DrawTPC()
{
  // schematically draw TPC detector
  fYX->cd();
  fYX->Range( -fROuterMax-2, -fROuterMax-2, fROuterMax+2, fROuterMax+2 );
  //fYX->Range( -fROuterMax*.7, -fROuterMax, fROuterMax*0., -fROuterMax*.5);
  fYX->Clear();
  {
    fArc.SetLineColor( kBlack );
    fArc.SetFillStyle( 0 );
    fYX->cd();

    TLatex Tl;
    Tl.SetTextSize(0.02);
    Tl.SetTextAlign(22);

    const int halfNumberOfSlices = AliHLTTPCCAParameters::NumberOfSlices/2;
    for ( int iSlice = 0; iSlice < 2*halfNumberOfSlices; iSlice++ ) {
      const AliHLTTPCCAParam &slicePar = fGB->Slice(iSlice).Param();
      fCrown.SetLineColor( kBlack );
      fCrown.SetFillStyle( 0 );
      fCrown.SetLineWidth( 0.1 );

      float fiBegin = slicePar.Alpha()-slicePar.DAlpha()/2.;
      float fiEnd = slicePar.Alpha()+slicePar.DAlpha()/2.;
//      std::cout << iSlice << (fiBegin + fiEnd)/2. << std::endl;
      int iSliceReal = slicePar.ISlice();
      float fiCenter = (fiBegin + fiEnd)/2.;
      float kText = (iSliceReal >= 12) ? 0.5 : 0.8;
      TString sISlice = "";
      sISlice += iSliceReal;
//      std::cout<<" --- slicePar.Alpha(): "<<slicePar.Alpha()<<";   slicePar.DAlpha(): "<<slicePar.DAlpha()<<"\n";
//      std::cout<<" --- fiBegin: "<<fiBegin<<";   fiEnd: "<<fiEnd<<";   iSlice: "<<iSlice<<";   iSliceReal: "<<iSliceReal<<"\n";
      std::cout<<" ----- fRInnerMin: "<<fRInnerMin<<";   fRInnerMax: "<<fRInnerMax<<";   kText: "<<kText<<";   sISlice: "<<sISlice<<"\n";
      std::cout<<" ------- fROuterMin: "<<fROuterMin<<";   fROuterMax: "<<fROuterMax<<"\n";
      if (iSlice%1 == 0)
        Tl.DrawLatex(cos(fiCenter)*fRInnerMin*kText, sin(fiCenter)*fRInnerMin*kText, sISlice);

      fiBegin *= 180./TMath::Pi();
      fiEnd *= 180./TMath::Pi();
//       float fiBegin = 360. / halfNumberOfSlices*iSlice;
//       float fiEnd = 360. / halfNumberOfSlices*( iSlice + 1 );
      fCrown.DrawCrown( 0, 0, fRInnerMin, fRInnerMax, fiBegin, fiEnd );
      fCrown.DrawCrown( 0, 0, fROuterMin, fROuterMax, fiBegin, fiEnd );
//       std::cout << fiBegin << " " << fiEnd << std::endl;
    }
  }
  fMarker.SetMarkerSize( 1.5 );
  fMarker.SetMarkerColor( kRed );
  fMarker.DrawMarker( 0, 0 );
  fZX->cd();
  fZX->Range( fZMin*1.01, -fROuterMax*1.01, fZMax*1.01, fROuterMax*1.01 );
  //std::cout << fTPCZMin<<"  "<<fTPCZMax<<std::endl;
  //fZX->Range( fTPCZMax*.1, -fROuterMax, fTPCZMax*.3, -fROuterMax*0.5 );
  fZX->Clear();
  TBox ZX;
  ZX.SetFillStyle( 0 );
  ZX.SetFillColor(0);
  ZX.SetLineWidth(0.1);
  ZX.DrawBox(fZMin,-fROuterMax,fZMax,fROuterMax);
  ZX.DrawBox(fZMin,-fROuterMax,0,fROuterMax);
}

void AliHLTTPCCADisplay::DrawSlice( AliHLTTPCCATracker *slice, bool DrawRows, bool DrawGrid )
{
  // draw current the TPC slice
  fYX->cd();
  double r0 = .5 * ( slice->Param().RMax() + slice->Param().RMin() );
  double dr = .5 * ( slice->Param().RMax() - slice->Param().RMin() );
  double cx = r0 * slice->Param().CosAlpha();
  double cy = r0 * slice->Param().SinAlpha();
  double raddeg = 180. / 3.1415;
  double a0 = raddeg * .5 * ( slice->Param().AngleMax() + slice->Param().AngleMin() );
  double da = raddeg * .5 * ( slice->Param().AngleMax() - slice->Param().AngleMin() );
  if ( fSliceView ) {
    cx = 0; cy = r0;
    a0 = 90.;
    fLatex.DrawLatex( cx - dr + dr*.05, cy - dr + dr*.05, Form( "YX, Slice %2i", slice->Param().ISlice() ) );
  } else {
    a0 += raddeg * TMath::ATan2( fSin, fCos );
  }
  fArc.SetLineColor( kBlack );
  fArc.SetFillStyle( 0 );
  fCrown.SetLineColor( kBlack );
  fCrown.SetFillStyle( 0 );
  fCrown.DrawCrown( 0, 0, fRInnerMin, fRInnerMax, a0 - da, a0 + da );
  fCrown.DrawCrown( 0, 0, fROuterMin, fROuterMax, a0 - da, a0 + da );
  //fCrown.DrawCrown(0,0, slice->Param().RMin(),slice->Param().RMax(), a0-da, a0+da );

  fLine.SetLineColor( kBlack );

  fZX->cd();

  double cz = .5 * ( slice->Param().ZMax() + slice->Param().ZMin() );
  double dz = .5 * ( slice->Param().ZMax() - slice->Param().ZMin() ) * 1.2;
  //fLine.DrawLine(cz+dz, cy-dr, cz+dz, cy+dr );
  if ( fSliceView ) fLatex.DrawLatex( cz - dz + dz*.05, cy - dr + dr*.05, Form( "ZX, Slice %2i", slice->Param().ISlice() ) );

  if ( DrawRows ) {
    fLine.SetLineWidth( 1 );
    fLine.SetLineColor( 17/*kBlack*/ );
    SetSliceTransform( fSlice );
    for ( int iRow = 0; iRow < fSlice->Param().NRows(); iRow++ ) {
      double x = fSlice->Data().RowX( iRow );
      const AliHLTTPCCARow &row = fSlice->Data().Row( iRow );
      double y = row.MaxY();
      double vx0, vy0, vx1, vy1;
      Slice2View( x, y, &vx0, &vy0 );
      Slice2View( x, -y, &vx1, &vy1 );
      fYX->cd();
      fLine.DrawLine( vx0, vy0, vx1, vy1 );
      fZX->cd();
      fLine.DrawLine( fTPCZMin, vy0, fTPCZMax, vy1 );
      if (DrawGrid) {
        const AliHLTTPCCAGrid &grid = row.Grid();
        int nBins =  grid.N();
        float Ymin;
        float Ymax;
        float Zmin;
        float Zmax;
//         std::cout << "row = " << iRow << " nHits = " << row.NHits() << " nBinsY = " << grid.Ny() << " nBinsZ = " << grid.Nz() << std::endl;
        for (int iBin = 0; iBin < nBins; iBin++){
          grid.GetBinBounds( iBin, Ymin, Ymax, Zmin, Zmax);
//          std::cout<<" --- iBin: "<<iBin<<";   x: "<<x<<";   Ymin: "<<Ymin<<";   Ymax: "<<Ymax<<";   Zmin: "<<Zmin<<";   Zmax: "<<Zmax<<"\n";

          Slice2View( x-1, Ymin, &vx0, &vy0 );
          Slice2View( x+1, Ymin, &vx1, &vy1 );
          fYX->cd();
          fLine.DrawLine( vx0, vy0, vx1, vy1 );
          fZX->cd();
          fLine.DrawLine( fabs(Zmin), vy0, fabs(Zmin), vy1 );

          Slice2View( x-1, Ymax, &vx0, &vy0 );
          Slice2View( x+1, Ymax, &vx1, &vy1 );
          fYX->cd();
          fLine.DrawLine( vx0, vy0, vx1, vy1 );
          fZX->cd();
          fLine.DrawLine( fabs(Zmax), vy0, fabs(Zmax), vy1 );
        }
      }
    }
  }

}

void AliHLTTPCCADisplay::DrawSlicePixel( AliHLTTPCCATracker *slice, float step, bool DrawRows, bool DrawGrid )
{
  // draw current the TPC slice
  const AliHLTTPCCARow &row_min = fSlice->Data().Row( 1 );
  const AliHLTTPCCARow &row_max = fSlice->Data().Row( 44 );
  double r01 = .5 * ( slice->Param().RMax() + slice->Param().RMin() );
  double dr1 = .5 * ( slice->Param().RMax() - slice->Param().RMin() );
  fYMin = -dr1;
  fYMax = dr1;
  double cx1 = 0;
  double cy1 = r01;
  double cz1 = .5 * ( slice->Param().ZMax() + slice->Param().ZMin() );
  double dz1 = .5 * ( slice->Param().ZMax() - slice->Param().ZMin() ) * 1.2;
//  fYX->Range( cx1 - dr1, -25, cx1 + dr1, cy1 + dr1 );
//  fYX->Range( -1, -100, 45*step, 100 );
  std::cout<<":   cx1: "<<cx1<<";   dr1: "<<dr1<<"\n";
  fYX->Range( (cx1 - dr1)*step, -25*step, (cx1 + dr1)*step, 45*step );
  fZX->Range( 0, -25*step, (cz1 + dz1)*step, 45*step );
  fYX->cd();
  double r0 = .5 * ( slice->Param().RMax() + slice->Param().RMin() );
  double dr = .5 * ( slice->Param().RMax() - slice->Param().RMin() );
  double cx = r0 * slice->Param().CosAlpha();
  double cy = r0 * slice->Param().SinAlpha();
  double raddeg = 180. / 3.1415;
  double a0 = raddeg * .5 * ( slice->Param().AngleMax() + slice->Param().AngleMin() );
  double da = raddeg * .5 * ( slice->Param().AngleMax() - slice->Param().AngleMin() );
  //
  for( int i = 0; i < 45; i++ ) {
    fLine.SetLineColor( kGray );
    fLine.DrawLine( (cx1 - dr1)*step, i*step, (cx1 + dr1)*step, i*step );
  }
//  if ( fSliceView ) {
//    cx = 0; cy = r0;
//    a0 = 90.;
//    fLatex.DrawLatex( cx - dr + dr*.05, cy - dr + dr*.05, Form( "YX, Slice %2i", slice->Param().ISlice() ) );
//  } else {
//    a0 += raddeg * TMath::ATan2( fSin, fCos );
//  }
//  fArc.SetLineColor( kBlack );
//  fArc.SetFillStyle( 0 );
//  fCrown.SetLineColor( kBlack );
//  fCrown.SetFillStyle( 0 );
//  fCrown.DrawCrown( 0, 0, fRInnerMin, fRInnerMax, a0 - da, a0 + da );
//  fCrown.DrawCrown( 0, 0, fROuterMin, fROuterMax, a0 - da, a0 + da );
  //fCrown.DrawCrown(0,0, slice->Param().RMin(),slice->Param().RMax(), a0-da, a0+da );

  fLine.SetLineColor( kBlack );

  fZX->cd();

  double cz = .5 * ( slice->Param().ZMax() + slice->Param().ZMin() );
  double dz = .5 * ( slice->Param().ZMax() - slice->Param().ZMin() ) * 1.2;
  //fLine.DrawLine(cz+dz, cy-dr, cz+dz, cy+dr );
//  if ( fSliceView ) fLatex.DrawLatex( cz - dz + dz*.05, cy - dr + dr*.05, Form( "ZX, Slice %2i", slice->Param().ISlice() ) );

//  if ( DrawRows ) {
//    fLine.SetLineWidth( 1 );
//    fLine.SetLineColor( 17/*kBlack*/ );
//    SetSliceTransform( fSlice );
//    for ( int iRow = 0; iRow < fSlice->Param().NRows(); iRow++ ) {
//      double x = fSlice->Data().RowX( iRow );
//      const AliHLTTPCCARow &row = fSlice->Data().Row( iRow );
//      double y = row.MaxY();
//      double vx0, vy0, vx1, vy1;
//      Slice2View( x, y, &vx0, &vy0 );
//      Slice2View( x, -y, &vx1, &vy1 );
//      fYX->cd();
//      fLine.DrawLine( vx0, vy0, vx1, vy1 );
//      fZX->cd();
//      fLine.DrawLine( fTPCZMin, vy0, fTPCZMax, vy1 );
//      if (DrawGrid) {
//        const AliHLTTPCCAGrid &grid = row.Grid();
//        int nBins =  grid.N();
//        float Ymin;
//        float Ymax;
//        float Zmin;
//        float Zmax;
////         std::cout << "row = " << iRow << " nHits = " << row.NHits() << " nBinsY = " << grid.Ny() << " nBinsZ = " << grid.Nz() << std::endl;
//        for (int iBin = 0; iBin < nBins; iBin++){
//          grid.GetBinBounds( iBin, Ymin, Ymax, Zmin, Zmax);
////          std::cout<<" --- iBin: "<<iBin<<";   x: "<<x<<";   Ymin: "<<Ymin<<";   Ymax: "<<Ymax<<";   Zmin: "<<Zmin<<";   Zmax: "<<Zmax<<"\n";
//
//          Slice2View( x-1, Ymin, &vx0, &vy0 );
//          Slice2View( x+1, Ymin, &vx1, &vy1 );
//          fYX->cd();
//          fLine.DrawLine( vx0, vy0, vx1, vy1 );
//          fZX->cd();
//          fLine.DrawLine( fabs(Zmin), vy0, fabs(Zmin), vy1 );
//
//          Slice2View( x-1, Ymax, &vx0, &vy0 );
//          Slice2View( x+1, Ymax, &vx1, &vy1 );
//          fYX->cd();
//          fLine.DrawLine( vx0, vy0, vx1, vy1 );
//          fZX->cd();
//          fLine.DrawLine( fabs(Zmax), vy0, fabs(Zmax), vy1 );
//        }
//      }
//    }
//  }

}

// ---
void AliHLTTPCCADisplay::DrawSliceGridPT( AliHLTTPCCATracker *slice, int np, int nt )
{
  // ---
  fLine.SetLineWidth( 1. );
  fLine.SetLineColor( 7 );
  double vx0, vy0, vx1, vy1;
//  Slice2View( 0, 100, &vx0, &vy0 );
//  Slice2View( /*fSlice->Data().RowX( 45 )*/100, 0, &vx1, &vy1 );
  fYX->cd();
//    fLine.DrawLine( vx0, vy0, vx1, vy1 );
//  fLine.DrawLine( 0, 0, 0, 190 );
  fZX->cd();
  //fLine.DrawLine( 0, 0, 0, 190 );
  //fLine.DrawLine( 0, 0, 200, 190 );
//  for( int i = 0; i < 300; i += 10 ) {
//      fLine.DrawLine( 0, 0, i, 190 );
//  }
  // ---
}

void AliHLTTPCCADisplay::DrawRow5()
{
  ClearViewPT();
  fLine.SetLineWidth( 1.5 );
  fLine.SetLineColor( kBlack );
  fPT->cd();
//  fLine.DrawLine( 0, -50, 0, 50 );
//  fLine.DrawLine( 0, -50, 200, -50 );
//  fLine.DrawLine( 0, 50, 200, 50 );
//  fLine.DrawLine( 200, -50, 200, 50 );
}

void AliHLTTPCCADisplay::SetSliceUV()
{
//  fYX->Clear();
//  fCos = slice->Param().SinAlpha();
//  fSin = slice->Param().CosAlpha();
//  fZMin = slice->Param().ZMin();
//  fZMax = slice->Param().ZMax();
//  double r0 = .5 * ( slice->Param().RMax() + slice->Param().RMin() );
//  double dr = .5 * ( slice->Param().RMax() - slice->Param().RMin() );
//  fYMin = -dr;
//  fYMax = dr;
//  double cx = 0;
//  double cy = r0;
//  double cz = .5 * ( slice->Param().ZMax() + slice->Param().ZMin() );
//  double dz = .5 * ( slice->Param().ZMax() - slice->Param().ZMin() ) * 1.2;
////    fYX->Range( cx - dr, cy - dr*1.05, cx + dr, cy + dr );
//  fYX->Range( cx - dr, -3, cx + dr, cy + dr );
//  fYX->Range( 0, -0.005, 0.02, 0.005 );
  fYX->Range( -0.005, /*0.005*/-0.0001, 0.005, 0.017 );
  fYX->Range( -0.05, /*0.005*/-0.06, 0.05, 0.06 );
  fYX->Range( -0.05, /*0.005*/-0.00003, 0.05, 0.00005 );
//  fYX->Range( -0.0021, /*0.005*/0., -0.0015, 0.0015 );
  fYX->Range( -0.01, -1, 0.01, 200 );
}

void AliHLTTPCCADisplay::DrawPointYX( float x, float y, int color, float width )
{
  fYX->cd();
  fMarker.SetMarkerSize( width );
  fMarker.SetMarkerColor( color );
  fMarker.DrawMarker( y, x );
  if( color == 0 || color == 10 ) {
    fMarker.SetMarkerSize( width/2 );
    fMarker.SetMarkerColor( kBlack );
    fMarker.DrawMarker( y, x );
  }
}

void AliHLTTPCCADisplay::DrawHitsRow5( AliHLTTPCCATracker *slice, int r, int color, Size_t width )
{

  // draw hits

//  for ( int iRow = 0; iRow < fSlice->Param().NRows(); iRow++ ) {
//    const AliHLTTPCCARow &row = fSlice->Data().Row( iRow );
//    for ( int ih = 0; ih < row.NHits(); ih++ ) {
//      DrawSliceHit( iRow, ih, color, width );
//    }
//  }
//  fZX->Clear();
  fYX->Clear();
//  for( int iRow = r*5+1; iRow < r*5+6; iRow++ ) {
  for( int iRow = 0; iRow < 45; iRow++ ) {
      std::cout<<" - iRow: "<<iRow<<"\n";
      const AliHLTTPCCARow &row = fSlice->Data().Row( iRow );
      for ( int ih = 0; ih < row.NHits(); ih++ ) {
	  const AliHLTTPCCARow &row = fSlice->Data().Row( iRow );
	  float x = fSlice->Data().HitDataXS( row, ih );
	  float y = fSlice->Data().HitPDataYS( row, ih );
	  float z = fSlice->Data().HitPDataZS( row, ih );
	  if ( fGB ) { // it looks like we have problem in HitDataXS functions above, so read coordinates from the other place
	      int id = fGB->FirstSliceHit()[slice->Param().ISlice()] + slice->Data().ClusterDataIndex( row, ih );
	      const AliHLTTPCCAGBHit &h = fGB->Hits()[id];
	      x = h.X();
	      y = h.Y();
	      z = h.Z();
	  }
	  float teta = atan(fabs(x)/fabs(z));
	  float phi = atan(y/fabs(x));
	  float u = x / ( x*x + y*y);
	  float v = y / ( x*x + y*y);
	  std::cout<<" ---  ih: "<<ih<<";   x: "<<x<<";   y: "<<y<<";   z: "<<z<<";   teta: "<<teta<<"\n";
	  std::cout<<" ----- u: "<<u<<";   v: "<<v<<"\n";
//	  fPT->cd();
//	  fZX->cd();
	  fYX->cd();
	  fMarker.SetMarkerSize( .5 );
//	  fMarker.SetMarkerColor( iRow%5 + 1 );
	  fMarker.SetMarkerColor( kBlue );
//	  fMarker.DrawMarker( fabs(z), y );
//	  fMarker.DrawMarker( teta, y );
//	  fMarker.DrawMarker( teta, phi );
	  fMarker.DrawMarker( v, u );
      }
  }
  fMarker.SetMarkerSize( 2.5 );
  fMarker.SetMarkerColor( kRed );
  fMarker.DrawMarker( 0, 0 );

  fLine.SetLineWidth( .3 );
  fLine.SetLineColor( kBlack );
  fLine.DrawLine( -0.0025, 0, 0.0025, 0 );
  fLine.DrawLine( 0, -15, 0, 15 );
  //
  TLatex Tl;
  Tl.SetTextSize(0.02);
  Tl.SetTextAlign(22);
  TString l = "U";
  Tl.DrawLatex(0.0007, 0.00017, l);
  l = "V";
  Tl.DrawLatex(0.0001, 0.001, l);

//  fYX->cd();
//  fMarker.SetMarkerSize( 2.5 );
//  fMarker.SetMarkerColor( kRed );
//  fMarker.DrawMarker( 0, 0 );
//  fLine.SetLineWidth( .3 );
//  fLine.SetLineColor( kBlack );
//  fLine.DrawLine( -25, 0, 25, 0 );
//  fLine.DrawLine( 0, -15, 0, 55 );
//  l = "X";
//  Tl.DrawLatex(10, 2.5, l);
//  l = "Y";
//  Tl.DrawLatex(2.5, 10, l);

  fZX->cd();
  fMarker.SetMarkerSize( 2.5 );
  fMarker.SetMarkerColor( kRed );
  fMarker.DrawMarker( 0, 0 );
  fLine.SetLineWidth( .3 );
  fLine.SetLineColor( kBlack );
  fLine.DrawLine( -25, 0, 150, 0 );
  fLine.DrawLine( 0, -15, 0, 55 );
  l = "Z";
  Tl.DrawLatex(10, 2.5, l);
  l = "R";
  Tl.DrawLatex(2.5, 10, l);
}

//void AliHLTTPCCADisplay::DrawHitRow5( AliHLTTPCCATracker *slice, int iRow, int iHit, int color, Size_t width )
//{
//  // draw hit
//  if ( !fSlice ) return;
//  const AliHLTTPCCARow &row = fSlice->Data().Row( iRow );
//  // float x = fSlice->Data().RowX( iRow );
//  float x = fSlice->Data().HitDataXS( row, iHit );
//  float y = fSlice->Data().HitPDataYS( row, iHit );
//  float z = fSlice->Data().HitPDataZS( row, iHit );
//
//  if ( fGB ) { // it looks like we have problem in HitDataXS functions above, so read coordinates from the other place
//    int id = fGB->FirstSliceHit()[fSlice->Param().ISlice()] + fSlice->Data().ClusterDataIndex( row, iHit );
//    const AliHLTTPCCAGBHit &h = fGB->Hits()[id];
//    x = h.X();
//    y = h.Y();
//    z = h.Z();
//  }
//
//  SetSliceTransform( fSlice );
//
//  if ( color < 0 ) {
//    if ( fPerf && fGB ) {
//      int id = fGB->FirstSliceHit()[fSlice->Param().ISlice()] + fSlice->Data().ClusterDataIndex( row, iHit );
//      const AliHLTTPCCAGBHit &h = fGB->Hits()[id];
//      int lab = -1;
//      if ( fPerf->HitLabelSize() > 0 ) lab = fPerf->HitLabel( h.ID() ).fLab[0];
//      color = GetColor( lab + 1 );
//      if ( lab >= 0 ) {
//        const AliHLTTPCCAMCTrack &mc = fPerf->MCTrack( lab );
//        if ( mc.P() >= 1. ) color = kRed;
//        else if ( fDrawOnlyRef ) return;
//      }
//    } else color = GetColorZ( z );
//  }
//  if ( width > 0 )fMarker.SetMarkerSize( width );
//  else fMarker.SetMarkerSize( .3 );
//  fMarker.SetMarkerColor( color );
//  double vx, vy;
//  Slice2View( x, y, &vx, &vy );
//  fYX->cd();
//  fMarker.DrawMarker( vx, vy );
//  fZX->cd();
//  fMarker.DrawMarker( fabs(z), vy );
////  fMarker.DrawMarker( z, vy );
//}
// ---

///mvz start 20.01.2010
void  AliHLTTPCCADisplay::DrawPoint(float x, float y, float z, int Start, float width )
{
  fMarker.SetMarkerSize( width );
  fMarker.SetMarkerColor( 2 +  Start);

  SetSliceTransform( fSlice );

  double vx, vy;

  Slice2View( x, y, &vx, &vy );

  fYX->cd();
  fMarker.DrawMarker( vx, vy );
  fZX->cd();
  fMarker.DrawMarker( z, vy );
}

void  AliHLTTPCCADisplay::DrawPointXYZ(float x, float y, float z, int color, Size_t width )
{
  fMarker.SetMarkerSize( width );
  fMarker.SetMarkerColor( color );

  SetSliceTransform( fSlice );

  double vx, vy;

  Slice2View( x, y, &vx, &vy );

  fYX->cd();
  fMarker.DrawMarker( vx, vy );
  fZX->cd();
  fMarker.DrawMarker( z, vy );
  if( color == 0 || color == 10 ) {
    fMarker.SetMarkerSize( width/2 );
    fMarker.SetMarkerColor( kBlack );
    fYX->cd();
    fMarker.DrawMarker( vx, vy );
    fZX->cd();
    fMarker.DrawMarker( z, vy );
  }
}

void  AliHLTTPCCADisplay::DrawPointXZfromXY(float x, float z, int color, Size_t width )
{
  fMarker.SetMarkerSize( width );
  fMarker.SetMarkerColor( color );

  SetSliceTransform( fSlice );

  double vx, vy;

  Slice2View( x, z, &vx, &vy );

  fZX->cd();
  fMarker.DrawMarker( vx, vy );
}
///mvz end 20.01.2010

///mvz start 28.01.2010
void  AliHLTTPCCADisplay::DrawGBPoint(const AliHLTTPCCAGBTracker &tracker, int iSlice, float x, float y, float z, int Start, Size_t width )
{
  AliHLTTPCCATracker &slice = tracker.Slices()[iSlice];
  SetSliceTransform( &slice );

  double vx, vy;
  Slice2View( x, y, &vx, &vy );

  fMarker.SetMarkerSize( width );
  fMarker.SetMarkerColor( 2 +  Start);

  fYX->cd();
  fMarker.DrawMarker( vx, vy );
  fZX->cd();
  fMarker.DrawMarker( z, vy );
}

void  AliHLTTPCCADisplay::DrawGBPoint(float x, float y, float z, int Start, Size_t width )
{
  fMarker.SetMarkerSize( width );
  fMarker.SetMarkerColor( Start);

  fYX->cd();
  fMarker.DrawMarker( x, y );
  fZX->cd();
  fMarker.DrawMarker( z, y );
}

void  AliHLTTPCCADisplay::DrawGBPoint(float p[6], int Start, int color, Size_t width )
{
  fMarker.SetMarkerSize( width );
  fMarker.SetMarkerColor( Start);
  fArrow.SetFillColor( color );
  fArrow.SetLineColor( color );
  fArrow.SetLineWidth( width );

  fYX->cd();
  fMarker.DrawMarker( p[0], p[1] );
  fArrow.DrawArrow(p[0], p[1], p[0]+p[3]*50, p[1]+p[4]*50, 0.003, "|>");
  fZX->cd();
  fMarker.DrawMarker( p[2], p[1] );
  fArrow.DrawArrow(p[2], p[1], p[2]+p[5]*50, p[1]+p[4]*50, 0.003, "|>");
}

///mvz end 28.01.2010
///mvz start 29.01.2010
void  AliHLTTPCCADisplay::DrawGBPoint(AliHLTTPCCATracker &slice, float x, float y, float z, int Start, Size_t width )
{
  SetSliceTransform( &slice );

  double vx, vy;
  Slice2View( x, y, &vx, &vy );

  fMarker.SetMarkerSize( width );
  fMarker.SetMarkerColor( 2 +  Start);

  fYX->cd();
  fMarker.DrawMarker( vx, vy );
  fZX->cd();
  fMarker.DrawMarker( z, vy );
}
///mvz end 29.01.2010

void AliHLTTPCCADisplay::Set2Slices( AliHLTTPCCATracker * slice )
{
  //* Set view for two neighbouring slices

  fSlice = slice;
  fSliceView = 0;
  const int halfNumberOfSlices = AliHLTTPCCAParameters::NumberOfSlices/2;
  fCos = TMath::Cos( TMath::Pi() / 2 - ( slice->Param().Alpha() + 1. / halfNumberOfSlices*TMath::Pi() ) );
  fSin = TMath::Sin( TMath::Pi() / 2 - ( slice->Param().Alpha() + 1. / halfNumberOfSlices*TMath::Pi() ) );
  fZMin = slice->Param().ZMin();
  fZMax = slice->Param().ZMax();
  ClearView();
  double r0 = .5 * ( slice->Param().RMax() + slice->Param().RMin() );
  double dr = .5 * ( slice->Param().RMax() - slice->Param().RMin() );
  double cx = 0;
  double cy = r0;
  fYX->Range( cx - 1.3*dr, cy - 1.1*dr, cx + 1.3*dr, cy + 1.1*dr );
  fYX->cd();
  int islice = slice->Param().ISlice();
  int jslice = slice->Param().ISlice() + 1;
  const int NumberOfSlices = AliHLTTPCCAParameters::NumberOfSlices;
  if ( islice == NumberOfSlices/2-1 ) jslice = 0;
  else if ( islice == NumberOfSlices-1 ) jslice = NumberOfSlices/2;
  fLatex.DrawLatex( cx - 1.3*dr + 1.3*dr*.05, cy - dr + dr*.05, Form( "YX, Slices %2i/%2i", islice, jslice ) );
  double cz = .5 * ( slice->Param().ZMax() + slice->Param().ZMin() );
  double dz = .5 * ( slice->Param().ZMax() - slice->Param().ZMin() ) * 1.2;
  fZX->Range( cz - dz, cy - 1.1*dr, cz + dz, cy + 1.1*dr );//+dr);
  fZX->cd();
  fLatex.DrawLatex( cz - dz + dz*.05, cy - dr + dr*.05, Form( "ZX, Slices %2i/%2i", islice, jslice ) );
}

int AliHLTTPCCADisplay::GetColor( int i ) const
{
  // Get color with respect to Z coordinate
  const Color_t kMyColor[9] = { kGreen, kBlue, kYellow, kCyan, kOrange,
                                kSpring, kTeal, kAzure, kViolet
                              };
  if ( i < 0 ) i = 0;
  if ( i == 0 ) return kBlack;
  return kMyColor[( i-1 )%9];
}

int AliHLTTPCCADisplay::GetColorZ( double z ) const
{
  // Get color with respect to Z coordinate
  const Color_t kMyColor[11] = { kGreen, kBlue, kYellow, kMagenta, kCyan,
                                 kOrange, kSpring, kTeal, kAzure, kViolet, kPink
                               };

  double zz = ( z - fZMin ) / ( fZMax - fZMin );
  int iz = ( int ) ( zz * 11 );
  if ( iz < 0 ) iz = 0;
  if ( iz > 10 ) iz = 10;
  return kMyColor[iz];
}

int AliHLTTPCCADisplay::GetColorY( double y ) const
{
  // Get color with respect to Z coordinate
  const Color_t kMyColor[11] = { kGreen, kBlue, kYellow, kMagenta, kCyan,
                                 kOrange, kSpring, kTeal, kAzure, kViolet, kPink
                               };

  double yy = ( y - fYMin ) / ( fYMax - fYMin );
  int iy = ( int ) ( yy * 11 );
  if ( iy < 0 ) iy = 0;
  if ( iy > 10 ) iy = 10;
  return kMyColor[iy];
}

int AliHLTTPCCADisplay::GetColorK( double k ) const
{
  // Get color with respect to Z coordinate
  const Color_t kMyColor[11] = { kRed, kBlue, kYellow, kMagenta, kCyan,
                                 kOrange, kSpring, kTeal, kAzure, kViolet, kPink
                               };
  const double kCLight = 0.000299792458;
  const double kBz = 5;
  double k2QPt = 100;
  if ( TMath::Abs( kBz ) > 1.e-4 ) k2QPt = 1. / ( kBz * kCLight );
  double qPt = k * k2QPt;
  double pt = 100;
  if ( TMath::Abs( qPt ) > 1.e-4 ) pt = 1. / TMath::Abs( qPt );

  double yy = ( pt - 0.1 ) / ( 1. - 0.1 );
  int iy = ( int ) ( yy * 11 );
  if ( iy < 0 ) iy = 0;
  if ( iy > 10 ) iy = 10;
  return kMyColor[iy];
}

void AliHLTTPCCADisplay::Global2View( double x, double y, double *xv, double *yv ) const
{
  // convert coordinates global->view
  *xv = x * fCos + y * fSin;
  *yv = y * fCos - x * fSin;
}


void AliHLTTPCCADisplay::Slice2View( double x, double y, double *xv, double *yv ) const
{
  // convert coordinates slice->view
  double xg = x * fSliceCos - y * fSliceSin;
  double yg = y * fSliceCos + x * fSliceSin;
  *xv = xg * fCos - yg * fSin;
  *yv = yg * fCos + xg * fSin;
}

void AliHLTTPCCADisplay::Slice2ViewPT( double x, double y, double z, double *phi, double *teta ) const
{
  // convert coordinates slice->view
  *phi = acos(x / sqrt(x*x + y*y));
  *teta = acos(x / sqrt(x*x + z*z));
}


void AliHLTTPCCADisplay::DrawGBHit( const AliHLTTPCCAGBTracker &tracker, int iHit, int color, Size_t width  )
{
  // draw hit
  const AliHLTTPCCAGBHit &h = tracker.Hits()[iHit];
  AliHLTTPCCATracker &slice = tracker.Slices()[h.ISlice()];
  SetSliceTransform( &slice );

  if ( color < 0 ) {
    if ( fPerf ) {
      int lab = fPerf->HitLabel( h.ID() ).fLab[0];
      color = GetColor( lab + 1 );
      if ( lab >= 0 ) {
        const AliHLTTPCCAMCTrack &mc = fPerf->MCTrack( lab );
        if ( mc.P() >= 1. ) color = kRed;
        else if ( fDrawOnlyRef ) return;
      }
    } else color = GetColorZ( h.Z() );
  }
  if ( width > 0 )fMarker.SetMarkerSize( width );
  else fMarker.SetMarkerSize( .3 );
  fMarker.SetMarkerColor( color );
  double vx, vy;
  Slice2View( h.X(), h.Y(), &vx, &vy );

  fYX->cd();
  fMarker.DrawMarker( vx, vy );
  fZX->cd();
  fMarker.DrawMarker( h.Z(), vy );
}

void AliHLTTPCCADisplay::DrawGBHits( const AliHLTTPCCAGBTracker &tracker, int color, Size_t width, int hitsType)
{
  // draw hits

  if ( !fPerf ) return;
  if ( width < 0 ) width = .3;

  for ( int iHit = 0; iHit < tracker.NHits(); iHit++ ) {
    const AliHLTTPCCAGBHit &h = tracker.Hits()[iHit];
//     if ((hitsType == 1) && (h.ISlice() >= 12)) continue;
//     if ((hitsType == 2) && (h.ISlice() < 12) ) continue;

    int col = color;
    if ( fPerf->GetHitLabels()->Size() > 0 ) {
      int imc = fPerf->HitLabel( h.ID() ).fLab[0];
      const AliHLTTPCCAMCTrack *mc = ( imc >= 0 ) ? &( fPerf->MCTrack( imc ) ) : 0;
      if ( fDrawOnlyRef && ( !mc || ( mc->P() < 1 ) ) ) continue;
      if ( color < 0 ) {
        if (hitsType == 1) {
          if (h.ISlice() >= 12) col = kBlue;
          if (h.ISlice() < 12) col = 8;
        }
        else{
          col = GetColor( imc + 1 ) ;
          if ( mc && ( mc->P() >= AliHLTTPCCAParameters::RefThreshold ) ) col = kRed;
        }
      }
    }

    AliHLTTPCCATracker &slice = tracker.Slices()[h.ISlice()];
    SetSliceTransform( &slice );

    fMarker.SetMarkerSize( width );
    fMarker.SetMarkerColor( col );
    double vx, vy;
    Slice2View( h.X(), h.Y(), &vx, &vy );

    fYX->cd();
    fMarker.DrawMarker( vx, vy );
    fZX->cd();
    fMarker.DrawMarker( h.Z(), vy );
  }
}

void AliHLTTPCCADisplay::DrawSliceHit( int iRow, int iHit, int color, Size_t width )
{
  // draw hit
  if ( !fSlice ) return;
  const AliHLTTPCCARow &row = fSlice->Data().Row( iRow );
  // float x = fSlice->Data().RowX( iRow );
  float x = fSlice->Data().HitDataXS( row, iHit );
  float y = fSlice->Data().HitPDataYS( row, iHit );
  float z = fSlice->Data().HitPDataZS( row, iHit );

  if ( fGB ) { // it looks like we have problem in HitDataXS functions above, so read coordinates from the other place
    int id = fGB->FirstSliceHit()[fSlice->Param().ISlice()] + fSlice->Data().ClusterDataIndex( row, iHit );
    const AliHLTTPCCAGBHit &h = fGB->Hits()[id];
    x = h.X();
    y = h.Y();
    z = h.Z();
  }

  SetSliceTransform( fSlice );

  if ( color < 0 ) {
    if ( fPerf && fGB ) {
      int id = fGB->FirstSliceHit()[fSlice->Param().ISlice()] + fSlice->Data().ClusterDataIndex( row, iHit );
      const AliHLTTPCCAGBHit &h = fGB->Hits()[id];
      int lab = -1;
      if ( fPerf->HitLabelSize() > 0 ) lab = fPerf->HitLabel( h.ID() ).fLab[0];
      color = GetColor( lab + 1 );
      if ( lab >= 0 ) {
        const AliHLTTPCCAMCTrack &mc = fPerf->MCTrack( lab );
        if ( mc.P() >= 1. ) color = kRed;
        else if ( fDrawOnlyRef ) return;
      }
    } else color = GetColorZ( z );
  }
  if ( width > 0 )fMarker.SetMarkerSize( width );
  else fMarker.SetMarkerSize( .3 );
  fMarker.SetMarkerColor( color );
  double vx, vy;
  Slice2View( x, y, &vx, &vy );
  fYX->cd();
  fMarker.DrawMarker( vx, vy );
  fZX->cd();
  fMarker.DrawMarker( fabs(z), vy );
//  fMarker.DrawMarker( z, vy );
}

void AliHLTTPCCADisplay::DrawSliceHitPT( int iRow, int iHit, int color, Size_t width )
{
  // draw hit
  if ( !fSlice ) return;
  const AliHLTTPCCARow &row = fSlice->Data().Row( iRow );
  // float x = fSlice->Data().RowX( iRow );
  float x = fSlice->Data().HitDataXS( row, iHit );
  float y = fSlice->Data().HitPDataYS( row, iHit );
  float z = fSlice->Data().HitPDataZS( row, iHit );

  if ( fGB ) { // it looks like we have problem in HitDataXS functions above, so read coordinates from the other place
    int id = fGB->FirstSliceHit()[fSlice->Param().ISlice()] + fSlice->Data().ClusterDataIndex( row, iHit );
    const AliHLTTPCCAGBHit &h = fGB->Hits()[id];
    x = h.X();
    y = h.Y();
    z = h.Z();
  }

  SetSliceTransform( fSlice );

  if ( color < 0 ) {
    if ( fPerf && fGB ) {
      int id = fGB->FirstSliceHit()[fSlice->Param().ISlice()] + fSlice->Data().ClusterDataIndex( row, iHit );
      const AliHLTTPCCAGBHit &h = fGB->Hits()[id];
      int lab = -1;
      if ( fPerf->HitLabelSize() > 0 ) lab = fPerf->HitLabel( h.ID() ).fLab[0];
      color = GetColor( lab + 1 );
      if ( lab >= 0 ) {
        const AliHLTTPCCAMCTrack &mc = fPerf->MCTrack( lab );
        if ( mc.P() >= 1. ) color = kRed;
        else if ( fDrawOnlyRef ) return;
      }
    } else color = GetColorZ( z );
  }
  if ( width > 0 )fMarker.SetMarkerSize( width );
  else fMarker.SetMarkerSize( .3 );
  fMarker.SetMarkerColor( color );
  double vx, vy;
  Slice2View( x, y, &vx, &vy );
  double phi0, teta0;
  Slice2ViewPT(x, y, z, &phi0, &teta0);
//  fYX->cd();
  fPT->cd();
//  fMarker.DrawMarker( 200*phi0-50, 100*teta0+70 );
//  fMarker.DrawMarker( phi0, teta0 );
  fMarker.DrawMarker( teta0, phi0 );
}

void AliHLTTPCCADisplay::DrawSliceHits( int color, Size_t width )
{

  // draw hits

  for ( int iRow = 0; iRow < fSlice->Param().NRows(); iRow++ ) {
    const AliHLTTPCCARow &row = fSlice->Data().Row( iRow );
    for ( int ih = 0; ih < row.NHits(); ih++ ) {
      DrawSliceHit( iRow, ih, color, width );
    }
  }
}

void AliHLTTPCCADisplay::DrawSliceHitsTest( int color, Size_t width )
{

  // draw hits

//  for ( int iRow = 0; iRow < fSlice->Param().NRows(); iRow++ ) {
//    const AliHLTTPCCARow &row = fSlice->Data().Row( iRow );
//    for ( int ih = 0; ih < row.NHits(); ih++ ) {
//      DrawSliceHit( iRow, ih, color, width );
//    }
//  }
  fLine.SetLineColor( kRed );
  fLine.SetLineWidth( 2. );
  for ( int iRow = fSlice->Param().NRows() - 1; iRow < fSlice->Param().NRows(); iRow++ ) {
//    std::cout<<" - iRow: "<<iRow<<"\n";
    const AliHLTTPCCARow &row = fSlice->Data().Row( iRow );
    const AliHLTTPCCARow &prow = fSlice->Data().Row( iRow - 1 );
    for ( int ih = 0; ih < row.NHits(); ih++ ) {
//	  std::cout<<" - ih: "<<ih<<"\n";
	  float dist = 1000;
	  int neighbour = -1;
	  int id = fGB->FirstSliceHit()[fSlice->Param().ISlice()] + fSlice->Data().ClusterDataIndex( row, ih );
	  const AliHLTTPCCAGBHit &h = fGB->Hits()[id];
	  float x = h.X();
	  float y = h.Y();
	  float z = h.Z();
	  float xx, yy, zz;
	  for ( int ih0 = 0; ih0 < prow.NHits(); ih0++ ) {
//	      std::cout<<" - ih0: "<<ih0<<"\n";
	      int id0 = fGB->FirstSliceHit()[fSlice->Param().ISlice()] + fSlice->Data().ClusterDataIndex( prow, ih0 );
	      const AliHLTTPCCAGBHit &h0 = fGB->Hits()[id0];
	      float x0 = h0.X();
	      float y0 = h0.Y();
	      float z0 = h0.Z();
	      float d0 = sqrt((y0-y)*(y0-y) + (z0-z)*(z0-z));
//	      float d0 = fabs(y0-y);
//	      std::cout<<" > d0: "<<d0<<"\n";
	      if( d0 < dist ) {
		  dist = d0;
		  neighbour = ih0;
		  xx = x0; yy = y0; zz = z0;
	      }
	      // ---
//	      double vx, vy, vx1, vy1;
//	      Slice2View( h.X(), h.Y(), &vx, &vy );
//	      Slice2View( h0.X(), h0.Y(), &vx1, &vy1 );
////	      std::cout<<" - Draw vx: "<<vx<<";   vy: "<<vy<<";   vx1: "<<vx1<<";   vy1: "<<vy1<<"\n";
//	      fYX->cd();
//	      fLine.DrawLine( vx, vy, vx1, vy1 );
//	      fZX->cd();
//	      fLine.DrawLine( fabs(h.Z()), vy, fabs(h0.Z()), vy1 );
	      // ---
	  }
//	  std::cout<<" - neighbour: "<<neighbour<<";   dist: "<<dist<<"\n";
	  if( neighbour >= 0 ) {
	      double vx, vy, vx1, vy1;
	      Slice2View( h.X(), h.Y(), &vx, &vy );
	      Slice2View( xx, yy, &vx1, &vy1 );
//	      std::cout<<" - Draw vx: "<<vx<<";   vy: "<<vy<<";   vx1: "<<vx1<<";   vy1: "<<vy1<<"\n";
	      fYX->cd();
	      fLine.DrawLine( vx, vy, vx1, vy1 );
	      fZX->cd();
	      fLine.DrawLine( fabs(h.Z()), vy, fabs(zz), vy1 );
	  }
    }
  }
}

void AliHLTTPCCADisplay::DrawSliceLineUVXYtest( float x1, float y1, float u1, float v1, float x2, float y2, float u2, float v2, int color, float width )
{
  fLine.SetLineColor( color );
  fLine.SetLineWidth( width );
  double vx, vy, vx1, vy1;
  Slice2View( x1, y1, &vx, &vy );
  Slice2View( x2, y2, &vx1, &vy1 );
  fZX->cd();
  fLine.DrawLine( vx, vy, vx1, vy1 );
  fYX->cd();
  fLine.DrawLine( v1, u1, v2, u2 );
}

void AliHLTTPCCADisplay::DrawSliceLine( float x1, float y1, float z1, float x2, float y2, float z2, int color, float width )
{
  fLine.SetLineColor( color );
  fLine.SetLineWidth( width );
  double vx, vy, vx1, vy1;
  Slice2View( x1, y1, &vx, &vy );
  Slice2View( x2, y2, &vx1, &vy1 );
  fYX->cd();
  fLine.DrawLine( vx, vy, vx1, vy1 );
//  std::cout<<"--- draw: vx: "<<vx<<";   vx1: "<<vx1<<";   vy: "<<vy<<";   vy1: "<<vy1<<"\n";
  fZX->cd();
  fLine.DrawLine( fabs(z1), vy, fabs(z2), vy1 );
}

void AliHLTTPCCADisplay::DrawSliceLine1( float x1, float y1, float z1, float x2, float y2, float z2, int color, float width )
{
  fLine.SetLineColor( color );
  fLine.SetLineWidth( width );
//  double vx, vy, vx1, vy1;
//  Slice2View( x1, y1, &vx, &vy );
//  Slice2View( x2, y2, &vx1, &vy1 );
  fYX->cd();
  fLine.DrawLine( y1, x1, y2, x2 );
//  std::cout<<"--- draw: vx: "<<vx<<";   vx1: "<<vx1<<";   vy: "<<vy<<";   vy1: "<<vy1<<"\n";
  fZX->cd();
  fLine.DrawLine( fabs(z1), x1, fabs(z2), x2 );
}

void  AliHLTTPCCADisplay::DrawCircle( float x, float y, float r, int color, float width )
{
  fYX->cd();
  double vx, vy;
  Slice2View( x, y, &vx, &vy );
  TEllipse *el1 = new TEllipse( vx, vy, r );
  el1->SetLineColor( color );
  el1->SetLineWidth( width );
  el1->SetFillStyle(0000);
  el1->Draw();
}

void  AliHLTTPCCADisplay::DrawCircleXZfromXY( float x, float y, float r, int color, float width )
{
  fZX->cd();
  double vx, vy;
  Slice2View( x, y, &vx, &vy );
  TEllipse *el1 = new TEllipse( vx, vy, r );
  el1->SetLineColor( color );
  el1->SetLineWidth( width );
  el1->SetFillStyle(0000);
  el1->Draw();
}

void AliHLTTPCCADisplay::InitUVXYtest( AliHLTTPCCATracker *slice, float y0, float x0, float y1, float x1 )
{
//  fYX->Clear();
//  fZX->Clear();

  fYX->Range( y0*35, x0, y1*35, x1 );
//  fYX->Range( y0/100., x0, y1/100., x1 );
//  fYX->Range( -0.005, /*0.005*/-0.0001, 0.005, 0.017 );
//  fYX->Range( -0.01, -1, 0.01, 200 );
//  fYX->Range( -5000000, -1, 5000000, 200 );
//  fYX->Range( -15000, -1, 15000, 200 );
//  fZX->Range( -fROuterMax-2, -fROuterMax-2, fROuterMax+2, fROuterMax+2 );
//  fZX->Range( -fROuterMax-2, -50, fROuterMax+2, 50 );
  double r01 = .5 * ( slice->Param().RMax() + slice->Param().RMin() );
  double dr1 = .5 * ( slice->Param().RMax() - slice->Param().RMin() );
  fYMin = -dr1;
  fYMax = dr1;
  double cx1 = 0;
  double cy1 = r01;
  double cz1 = .5 * ( slice->Param().ZMax() + slice->Param().ZMin() );
  double dz1 = .5 * ( slice->Param().ZMax() - slice->Param().ZMin() ) * 1.2;
  fZX->Range( cx1 - dr1, -25, cx1 + dr1, cy1 + dr1 );

  fZX->cd();
  double r0 = .5 * ( slice->Param().RMax() + slice->Param().RMin() );
  double dr = .5 * ( slice->Param().RMax() - slice->Param().RMin() );
  double cx = r0 * slice->Param().CosAlpha();
  double cy = r0 * slice->Param().SinAlpha();
  double raddeg = 180. / 3.1415;
  double a0 = raddeg * .5 * ( slice->Param().AngleMax() + slice->Param().AngleMin() );
  double da = raddeg * .5 * ( slice->Param().AngleMax() - slice->Param().AngleMin() );
  if ( fSliceView ) {
    cx = 0; cy = r0;
    a0 = 90.;
    fLatex.DrawLatex( cx - dr + dr*.05, cy - dr + dr*.05, Form( "YX, Slice %2i", slice->Param().ISlice() ) );
  } else {
    a0 += raddeg * TMath::ATan2( fSin, fCos );
  }
  fArc.SetLineColor( kBlack );
  fArc.SetFillStyle( 0 );
  fCrown.SetLineColor( kBlack );
  fCrown.SetFillStyle( 0 );
//  fCrown.DrawCrown( 0, 0, fRInnerMin, fRInnerMax, a0 - da, a0 + da );
//  fCrown.DrawCrown( 0, 0, fROuterMin, fROuterMax, a0 - da, a0 + da );
  fLine.SetLineColor( kBlack );
  //
//  fMarker.SetMarkerColor( kBlue );
//  fMarker.SetMarkerSize( 1. );
  fLine.SetLineColor( kBlue );
  fYX->cd();
  for( int i = 0; i < 44; i++ ) {
    const AliHLTTPCCARow &row1 = slice->Data().Row( i );
    const AliHLTTPCCARow &row2 = slice->Data().Row( i + 1 );
    float x1 = slice->Data().RowX( i );
    float x2 = slice->Data().RowX( i + 1 );
    float y11 = row1.MaxY();
    float y12 = - y11;
    float y21 = row2.MaxY();
    float y22 = - y21;
    const float dXY11 = ( x1*x1 + y11*y11 );
    float u11 = x1 / dXY11;
    float v11 = - y11 / dXY11;
    const float dXY12 = ( x1*x1 + y12*y12 );
    float u12 = x1 / dXY12;
    float v12 = - y12 / dXY12;
    const float dXY21 = ( x2*x2 + y21*y21 );
    float u21 = x2 / dXY21;
    float v21 = - y21 / dXY21;
    const float dXY22 = ( x2*x2 + y22*y22 );
    float u22 = x2 / dXY22;
    float v22 = - y22 / dXY22;
    fLine.DrawLine( v11*x1, 1./u11, v21*x2, 1./u21 );
    fLine.DrawLine( v12*x1, 1./u12, v22*x2, 1./u22 );
//    fLine.DrawLine( v11/x1, 1./u11, v21/x2, 1./u21 );
//    fLine.DrawLine( v12/x1, 1./u12, v22/x2, 1./u22 );
  }
}

void  AliHLTTPCCADisplay::DrawCircleUVXYtest( float x, float y, float r, int color, float width )
{
  fZX->cd();
  double vx, vy;
  Slice2View( x, y, &vx, &vy );
  TEllipse *el1 = new TEllipse( vx, vy, r );
  el1->SetLineColor( color );
  el1->SetLineWidth( width );
  el1->SetFillStyle(0000);
  el1->Draw();
}

void AliHLTTPCCADisplay::DrawRectangleXYZ( float x, float y, float z, float lx, float ly, float lz, int color, int border )
{
//  double vx, vy, vx1, vy1;
  float x1 = x + lx;
  float y1 = y + ly;
  float z1 = fabs(z) + lz;
//  Slice2View( x, y, &vx, &vy );
//  Slice2View( x1, y1, &vx1, &vy1 );
  fYX->cd();
//  TBox *sq1 = new TBox();
//  sq1->SetFillColor( color );
//  sq1->DrawBox( y, x, y1, x1 );
  TWbox *sqb1 = new TWbox();
//  sqb1->SetFillColor( color );
  sqb1->DrawWbox( y, x, y1, x1, color, 1, border );
  fZX->cd();
//  std::cout<<" > draw z: z: "<<z<<";   x: "<<x<<";   z1: "<<z1<<";   x1: "<<x1<<"\n";
//  sq1->DrawBox( fabs(z), x, z1, x1 );
  sqb1->DrawWbox( fabs(z), x, z1, x1, color, 1, border );
}

void  AliHLTTPCCADisplay::DrawPointXZ(float x, float z, int color, Size_t width )
{
  fMarker.SetMarkerSize( width );
  fMarker.SetMarkerColor( color );

  fZX->cd();
  fMarker.DrawMarker( z, x );
}

void  AliHLTTPCCADisplay::DrawPointXYZl( float x, float y, float z, int color, Size_t width )
{
  fMarker.SetMarkerSize( width );
  fMarker.SetMarkerColor( color );

  fYX->cd();
  fMarker.DrawMarker( y, x );

  fZX->cd();
  fMarker.DrawMarker( z, x );
}

void AliHLTTPCCADisplay::DrawSliceLineUV( float x1, float y1, float z1, float x2, float y2, float z2, int color, float width )
{
  fLine.SetLineColor( color );
  fLine.SetLineWidth( width );
  double vx, vy, vx1, vy1;
  Slice2View( x1, y1, &vx, &vy );
  Slice2View( x2, y2, &vx1, &vy1 );
  fYX->cd();
  fLine.DrawLine( vx, vy, vx1, vy1 );
//  fZX->cd();
//  fLine.DrawLine( fabs(z1), vy, fabs(z2), vy1 );
}

void AliHLTTPCCADisplay::DrawSliceLineUVXZ( float x1, float u1, float v1, float z1, float x2, float u2, float v2, float z2, int color, float width )
{
  fLine.SetLineColor( color );
  fLine.SetLineWidth( width );
//  TLatex Tl;
  double vu, vv, vu1, vv1;
  Slice2View( u1, v1, &vu, &vv );
  Slice2View( u2, v2, &vu1, &vv1 );
  fYX->cd();
  fLine.DrawLine( vu, vv, vu1, vv1 );
//  TString l = "N";
//  Tl.DrawLatex(vu, vv, l);
  fZX->cd();
//  Slice2View( u1, v1, &vu, &vv );
//  Slice2View( u2, v2, &vu1, &vv1 );
  fLine.DrawLine( fabs(z1), x1, fabs(z2), x2 );
}

void AliHLTTPCCADisplay::DrawSliceLineUVXZnoFabs( float x1, float u1, float v1, float z1, float x2, float u2, float v2, float z2, int color, float width )
{
  fLine.SetLineColor( color );
  fLine.SetLineWidth( width );
//  TLatex Tl;
  double vu, vv, vu1, vv1;
  Slice2View( u1, v1, &vu, &vv );
  Slice2View( u2, v2, &vu1, &vv1 );
  fYX->cd();
  fLine.DrawLine( vu, vv, vu1, vv1 );
//  TString l = "N";
//  Tl.DrawLatex(vu, vv, l);
  fZX->cd();
//  Slice2View( u1, v1, &vu, &vv );
//  Slice2View( u2, v2, &vu1, &vv1 );
//  float z1Draw = z1;
//  float z2Draw = z2;
//  if( z1 < 0 && z2 < 0 ) {
//    z1Draw = fabs(z1);
//    z2Draw = fabs(z2);
//  }
//  fLine.DrawLine( z1Draw, x1, z2Draw, x2 );
  fLine.DrawLine( z1, x1, z2, x2 );
}

void AliHLTTPCCADisplay::DrawPV( float x, float y, float z )
{
  fLine.SetLineColor( kRed );
  fLine.SetLineWidth( 3. );
  double vx, vy;
  Slice2View( x, y, &vx, &vy );
  TLatex Tl;
  Tl.SetTextSize( 0.02 );
  Tl.SetTextColor( kRed );
  TString l = "( ";
  l += y;
  l += "; ";
  l += x;
  l += " )";
  fYX->cd();
  fLine.DrawLine( vx-1, vy-1, vx+1, vy+1 );
  fLine.DrawLine( vx-1, vy+1, vx+1, vy-1 );
  Tl.DrawLatex(vx+3, vy, l);
}

void AliHLTTPCCADisplay::DrawPVUVXY( float x, float y, float u, float v )
{
  fMarker.SetMarkerColor( kRed );
  fMarker.SetMarkerSize( 3. );
  fYX->cd();
  fMarker.DrawMarker( x, y );
  fZX->cd();
  fMarker.DrawMarker( u, v );
}

void AliHLTTPCCADisplay::DrawNumber( float x, float y, float z, int num, int color )
{
//  fLine.SetLineColor( color );
//  fLine.SetLineWidth( 3. );
  double vx, vy;
  Slice2View( x, y, &vx, &vy );
  TLatex Tl;
  Tl.SetTextSize( 0.01 );
  Tl.SetTextColor( color );
  TString l = "";//"( ";
  l += num;
//  l += " )";
  fYX->cd();
//  fLine.DrawLine( vx-1, vy-1, vx+1, vy+1 );
//  fLine.DrawLine( vx-1, vy+1, vx+1, vy-1 );
  Tl.DrawLatex(vx+1, vy-0.5, l);
  fZX->cd();
  Tl.DrawLatex(z+1, x-0.5, l);
}

void AliHLTTPCCADisplay::DrawUVZero( float x, float y, float z )
{
  fLine.SetLineColor( kRed );
  fLine.SetLineWidth( 3. );
  double vx, vy;
  Slice2View( x, y, &vx, &vy );
  fYX->cd();
  fLine.DrawLine( vx-0.0001, vy, vx+0.0001, vy );
  fLine.DrawLine( vx, vy+0.0001, vx, vy-0.0001 );
}

void AliHLTTPCCADisplay::DrawNumber( float x, float u, float v, float z, int N, float width )
{
  TLatex Tl;
  Tl.SetTextSize(0.015);
  double vu, vv;
  Slice2View( u, v, &vu, &vv );
  fYX->cd();
  TString l = "";
  l += N;
  Tl.DrawLatex(vu, vv, l);
  fZX->cd();
  Tl.DrawLatex(fabs(z), x, l);
//  TString sISlice = "";
//  sISlice += iSliceReal;
}

void AliHLTTPCCADisplay::DrawSliceHitsPT( int color, Size_t width )
{

  // draw hits

  for ( int iRow = 0; iRow < fSlice->Param().NRows(); iRow++ ) {
    const AliHLTTPCCARow &row = fSlice->Data().Row( iRow );
    for ( int ih = 0; ih < row.NHits(); ih++ ) {
      DrawSliceHitPT( iRow, ih, color, width );
    }
  }
}


void AliHLTTPCCADisplay::DrawSliceLink( int iRow, int iHit, int colorUp, int colorDn, int width )
{
  // draw link between clusters

  if ( !fPerf || !fGB ) return;
  const AliHLTTPCCAGBTracker &tracker = *fGB;
  if ( width < 0 ) width = 1.;
  fLine.SetLineWidth( width );
  const int colUp = colorUp >= 0 ? colorUp : kMagenta;
  const int colDn = colorDn >= 0 ? colorDn : kBlack;
  const int colBoth = 8;

  const int rowStep = AliHLTTPCCAParameters::RowStep;
  if ( iRow < rowStep || iRow >= fSlice->Param().NRows() - rowStep ) return;

  const AliHLTTPCCARow& row = fSlice->Data().Row( iRow );
  const AliHLTTPCCARow& rowUp = fSlice->Data().Row( iRow + rowStep );
  const AliHLTTPCCARow& rowDn = fSlice->Data().Row( iRow - rowStep );

  int id = fSlice->Data().ClusterDataIndex( row, iHit );
  const AliHLTTPCCAGBHit &h = tracker.Hits()[tracker.FirstSliceHit()[fSlice->Param().ISlice()] + id];
  short iUp = fSlice->Data().HitLinkUpDataS( row, iHit );
  short iDn = fSlice->Data().HitLinkDownDataS( row, iHit );

//   double offset = (rowStep == 1) ? 0 : (( iRow & 1 ) ? 1. : -1.);
  const float offset = 0;
  double offsetUp = offset;
  double offsetDn = offset;

  if ( iUp >= 0 ) {
    int id1 = fSlice->Data().ClusterDataIndex( rowUp, iUp );
    const AliHLTTPCCAGBHit &h1 = tracker.Hits()[tracker.FirstSliceHit()[fSlice->Param().ISlice()] + id1];
    double vx, vy, vx1, vy1;
    Slice2View( h.X(), h.Y(), &vx, &vy );
    Slice2View( h1.X(), h1.Y(), &vx1, &vy1 );
    fLine.SetLineColor( colUp );
//     fLine.SetLineWidth( 0.2 );
    fYX->cd();
    fLine.DrawLine( vx + offsetUp, vy, vx1 + offsetUp, vy1 );
    fZX->cd();
//    fLine.DrawLine( h.Z() + offsetUp, vy, h1.Z() + offsetUp, vy1 );
    fLine.DrawLine( fabs(h.Z() + offsetUp), vy, fabs(h1.Z() + offsetUp), vy1 );
  }
  if ( iDn >= 0 ) {
    if ( fSlice->Data().HitLinkUpDataS( rowDn, iDn ) == iHit ) {
      fLine.SetLineColor( colBoth );
    } else {
      fLine.SetLineColor( colDn );
    }
//     fLine.SetLineWidth( 0.1 );
    int id1 = fSlice->Data().ClusterDataIndex( rowDn, iDn );
    const AliHLTTPCCAGBHit &h1 = tracker.Hits()[tracker.FirstSliceHit()[fSlice->Param().ISlice()] + id1];
    double vx, vy, vx1, vy1;
    Slice2View( h.X(), h.Y(), &vx, &vy );
    Slice2View( h1.X(), h1.Y(), &vx1, &vy1 );
    fYX->cd();
    fLine.DrawLine( vx + offsetDn, vy, vx1 + offsetDn, vy1 );
    fZX->cd();
    fLine.DrawLine( fabs(h.Z() + offsetDn), vy, fabs(h1.Z() + offsetDn), vy1 );
  }
}

void AliHLTTPCCADisplay::DrawSliceLinkPT( int iRow, int iHit, int colorUp, int colorDn, int width )
{
  // draw link between clusters

  if ( !fPerf || !fGB ) return;
  const AliHLTTPCCAGBTracker &tracker = *fGB;
  if ( width < 0 ) width = 1.;
  fLine.SetLineWidth( width );
  const int colUp = colorUp >= 0 ? colorUp : kMagenta;
  const int colDn = colorDn >= 0 ? colorDn : kBlack;
  const int colBoth = 8;

  const int rowStep = AliHLTTPCCAParameters::RowStep;
  if ( iRow < rowStep || iRow >= fSlice->Param().NRows() - rowStep ) return;

  const AliHLTTPCCARow& row = fSlice->Data().Row( iRow );
  const AliHLTTPCCARow& rowUp = fSlice->Data().Row( iRow + rowStep );
  const AliHLTTPCCARow& rowDn = fSlice->Data().Row( iRow - rowStep );

  int id = fSlice->Data().ClusterDataIndex( row, iHit );
  const AliHLTTPCCAGBHit &h = tracker.Hits()[tracker.FirstSliceHit()[fSlice->Param().ISlice()] + id];
  short iUp = fSlice->Data().HitLinkUpDataS( row, iHit );
  short iDn = fSlice->Data().HitLinkDownDataS( row, iHit );

//   double offset = (rowStep == 1) ? 0 : (( iRow & 1 ) ? 1. : -1.);
  const float offset = 0;
  double offsetUp = offset;
  double offsetDn = offset;

  if ( iUp >= 0 ) {
    int id1 = fSlice->Data().ClusterDataIndex( rowUp, iUp );
    const AliHLTTPCCAGBHit &h1 = tracker.Hits()[tracker.FirstSliceHit()[fSlice->Param().ISlice()] + id1];
    double vx, vy, vx1, vy1;
    Slice2View( h.X(), h.Y(), &vx, &vy );
    Slice2View( h1.X(), h1.Y(), &vx1, &vy1 );
    fLine.SetLineColor( colUp );
//     fLine.SetLineWidth( 0.2 );
    float phi = acos(h.X() / sqrt(h.X()*h.X() + h.Y()*h.Y()))*200;
    float t = 70+acos(h.X() / sqrt(h.X()*h.X() + h.Z()*h.Z()))*100;
    float phi1 = acos(h1.X() / sqrt(h1.X()*h1.X() + h1.Y()*h1.Y()))*200;
    float t1 = 70+acos(h1.X() / sqrt(h1.X()*h1.X() + h1.Z()*h1.Z()))*100;
    double phi0, phi01, teta0, teta01;
    Slice2ViewPT(h.X(), h.Y(), h.Z(), &phi0, &teta0);
    Slice2ViewPT(h1.X(), h1.Y(), h1.Z(), &phi01, &teta01);
    fPT->cd();
//    fLine.DrawLine( vx + offsetUp, vy, vx1 + offsetUp, vy1 );
//    fLine.DrawLine( phi, t, phi1, t1 );
//    std::cout<<">phi: "<<phi<<";     phi0: "<<phi0<<"\n";
//    std::cout<<"_teta: "<<t<<";     teta0: "<<teta0<<"\n";
//    fLine.DrawLine( phi0, teta0, phi01, teta01 );
    fLine.DrawLine( teta0, phi0, teta01, phi01 );
//    fZX->cd();
//    fLine.DrawLine( h.Z() + offsetUp, vy, h1.Z() + offsetUp, vy1 );
//    fLine.DrawLine( fabs(h.Z() + offsetUp), vy, fabs(h1.Z() + offsetUp), vy1 );
//    fLine.DrawLine( phi, t, phi1, t1 );
  }
  if ( iDn >= 0 ) {
    if ( fSlice->Data().HitLinkUpDataS( rowDn, iDn ) == iHit ) {
      fLine.SetLineColor( colBoth );
    } else {
      fLine.SetLineColor( colDn );
    }
//     fLine.SetLineWidth( 0.1 );
    int id1 = fSlice->Data().ClusterDataIndex( rowDn, iDn );
    const AliHLTTPCCAGBHit &h1 = tracker.Hits()[tracker.FirstSliceHit()[fSlice->Param().ISlice()] + id1];
    double vx, vy, vx1, vy1;
    Slice2View( h.X(), h.Y(), &vx, &vy );
    Slice2View( h1.X(), h1.Y(), &vx1, &vy1 );
    float phi = -50+acos(h.X() / sqrt(h.X()*h.X() + h.Y()*h.Y()))*200;
    float t = 70+acos(h.X() / sqrt(h.X()*h.X() + h.Z()*h.Z()))*100;
    float phi1 = -50+acos(h1.X() / sqrt(h1.X()*h1.X() + h1.Y()*h1.Y()))*200;
    float t1 = 70+acos(h1.X() / sqrt(h1.X()*h1.X() + h1.Z()*h1.Z()))*100;
    fPT->cd();
//    fLine.DrawLine( vx + offsetDn, vy, vx1 + offsetDn, vy1 );
    fLine.DrawLine( phi, t, phi1, t1 );
//    fZX->cd();
//    fLine.DrawLine( fabs(h.Z() + offsetDn), vy, fabs(h1.Z() + offsetDn), vy1 );
//    fLine.DrawLine( phi, t, phi1, t1 );
  }
}


void AliHLTTPCCADisplay::DrawSliceLinks( int colorUp, int colorDn, int width )
{
  // draw links between clusters

  for ( int iRow = 1; iRow < fSlice->Param().NRows() - 1; iRow++ ) {
    const AliHLTTPCCARow& row = fSlice->Data().Row( iRow );
    for ( int ih = 0; ih < row.NHits(); ih++ ) {
      DrawSliceLink( iRow, ih, colorUp, colorDn, width );
    }
  }
}

void AliHLTTPCCADisplay::DrawSliceLinksPT( int colorUp, int colorDn, int width )
{
  // draw links between clusters

  for ( int iRow = 1; iRow < fSlice->Param().NRows() - 1; iRow++ ) {
    const AliHLTTPCCARow& row = fSlice->Data().Row( iRow );
    for ( int ih = 0; ih < row.NHits(); ih++ ) {
      DrawSliceLinkPT( iRow, ih, colorUp, colorDn, width );
    }
  }
}



int AliHLTTPCCADisplay::GetTrackMC( const AliHLTTPCCADisplayTmpHit *vHits, int NHits )
{
  // get MC label for the track

  const AliHLTTPCCAGBTracker &tracker = *fGB;

  int label = -1;
  double purity = 0;
  int *lb = new int[NHits*3];
  int nla = 0;
  //std::cout<<"\n\nTrack hits mc: "<<std::endl;
  for ( int ihit = 0; ihit < NHits; ihit++ ) {
    const AliHLTTPCCAGBHit &h = tracker.Hits()[vHits[ihit].ID()];
    const AliHLTTPCCAHitLabel &l = fPerf->HitLabel( h.ID() );
    if ( l.fLab[0] >= 0 ) lb[nla++] = l.fLab[0];
    if ( l.fLab[1] >= 0 ) lb[nla++] = l.fLab[1];
    if ( l.fLab[2] >= 0 ) lb[nla++] = l.fLab[2];
    //std::cout<<ihit<<":  "<<l.fLab[0]<<" "<<l.fLab[1]<<" "<<l.fLab[2]<<std::endl;
  }
  sort( lb, lb + nla );
  int labmax = -1, labcur = -1, lmax = 0, lcurr = 0, nh = 0;
  //std::cout<<"MC track IDs :"<<std::endl;
  for ( int i = 0; i < nla; i++ ) {
    if ( lb[i] != labcur ) {
      if ( 0 && i > 0 && lb[i-1] >= 0 ) {
        const AliHLTTPCCAMCTrack &mc = fPerf->MCTrack( lb[i-1] );
        std::cout << lb[i-1] << ": nhits=" << nh << ", pdg=" << mc.PDG() << ", Pt=" << mc.Pt() << ", P=" << mc.P()
                  << ", par=" << mc.Par()[0] << " " << mc.Par()[1] << " " << mc.Par()[2]
                  << " " << mc.Par()[3] << " " << mc.Par()[4] << " " << mc.Par()[5] << " " << mc.Par()[6] << std::endl;

      }
      nh = 0;
      if ( labcur >= 0 && lmax < lcurr ) {
        lmax = lcurr;
        labmax = labcur;
      }
      labcur = lb[i];
      lcurr = 0;
    }
    lcurr++;
    nh++;
  }
  if ( 0 && nla - 1 > 0 && lb[nla-1] >= 0 ) {
    const AliHLTTPCCAMCTrack &mc = fPerf->MCTrack( lb[nla-1] );
    std::cout << lb[nla-1] << ": nhits=" << nh << ", pdg=" << mc.PDG() << ", Pt=" << mc.Pt() << ", P=" << mc.P()
              << ", par=" << mc.Par()[0] << " " << mc.Par()[1] << " " << mc.Par()[2]
              << " " << mc.Par()[3] << " " << mc.Par()[4] << " " << mc.Par()[5] << " " << mc.Par()[6] << std::endl;

  }
  if ( labcur >= 0 && lmax < lcurr ) {
    lmax = lcurr;
    labmax = labcur;
  }
  lmax = 0;
  for ( int ihit = 0; ihit < NHits; ihit++ ) {
    const AliHLTTPCCAGBHit &h = tracker.Hits()[vHits[ihit].ID()];
    const AliHLTTPCCAHitLabel &l = fPerf->HitLabel( h.ID() );
    if ( l.fLab[0] == labmax || l.fLab[1] == labmax || l.fLab[2] == labmax
       ) lmax++;
  }
  label = labmax;
  purity = ( ( NHits > 0 ) ? double( lmax ) / double( NHits ) : 0 );
  if ( lb ) delete[] lb;
  if ( purity < .9 ) label = -1;
  return label;
}


bool AliHLTTPCCADisplay::DrawTrack( AliHLTTPCCATrackParam t, double Alpha, const AliHLTTPCCADisplayTmpHit *vHits,
                                    int NHits, int color, Size_t width, bool pPoint )
{
  // draw track
#ifndef DRAW_3D
  bool drawEndPoints = 1;
#else
  bool drawEndPoints = 0;
#endif // DRAW_3D

  if ( NHits < 2 ) return 0;

  const AliHLTTPCCAGBTracker &tracker = *fGB;
  if ( width < 0 ) width = 2;

  if ( fDrawOnlyRef ) {
    int lab = GetTrackMC( vHits, NHits );
    if ( lab < 0 ) return 0;
    const AliHLTTPCCAMCTrack &mc = fPerf->MCTrack( lab );
    if ( mc.P() < 1 ) return 0;
  }

  if ( color < 0 ) {
    //color = GetColorZ( (vz[0]+vz[mHits-1])/2. );
    //color = GetColorK(t.GetKappa());
    int lab = GetTrackMC( vHits, NHits );
    color = GetColor( lab + 1 );
    if ( lab >= 0 ) {
      const AliHLTTPCCAMCTrack &mc = fPerf->MCTrack( lab );
      if ( mc.P() >= AliHLTTPCCAParameters::RefThreshold ) color = kRed;
    }
  }

  if ( t.SinPhi() > .999 )  t.SetSinPhi( .999 );
  else if ( t.SinPhi() < -.999 )  t.SetSinPhi( -.999 );

  //  int iSlice = fSlice->Param().ISlice();

  //sort(vHits, vHits + NHits, AliHLTTPCCADisplayTmpHit::CompareHitZ );

  double vx[2000], vy[2000], vz[2000];
  int mHits = 0;

  //int oldSlice = -1;
  double alpha = ( TMath::Abs( Alpha + 1 ) < 1.e-4 ) ? fSlice->Param().Alpha() : Alpha;
  AliHLTTPCCATrackParam tt = t;

  for ( int iHit = 0; iHit < NHits; iHit++ ) {

    const AliHLTTPCCAGBHit &h = tracker.Hits()[vHits[iHit].ID()];

    double hCos = TMath::Cos( alpha - tracker.Slices()[h.ISlice()].Param().Alpha() );
    double hSin = TMath::Sin( alpha - tracker.Slices()[h.ISlice()].Param().Alpha() );
    double x0 = h.X(), y0 = h.Y(), z1 = h.Z();
    double x1 = x0 * hCos + y0 * hSin;
    double y1 = y0 * hCos - x0 * hSin;

    {
      double dx = x1 - tt.X();
      double dy = y1 - tt.Y();
      if ( dx*dx + dy*dy > 1. ) {
        double dalpha = TMath::ATan2( dy, dx );
        if ( tt.Rotate( dalpha ) ) {
          alpha += dalpha;
          hCos = TMath::Cos( alpha - tracker.Slices()[h.ISlice()].Param().Alpha() );
          hSin = TMath::Sin( alpha - tracker.Slices()[h.ISlice()].Param().Alpha() );
          x1 = x0 * hCos + y0 * hSin;
          y1 = y0 * hCos - x0 * hSin;
        }
      }
    }
    SetSliceTransform( alpha );

    bool ok;
    //t.GetDCAPoint( x1, y1, z1, x1, y1, z1 );
//     ok = tt.TransportToX( x1, .999 );  // iklm. !!! commented because of Just link hits, no fit.
//     if ( 1 || ok ) {
//       x1 = tt.X();
//       y1 = tt.Y();
//       z1 = tt.Z();
//     }

    Slice2View( x1, y1, &x1, &y1 );
    vx[mHits] = x1;
    vy[mHits] = y1;
    vz[mHits] = z1;
    mHits++;
    for ( int j = 0; j < 0; j++ ) {
      x0 = h.X() + j; y0 = h.Y(); z1 = h.Z();
      x1 = x0 * hCos + y0 * hSin;
      y1 = y0 * hCos - x0 * hSin;
      ok = tt.TransportToX( x1, .999 );
      if ( ok ) {
        x1 = tt.X();
        y1 = tt.Y();
        z1 = tt.Z();
      }

      Slice2View( x1, y1, &x1, &y1 );
      vx[mHits] = x1;
      vy[mHits] = y1;
      vz[mHits] = z1;
      mHits++;
    }
  }
  if ( pPoint ) {
    double x1 = t.X(), y1 = t.Y(), z1 = t.Z();
    double a = ( TMath::Abs( Alpha + 1 ) < 1.e-4 ) ? fSlice->Param().Alpha() : Alpha;
    SetSliceTransform( a );

    Slice2View( x1, y1, &x1, &y1 );
    double dx = x1 - vx[0];
    double dy = y1 - vy[0];
    //std::cout<<x1<<" "<<y1<<" "<<vx[0]<<" "<<vy[0]<<" "<<dx<<" "<<dy<<std::endl;
    double d0 = dx * dx + dy * dy;
    dx = x1 - vx[mHits-1];
    dy = y1 - vy[mHits-1];
    //std::cout<<x1<<" "<<y1<<" "<<vx[mHits-1]<<" "<<vy[mHits-1]<<" "<<dx<<" "<<dy<<std::endl;
    double d1 = dx * dx + dy * dy;
    //std::cout<<"d0, d1="<<d0<<" "<<d1<<std::endl;
    if ( d1 < d0 ) {
      vx[mHits] = x1;
      vy[mHits] = y1;
      vz[mHits] = z1;
      mHits++;
    } else {
      for ( int i = mHits; i > 0; i-- ) {
        vx[i] = vx[i-1];
        vy[i] = vy[i-1];
        vz[i] = vz[i-1];
      }
      vx[0] = x1;
      vy[0] = y1;
      vz[0] = z1;
      mHits++;
    }
  }

#ifdef DRAW_3D
  const float zoom = 0.6;
  const float z_zoom = 1.2;
  const float z0_zoom = 1000.;
  for ( int i = mHits - 1; i >= 0; i-- ) {
    TVector3 v(vx[i], -vz[i], vy[i]);

    v.RotateX(TMath::Pi()/70);
    v.RotateZ(TMath::Pi()/12);
    vx[i] = v.X()*zoom;
    vy[i] = v.Z()*zoom;
    vz[i] = -v.Y()*zoom;

    vx[i] *= (z0_zoom-vz[i])/z0_zoom * z_zoom;
    vy[i] *= (z0_zoom-vz[i])/z0_zoom * z_zoom;
  }
#endif // DRAW_3D


  fLine.SetLineColor( color );
  fLine.SetLineWidth( width );
  fArc.SetFillStyle( 0 );
  fArc.SetLineColor( color );
  fArc.SetLineWidth( width );
  TPolyLine pl;
  pl.SetLineColor( color );
  pl.SetLineWidth( width );
  TPolyLine plZ;
  plZ.SetLineColor( color );
  plZ.SetLineWidth( width );

  fMarker.SetMarkerSize( width / 2. );
  fMarker.SetMarkerColor( color );

  fYX->cd();
  pl.DrawPolyLine( mHits, vx, vy );
  if (drawEndPoints) {
    fMarker.DrawMarker( vx[0], vy[0] );
    fMarker.DrawMarker( vx[mHits-1], vy[mHits-1] );
  }
  fZX->cd();
  plZ.DrawPolyLine( mHits, vz, vy );
  if (drawEndPoints) {
    fMarker.DrawMarker( vz[0], vy[0] );
    fMarker.DrawMarker( vz[mHits-1], vy[mHits-1] );
  }

  fLine.SetLineWidth( 1 );
  return 1;
}


bool AliHLTTPCCADisplay::DrawTracklet( AliHLTTPCCATrackParam &track, const int *hitstore, int color, int width, bool pPoint )
{
  // draw tracklet
  const AliHLTTPCCAGBTracker &tracker = *fGB;
  AliHLTTPCCADisplayTmpHit vHits[200];
  int nHits = 0;
  for ( int iRow = 0; iRow < fSlice->Param().NRows(); iRow++ ) {
    int iHit = hitstore[iRow];
    if ( iHit < 0 ) continue;
    const AliHLTTPCCARow &row = fSlice->Data().Row( iRow );
    int id = fSlice->Data().ClusterDataIndex( row, iHit );
    int iGBHit = tracker.FirstSliceHit()[fSlice->Param().ISlice()] + id;
    const AliHLTTPCCAGBHit &h = tracker.Hits()[iGBHit];
    vHits[nHits].SetID( iGBHit );
    vHits[nHits].SetS( 0 );
    vHits[nHits].SetZ( h.Z() );
    nHits++;
  }
  return DrawTrack( track, -1, vHits, nHits, color, width, pPoint );
}


void AliHLTTPCCADisplay::DrawSliceOutTrack( AliHLTTPCCATrackParam &t, double alpha, int itr, int color, Size_t width )
{
  // draw slice track
  const AliHLTTPCCASliceOutput *sliceData = fSlice->Output();

  const AliHLTTPCCASliceTrack &track = sliceData->Track( itr );
  if ( track.NClusters() < 2 ) return;

  const AliHLTTPCCAGBTracker &tracker = *fGB;
  AliHLTTPCCADisplayTmpHit vHits[200];

  for ( int ih = 0; ih < track.NClusters(); ih++ ) {

    const int outTrackHitIndex = track.FirstClusterRef() + ih;
    assert( outTrackHitIndex < sliceData->NTrackClusters() );
    int iClu = sliceData->ClusterIDrc( outTrackHitIndex ).Cluster();
    int iRow = sliceData->ClusterIDrc( outTrackHitIndex ).Row();
    const int id = fGB->FirstSliceHit(fSlice->Param().ISlice()) + fSlice->ClusterData().RowOffset( iRow ) + iClu;

    const AliHLTTPCCAGBHit &h = tracker.Hits()[id];
    vHits[ih].SetID( id );
    vHits[ih].SetS( 0 );
    vHits[ih].SetZ( h.Z() );
  }

  DrawTrack( t, alpha, vHits, track.NClusters(), color, width, 1 );
}

///mvz start 03.02.2010
#ifdef DO_TPCCATRACKER_EFF_PERFORMANCE
void AliHLTTPCCADisplay::DrawSliceOutTrackParam( int itr, int color, Size_t width )
{
  fYX->cd();

  AliHLTTPCCAOutTrack &track = fSlice->OutTracks1()[itr];

  AliHLTTPCCATrackParam t0 = track.StartPoint();
  AliHLTTPCCATrackParam t1 = track.EndPoint();

  double xStart,yStart,xEnd,yEnd;

  double vxStart = t0.X();
  double vxEnd   = t1.X();

  double vyStart = t0.Y();
  double vyEnd   = t1.Y();

  Slice2View( vxStart, vyStart, &xStart, &yStart );
  Slice2View( vxEnd, vyEnd, &xEnd, &yEnd );

  double r1 = 1./(t0.QPt() * fSlice->Param().cBz());

//std::cout << "r1  "<<r1<<"  r2  "<<r2 << endl;

  double R2 = r1*r1;
  double R = CAMath::Sqrt(R2);

  double a = r1/CAMath::Abs(r1);

  double xk = (xStart + xEnd)*0.5;
  double yk = (yStart + yEnd)*0.5;
  double l2 = (xStart - xEnd)*(xStart - xEnd) + (yStart - yEnd)*(yStart - yEnd);
  double l  = CAMath::Sqrt(l2);
  double li = 1./l;
  double d2 = R2 - l2*0.25;
  double d  = CAMath::Sqrt(d2);
  double xc = xk - a*d*li*(yEnd - yStart);
  double yc = yk + a*d*li*(xEnd - xStart);

  double fCos2 = fSliceCos*fSliceCos;
  double fSin2 = fSliceSin*fSliceSin;

  double dxStart2 = fSin2 * t0.GetErr2Y();
  double dyStart2 = fCos2 * t0.GetErr2Y();

  double dxEnd2 = fSin2 * t1.GetErr2Y();
  double dyEnd2 = fCos2 * t1.GetErr2Y();

  double ddx2 = dxStart2 + dxEnd2;
  double ddy2 = dyStart2 + dyEnd2;

  double dR2 = R2/(t0.QPt()*t0.QPt()) * t0.GetErr2QPt();

  double dl22 = ((xStart - xEnd)*(xStart - xEnd)*ddx2 + (yStart - yEnd)*(yStart - yEnd)*ddy2);
  double dl2  = li*li*dl22;
  double dd22 = R2*dR2 + 0.25*dl22;
  double dd2  = dd22/d2;


  double dxc2;
  double dyc2;

  double k = d*li*(yEnd - yStart);
  double ss = (yEnd - yStart)*(yEnd - yStart);
  dxc2 = 0.25*ddx2 + k*k*(ddy2/ss + dl2/(l*l) + dd2/d2);
  k = d*li*(xEnd - xStart);
  ss = (xEnd - xStart)*(xEnd - xStart);
  dyc2 = 0.25*ddy2 + k*k*(ddx2/ss + dl2/(l*l) + dd2/d2);

  std::cout <<"  XStart  "<<xStart<<"  YStart  "<<yStart<<"  xc  "<< xc <<" +- "<<sqrt(dxc2)<<"    yc  "<< yc <<" +- "<<sqrt(dyc2)<<std::endl;

  //std::cout <<"r  "<< sqrt(dR2/R2)*100 <<"  l  "<< sqrt(dl2/(l*l))*100<<"  d  "<< sqrt(dd2/d2)*100 << std::endl;
  //std::cout <<"xc "<< sqrt(dxc2/(xc*xc))*100 <<"  yc "<< sqrt(dyc2/(yc*yc))*100<< std::endl;

//  double rend = CAMath::Sqrt((xc - xEnd)*(xc - xEnd)+(yc - yEnd)*(yc - yEnd)) - R;
//  double rStart = CAMath::Sqrt((xc - xStart)*(xc - xStart)+(yc - yStart)*(yc - yStart)) - R;

//  std::cout <<"XC  "<< xc <<"  YC  "<<yc<<"  rEnd  "<<rend<<"  rStart  "<<rStart<<"  R  "<<R<< std::endl;
  TArc *trackpar = new TArc();
  trackpar->SetLineColor(color);
  trackpar->SetLineWidth(width);
  trackpar->SetFillStyle(0);
 // trackpar->DrawArc(xc,yc,R,0,360);

  fMarker.SetMarkerSize( 0.8 );
  fMarker.SetMarkerColor( color );

  double b = r1*t0.DzDs();

  double x = xStart;
  double y = yStart;
  double z = t0.Z();

  double xq = xStart;
  double yq = yStart;
  double zq = t0.Z();

  double p;
  double p0 = asin((yq-yc)/CAMath::Abs(r1));

  double pend = asin((yEnd-yc)/CAMath::Abs(r1));
  if(pend > 3.14) pend =  -(2*3.14 - pend);

  double c = (xq-xc)/CAMath::Abs((xq-xc));

  p = p0;
  z = zq + c*b*(p0 -p0) ;
  y = yc + R*sin(p0);
  x = xc + c*R*cos(p0);

    fYX->cd();
    fMarker.DrawMarker( x, y);
    fZX->cd();
    fMarker.DrawMarker( z, y);

  double zPrev = z;
  double xPrev = x;
  double yPrev = y;

  fLine.SetLineColor( color );
  fLine.SetLineWidth( width );

  for(int i=1; i<100; i++)
  {
    z = zq + (t1.Z()-zq)/100*i;
    p = p0 + c*(z-zq)/b;
    y = yc + R*sin(p);
    x = xc + c*R*cos(p);

    fYX->cd();
    fLine.DrawLine( x, y, xPrev, yPrev);
    fZX->cd();
    fLine.DrawLine( z, y, zPrev, yPrev);

    xPrev = x;
    yPrev = y;
    zPrev = z;
  }

/*  for(int i=0; i<30; i++)
  {
    p = p0 + c*CAMath::Abs(r1)/r1*(pend-p0)/30*i;
    z = zq + c*b*(p -p0) ;
    y = yc + R*sin(p);
    x = xc + c*R*cos(p);

    fYX->cd();
    fMarker.DrawMarker( x, y);
    fZX->cd();
    fMarker.DrawMarker( z, y);
  }*/

/*  p = p0 + c*CAMath::Abs(r1)/r1*0.08;
  z = zq + c*b*(p -p0) ;
  y = yc + R*sin(p);
  x = xc + c*R*cos(p);

    fYX->cd();
    fMarker.DrawMarker( x, y);
    fZX->cd();
    fMarker.DrawMarker( z, y);


  while(z<200)
  {
    z += 3 ;
    p = p0 + (z-zq)/b;
    y = yc - r1*sin(p);
    x = xc - r1*cos(p);

    fYX->cd();
    fMarker.DrawMarker( x, y);
    fZX->cd();
    fMarker.DrawMarker( z, y);
  }*/
}

void AliHLTTPCCADisplay::DrawSliceOutTrack1( int itr, int color, Size_t width )
{
  const AliHLTTPCCASliceOutput *sliceData = fSlice->Output();

  // draw slice track
  AliHLTTPCCAOutTrack &track = fSlice->OutTracks1()[itr];
  if ( track.NHits() < 2 ) return;

  const AliHLTTPCCAGBTracker &tracker = *fGB;
  AliHLTTPCCADisplayTmpHit vHits[200];

  for ( int ih = 0; ih < track.NHits(); ih++ ) {
    const int outTrackHitIndex = track.FirstHitRef()+ih;
    assert( outTrackHitIndex < sliceData->NTrackClusters() );
    int iClu = sliceData->ClusterIDrc( outTrackHitIndex ).Cluster();
    int iRow = sliceData->ClusterIDrc( outTrackHitIndex ).Row();
    const int id = fGB->FirstSliceHit(fSlice->Param().ISlice()) + fSlice->ClusterData().RowOffset( iRow ) + iClu;

    const AliHLTTPCCAGBHit &h = tracker.Hits()[id];
    vHits[ih].SetID( id );
    vHits[ih].SetS( 0 );
    vHits[ih].SetZ( h.Z() );
  }

  DrawTrack( track.StartPoint(), -1, vHits, track.NHits(), color, width );
}
#endif //DO_TPCCATRACKER_EFF_PERFORMANCE

///mvz end 03.02.2010
///mvz start 10.02.2010
void AliHLTTPCCADisplay::DrawHelix(float p0, float c, float z, float zStart, float z0, float xc, float yc, float r, float b, int color, Size_t width)
{
  fLine.SetLineColor(color);
  fLine.SetLineWidth(width);
  // draw slice track
  float x,y,p;
  p = p0 + c*(zStart-z0)/b;
  y = yc + r*sin(p);
  x = xc + c*r*cos(p);

  float zPrev = zStart;
  float xPrev = x;
  float yPrev = y;
  float zEnd = z;

  for(int i=1; i<100; i++)
  {
    z = zStart + (zEnd-zStart)/100*i;
    p = p0 + c*(z-z0)/b;
    y = yc + r*sin(p);
    x = xc + c*r*cos(p);

    fYX->cd();
    fLine.DrawLine( x, y, xPrev, yPrev);
    fZX->cd();
    fLine.DrawLine( z, y, zPrev, yPrev);

    xPrev = x;
    yPrev = y;
    zPrev = z;
  }
}
///mvz end 10.02.2010

void AliHLTTPCCADisplay::DrawParticleGlobal(float *param, float q, float tStart, float tEnd, float b, int color, Size_t width)
{
  fLine.SetLineColor(color);
  fLine.SetLineWidth(width);
  fArrow.SetFillColor( color );
  fArrow.SetLineColor( color );
  fArrow.SetLineWidth( width );

  float p[8];
  for(int iP=0; iP<8; iP++)
    p[iP] = param[iP];

  float t = tStart;

  float xPrev=0, yPrev=0, zPrev=0;

  const float kCLight = 0.000299792458;
  b = b*q*kCLight;

  for(int i=0; i<=100; i++)
  {
    t = tEnd/100*i;
    float bs= b*t;
    float s = sin(bs), c = cos(bs);
    float sB, cB;

    const float kOvSqr6 = 1./sqrt(6.);

    sB = (1.e-8 < fabs(bs)) ? (s/b) : ((1-bs*kOvSqr6)*(1+bs*kOvSqr6)*t) ;
    cB = (1.e-8 < fabs(bs)) ? ((1-c)/b) : (.5*sB*bs) ;

    float px = param[3];
    float py = param[4];
    float pz = param[5];

    p[0] = param[0] + sB*px + cB*py;
    p[1] = param[1] - cB*px + sB*py;
    p[2] = param[2] +  t*pz;
    p[3] =          c*px + s*py;
    p[4] =         -s*px + c*py;
    p[5] = param[5];
    p[6] = param[6];
    p[7] = param[7];

    if(i>0)
    {
      fYX->cd();
      fLine.DrawLine( p[0], p[1], xPrev, yPrev);
      fZX->cd();
      fLine.DrawLine( p[2], p[1], zPrev, yPrev);
    }

    xPrev = p[0];
    yPrev = p[1];
    zPrev = p[2];
  }

  DrawGBPoint( p[0], p[1], p[2], color );
  fYX->cd();
  fArrow.DrawArrow(p[0], p[1], p[0]+p[3]*50, p[1]+p[4]*50, 0.003, "|>");
  fZX->cd();
  fArrow.DrawArrow(p[2], p[1], p[2]+p[5]*50, p[1]+p[4]*50, 0.003, "|>");
}

void AliHLTTPCCADisplay::DrawSliceOutTrack( int itr, int color, Size_t width )
{
  // draw slice track
  const AliHLTTPCCASliceOutput *sliceData = fSlice->Output();

  // draw slice track
  const AliHLTTPCCASliceTrack &track = sliceData->Track( itr );
  if ( track.NClusters() < 2 ) return;

  const AliHLTTPCCAGBTracker &tracker = *fGB;
  AliHLTTPCCADisplayTmpHit vHits[200];

  for ( int ih = 0; ih < track.NClusters(); ih++ ) {
    const int outTrackHitIndex = track.FirstClusterRef() + ih;
    assert( outTrackHitIndex < sliceData->NTrackClusters() );
    int iClu = sliceData->ClusterIDrc( outTrackHitIndex ).Cluster();
    int iRow = sliceData->ClusterIDrc( outTrackHitIndex ).Row();
    const int id = fGB->FirstSliceHit(fSlice->Param().ISlice()) + fSlice->ClusterData().RowOffset( iRow ) + iClu;

    const AliHLTTPCCAGBHit &h = tracker.Hits()[id];
    vHits[ih].SetID( id );
    vHits[ih].SetS( 0 );
    vHits[ih].SetZ( h.Z() );
  }

  DrawTrack( track.Param(), -1, vHits, track.NClusters(), color, width );
}

/*
void AliHLTTPCCADisplay::DrawSliceTrack( int itr, int color )
{
  // draw slice track

  const AliHLTTPCCATrack &track = fSlice->Tracks()[itr];
  if ( track.NHits() < 2 ) return;

  const AliHLTTPCCAGBTracker &tracker = *fGB;
  AliHLTTPCCADisplayTmpHit vHits[200];
  for ( int ith = 0; ith < track.NHits(); ith++ ) {
    AliHLTTPCCAHitId ic = ( fSlice->TrackHits()[track.FirstHitID()+ith] );
    const AliHLTTPCCARow &row = fSlice->Data().Row( ic.RowIndex() );
    int ih = ic.HitIndex();
    int id = fSlice->Data().ClusterDataIndex( row, ih );
    int gbID = tracker.FirstSliceHit( fSlice->Param().ISlice() ) + id;
    const AliHLTTPCCAGBHit &h = tracker.Hit( gbID );
    vHits[ith].SetID( gbID );
    vHits[ith].SetS( 0 );
    vHits[ith].SetZ( h.Z() );
  }

  DrawTrack( track.Param(), -1, vHits, track.NHits(), color, -1 );
  //track.Param().Print();
}
*/

void AliHLTTPCCADisplay::DrawGBTrack( int itr, int color, int width )
{
  // draw global track

  const AliHLTTPCCAGBTracker &tracker = *fGB;
  AliHLTTPCCADisplayTmpHit vHits[1000];

  const AliHLTTPCCAGBTrack &track = tracker.Tracks()[itr];
  if ( track.NHits() < 2 ) return;
  for ( int ih = 0; ih < track.NHits(); ih++ ) {
    const int i = tracker.TrackHit( track.FirstHitRef() + ih );
    const AliHLTTPCCAGBHit &h = tracker.Hit( i );
    vHits[ih].SetID( i );
    vHits[ih].SetS( 0 );
    vHits[ih].SetZ( h.Z() );
  }

  DrawTrack( track.Param(), track.Alpha(), vHits, track.NHits(), color, width );
}


void AliHLTTPCCADisplay::DrawGBTrackFast( const AliHLTTPCCAGBTracker &tracker, int itr, int color )
{
  // draw global track

  AliHLTTPCCAGBTrack &track = tracker.Tracks()[itr];
  if ( track.NHits() < 2 ) return;
  int width = 1;

  AliHLTTPCCADisplayTmpHit *vHits = new AliHLTTPCCADisplayTmpHit[track.NHits()];
  AliHLTTPCCATrackParam t = track.Param();

  for ( int ih = 0; ih < track.NHits(); ih++ ) {
    int i = tracker.TrackHits()[ track.FirstHitRef() + ih];
    const AliHLTTPCCAGBHit *h = &( tracker.Hits()[i] );
    vHits[ih].SetID( i );
    vHits[ih].SetS( 0 );
    vHits[ih].SetZ( h->Z() );
  }

  sort( vHits, vHits + track.NHits(), AliHLTTPCCADisplayTmpHit::CompareHitZ );
  int colorY = color;
  {
    const AliHLTTPCCAGBHit &h1 = tracker.Hits()[ vHits[0].ID()];
    const AliHLTTPCCAGBHit &h2 = tracker.Hits()[ vHits[track.NHits()-1].ID()];
    if ( color < 0 ) color = GetColorZ( ( h1.Z() + h2.Z() ) / 2. );
    double gx1, gy1, gx2, gy2;
    Slice2View( h1.X(), h1.Y(), &gx1, &gy1 );
    Slice2View( h2.X(), h2.Y(), &gx2, &gy2 );
    if ( colorY < 0 ) colorY = GetColorY( ( gy1 + gy2 ) / 2. );
    color = colorY = GetColorK( t.GetQPt() );
  }

  fMarker.SetMarkerColor( color );//kBlue);
  fMarker.SetMarkerSize( 1. );
  fLine.SetLineColor( color );
  fLine.SetLineWidth( width );
  fArc.SetFillStyle( 0 );
  fArc.SetLineColor( color );
  fArc.SetLineWidth( width );
  TPolyLine pl;
  pl.SetLineColor( colorY );
  pl.SetLineWidth( width );

  int oldSlice = -1;
  double alpha = track.Alpha();
  // YX
  {

    const AliHLTTPCCAGBHit &h1 = tracker.Hits()[vHits[0].ID()];
    const AliHLTTPCCAGBHit &h2 = tracker.Hits()[vHits[track.NHits()-1].ID()];
    float x1, y1, z1, x2, y2, z2;
    double vx1, vy1, vx2, vy2;

    if ( h1.ISlice() != oldSlice ) {
      t.Rotate( tracker.Slices()[h1.ISlice()].Param().Alpha() - alpha );
      oldSlice = h1.ISlice();
      alpha = tracker.Slices()[h1.ISlice()].Param().Alpha();
      SetSliceTransform( &( tracker.Slices()[oldSlice] ) );
    }
    t.GetDCAPoint( h1.X(), h1.Y(), h1.Z(), x1, y1, z1, fSlice->Param().Bz()  );
    Slice2View( x1, y1, &vx1, &vy1 );

    if ( h2.ISlice() != oldSlice ) {
      t.Rotate( tracker.Slices()[h2.ISlice()].Param().Alpha() - alpha );
      oldSlice = h2.ISlice();
      alpha = tracker.Slices()[h2.ISlice()].Param().Alpha();
      SetSliceTransform( &( tracker.Slices()[oldSlice] ) );
    }
    t.GetDCAPoint( h2.X(), h2.Y(), h2.Z(), x2, y2, z2, fSlice->Param().Bz()  );
    Slice2View( x2, y2, &vx2, &vy2 );

    double x0 = t.GetX();
    double y0 = t.GetY();
    double sinPhi = t.GetSinPhi();
    double k = t.GetKappa( fSlice->Param().Bz() );
    double ex = t.GetCosPhi();
    double ey = sinPhi;

    if ( TMath::Abs( k ) > 1.e-4 ) {

      fYX->cd();

      double r = 1 / TMath::Abs( k );
      double xc = x0 - ey * ( 1 / k );
      double yc = y0 + ex * ( 1 / k );

      double vx, vy;
      Slice2View( xc, yc, &vx, &vy );

      double a1 = TMath::ATan2( vy1 - vy, vx1 - vx ) / TMath::Pi() * 180.;
      double a2 = TMath::ATan2( vy2 - vy, vx2 - vx ) / TMath::Pi() * 180.;
      if ( a1 < 0 ) a1 += 360;
      if ( a2 < 0 ) a2 += 360;
      if ( a2 < a1 ) a2 += 360;
      double da = TMath::Abs( a2 - a1 );
      if ( da > 360 ) da -= 360;
      if ( da > 180 ) {
        da = a1;
        a1 = a2;
        a2 = da;
        if ( a2 < a1 ) a2 += 360;
      }
      fArc.DrawArc( vx, vy, r, a1, a2, "only" );
      //fArc.DrawArc(vx,vy,r, 0,360,"only");
    } else {
      fYX->cd();
      fLine.DrawLine( vx1, vy1, vx2, vy2 );
    }
  }

  // ZX
  AliHLTResizableArray<double> py( track.NHits() ), pz( track.NHits() );

  for ( int iHit = 0; iHit < track.NHits(); iHit++ ) {

    const AliHLTTPCCAGBHit &h1 = tracker.Hits()[vHits[iHit].ID()];
    float x1, y1, z1;
    double vx1, vy1;
    if ( h1.ISlice() != oldSlice ) {
      t.Rotate( tracker.Slices()[h1.ISlice()].Param().Alpha() - alpha );
      oldSlice = h1.ISlice();
      alpha = tracker.Slices()[h1.ISlice()].Param().Alpha();
      SetSliceTransform( &( tracker.Slices()[oldSlice] ) );
    }
    t.GetDCAPoint( h1.X(), h1.Y(), h1.Z(), x1, y1, z1, fSlice->Param().Bz()  );
    Slice2View( x1, y1, &vx1, &vy1 );
    py[iHit] = vy1;
    pz[iHit] = z1;
  }


  fZX->cd();
  pl.DrawPolyLine( track.NHits(), pz.Data(), py.Data() );

  fLine.SetLineWidth( 1 );
  if (vHits) delete[] vHits;
}





#ifdef XXXX




void AliHLTTPCCADisplay::DrawMergedHit( int iRow, int iHit, int color )
{
  // connect two cells on display

#ifdef XXX

  const AliHLTTPCCARow &row = fSlice->Data().Row( iRow );
  const AliHLTTPCCAHit &h = row.Hits()[iHit];
  const AliHLTTPCCAHit &hyz = row.HitsYZ()[iHit];

  double x = row.X();
  double y = hyz.Y();
  double z = hyz.Z();
  double x1 = x, x2 = x;
  double y1 = y, y2 = y;
  double z1 = z, z2 = z;
  int iRow1 = iRow, iHit1 = iHit;
  int iRow2 = iRow, iHit2 = iHit;

  if ( fSlice->HitLinksDown()[] >= 0 ) {
    iRow1 = iRow - 1;
    iHit1 = h.LinkDown();
    const AliHLTTPCCARow &row1 = fSlice->Rows()[iRow1];
    AliHLTTPCCAHitYZ &h1 = row1.HitsYZ()[iHit1];
    x1 = row1.X();
    y1 = h1.Y();
    z1 = h1.Z();
  }
  if ( h.LinkUp() >= 0 ) {
    iRow2 = iRow + 1;
    iHit2 = h.LinkUp();
    const AliHLTTPCCARow &row2 = fSlice->Rows()[iRow2];
    AliHLTTPCCAHitYZ &h2 = row2.HitsYZ()[iHit2];
    x2 = row2.X();
    y2 = h2.Y();
    z2 = h2.Z();
  }
  if ( color < 0 ) color = GetColorZ( ( z + z1 + z2 ) / 3. );


  Slice2View( x, y, &x, &y );
  Slice2View( x1, y1, &x1, &y1 );
  Slice2View( x2, y2, &x2, &y2 );

  double lx[] = { x1, x, x2 };
  double ly[] = { y1, y, y2 };
  double lz[] = { z1, z, z2 };

  fPLine.SetLineColor( color );
  fPLine.SetLineWidth( 1 );
  //fPLine.SetFillColor(color);
  fPLine.SetFillStyle( -1 );

  fYX->cd();
  fPLine.DrawPolyLine( 3, lx, ly );
  fZX->cd();
  fPLine.DrawPolyLine( 3, lz, ly );
  DrawHit( iRow, iHit, color );
  DrawHit( iRow1, iHit1, color );
  DrawHit( iRow2, iHit2, color );
#endif
}


void AliHLTTPCCADisplay::DrawTrack( const AliHLTTPCCATrack &track, int color, bool DrawHits )
{
  // draw track

  if ( track.NHits() < 2 ) return;
  int width = 2;

  AliHLTTPCCADisplayTmpHit *vHits = new AliHLTTPCCADisplayTmpHit[track.NHits()];
  AliHLTTPCCATrackParam &t = track.Param();

  int iID = track.FirstHitID();
  int nhits = 0;
  {
    int iHit = 0;
    for ( int ih = 0; ih < track.NHits(); ih++ ) {
      int i = fSlice->TrackHits()[iID];
      const AliHLTTPCCAHit *h = &( fSlice->ID2Hit( i ) );
      const AliHLTTPCCARow &row = fSlice->ID2Row( i );
      vHits[iHit].ID() = i;
      vHits[iHit].S() = t.GetS( row.X(), h->Y() );
      vHits[iHit].Z() = h->Z();
      iHit++;
      nhits++;
      iID++;
    }
  }
  sort( vHits, vHits + track.NHits(), AliHLTTPCCADisplayTmpHit::CompareHitZ );
  //cout<<"Draw track, nhits = "<<nhits<<endl;
  {
    const AliHLTTPCCAHit &c1 = fSlice->ID2Hit( vHits[0].ID() );
    const AliHLTTPCCAHit &c2 = fSlice->ID2Hit( vHits[track.NHits()-1].ID() );
    if ( color < 0 ) color = GetColorZ( ( c1.Z() + c2.Z() ) / 2. );
  }

  fMarker.SetMarkerColor( color );//kBlue);
  fMarker.SetMarkerSize( 1. );
  /*
  for( int i=0; i<3; i++){
    const AliHLTTPCCAHit &c1 = fSlice->ID2Hit(track.HitID()[i]);
    const AliHLTTPCCARow &row1 = fSlice->ID2Row(track.HitID()[i]);
    double vx1, vy1;
    Slice2View(row1.X(), c1.Y(), &vx1, &vy1 );
    fYX->cd();
    fMarker.DrawMarker(vx1,vy1);
    fZX->cd();
    fMarker.DrawMarker(c1.Z(),vy1);
  }
  */

  //DrawTrackletPoint( fSlice->ID2Point(track.PointID()[0]).Param(), kBlack);//color );
  //DrawTrackletPoint( fSlice->ID2Point(track.PointID()[1]).Param(), kBlack);//color );
  //cout<<"DrawTrack end points x = "<<fSlice->ID2Point(track.PointID()[0]).Param().GetX()<<" "<<fSlice->ID2Point(track.PointID()[1]).Param().GetX()<<endl;
  for ( int iHit = 0; iHit < track.NHits() - 1; iHit++ ) {
    const AliHLTTPCCAHit &c1 = fSlice->ID2Hit( vHits[iHit].ID() );
    const AliHLTTPCCAHit &c2 = fSlice->ID2Hit( vHits[iHit+1].ID() );
    const AliHLTTPCCARow &row1 = fSlice->ID2Row( vHits[iHit].ID() );
    const AliHLTTPCCARow &row2 = fSlice->ID2Row( vHits[iHit+1].ID() );
    float x1, y1, z1, x2, y2, z2;
    t.GetDCAPoint( row1.X(), c1.Y(), c1.Z(), x1, y1, z1, fSlice->Param().cBz()  );
    t.GetDCAPoint( row2.X(), c2.Y(), c2.Z(), x2, y2, z2, fSlice->Param().cBz()  );

    //if( color<0 ) color = GetColorZ( (z1+z2)/2. );
    double vx1, vy1, vx2, vy2;
    Slice2View( x1, y1, &vx1, &vy1 );
    Slice2View( x2, y2, &vx2, &vy2 );

    fLine.SetLineColor( color );
    fLine.SetLineWidth( width );

    double x0 = t.GetX();
    double y0 = t.GetY();
    double sinPhi = t.GetSinPhi();
    double k = t.GetKappa();
    double ex = t.GetCosPhi();
    double ey = sinPhi;

    if ( TMath::Abs( k ) > 1.e-4 ) {

      fArc.SetFillStyle( 0 );
      fArc.SetLineColor( color );
      fArc.SetLineWidth( width );

      fYX->cd();

      double r = 1 / TMath::Abs( k );
      double xc = x0 - ey * ( 1 / k );
      double yc = y0 + ex * ( 1 / k );

      double vx, vy;
      Slice2View( xc, yc, &vx, &vy );

      double a1 = TMath::ATan2( vy1 - vy, vx1 - vx ) / TMath::Pi() * 180.;
      double a2 = TMath::ATan2( vy2 - vy, vx2 - vx ) / TMath::Pi() * 180.;
      if ( a1 < 0 ) a1 += 360;
      if ( a2 < 0 ) a2 += 360;
      if ( a2 < a1 ) a2 += 360;
      double da = TMath::Abs( a2 - a1 );
      if ( da > 360 ) da -= 360;
      if ( da > 180 ) {
        da = a1;
        a1 = a2;
        a2 = da;
        if ( a2 < a1 ) a2 += 360;
      }
      fArc.DrawArc( vx, vy, r, a1, a2, "only" );
      //fArc.DrawArc(vx,vy,r, 0,360,"only");
    } else {
      fYX->cd();
      fLine.DrawLine( vx1, vy1, vx2, vy2 );
    }
  }

  for ( int iHit = 0; iHit < track.NHits() - 1; iHit++ ) {
    const AliHLTTPCCAHit &c1 = fSlice->ID2Hit( vHits[iHit].ID() );
    const AliHLTTPCCAHit &c2 = fSlice->ID2Hit( vHits[iHit+1].ID() );
    const AliHLTTPCCARow &row1 = fSlice->ID2Row( vHits[iHit].ID() );
    const AliHLTTPCCARow &row2 = fSlice->ID2Row( vHits[iHit+1].ID() );

    //if( DrawHits ) ConnectHits( fSlice->ID2IRow(vHits[iHit].ID()),c1,
    //fSlice->ID2IRow(vHits[iHit+1].ID()),c2, color );
    float x1, y1, z1, x2, y2, z2;
    t.GetDCAPoint( row1.X(), c1.Y(), c1.Z(), x1, y1, z1, fSlice->Param().Bz()  );
    t.GetDCAPoint( row2.X(), c2.Y(), c2.Z(), x2, y2, z2, fSlice->Param().Bz()  );

    double vx1, vy1, vx2, vy2;
    Slice2View( x1, y1, &vx1, &vy1 );
    Slice2View( x2, y2, &vx2, &vy2 );

    fLine.SetLineColor( color );
    fLine.SetLineWidth( width );

    fZX->cd();
    fLine.DrawLine( z1, vy1, z2, vy2 );
  }
  fLine.SetLineWidth( 1 );
  if (vHits) delete[] vHits;
}

#endif //XXXX

void AliHLTTPCCADisplay::DrawTrackletPoint( const AliHLTTPCCATrackParam &t, int color )
{
  // draw tracklet point

  double x = t.GetX();
  double y = t.GetY();
  double sinPhi = t.GetSinPhi();
  double z = t.GetZ();
  double dzds = t.GetDzDs();
  double ex = t.GetCosPhi();
  double ey = sinPhi;

  int width = 1;

  if ( color < 0 ) color = GetColorZ( t.GetZ() );

  fMarker.SetMarkerColor( color );
  fMarker.SetMarkerSize( .5 );
  fLine.SetLineWidth( width );
  fLine.SetLineColor( color );

  double vx, vy, vex, vey, vdx, vdy;
  double dz = TMath::Sqrt( t.GetErr2Z() );
  Slice2View( x, y, &vx, &vy );
  Slice2View( ex, ey, &vex, &vey );
  Slice2View( 0, TMath::Sqrt( t.GetErr2Y() )*3.5, &vdx, &vdy );
  double d = TMath::Sqrt( vex * vex + vey * vey );
  vex /= d;
  vey /= d;
  fYX->cd();
  fMarker.DrawMarker( vx, vy );
  fLine.DrawLine( vx, vy, vx + vex*4, vy + vey*4 );
  fLine.DrawLine( vx - vdx, vy - vdy, vx + vdx, vy + vdy );
  fZX->cd();
  fMarker.DrawMarker( z, vy );
  fLine.DrawLine( z, vy, z + dzds*4, vy + vey*4 );
  fLine.DrawLine( z - 3.5*dz, vy - vdy, z + 3.5*dz, vy + vdy );
  fLine.SetLineWidth( 1 );
}

void AliHLTTPCCADisplay::DrawTrackParam( TrackParam t, int color )
{
  for ( int i = 0; i < 100; ++i ) {
    double x = t.GetX();
    double y = t.GetY();
    double sinPhi = t.GetSinPhi();
    double z = t.GetZ();
    double dzds = t.GetDzDs();
    double ex = t.GetCosPhi();
    double ey = sinPhi;

    fLine.SetLineWidth( 1 );
    fLine.SetLineColor( color );

    double vx, vy, vex, vey;
    Slice2View( x, y, &vx, &vy );
    Slice2View( ex, ey, &vex, &vey );
    double d = CAMath::RSqrt( vex * vex + vey * vey );
    vex *= d;
    vey *= d;

    fYX->cd();
    fLine.DrawLine( vx, vy, vx + vex*4, vy + vey*4 );
    fZX->cd();
    fLine.DrawLine( z, vy, z + dzds*4, vy + vey*4 );

    t.TransportToX( x + ex * CAMath::RSqrt( ex * ex + ey * ey ), fSlice->Param().cBz() );
  }
}

void AliHLTTPCCADisplay::SaveCanvasToFile( TString fileName){
  fCanvas->SaveAs(fileName);
}

void AliHLTTPCCADisplay::SaveCanvasToFilePT( TString fileName){
  fCanvasPT->SaveAs(fileName);
}

void AliHLTTPCCADisplay::SetTPC( const AliHLTTPCCAParam& tpcParam){ // iklm
  fZMin = tpcParam.ZMin();
  fZMax = tpcParam.ZMax();
  fYMin = -250; // s potolka?
  fYMax = 250;
  fRInnerMin = tpcParam.RMin();
  fRInnerMax = 123.; // approximate TODO: from file!
  fROuterMin = 123.;
  fROuterMax = 194;//tpcParam.RMax();
}

// void AliHLTTPCCADisplay::DrawGBLinks( const AliHLTTPCCAGBTracker &tracker, int color = -1, Size_t width = -1 ){
//
// }; // DrawGBLinks


/// IKu some new functional. supposed to be usefull

// ----------- TDrawObject ----------

AliHLTTPCCADisplay& AliHLTTPCCADisplay::TDrawObject::fDisplay = AliHLTTPCCADisplay::Instance();

AliHLTTPCCADisplay::TDrawObject::TDrawObject()
 :fColor(1),fWidth(0.05)
{
}

void AliHLTTPCCADisplay::TDrawObject::SetColor(int c)
{
  fColor = c;
}
void AliHLTTPCCADisplay::TDrawObject::SetWidth(float w)
{
  fWidth = w;
}

void AliHLTTPCCADisplay::TDrawObject::Global2View( double x, double y, double *xv, double *yv)
{
  AliHLTTPCCADisplay &d = AliHLTTPCCADisplay::Instance();
  d.Global2View( x, y, xv, yv );
}

void AliHLTTPCCADisplay::TDrawObject::Slice2View( double x, double y, double *xv, double *yv, float alpha)
{
  AliHLTTPCCADisplay &d = AliHLTTPCCADisplay::Instance();
  d.SetSliceTransform( alpha );
  d.Slice2View( x, y, xv, yv );
}

void AliHLTTPCCADisplay::TDrawObject::Slice2View( double x, double y, double *xv, double *yv, int iSlice)
{
  AliHLTTPCCADisplay &d = AliHLTTPCCADisplay::Instance();
  float alpha = GetAlpha(d.GetGB(), iSlice);
  Slice2View(x, y, xv, yv, alpha);
}


float AliHLTTPCCADisplay::TDrawObject::GetAlpha(const AliHLTTPCCAGBTracker *gbTracker, int iSlice)
{
  return gbTracker->Slice( iSlice ).Param().Alpha();
}



// ----------- TDrawHit ----------
AliHLTTPCCADisplay::TDrawHit::TDrawHit()
 :x(NON),y(NON),z(NON),alpha(NON),gx(NON),gy(NON),gz(NON)
{
}

bool AliHLTTPCCADisplay::TDrawHit::CheckGlobal()
{
  return (gx != NON) && (gy != NON) && (gz != NON);
}

bool AliHLTTPCCADisplay::TDrawHit::CheckLocal()
{
  return (x != NON) && (y != NON) && (z != NON);
}

bool AliHLTTPCCADisplay::TDrawHit::CheckAlpha()
{
  return alpha != NON;
}


void AliHLTTPCCADisplay::TDrawHit::FillGBCoor()
{
  assert( CheckLocal() );
  assert( CheckAlpha() );

  gx = x * cos(alpha) - y * sin(alpha);
  gy = y * cos(alpha) + x * sin(alpha);
  gz = z;
}


TMarker& AliHLTTPCCADisplay::TDrawHit::GetMarker()
{
  static TMarker m;
  m.SetMarkerStyle(20);
  // int color = (fColor == NON) ? fStatColor : fColor;
  // int width = (fWidth == NON) ? fStatWidth : fWidth;
  m.SetMarkerColor( fColor );
  m.SetMarkerSize ( fWidth );
  return m;
}

void AliHLTTPCCADisplay::TDrawHit::DrawLocal(int iSlice)
{
  double vx, vy;
  Slice2View( x, y, &vx, &vy, iSlice );

  TMarker& m = GetMarker();

  fDisplay.CanvasYX()->cd();
  m.DrawMarker( vx, vy );

  fDisplay.CanvasZX()->cd();
  m.DrawMarker( gz, vy );
} //

void AliHLTTPCCADisplay::TDrawHit::DrawGlobal()
{
  FillGBCoor();
  double vx, vy;
  Global2View( gx, gy, &vx, &vy );
//  Global2View( x, y, &vx, &vy );

  TMarker& m = GetMarker();

  fDisplay.CanvasYX()->cd();
  m.DrawMarker( vx, vy );
//  m.DrawMarker( x, y );

  fDisplay.CanvasZX()->cd();
#ifndef RZ
  m.DrawMarker( gz, vy );
#else
  /*if( vx > 0 )*/ m.DrawMarker( z, x );
//  else m.DrawMarker( z, -x );
#endif

} //


// ----------- TDrawTrack ----------

int AliHLTTPCCADisplay::TDrawTrack::fStatColor = 1;
float AliHLTTPCCADisplay::TDrawTrack::fStatWidth = 1;

AliHLTTPCCADisplay::TDrawTrack::TDrawTrack()
{
  fColor = NON;
  fWidth = NON;
}



void AliHLTTPCCADisplay::TDrawTrack::SetStatColor(int c)
{
  fStatColor = c;
}
void AliHLTTPCCADisplay::TDrawTrack::SetStatWidth(float w)
{
  fStatWidth = w;
}

TPolyLine& AliHLTTPCCADisplay::TDrawTrack::GetPolyLine()
{
  static TPolyLine pl;
  int color = (fColor == NON) ? fStatColor : fColor;
  int width = (fWidth == NON) ? fStatWidth : fWidth;
  pl.SetLineColor( color );
  pl.SetLineWidth( width );
  return pl;
}

void AliHLTTPCCADisplay::TDrawTrack::SetHitsColor(int c)
{
  for (unsigned i = 0; i < hits.size(); i++)
    hits[i].SetColor(c);
}

void AliHLTTPCCADisplay::TDrawTrack::SetHitsWidth(float w)
{
    for (unsigned i = 0; i < hits.size(); i++)
    hits[i].SetWidth(w);
}

void AliHLTTPCCADisplay::TDrawTrack::FillGBHitsCoor()
{
  for (unsigned i = 0; i < hits.size(); i++)
    hits[i].FillGBCoor();
}


void AliHLTTPCCADisplay::TDrawTrack::DrawHitsLocal(int iSlice)
{
  for (unsigned i = 0; i < hits.size(); i++)
    hits[i].DrawLocal(iSlice);
}


void AliHLTTPCCADisplay::TDrawTrack::DrawHitsGlobal()
{
  for (unsigned i = 0; i < hits.size(); i++) {
    hits[i].DrawGlobal();
  }
}


void AliHLTTPCCADisplay::TDrawTrack::DrawLocal(int iSlice)
{
  const int NHits = hits.size();
  if ( NHits < 2 ) return;
  if ( ! hits[0].CheckGlobal() ) FillGBHitsCoor();

  vector<double> vx, vy, vz;
  vx.resize(NHits);
  vy.resize(NHits);
  vz.resize(NHits);
  double *vxa = &(vx[0]),  *vya = &(vy[0]),  *vza = &(vz[0]);
  double *x = &(vx[0]),  *y = &(vy[0]),  *z = &(vz[0]);
  for (int ih = 0; ih < NHits; ih++, x++, y++, z++) {
    TDrawHit &hit = hits[ih];

    Slice2View( hit.x, hit.y, x, y, iSlice );
    *z = fabs(hit.z);
  } // for ih

  TPolyLine &pl = GetPolyLine();

  fDisplay.CanvasYX()->cd();
  pl.DrawPolyLine( NHits, vxa, vya );

  fDisplay.CanvasZX()->cd();
  pl.DrawPolyLine( NHits, vza, vya );
} // void AliHLTTPCCADisplay::SpecDrawTrackLocal

void AliHLTTPCCADisplay::TDrawTrack::DrawGlobal()
{
  const int NHits = hits.size();
  if ( NHits < 2 ) return;
  if ( ! hits[0].CheckGlobal() ) FillGBHitsCoor();

  vector<double> vx, vy, vz, vr;
  vx.resize(NHits);
  vy.resize(NHits);
  vz.resize(NHits);
  vr.resize(NHits);
  double *vxa = &(vx[0]),  *vya = &(vy[0]),  *vza = &(vz[0]), *vra = &(vr[0]);
  double *x = &(vx[0]),  *y = &(vy[0]),  *z = &(vz[0]), *r = &(vr[0]);
  for (int ih = 0; ih < NHits; ih++, x++, y++, z++) {
    TDrawHit &hit = hits[ih];

    Global2View( hit.gx, hit.gy, x, y );
    *z = hit.z;
#ifdef RZ
//    *r = hit.x;
//    if( *y < 0 ) *r = -*r;
    vra[ih] = hit.x;
    vza[ih] = hit.z;
//    if( y[0] < 0 ) vra[ih] = -vra[ih];
//    if( (ih>0) && (fabs(vra[ih] + vra[ih-1])<10) ) vra[ih] = -vra[ih];
#endif
  } // for ih

  TPolyLine &pl = GetPolyLine();

  fDisplay.CanvasYX()->cd();
  pl.DrawPolyLine( NHits, vxa, vya );

  fDisplay.CanvasZX()->cd();
#ifndef RZ
  pl.DrawPolyLine( NHits, vza, vya );
#else
  pl.DrawPolyLine( NHits, vza, vra );
#endif
} // void AliHLTTPCCADisplay::SpecDrawTrackGlobal


  // ----------- SpecDraw ----------

void AliHLTTPCCADisplay::SpecDrawMCTrackPointsGlobal(AliHLTTPCCAMCTrack& mcTrack, AliHLTResizableArray<AliHLTTPCCALocalMCPoint>* mcPointsArray, int color, float width )
{
  const int NPoints =  mcTrack.NMCPoints();
  if ( NPoints < 2 ) return;

  AliHLTTPCCALocalMCPoint *points = &((*mcPointsArray).Data()[mcTrack.FirstMCPointID()]);

  TDrawTrack drawTrack;

  TDrawHit hit;
//   hit.x = mcTrack.X();
//   hit.y = mcTrack.Y();
//   hit.z = mcTrack.Z();
//   hit.alpha = 0;
//   hit.SetColor(1);
//   hit.SetWidth(1);
//   drawTrack.hits.push_back(hit);

  for ( int ih = 0; ih < NPoints; ih++ ) {
    hit.x = points[ih].X();
    hit.y = points[ih].Y();
    hit.z = points[ih].Z();
    hit.alpha = TDrawHit::GetAlpha( fGB, points[ih].ISlice() );
    hit.SetColor(color);
    hit.SetWidth(width);
    drawTrack.hits.push_back(hit);
  }

  drawTrack.SetStatColor(color);

  drawTrack.SetStatWidth(0.5);
  drawTrack.DrawGlobal();
  drawTrack.DrawHitsGlobal();
} // void AliHLTTPCCADisplay::SpecDrawMCTrack

void AliHLTTPCCADisplay::DrawBadMCHits( AliHLTResizableArray<AliHLTTPCCAHitLabel>* hitLabels )
{
  TDrawTrack badTrack;
  int nHits = fGB->NHits();
  for ( int ih = 0; ih < nHits; ih++ ) {
      const AliHLTTPCCAGBHit &hit = fGB->Hit( ih );
      const AliHLTTPCCAHitLabel &l = (*hitLabels)[hit.ID()];
      if( l.fLab[0] == -1 ) {
      //  std::cout<<" bad lable for hit: "<<ih<<"\n";
        TDrawHit bhit;
        bhit.x = hit.X();
        bhit.y = hit.Y();
        bhit.z = hit.Z();
        bhit.alpha = TDrawHit::GetAlpha( fGB, hit.ISlice() );
        bhit.SetColor(kOrange);
        bhit.SetWidth(1.);
        badTrack.hits.push_back(bhit);
      }
  }
  badTrack.SetStatColor(kBlack);
    badTrack.SetStatWidth(0.5);
    badTrack.DrawHitsGlobal();
}

void AliHLTTPCCADisplay::SpecDrawMCTrackHitsGlobal(AliHLTTPCCAMCTrack& mcTrack, AliHLTResizableArray<AliHLTTPCCALocalMCPoint>* mcPointsArray, AliHLTResizableArray<AliHLTTPCCAHitLabel>* hitLabels, int color, float width )
{
  const int NPoints =  mcTrack.NMCPoints();
//  std::cout<<" --->>> NPoints: "<<NPoints<<";   FirstMCPointID: "<<mcTrack.FirstMCPointID()<<"\n";
  if ( NPoints < 2 ) return;

//  int fstSlice = -1;

  TDrawTrack drawTrack;

//  TDrawTrack badTrack;

    int nHits = fGB->NHits();
//    std::cout<<" ------- NPoints: "<<NPoints<<";   nHits: "<<nHits<<"\n";
    int nTrHits = 0;
    for ( int ih = 0; ih < nHits; ih++ ) {
      const AliHLTTPCCAGBHit &hit = fGB->Hit( ih );
//if( hit.ISlice() != 7 ) continue;
//if( hit.ISlice() != 10 && hit.ISlice() != 12 ) continue;
      const AliHLTTPCCAHitLabel &l = (*hitLabels)[hit.ID()];

      const int iMC = (*mcPointsArray)[mcTrack.FirstMCPointID()].TrackI();
//if( l.fLab[0] == -1 ) {
////  std::cout<<" bad lable for hit: "<<ih<<"\n";
//  TDrawHit bhit;
//  bhit.x = hit.X();
//  bhit.y = hit.Y();
//  bhit.z = hit.Z();
//  bhit.alpha = TDrawHit::GetAlpha( fGB, hit.ISlice() );
//  bhit.SetColor(kOrange);
//  bhit.SetWidth(0.5);
//  badTrack.hits.push_back(bhit);
//}
      if ( l.fLab[0] != iMC && l.fLab[1] != iMC && l.fLab[2] != iMC )
        continue;

      nTrHits++;
//      if( fstSlice < 0 ) fstSlice = hit.ISlice();

//      std::cout<<hit.X()<<" "<<hit.Y()<<" "<<hit.Z()<<"\n";
//      std::cout<<"0 0 0 0\n";
//      std::cout<<hit.ISlice()<<" "<<hit.IRow()<<" 0 0\n";

//      std::cout<<"nHit: "<<nTrHits-1<<";   iHit: "<<ih/*<<";   iMCPoint: "<<iMC+ih*/<<"\n hit.x: "<<hit.X()<<";   hit.y: "<<hit.Y()<<";   hit.z: "<<hit.Z()<<"\n";
//      std::cout<<" hit.iSlice: "<<hit.ISlice()<<";   hit.IRow: "<<hit.IRow()<<"\n\n";
//      std::cout<<" ISlice: "<<(*mcPointsArray)[mcTrack.FirstMCPointID()+ih].ISlice()<<";   IRow: "<<(*mcPointsArray)[mcTrack.FirstMCPointID()+ih].IRow()<<"\n\n";
//      std::cout<<"..... hit.z: "<<hit.Z()<<";   mc.z: "<<(*mcPointsArray)[mcTrack.FirstMCPointID()+ih].Z()<<"\n";
      TDrawHit dhit;
//      std::cout<<"... dhit: x: "<<dhit.x<<";   y: "<<dhit.y<<";   z: "<<dhit.z<<";   gx: "<<dhit.gx<<";   gy: "<<dhit.gy<<";   gz: "<<dhit.gz<<"\n";
      dhit.x = hit.X();
      dhit.y = hit.Y();
      dhit.z = hit.Z();
      dhit.alpha = TDrawHit::GetAlpha( fGB, hit.ISlice() );
      dhit.SetColor(color);
      dhit.SetWidth(width);
//      std::cout<<"..... dhit: x: "<<dhit.x<<";   y: "<<dhit.y<<";   z: "<<dhit.z<<";   gx: "<<dhit.gx<<";   gy: "<<dhit.gy<<";   gz: "<<dhit.gz<<"\n";
      drawTrack.hits.push_back(dhit);
    }
    drawTrack.SetStatColor(color);
//    std::cout<<"         draw hits: "<<nTrHits<<"\n";

  drawTrack.SetStatWidth(0.5);
  drawTrack.DrawGlobal();
  drawTrack.DrawHitsGlobal();
  // ---
//  badTrack.SetStatColor(kBlack);
//  badTrack.SetStatWidth(0.5);
//  badTrack.DrawHitsGlobal();
//  if( fstSlice < 0 ) fstSlice = 0;
//  TDrawTrack drawTrack1;
//  drawTrack1.SetColor(kBlue);
//  TDrawHit dhit1;
//  dhit1.x = 0;
//  dhit1.y = 0;
//  dhit1.z = 0;
//  dhit1.alpha = TDrawHit::GetAlpha( fGB, fstSlice );
//  dhit1.SetColor(kBlue);
//  dhit1.SetWidth(width);
//  drawTrack1.hits.push_back(dhit1);
//  dhit1.x = fabs( mcTrack.Px() )*100;
//  dhit1.y = mcTrack.Py()*100;
//  dhit1.z = mcTrack.Pz()*100;
//  drawTrack1.hits.push_back(dhit1);
//  drawTrack1.DrawGlobal();
  // ---
} // void AliHLTTPCCADisplay::SpecDrawMCTrack

void AliHLTTPCCADisplay::SpecDrawMCTrackLocal(AliHLTTPCCAMCTrack& mcTrack, AliHLTResizableArray<AliHLTTPCCALocalMCPoint>* mcPointsArray, int iSlice)
{
  const int NPoints =  mcTrack.NMCPoints();
  if ( NPoints < 2 ) return;

  AliHLTTPCCALocalMCPoint *points = &((*mcPointsArray).Data()[mcTrack.FirstMCPointID()]);

  TDrawTrack drawTrack;

  for ( int ih = 0; ih < NPoints; ih++ ) {
    TDrawHit hit;
    hit.x = points[ih].X();
    hit.y = points[ih].Y();
    hit.z = points[ih].Z();
    hit.alpha = TDrawHit::GetAlpha( fGB, iSlice );
    hit.SetColor(kBlue);
    hit.SetWidth(0.05);
    drawTrack.hits.push_back(hit);
  }

  drawTrack.SetStatColor(2);
  drawTrack.SetStatWidth(0.5);
  drawTrack.DrawLocal(iSlice);
  drawTrack.DrawHitsLocal(iSlice);
} // void AliHLTTPCCADisplay::SpecDrawMCTrack

void AliHLTTPCCADisplay::SpecDrawHitsFromMCTrackLocal(int iMC, const AliHLTResizableArray<AliHLTTPCCAHitLabel>* hitLabels, int iSlice)
{
  TDrawTrack drawTrack;

  for ( int ih = 0; ih < fGB->NHits(); ih++ ) {
    const AliHLTTPCCAGBHit& h = fGB->Hit(ih);
    if ( (*hitLabels)[h.ID()].fLab[0] != iMC ) continue;
    TDrawHit hit;
    hit.x = h.X();
    hit.y = h.Y();
    hit.z = h.Z();
    hit.alpha = TDrawHit::GetAlpha( fGB, iSlice );
    hit.SetColor(kRed);
    hit.SetWidth(0.05);
    hit.DrawLocal(iSlice);
  }
} // void AliHLTTPCCADisplay::SpecDrawMCTrack

void AliHLTTPCCADisplay::SpecDrawRecoTrackGlobal( int iTr, int color, float width )
{
  const AliHLTTPCCAGBTrack &track = fGB->Tracks()[iTr];
//  if( track.IsMerged() ) {
//      color = kGreen;
//      width = 0.5;
//  }
//
//  if( track.IsClone() ) {
//      color = kRed;
//      width = 0.3;
//  }
//  std::cout<<"> Track "<<iTr<<";   p: "<<track.Param().QPt()<<"\n";

  const int NHits =  track.NHits();

  TDrawTrack drawTrack;

  int ih = track.FirstHitRef();
  for ( int i = 0; i < NHits; i++, ih++ ) {
    const int iH = fGB->TrackHit(ih);
    const AliHLTTPCCAGBHit &hit = fGB->Hit(iH);
    TDrawHit dhit;
    dhit.x = hit.X();
    dhit.y = hit.Y();
    dhit.z = hit.Z();
//if( /*hit.ISlice() != 10 && hit.ISlice() != 11 &&*/ hit.ISlice() != 12 && hit.ISlice() != 23 ) return;
//if( hit.ISlice() != 2 && hit.ISlice() != 3 && hit.ISlice() != 4 /*&& hit.ISlice() != 18 && hit.ISlice() != 19 && hit.ISlice() != 20*/ ) return;
//if( hit.ISlice() != 6 && hit.ISlice() != 7 && hit.ISlice() != 15 && hit.ISlice() != 16 ) return;
//if( hit.ISlice() != 14 && hit.ISlice() != 13 ) continue;
//if( hit.ISlice() != 7 && hit.ISlice() != 8 && hit.ISlice() != 9 ) continue;
//if( hit.ISlice() != 22 && hit.ISlice() != 21 ) continue;
    dhit.alpha = TDrawHit::GetAlpha( fGB, hit.ISlice() );
    dhit.SetColor(color);
    dhit.SetWidth(width);
    drawTrack.hits.push_back(dhit);
//    std::cout<<" - hit "<<i<<";   slice: "<<hit.ISlice()<<";   x: "<<hit.X()<<";   y: "<<hit.Y()<<";   y: "<<hit.Z()<<"\n";
  }
  drawTrack.SetStatColor(color);

  drawTrack.SetStatWidth(0.5);
  drawTrack.DrawGlobal();
  drawTrack.DrawHitsGlobal();
} // void AliHLTTPCCADisplay::SpecDrawMCTrack

void AliHLTTPCCADisplay::SpecDrawRecoTrackGlobalPT( int iTr, int color, float width )
{
  const AliHLTTPCCAGBTrack &track = fGB->Tracks()[iTr];

  const int NHits =  track.NHits();

  TDrawTrack drawTrack;

  int ih = track.FirstHitRef();
  for ( int i = 0; i < NHits; i++, ih++ ) {
    const int iH = fGB->TrackHit(ih);
    const AliHLTTPCCAGBHit &hit = fGB->Hit(iH);
    TDrawHit dhit;
    dhit.x = hit.X();
    dhit.y = hit.Y();
    dhit.z = hit.Z();
    dhit.alpha = TDrawHit::GetAlpha( fGB, hit.ISlice() );
    dhit.SetColor(color);
    dhit.SetWidth(width);
    // ---
    dhit.slice = hit.ISlice();
    // ---
    drawTrack.hits.push_back(dhit);
  }
  drawTrack.SetStatColor(color);

  drawTrack.SetStatWidth(0.5);
//  drawTrack.DrawGlobalPT();
//  drawTrack.DrawGlobalPpad();
  drawTrack.DrawGlobalTpad();
//  drawTrack.DrawHitsGlobal();
}

void AliHLTTPCCADisplay::SpecDrawMCTrackGlobalPT( AliHLTTPCCAMCTrack& mcTrack, AliHLTResizableArray<AliHLTTPCCALocalMCPoint>* mcPointsArray, int color, float width )
{
//  const AliHLTTPCCAGBTrack &track = fGB->Tracks()[iTr];
//
//  const int NHits =  track.NHits();
//
//  TDrawTrack drawTrack;
//
//  int ih = track.FirstHitRef();
//  for ( int i = 0; i < NHits; i++, ih++ ) {
//    const int iH = fGB->TrackHit(ih);
//    const AliHLTTPCCAGBHit &hit = fGB->Hit(iH);
//    TDrawHit dhit;
//    dhit.x = hit.X();
//    dhit.y = hit.Y();
//    dhit.z = hit.Z();
//    dhit.alpha = TDrawHit::GetAlpha( fGB, hit.ISlice() );
//    dhit.SetColor(color);
//    dhit.SetWidth(width);
//    drawTrack.hits.push_back(dhit);
//  }
//  drawTrack.SetStatColor(color);
//
//  drawTrack.SetStatWidth(0.5);
//  drawTrack.DrawGlobalPT();

  // ---
  const int NPoints =  mcTrack.NMCPoints();
    if ( NPoints < 2 ) return;

    AliHLTTPCCALocalMCPoint *points = &((*mcPointsArray).Data()[mcTrack.FirstMCPointID()]);

    TDrawTrack drawTrack;

    TDrawHit hit;

    for ( int ih = 0; ih < NPoints; ih++ ) {
	if( (ih > 0) && (drawTrack.hits[drawTrack.hits.size()-1].row == points[ih].IRow()) ) continue;
      hit.x = points[ih].X();
      hit.y = points[ih].Y();
      hit.z = points[ih].Z();
      hit.alpha = TDrawHit::GetAlpha( fGB, points[ih].ISlice() );
      hit.SetColor(color);
      hit.SetWidth(width);
      // ---
      hit.slice = points[ih].ISlice();
      hit.row = points[ih].IRow();
      // ---
      drawTrack.hits.push_back(hit);
//      if( () && (drawTrack.hits[]) )
    }

    drawTrack.SetStatColor(color);

    drawTrack.SetStatWidth(0.5);
    drawTrack.DrawGlobalPT();
//    drawTrack.DrawGlobalTpad();
//    drawTrack.DrawGlobalPpad();
}

void AliHLTTPCCADisplay::SpecDrawMCTrackGlobalPT1( AliHLTTPCCAMCTrack& mcTrack, AliHLTResizableArray<AliHLTTPCCALocalMCPoint>* mcPointsArray, int color, float width )
{
  const int NPoints =  mcTrack.NMCPoints();
    if ( NPoints < 2 ) return;

    AliHLTTPCCALocalMCPoint *points = &((*mcPointsArray).Data()[mcTrack.FirstMCPointID()]);
    int iSlice(points[0].ISlice()), pSlice(points[0].ISlice()), nSlice(points[0].ISlice());
    float iTeta, nTeta;
    int pHit(0);

    TDrawTrack drawTrack;

    TDrawHit hit;
    int st(iSlice), stn(iSlice), nn(0);
    for ( int ih = 0; ih < NPoints; ih++ ) {
	stn = points[ih].ISlice();
	if( stn != st ) {
	    st = stn;
	    nn++;
	}
	if( nn > 10 ) return;
    }

    int pih(0), ppSlice(pSlice);
    vector<float> tetas;
    vector<int> fstsl, lstsl;

    for ( int ih = 0; ih < NPoints; ih++ ) {
      hit.x = points[ih].X();
      hit.y = points[ih].Y();
      hit.z = points[ih].Z();
      hit.alpha = TDrawHit::GetAlpha( fGB, points[ih].ISlice() );
      // ---
      hit.slice = points[ih].ISlice();
      hit.row = points[ih].IRow();
      // ---
      drawTrack.hits.push_back(hit);
      if( points[ih].ISlice() != iSlice ) nSlice = points[ih].ISlice();

      if( ih > 1 && points[ih].ISlice() != pSlice ){
        float x = drawTrack.hits[ih-1].x - drawTrack.hits[pHit].x;
        float z = drawTrack.hits[ih-1].z - drawTrack.hits[pHit].z;
        iTeta = atan( x / z );
        if( ih-1 == pHit ) return;
        tetas.push_back(iTeta);
        fstsl.push_back(pSlice);
        lstsl.push_back(points[ih].ISlice());
        pSlice = points[ih].ISlice();
        pHit = ih;
      }
    }
    float x = drawTrack.hits[NPoints-1].x - drawTrack.hits[pHit].x;
    float z = drawTrack.hits[NPoints-1].z - drawTrack.hits[pHit].z;
    iTeta = atan( x / z );
    tetas.push_back(iTeta);
    fstsl.push_back(ppSlice);
    lstsl.push_back(pSlice);
    if( tetas.size() == 1 ) {
	drawTrack.DrawDrawTetaTrackInt( tetas[0], iSlice, pSlice, nSlice, true, kBlack );
    }
    else {
	for( int i = 0; i < tetas.size()-1; i++ ) {
	    drawTrack.DrawDrawTetaTrackIntMerged( tetas[i], tetas[i+1], fstsl[i], lstsl[i], false, kBlack );
	}
    }
}

void AliHLTTPCCADisplay::SpecDrawMCTrackGlobalDzDsQPt( AliHLTTPCCAMCTrack& mcTrack, AliHLTResizableArray<AliHLTTPCCALocalMCPoint>* mcPointsArray, int color, float width )
{
  const int NPoints =  mcTrack.NMCPoints();
    if ( NPoints < 2 ) return;

    AliHLTTPCCALocalMCPoint *points = &((*mcPointsArray).Data()[mcTrack.FirstMCPointID()]);
    int iSlice(points[0].ISlice()), pSlice(points[0].ISlice()), nSlice(points[0].ISlice());
    float iTeta, nTeta;
    int pHit(0);

    TDrawTrack drawTrack;

    TDrawHit hit;
    int st(iSlice), stn(iSlice), nn(0);
    for ( int ih = 0; ih < NPoints; ih++ ) {
	stn = points[ih].ISlice();
	if( stn != st ) {
	    st = stn;
	    nn++;
	}
	if( nn > 10 ) return;
    }

    int pih(0), ppSlice(pSlice);
    vector<float> tetas;
    vector<int> fstsl, lstsl;
    float qp0 = fabs(points[0].QP());
    float qp1 = fabs(points[NPoints-1].QP());
    if( fabs(mcTrack.Pt()) < 100 ) {
//	std::cout<<">>> QP0: "<<qp0<<";   Pt: "<<mcTrack.Pt()<<";   Q/Pt: "<<1/mcTrack.Pt()<<"\n";
	qp0 = 1/mcTrack.Pt();
	qp1 = qp0;
    }

//    std::cout<<"\n\n\nStart MCTrack\n";
    for ( int ih = 0; ih < NPoints; ih++ ) {
      hit.x = points[ih].X();
      hit.y = points[ih].Y();
      hit.z = points[ih].Z();
      hit.alpha = TDrawHit::GetAlpha( fGB, points[ih].ISlice() );
//      std::cout<<"ih: "<<ih<<";   QP: "<<points[ih].QP()<<"\n";
      // ---
      hit.slice = points[ih].ISlice();
      hit.row = points[ih].IRow();
      // ---
      drawTrack.hits.push_back(hit);
      if( points[ih].ISlice() != iSlice ) nSlice = points[ih].ISlice();

      if( ih > 1 && points[ih].ISlice() != pSlice ){
//	  std::cout<<" ----- net slice -----\n";
        float x = drawTrack.hits[ih-1].x - drawTrack.hits[pHit].x;
        float z = drawTrack.hits[ih-1].z - drawTrack.hits[pHit].z;
        iTeta = atan( x / z );
        if( ih-1 == pHit ) return;
        tetas.push_back(iTeta);
        fstsl.push_back(pSlice);
        lstsl.push_back(points[ih].ISlice());
        pSlice = points[ih].ISlice();
        pHit = ih;
      }
    }
    float x = drawTrack.hits[NPoints-1].x - drawTrack.hits[pHit].x;
    float z = drawTrack.hits[NPoints-1].z - drawTrack.hits[pHit].z;
    iTeta = atan( x / z );
    tetas.push_back(iTeta);
    fstsl.push_back(ppSlice);
    lstsl.push_back(pSlice);
//    std::cout<<"draw MC teta0: "<<tetas[0]<<";   qp0: "<<qp0<<";   qp1: "<<qp1<<"\n";
    if( tetas.size() == 1 ) {
//	drawTrack.DrawDrawTetaTrackInt( tetas[0], iSlice, pSlice, nSlice, true, kBlack );
	drawTrack.DrawDrawTetaTrackIntMerged( tetas[0], tetas[0], qp0, qp1, false, kBlack );
	TDrawHit dhit;
	dhit.x = tetas[0];
	dhit.y = qp0;
	dhit.SetColor(kBlack);
	dhit.SetWidth(0.5);
	dhit.DrawHitPT();
    }
    else {
	for( int i = 0; i < tetas.size()-1; i++ ) {
	    drawTrack.DrawDrawTetaTrackIntMerged( tetas[i], tetas[i+1], qp0, qp1, false, kBlack );
	    TDrawHit dhit;
	    dhit.x = tetas[i];
	    dhit.y = qp0;
	    dhit.SetColor(kBlack);
	    dhit.SetWidth(0.5);
	    dhit.DrawHitPT();
	}
    }
//    TDrawHit dhit;
//      dhit.x = dzds;
//      dhit.y = pt;
//      dhit.SetColor(kGreen);
//      dhit.SetWidth(1);
//      dhit.DrawHitPT();
}

void AliHLTTPCCADisplay::TDrawTrack::DrawGlobalPT()
{
  const int NHits = hits.size();
  if ( NHits < 2 ) return;
  if ( ! hits[0].CheckGlobal() ) FillGBHitsCoor();

  vector<double> phi, teta;
  phi.resize(NHits-1);
  teta.resize(NHits-1);
  double *vphi = &(phi[0]), *vteta = &(teta[0]);
//  TDrawHit &hit0 = hits[i-1];
//  double phi0 = acos(hit0.x / sqrt(hit0.x*hit0.x + hit0.y*hit0.y));
//  if( hit0.y < 0 ) phi0 = -phi0;
//  phi0 += hit0.alpha;
//  std::cout<<"\n";
  for( int i = 1; i < NHits; i++ ) {
//  for( int i = 1; i < NHits-1; i++ ) {
    TDrawHit &hit = hits[i];
    TDrawHit &hit0 = hits[i-1];
//    TDrawHit &hit0 = hits[0];
//    *phi = acos(x / sqrt(x*x + y*y));
//    *teta = acos(x / sqrt(x*x + z*z));
//    double x = hits[i].x, y = hits[i].y, z = hits[i].z;
    double x = hits[i+1].x, y = hits[i+1].y, z = hits[i+1].z;
    x -= hit0.x;
    y -= hit0.y;
    z -= hit0.z;
//    double x = hits[i].x, y = hits[i].y, z = hits[i].z;
    teta[i-1] = acos(x / sqrt(x*x + z*z));
    if( z < 0 ) teta[i-1] = -teta[i-1];

    double phi0 = acos(hit0.x / sqrt(hit0.x*hit0.x + hit0.y*hit0.y));
    if( hit0.y < 0 ) phi0 = -phi0;

//    double x1 = hits[i].x, y1 = hits[i].y, z1 = hits[i].z;
    double x1 = hits[i+1].x, y1 = hits[i+1].y, z1 = hits[i+1].z;
    double y3 = x1*tan(phi0);
    std::cout<<"x1: "<<x1<<";   phi0: "<<phi0<<"\n";
    if( hits[i].y < 0 ) y3 = -y3;
    std::cout<<"y3: "<<y3<<"\n";
    double p = fabs(y1 - y3);
    double a = sqrt((y1 - hit0.y)*(y1 - hit0.y) + (x1 - hit0.x)*(x1 - hit0.x));
    double b = sqrt((y3 - hit0.y)*(y3 - hit0.y) + (x1 - hit0.x)*(x1 - hit0.x));
    std::cout<<"p: "<<p<<";   a: "<<a<<";   b: "<<b<<"\n";
    double tmp = (a*a + b*b - p*p) / (2*a*b);
    double phi_h = acos(tmp);
    std::cout<<"tmp: "<<tmp<<";   phi_h: "<<phi_h<<"\n";
    phi[i-1] = phi_h;
//    if( i > 1 && fabs(teta[i-2] - teta[i-1]) > 0.5 ) return;
//    if( hits[i].y < 0 ) phi[i-1] = -phi[i-1];

    /*double phi1 = acos(hits[i].x / sqrt(hits[i].x*hits[i].x + hits[i].y*hits[i].y));
    if( hits[i].y < 0 ) phi1 = -phi1;
    phi1 += hits[i].alpha;
//    x -= hits[0].x;
//    y -= hits[0].y;
//    z -= hits[0].z;
    std::cout<<"phi0: "<<phi0<<";   phi1: "<<phi1<<";   dphi: "<<phi1-phi0<<"\n";
    if( fabs(phi1 - phi0) < 3.14 ) phi[i-1] = phi1 - phi0;
    else {
	if( (phi1 - phi0) > 3.14 ) phi[i-1] = phi1 - phi0 - 6.28;
	else phi[i-1] = phi1 - phi0 + 6.28;
    }
    phi0 = phi1;
    teta[i-1] = acos(x / sqrt(x*x + z*z));
    if( z < 0 ) teta[i-1] = -teta[i-1];
    if( i > 1 && fabs(teta[i-2] - teta[i-1]) > 0.5 ) return;*/
  }

  TPolyLine &pl = GetPolyLine();

  fDisplay.CanvasPT()->cd();
  pl.DrawPolyLine( NHits-2, vteta, vphi );

  vteta[0] = -10; vteta[1] = 10;
  vphi[0] = 0; vphi[1] = 0;
  pl.DrawPolyLine( 2, vteta, vphi );
}

void AliHLTTPCCADisplay::TDrawTrack::DrawGlobalTpad()
{
  const int NHits = hits.size();
  if ( NHits < 2 ) return;
  if ( ! hits[0].CheckGlobal() ) FillGBHitsCoor();

  vector<double> phi, teta;
  phi.resize(NHits-1);
  teta.resize(NHits-1);
  double *vphi = &(phi[0]), *vteta = &(teta[0]);
//  TDrawHit &hit0 = hits[0];
  std::cout<<" --- ";
//  for( int i = 1; i < NHits; i++ ) {
  for( int i = 1; i < NHits-1; i++ ) {
    TDrawHit &hit = hits[i];
//    TDrawHit &hit0 = hits[i-1];
    TDrawHit &hit0 = hits[0];
//    *phi = acos(x / sqrt(x*x + y*y));
//    *teta = acos(x / sqrt(x*x + z*z));
    double x = hits[i].x, y = hits[i].y, z = hits[i].z;
//    double x = hits[i+1].x, y = hits[i+1].y, z = hits[i+1].z;
    x -= hit0.x;
    y -= hit0.y;
    z -= hit0.z;
//    x -= hits[i-1].x;
//    y -= hits[i-1].y;
//    z -= hits[i-1].z;
//    phi[i-1] = acos(hits[i].x / sqrt(hits[i].x*hits[i].x + hits[i].y*hits[i].y));
    phi[i-1] = acos(x / sqrt(x*x + y*y));
//    if( y < 0 ) phi[i-1] = -phi[i-1];
//    if( hits[i].slice != hits[0].slice ) return;
    if( hits[i].slice < hits[0].slice ) phi[i-1] = 0.87 - phi[i-1];
//    if( hits[i].slice > hits[0].slice ) return;
    if( hits[i].slice > hits[0].slice ) phi[i-1] = 1 - phi[i-1];
//    phi[i-1] += hits[i].alpha-3.14;
//    std::cout<<"ihit: "<<i<<";   phi: "<<phi[i-1]<<";   slice: "<<hits[i].slice<<";   alpha: "<<hits[i].alpha<<"\n";
//    phi[i-1] -= 12;
//    phi[i-1] += hit0.slice;
//    std::cout<<"hit: "<<i<<";   hit0.slice: "<<hit0.slice<<";   phi[i-1]: "<<phi[i-1]<<"\n";
//    if( y < 0 ) phi[i-1] = -phi[i-1];
//    teta[i-1] = acos(hits[i].x / sqrt(hits[i].x*hits[i].x + hits[i].z*hits[i].z));
    teta[i-1] = acos(x / sqrt(x*x + z*z));
    if( z < 0 ) teta[i-1] = -teta[i-1];
//    if( i > 1 && fabs(teta[i-2] - teta[i-1]) > 0.5 ) return;
//    if( i > 1 && fabs(teta[i-2] - teta[i-1]) > 0.5 ) {
//	std::cout<<" - i: "<<i<<";   x0: "<<hits[i-1].x<<";   row0: "<<hits[i-1].row<<";   z0: "<<hits[i-1].z<<";   z1: "<<hits[i].z<<";   teta: "<<teta[i-1]<<"\n";
//    }
//    if( (i > 1) && (x[i] < x[i-1]) )
//    phi[i-1] = hits[i].row;
    phi[i-1] = hits[i].x;
//    teta[i-1] = hits[i].row;
  }

  TPolyLine &pl = GetPolyLine();

  fDisplay.CanvasPT()->cd();
  pl.DrawPolyLine( NHits-2, vteta, vphi );
//  pl.DrawPolyLine( NHits-1, vphi, vteta );
}

void AliHLTTPCCADisplay::TDrawTrack::DrawGlobalPpad()
{
  const int NHits = hits.size();
  if ( NHits < 2 ) return;
  if ( ! hits[0].CheckGlobal() ) FillGBHitsCoor();

  vector<double> phi, teta;
  phi.resize(NHits-1);
  teta.resize(NHits-1);
  double *vphi = &(phi[0]), *vteta = &(teta[0]);
//  TDrawHit &hit0 = hits[0];
//  double phi0 = acos(hit0.x / sqrt(hit0.x*hit0.x + hit0.y*hit0.y));
//  if( hit0.y < 0 ) phi0 = -phi0;
//  double phi00 = phi0;
//  phi0 += hit0.alpha;
//  for( int i = 1; i < NHits; i++ ) {
  for( int i = 1; i < NHits-1; i++ ) {
    TDrawHit &hit = hits[i];
//    TDrawHit &hit0 = hits[i-1];
    TDrawHit &hit0 = hits[0];
//    *phi = acos(x / sqrt(x*x + y*y));
//    *teta = acos(x / sqrt(x*x + z*z));
    double x = hits[i].x, y = hits[i].y, z = hits[i].z;
//    double x = hits[i+1].x, y = hits[i+1].y, z = hits[i+1].z;
    double phi1 = acos(x / sqrt(x*x + y*y));
    if( hits[i].y < 0 ) phi1 = -phi1;
//    phi1 += hits[i].alpha;
//    x -= hits[0].x;
//    y -= hits[0].y;
//    z -= hits[0].z;
    x -= hit0.x;
    y -= hit0.y;
    z -= hit0.z;
    double phi0 = acos(hit0.x / sqrt(hit0.x*hit0.x + hit0.y*hit0.y));
    if( hit0.y < 0 ) phi0 = -phi0;
//    phi0 += hit[i-1].alpha;
    double phi00 = phi0;
    if( fabs(phi1 - phi0) < 3.14 ) phi[i-1] = phi1 - phi0;
    else {
//      return;
      if( (phi1 - phi0) > 3.14 ) phi[i-1] = phi1 - phi0 - 6.28;
      else phi[i-1] = phi1 - phi0 + 6.28;
    }
    phi0 = phi1;
//    phi[i-1] = acos(hits[i].x / sqrt(hits[i].x*hits[i].x + hits[i].y*hits[i].y));
//    phi[i-1] = acos(x / sqrt(x*x + y*y));
//    if( y < 0 ) phi[i-1] = -phi[i-1];
//    if( hits[i].slice != hits[0].slice ) return;
//    if( hits[i].slice < hits[0].slice ) phi[i-1] = 0.87 - phi[i-1];
//    if( hits[i].slice > hits[0].slice ) return;
//    if( hits[i].slice > hits[0].slice ) phi[i-1] = 1 - phi[i-1];
//    phi[i-1] += hits[i].alpha-3.14;
    std::cout<<"ihit: "<<i<<";   phi: "<<phi[i-1]<<";   slice: "<<hits[i].slice<<";   alpha: "<<hits[i].alpha<<"\n";
    std::cout<<"----- x: "<<hits[i].x<<";   y: "<<hits[i].y<<";   z: "<<hits[i].z<<";   phi1: "<<phi1<<"\n";
    // --- 2 hit segment phi ---
//    double x1 = hits[i+1].x, y1 = hits[i+1].y, z1 = hits[i+1].z;
    double x1 = hits[i].x, y1 = hits[i].y, z1 = hits[i].z;
    double y3 = x1*tan(phi00);
//    if( hits[i].y < 0 ) y3 = -y3;
    std::cout<<"------- y3: "<<y3<<"\n";
    double p = fabs(y1 - y3);
    double a = sqrt((y1 - hit0.y)*(y1 - hit0.y) + (x1 - hit0.x)*(x1 - hit0.x));
    double b = sqrt((y3 - hit0.y)*(y3 - hit0.y) + (x1 - hit0.x)*(x1 - hit0.x));
    std::cout<<"------- p: "<<p<<";   a: "<<a<<";   b: "<<b<<"\n";
    double tmp = (a*a + b*b - p*p) / (2*a*b);
    std::cout<<"------- tmp: "<<tmp<<"\n";
    double phi_h = acos(tmp);
    std::cout<<"------- phi_h: "<<phi_h<<"\n";
    phi[i-1] = phi_h;
//    if( hits[i].y < 0 ) phi[i-1] = -phi[i-1];
//	if( hits[i].slice < hits[i-1].slice ) phi[i-1] = 0.87 - phi[i-1];
//        if( hits[i].slice > hits[0].slice ) return;//phi[i-1] = 1 - phi[i-1];
//        if( hits[i].slice != hits[0].slice ) return;
    phi[i-1] = phi_h + hits[0].alpha;
//    if( i > 1 && (hits[i+1].slice != hits[i].slice || hits[i].slice != hits[i-1].slice || hits[i-1].slice != hits[i+1].slice) ) phi[i-1] = phi[i-2];
    if( i > 1 && (hits[i-1].slice != hits[i].slice) ) phi[i-1] = phi[i-2];
//    if( hits[i].y < 0 ) phi[i-1] = -phi[i-1];
    // ---
//    phi[i-1] -= 12;
//    phi[i-1] += hit0.slice;
//    std::cout<<"hit: "<<i<<";   hit0.slice: "<<hit0.slice<<";   phi[i-1]: "<<phi[i-1]<<"\n";
//    if( y < 0 ) phi[i-1] = -phi[i-1];
//    teta[i-1] = acos(hits[i].x / sqrt(hits[i].x*hits[i].x + hits[i].z*hits[i].z));
    teta[i-1] = acos(x / sqrt(x*x + z*z));
    if( z < 0 ) teta[i-1] = -teta[i-1];
//    if( i > 1 && fabs(teta[i-2] - teta[i-1]) > 0.5 ) return;
//    phi[i-1] = hits[i].row;
//    teta[i-1] = hits[i].row;
    teta[i-1] = hits[i].x;
  }

  TPolyLine &pl = GetPolyLine();

  fDisplay.CanvasPT()->cd();
//  pl.DrawPolyLine( NHits-1, vteta, vphi );
  pl.DrawPolyLine( NHits-2, vphi, vteta );
}

void AliHLTTPCCADisplay::DrawTrackFromMerger( std::vector<double> x, std::vector<double> y, std::vector<double> z, int iSlice, std::vector<int> row, int color, Size_t width )
{
//  double *xv = &(x[0]), *yv = &(y[0]), *zv = &(z[0]);
//  TPolyLine &pl = GetPolyLine();
//  fYX->cd();
//  pl.DrawPolyLine( x.size(), vy, vx );

  TDrawTrack drawTrack;

//  int ih = track.FirstHitRef();
  for ( int i = 0; i < x.size(); i++ ) {
//      const int iH = fGB->TrackHit(ih);
//      const AliHLTTPCCAGBHit &hit = fGB->Hit(iH);
      TDrawHit dhit;
      dhit.x = x[i];
      dhit.y = y[i];
//      double *xv, *yv;
//      Global2View( (double)x[i], (double)y[i], xv, yv );
//      dhit.x = *xv;
//      dhit.y = *yv;
      dhit.z = z[i];
      dhit.alpha = TDrawHit::GetAlpha( fGB, iSlice );
      dhit.row = row[i];
      dhit.SetColor(color);
      dhit.SetWidth(width);
      drawTrack.hits.push_back(dhit);
      std::cout<<" - hit "<<i<<";   slice: "<<dhit.slice<<";   x: "<<dhit.x<<";   y: "<<dhit.y<<";   z: "<<dhit.z<<"\n";
  }
    drawTrack.SetStatColor(color);
//
    drawTrack.SetStatWidth(0.5);
    drawTrack.DrawGlobal();
    drawTrack.DrawHitsGlobal();
//    drawTrack.DrawGlobalPT();
//    drawTrack.DrawGlobalTpad();
}

void AliHLTTPCCADisplay::TDrawTrack::DrawGlobalTpad2hit( int nhits0 )
{
  const int NHits = hits.size();
  const int NHits1 = NHits - nhits0;
//  if ( NHits < 2 ) return;
//  if ( ! hits[0].CheckGlobal() ) FillGBHitsCoor();

  vector<double> nn, teta;
//  phi.resize(NHits-1);
  teta.resize(2);
  nn.resize(2);
  double *vnn = &(nn[0]), *vteta = &(teta[0]);

  TDrawHit &hit_start = hits[0];
  TDrawHit &hit_end = hits[nhits0-1];
  double x = hit_end.x - hit_start.x;
  double y = hit_end.y - hit_start.y;
  double z = hit_end.z - hit_start.z;
//  teta[0] = acos(x / sqrt(x*x + z*z));
//  teta[0] = asin(z / sqrt(x*x + z*z));
//  teta[0] = asin(x / sqrt(x*x + z*z));
  teta[0] = atan(x / z);
//  if( hit_start.z < 0 ) teta[0] = -teta[0];
  nn[0] = 0.25 + hit_start.slice;
  std::cout<<"-hits.size(): "<<hits.size()<<"\n";
  std::cout<<">last x 0: "<<hit_end.x<<";   last z 0: "<<hit_end.z<<"\n";
  std::cout<<">last x 0: "<<hits[nhits0-1].x<<";   last z 0: "<<hits[nhits0-1].z<<"\n";
  std::cout<<" 1. start z: "<<hit_start.z<<";   slice: "<<hit_start.slice<<"\n";
  std::cout<<" 1.   end z: "<<hit_end.z<<";   slice: "<<hit_end.slice<<"\n";

  TDrawHit &hit_start1 = hits[nhits0];
  TDrawHit &hit_end1 = hits[NHits-1];
  x = hit_end1.x - hit_start1.x;
  y = hit_end1.y - hit_start1.y;
  z = hit_end1.z - hit_start1.z;
//  teta[1] = acos(x / sqrt(x*x + z*z));
//  teta[1] = asin(x / sqrt(x*x + z*z));
  teta[1] = atan(x / z);
//  if( hit_start1.z < 0 ) teta[1] = -teta[1];
  if( teta[0] < 0 && teta[1] > 0 ) teta[1] = -teta[1];
  if( teta[0] > 0 && teta[1] < 0 ) teta[1] = -teta[1];
  nn[1] = 0.75 + hit_start1.slice;
  std::cout<<">first x 1: "<<hit_start1.x<<";   first z 1: "<<hit_start1.z<<"\n";
  std::cout<<" 2. start z: "<<hit_start1.z<<";   slice: "<<hit_start1.slice<<"\n";
  std::cout<<" 2.   end z: "<<hit_end1.z<<";   slice: "<<hit_end1.slice<<"\n";
  std::cout<<" - teta0: "<<teta[0]<<";   teta1: "<<teta[1]<<"\n";
  std::cout<<">last row 0: "<<hits[nhits0-1].row<<";   first row 1: "<<hit_start1.row<<"\n";
  std::cout<<">last x 0: "<<hits[nhits0-1].x<<";   first x 1: "<<hit_start1.x<<"\n";
  std::cout<<">last z 0: "<<hits[nhits0-1].z<<";   first z 1: "<<hit_start1.z<<"\n";
  std::cout<<"-hits.size(): "<<hits.size()<<"\n";
  std::cout<<">>>last x 0: "<<hits[nhits0-1].x<<";   last z 0: "<<hits[nhits0-1].z<<"\n";
//  if( fabs( fabs(hits[nhits0-1].z) - fabs(hit_start.z) ) > 15 ) {
  if( fabs( hits[nhits0-1].row - hit_start1.row ) > 5 ) {
    std::cout<<" --- Bad merging!\n";
    return;
  }
  SetColor(kBlue);
  SetStatWidth(0.75);

  TPolyLine &pl = GetPolyLine();

  fDisplay.CanvasPT()->cd();
  pl.DrawPolyLine( 2, vteta, vnn );
}

void AliHLTTPCCADisplay::DrawTrackFromMergerPT_test( double x0, double x1, double y0, double y1, double z0, double z1, int row0, int row1, int slice0, int slice1, int color, Size_t width )
{
  TDrawTrack drawTrack;
  TDrawHit dhit;
  dhit.x = x0;
  dhit.y = y0;
  dhit.z = z0;
  dhit.alpha = TDrawHit::GetAlpha( fGB, slice0 );
  dhit.slice = slice0;
  dhit.row = row0;
  dhit.SetColor(kGreen);
  dhit.SetWidth(width);
  drawTrack.hits.push_back(dhit);
  dhit.x = x1;
  dhit.y = y1;
  dhit.z = z1;
  dhit.alpha = TDrawHit::GetAlpha( fGB, slice1 );
  dhit.slice = slice1;
  dhit.row = row1;
  dhit.SetColor(kRed);
  dhit.SetWidth(width);
  drawTrack.hits.push_back(dhit);
  drawTrack.SetStatColor(color);
//  drawTrack.DrawGlobalTpad2hit();
  drawTrack.DrawHitsGlobal();
}

void AliHLTTPCCADisplay::TDrawTrack::DrawDrawTetaTrackInt( float iTeta, int iSlice, int pSlice, int nSlice, bool ok, int color )
{
  vector<double> nn, teta;
  //  phi.resize(NHits-1);
  teta.resize(2);
  nn.resize(2);
  double *vnn = &(nn[0]), *vteta = &(teta[0]);
  teta[0] = teta[1] = iTeta;
  nn[0] = 0.25 + iSlice;
  nn[1] = 0.75 + iSlice;
  SetColor(color);
  SetStatWidth(3);
  if( ok ) SetStatWidth(1);

  TPolyLine &pl = GetPolyLine();

  fDisplay.CanvasPT()->cd();
  pl.DrawPolyLine( 2, vteta, vnn );

//  nn[0] = 0.25 + pSlice;
//    nn[1] = 0.75 + nSlice;
//    SetColor(kBlue);
//    SetStatWidth(0.5);
//    TPolyLine &pl1 = GetPolyLine();
//    pl1.DrawPolyLine( 2, vteta, vnn );
}

void AliHLTTPCCADisplay::TDrawTrack::DrawDrawTetaTrackIntMerged( float iTeta, float nTeta, float iSlice, float nSlice, bool ok, int color )
{
  vector<double> nn, teta;
  //  phi.resize(NHits-1);
  teta.resize(2);
  nn.resize(2);
  double *vnn = &(nn[0]), *vteta = &(teta[0]);
  teta[0] = iTeta;
  teta[1] = nTeta;
  nn[0] = 0.5 + iSlice;
  nn[1] = 0.5 + nSlice;
#ifdef DRAWDZDSQPT
  nn[0] = iSlice;
  nn[1] = nSlice;
#endif
  SetColor(color);
  SetStatWidth(1);
  if( ok ) {
      SetColor(kRed);
      SetStatWidth(2);
  }
  if( color == kOrange ) {
    SetColor(kOrange);
    SetStatWidth(0.1);
  }
//  std::cout<<"-----dzds0: "<<teta[0]<<";   dzds1: "<<teta[1]<<";   qp0: "<<nn[0]<<";   qp1: "<<nn[1]<<"\n";

  TPolyLine &pl = GetPolyLine();

  fDisplay.CanvasPT()->cd();
  pl.DrawPolyLine( 2, vteta, vnn );
}

void AliHLTTPCCADisplay::DrawTetaTrack( float iTeta, int iSlice, int pSlice, int nSlice )
{
  TDrawTrack drawTrack;
  drawTrack.DrawDrawTetaTrackInt( iTeta, iSlice, pSlice, nSlice );
}



void AliHLTTPCCADisplay::DrawTetaTrackMerged( float iTeta, float nTeta, float iSlice, float nSlice, bool ok, bool okmc )
{
  TDrawTrack drawTrack;
  if( !okmc ) {
    drawTrack.DrawDrawTetaTrackIntMerged( iTeta, nTeta, iSlice, nSlice, ok );
  }
  else {
    drawTrack.DrawDrawTetaTrackIntMerged( iTeta, nTeta, iSlice, nSlice, ok, kOrange );
  }
}

void AliHLTTPCCADisplay::DrawCorrectMerge( float x0, float x1, float y0, float y1, bool ok )
{
  TDrawTrack drawTrack;
  drawTrack.DrawCorrectMerge( x0, x1, y0, y1, ok );
}

void AliHLTTPCCADisplay::TDrawTrack::DrawCorrectMerge( float x0, float x1, float y0, float y1, bool ok )
{
  vector<double> xx, yy;
  xx.resize(2);
  yy.resize(2);
  double *vxx = &(xx[0]), *vyy = &(yy[0]);
  static TPolyLine pl;
  int color = kOrange;
  if( ok ) color = kBlack;
  pl.SetLineColor( color );
  pl.SetLineWidth( 1 );
#ifdef CANVAS_PT
  fDisplay.CanvasPT()->cd();
  xx[0] = x0; xx[1] = x1;
  yy[0] = y0; yy[1] = y1;
  pl.DrawPolyLine( 2, vxx, vyy );
#endif

//  xx[0] = x0 - 0.03; xx[1] = x0 + 0.03;
//  yy[0] = y0 + 0.3; yy[1] = y0 - 0.3;
//  pl.DrawPolyLine( 2, vxx, vyy );
//
//  xx[0] = x1 - 0.03; xx[1] = x1 + 0.03;
//  yy[0] = y1 - 0.3; yy[1] = y1 + 0.3;
//  pl.DrawPolyLine( 2, vxx, vyy );
//
//  xx[0] = x1 - 0.03; xx[1] = x1 + 0.03;
//  yy[0] = y1 + 0.3; yy[1] = y1 - 0.3;
//  pl.DrawPolyLine( 2, vxx, vyy );
}

void AliHLTTPCCADisplay::DrawWrongMerge( float x0, float x1, float y0, float y1 )
{
  TDrawTrack drawTrack;
  drawTrack.DrawWrongMerge( x0, x1, y0, y1 );
}

void AliHLTTPCCADisplay::TDrawTrack::DrawWrongMerge( float x0, float x1, float y0, float y1 )
{
  vector<double> xx, yy;
  xx.resize(2);
  yy.resize(2);
  double *vxx = &(xx[0]), *vyy = &(yy[0]);
  static TPolyLine pl;
  int color = kBlack;
  pl.SetLineColor( kBlack );
  pl.SetLineWidth( 2 );
#ifdef CANVAS_PT
  fDisplay.CanvasPT()->cd();
  xx[0] = x0 - 0.03; xx[1] = x0 + 0.03;
  yy[0] = y0 - 0.3; yy[1] = y0 + 0.3;
  pl.DrawPolyLine( 2, vxx, vyy );

  xx[0] = x0 - 0.03; xx[1] = x0 + 0.03;
  yy[0] = y0 + 0.3; yy[1] = y0 - 0.3;
  pl.DrawPolyLine( 2, vxx, vyy );

  xx[0] = x1 - 0.03; xx[1] = x1 + 0.03;
  yy[0] = y1 - 0.3; yy[1] = y1 + 0.3;
  pl.DrawPolyLine( 2, vxx, vyy );

  xx[0] = x1 - 0.03; xx[1] = x1 + 0.03;
  yy[0] = y1 + 0.3; yy[1] = y1 - 0.3;
  pl.DrawPolyLine( 2, vxx, vyy );
#endif
}

void AliHLTTPCCADisplay::DrawDzDsPtTrack( float dzds, float pt, bool ok, bool ok1 )
{
  TDrawHit dhit;
  dhit.x = dzds;
  dhit.y = pt;
  dhit.SetColor(kGreen);
  dhit.SetWidth(1);
  if( ok ) {
      dhit.SetColor(kRed);
      dhit.SetWidth(0.5);
  }
  if( ok1 ) {
      dhit.SetColor(kBlue);
      dhit.SetWidth(0.7);
  }
  if( ok && ok1 ) {
      dhit.SetColor(kBlack);
      dhit.SetWidth(0.7);
  }
//  std::cout<<"---dzds: "<<dhit.x<<";   qp: "<<dhit.y<<"\n";
  dhit.DrawHitPT();
}

void AliHLTTPCCADisplay::TDrawHit::DrawHitPT()
{
  TMarker& m = GetMarker();
  fDisplay.CanvasPT()->cd();
  m.DrawMarker( x, y );
}

void AliHLTTPCCADisplay::DrawTrackFromMergerPT( std::vector<double> x0, std::vector<double> y0, std::vector<double> z0, int iSlice0, std::vector<int> row0, std::vector<double> x1, std::vector<double> y1, std::vector<double> z1, int iSlice1, std::vector<int> row1, int color, Size_t width )
{
  TDrawTrack drawTrack;
  int NHits0;
//  if(iSlice0 == iSlice1) return;

  for ( int i = 0; i < x0.size(); i++ ) {
    TDrawHit dhit;
    dhit.x = x0[i];
    dhit.y = y0[i];
    dhit.z = z0[i];
    dhit.alpha = TDrawHit::GetAlpha( fGB, iSlice0 );
    dhit.row = row0[i];
    dhit.slice = iSlice0;
    dhit.SetColor(kGreen);
    dhit.SetWidth(width);
    drawTrack.hits.push_back(dhit);
    std::cout<<" - add hit 0; i: "<<i<<";   z0: "<<z0[i]<<";   slice0: "<<iSlice0<<"\n";
  }
  NHits0 = drawTrack.hits.size();
  for ( int i = 0; i < x1.size(); i++ ) {
    TDrawHit dhit;
    dhit.x = x1[i];
    dhit.y = y1[i];
    dhit.z = z1[i];
    dhit.alpha = TDrawHit::GetAlpha( fGB, iSlice1 );
    dhit.row = row1[i];
    dhit.slice = iSlice1;
    dhit.SetColor(kRed);
    dhit.SetWidth(width);
    drawTrack.hits.push_back(dhit);
    std::cout<<" - add hit 1; i: "<<i<<";   z1: "<<z1[i]<<";   slice1: "<<iSlice1<<"\n";
  }
  drawTrack.SetStatColor(color);
  drawTrack.SetStatWidth(0.5);
  drawTrack.DrawGlobalTpad2hit( NHits0 );
//      drawTrack.DrawGlobal();
//      drawTrack.DrawHitsGlobal();
  //    drawTrack.DrawGlobalPT();
//      drawTrack.DrawGlobalTpad();
}

void AliHLTTPCCADisplay::TDrawTrack::DrawSlicesForTeta()
{
  for( int i = 0; i < 50; i++ ) {
    vector<double> nn, teta;
    teta.resize(2);
    nn.resize(2);
    double *vnn = &(nn[0]), *vteta = &(teta[0]);
    teta[0] = -1.5;
    teta[1] = 1.5;
    nn[0] = nn[1] = 0.25 + i*0.5;
    if( i%2 == 0 ) SetColor(kGreen);
    else SetColor(kRed);
    TPolyLine &pl = GetPolyLine();
    fDisplay.CanvasPT()->cd();
    pl.DrawPolyLine( 2, vteta, vnn );
  }
}

void AliHLTTPCCADisplay::DrawSlicesForTeta()
{
  TDrawTrack drawTrack;
  drawTrack.SetStatColor(kGreen);
  drawTrack.SetStatWidth(0.25);
  drawTrack.DrawSlicesForTeta();
}

void AliHLTTPCCADisplay::DrawFieldDzDsP()
{
  TDrawTrack drawTrack;
  drawTrack.DrawFieldDzDsP();
}

void AliHLTTPCCADisplay::TDrawTrack::DrawFieldDzDsP()
{
  vector<double> xA, yA;
  xA.resize(2);
  yA.resize(2);
  double *vxA = &(xA[0]), *vyA = &(yA[0]);
  TPolyLine &pl = GetPolyLine();
  // ---
  TLatex Tl;
  Tl.SetTextSize(0.02);
  Tl.SetTextAlign(22);
//  Tl.DrawLatex(cos(fiCenter)*fRInnerMin*kText, sin(fiCenter)*fRInnerMin*kText, sISlice);
//  TString sISlice = "";
//  sISlice += iSliceReal;
  // ---
  fDisplay.CanvasPT()->cd();
  // X
  xA[0] = -3; xA[1] = 3;
  yA[0] = 0; yA[1] = 0;
  pl.DrawPolyLine( 2, vxA, vyA );
  xA[0] = 2.9; xA[1] = 3;
  yA[0] = 0.5; yA[1] = 0;
  pl.DrawPolyLine( 2, vxA, vyA );
  xA[0] = 2.9; xA[1] = 3;
  yA[0] = -0.5; yA[1] = 0;
  pl.DrawPolyLine( 2, vxA, vyA );
  TString nameX = "DzDs";
  Tl.DrawLatex(3, -1, nameX);
  for( float xit = -3; xit < 3; xit+=0.5 ) {
      xA[0] = xit; xA[1] = xit;
      yA[0] = -0.5; yA[1] = 0.5;
      pl.DrawPolyLine( 2, vxA, vyA );
      TString nA = "";
      nA += xit;
      Tl.DrawLatex(xit, -0.7, nA);
  }
  for( float xit = -3; xit < 3; xit+=0.1 ) {
      xA[0] = xit; xA[1] = xit;
      yA[0] = -0.1; yA[1] = 0.1;
      pl.DrawPolyLine( 2, vxA, vyA );
  }
  // Y
  xA[0] = -3; xA[1] = -3;
  yA[0] = 0; yA[1] = 31;
  pl.DrawPolyLine( 2, vxA, vyA );
  xA[0] = -3.05; xA[1] = -3;
  yA[0] = 30.5; yA[1] = 31;
  pl.DrawPolyLine( 2, vxA, vyA );
  xA[0] = -2.95; xA[1] = -3;
  yA[0] = 30.5; yA[1] = 31;
  pl.DrawPolyLine( 2, vxA, vyA );
  TString nameY = "QPt";
  Tl.DrawLatex(-3.1, 31.5, nameY);
  for( float yit = 0; yit <= 30; yit+=5 ) {
      xA[0] = -3.03; xA[1] = -2.97;
      yA[0] = yit; yA[1] = yit;
      pl.DrawPolyLine( 2, vxA, vyA );
      TString nA = "";
      nA += yit;
      Tl.DrawLatex(-3.1, yit, nA);
  }
  for( float yit = 0; yit <= 30; yit++ ) {
      xA[0] = -3.01; xA[1] = -2.99;
      yA[0] = yit; yA[1] = yit;
      pl.DrawPolyLine( 2, vxA, vyA );
  }
  // Legend
//  TMarker& m = GetMarker();
  static TMarker m;
  m.SetMarkerStyle(20);
//  m.SetColor(kGreen);
//  m.SetWidth(3);
  m.SetMarkerColor( kGreen );
  m.SetMarkerSize ( 1.5 );
  m.DrawMarker( -2.9, 29 );
  TString l = "Reco tracklets";
  Tl.DrawLatex(-2.6, 29, l);
  m.SetMarkerColor( kBlue );
  m.DrawMarker( -2.9, 28 );
  l = "Merging candidates";
  Tl.DrawLatex(-2.5, 28, l);
  m.SetMarkerColor( kRed );
  m.DrawMarker( -2.9, 27 );
  l = "Merged tracklets";
  Tl.DrawLatex(-2.55, 27, l);
  m.SetMarkerColor( kBlack );
  m.DrawMarker( -2.9, 26 );
  l = "MC tracks";
  Tl.DrawLatex(-2.65, 26, l);
}

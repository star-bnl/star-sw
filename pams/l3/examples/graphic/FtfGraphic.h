#ifndef FTFGRAPHIC
#define FTFGRAPHIC
#include "FtfHit.h"
#include "FtfTrack.h"
#include "TObject.h"
#include "TCanvas.h"
#include "TDialogCanvas.h"
#include "TSlider.h"
#include "TView.h"
#include "TList.h"

#ifdef SL3ROOT
#include "Rtypes.h"
#else
#define ClassDef(a,b)
#endif


class FtfGraphic: public TObject {
public:   
   TCanvas *ftfCanvas ;

   TDialogCanvas *control ;
   TSlider* phiSlider ;
   TSlider* thetaSlider ;
   TSlider* psiSlider ;
   
   TView   *mView ;
   float  bField ;
   float  phi   ;
   float  theta ;
   float  psi   ;
   float  phiMin ;
   float  phiMax ;
   float  etaMin ;
   float  etaMax ;
   float  xMin ;
   float  xMax ;
   float  yMin ;
   float  yMax ;
   float  zMin ;
   float  zMax ; 
   int    hitColor ;
   int    hitMarker ; 
   int    trackColor ;
   int    trackWidth  ;
   int    fitColor ;
   int    fitWidth  ;
   TList* hitList ;
   TList* trackList ;
//
   FtfGraphic ( ) ;
   ~FtfGraphic ( ) ;
//
   int  plotDetector ( ) ;
   int  plotHits     ( ) ;
   int  plotTracks   ( ) ;
   int  plotTracks   ( int thisTrack ) ;
   int  plotTracks   ( int firstTrack, int lastTrack ) ;
   int  plotFits     ( ) ;
   int  plotFits     ( int thisTrack ) ;
   int  plotFits     ( int firstTrack, int lastTrack ) ;
   int  plotFitsa    ( int thisTrack ) ;
   int  plotFitsa    ( int firstTrack, int lastTrack ) ;
   int  plotFit      ( FtfBaseTrack *lTrack, float rMin, float rMax ) ;
   void ExecuteEvent ( Int_t event, Int_t px, Int_t py ) ;
   int  set          ( TList* hitListIn, TList* trackListIn ) ;

   long setDefaults  ( ) ;
   void setXy ( ) { 
      phi = 180. ; theta =  0. ; psi = 90. ; 
      int irep ;
      mView->SetView ( phi, theta, psi, irep ) ;
      ftfCanvas->Draw();
   } ;
   void setYz ( ) { 
      phi = 180. ; theta = 90. ; psi = 90. ; 
      int irep ;
      mView->SetView ( phi, theta, psi, irep ) ;
      ftfCanvas->Draw();
   } ;
   void setXz ( ) { 
      phi = 90. ; theta = 90. ; psi = 90. ; 
      int irep ;
      mView->SetView ( phi, theta, psi, irep ) ;
      ftfCanvas->Draw();
   } ;

   void shiftX ( float dX ) { 
      xMin = xMin + dX * (xMax-xMin) ;
      xMax = xMax + dX * (xMax-xMin) ;
      mView->SetRange( xMin, yMin, zMin, xMax, yMax, zMax );
      ftfCanvas->Draw();
   } ;

   void shiftY ( float dY ) { 
      yMin = yMin + dY * (yMax-yMin) ;
      yMax = yMax + dY * (yMax-yMin) ;
      mView->SetRange( xMin, yMin, zMin, xMax, yMax, zMax );
      ftfCanvas->Draw();
   } ;

   void shiftZ ( float dZ ) { 
      zMin = zMin + dZ * (zMax-zMin) ;
      zMax = zMax + dZ * (zMax-zMin) ;
      mView->SetRange( xMin, yMin, zMin, xMax, yMax, zMax );
      ftfCanvas->Draw();
   } ;

   void shiftPhi ( float dPhi ) { 
      phi  += dPhi ;
      phi  = fmod(phi,360);
      int irep ;
      mView->SetView ( phi, theta, psi, irep ) ;
      ftfCanvas->Draw();
   } ;

   void shiftTheta ( float dTheta ) { 
      theta  += dTheta ;
      theta  = fmod(theta,360);
      int irep ;
      mView->SetView ( phi, theta, psi, irep ) ;
      ftfCanvas->Draw();
   } ;

   void shiftPsi ( float dPsi ) { 
      psi    += dPsi ;
      psi    = fmod(psi,360);
      int irep ;
      mView->SetView ( phi, theta, psi, irep ) ;
      ftfCanvas->Draw();
   } ;

   void zoom ( float factor ) {
      xMin *= factor ;
      xMax *= factor ;
      yMin *= factor ;
      yMax *= factor ;
      zMin *= factor ;
      zMax *= factor ;
      mView->SetRange( xMin, yMin, zMin, xMax, yMax, zMax );
      ftfCanvas->Draw();
   };

//
  ClassDef(FtfGraphic,1)
};
#endif

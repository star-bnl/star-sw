#include "FtfGraphic.h"
#include "TNode.h"
#include "TBRIK.h"
#include "TTUBE.h"
#include "TView.h"
#include "TPolyMarker3D.h"
#include "TPolyLine3D.h"
#include "THelix.h"

#ifdef SL3ROOT
ClassImp(FtfGraphic)
#endif
//
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
FtfGraphic::FtfGraphic ( ) {
   setDefaults ( ) ;
}
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
FtfGraphic::~FtfGraphic ( ) {
}
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
int FtfGraphic::plotDetector ( ) {
//
 Int_t irep;
//
// To see the output of this macro, click  here 
//
  ftfCanvas = new TCanvas("FtfCanvas","Geometry Shapes",0,0,700,700);
  TView   *mView   = new TView(1);
//
//  Define some volumes
//
  TBRIK *box  = new TBRIK("BOX","BOX","void",210,210,400);
  TTUBE *tube = new TTUBE("TUBE","TUBE","void",50,200,400);

//  Set shapes attributes
  tube->SetLineColor(6);
  box->SetLineColor(7);
  tube->SetNumberOfDivisions(22);
//
//  Build the geometry hierarchy
//
//const Text_t *matrixname="";
//Option_t *option=" ";
//TNode *cave = new TNode("CAVE","CAVE","BOX",0.,0.,0.,matrixname,option);
//cave->cd();
//TNode *tpc = new TNode("TPC","TPC","TUBE");
//
// Draw this geometry in the current canvas
// 
//cave->Draw();
//
  ftfCanvas->cd();
  mView->SetRange( xMin, yMin, zMin, xMax, yMax, zMax );
  mView->SetView ( phi, theta, psi, irep ) ;
  tube->Draw();
  ftfCanvas->Update();
//
   return 0 ;
}
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
int FtfGraphic::plotHits ( ) {
//
   if ( nHits < 1 ) {
      printf ( "FtfGraphic::plotHits: No hits to plot \n") ;
      return 1 ;
   }
//
//  Allocate Polymarker
//
   TPolyMarker3D  *hitPM = new TPolyMarker3D( nHits+10 );
   Float_t x, y, z;        // set points
   Float_t r, phi, eta, theta ;
   for( int i = 0; i < nHits; i++ ) {
 
//      printf ( " %f %f %f \n ", tHit[i].x, tHit[i].y, tHit[i].z ) ;
     x = hit[i].x ;
     y = hit[i].y ;
     z = hit[i].z ;

     phi = atan2(y,x)*toDeg;
     if ( phi < 0 ) phi += 360.;
     if ( phi < phiMin ) continue;
     if ( phi > phiMax ) continue;

     r = sqrt(x*x+y*y);
     theta = atan2(r,z);
     eta   = -log(tan(theta/2.));
     if ( eta < etaMin ) continue ;
     if ( eta > etaMax ) continue ;
//
     hitPM->SetPoint( i, x, y, z );
  }
//
   hitPM->SetMarkerColor(hitColor);
   hitPM->SetMarkerStyle(hitMarker);
   hitPM->Draw();
//
   ftfCanvas->Modified();
   ftfCanvas->Update();
//
}
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
int FtfGraphic::plotTracks ( ) {
   if ( nTracks < 1 ) {
      printf ( "FtfGraphic::plotTracks: No tracks to plot \n") ;
      return 1 ;
   }
   plotTracks ( 0, nTracks-1 ) ;
}
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
int FtfGraphic::plotTracks ( int thisTrack ) {
   if ( thisTrack < 0 || thisTrack >= nTracks ) {
      printf ( "FtfGraphic::plotTracks: No track %d to plot \n", thisTrack ) ;
      return 1 ;
   }
   plotTracks ( thisTrack, thisTrack ) ;
}
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
int FtfGraphic::plotTracks ( int firstTrack, int lastTrack ) {
//
   if ( firstTrack < 0       ) firstTrack = 0 ;
   if ( lastTrack  >= nTracks ) lastTrack = nTracks - 1;
//
//     Loop over tracks
//
   Float_t x, y, z;        // set points
   Float_t r, phi, eta, theta ;
   for( int i = firstTrack ; i < lastTrack+1 ; i++ ) {
      TPolyLine3D  *hitPM = new TPolyLine3D( track[i].nHits+1 );
//
      FtfTrack *lTrack = &(track[i]) ;
      int      counter = 0 ;
      for ( lTrack->startLoop() ; lTrack->done() ; lTrack->nextHit() ) {
         x = lTrack->currentHit->x ;
         y = lTrack->currentHit->y ;
         z = lTrack->currentHit->z ;
//
         phi = atan2(y,x)*toDeg;
         if ( phi < 0 ) phi += 360.;
         if ( phi < phiMin ) continue;
         if ( phi > phiMax ) continue;
//
         r = sqrt(x*x+y*y);
         theta = atan2(r,z);
         eta   = -log(tan(theta/2.));
         if ( eta < etaMin ) continue ;
         if ( eta > etaMax ) continue ;
//
         hitPM->SetPoint( counter, x, y, z );
         counter++ ;
      }
//
      hitPM->SetLineColor(trackColor);
      hitPM->SetLineWidth(trackWidth);
      hitPM->Draw();
   }
   ftfCanvas->Modified();
   ftfCanvas->Update();
}
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
int FtfGraphic::plotFits ( ) {
   if ( nTracks < 1 ) {
      printf ( "FtfGraphic::plotFits: No tracks to plot \n") ;
      return 1 ;
   }
   plotFits ( 0, nTracks-1 ) ;
}
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
int FtfGraphic::plotFits ( int thisTrack ) {
   if ( thisTrack < 0 || thisTrack >= nTracks ) {
      printf ( "FtfGraphic::plotFits: No track %d to plot \n", thisTrack ) ;
      return 1 ;
   }
   plotFits ( thisTrack, thisTrack ) ;
}
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
int FtfGraphic::plotFits ( int firstTrack, int lastTrack ) {
//
   if ( firstTrack < 0        ) firstTrack = 0 ;
   if ( lastTrack  >= nTracks ) lastTrack = nTracks - 1;
//
//     Loop over tracks
//
   Double_t x,y,z,px,py,pz,w ;
   Double_t rh, h, phi0l  ;
   bField = 0.5 ;
   w = 100 ;
// rh     = (float)(pt / ( 2.9979e-3 * gr->BField() ) );
// coslam = (float)cos ( lambda )  / rh  ;

   for( int i = firstTrack ; i < lastTrack+1 ; i++ ) {
//
      FtfTrack *lTrack = &(track[i]) ;
      printf ( "i %d psi %e \n", i, lTrack->psi ) ;
      x     = lTrack->r0 * cos(lTrack->phi0);
      y     = lTrack->r0 * sin(lTrack->phi0);
      z     = lTrack->z0 ;
      px    = lTrack->pt * cos(lTrack->psi);
      py    = lTrack->pt * sin(lTrack->psi);
      pz    = lTrack->pt * lTrack->tanl ;
      w     = lTrack->q * bField * 2.9979e-3 ;

//    printf ( " %e %e %e %e %e %e\n", x,y,z,px,py,pz ) ;
      THelix  *helix = new THelix( x, y, z, px, py, pz, w ) ;
      helix->SetRange ( 2.*M_PI/w, kHelixZ ) ;
      helix->Draw();
   }
   ftfCanvas->Modified();
   ftfCanvas->Update();
}
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
int FtfGraphic::plotFitsa ( int trackId ) {
   plotFitsa ( trackId, trackId ) ;
}
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
int FtfGraphic::plotFitsa ( int firstTrack, int lastTrack ) {
   if ( firstTrack < 0        ) firstTrack = 0 ;
   if ( lastTrack  >= nTracks ) lastTrack = nTracks - 1;
//
//     Loop over tracks
//

   for( int i = firstTrack ; i < lastTrack+1 ; i++ ) {
      plotFit ( &(track[i]), 10, 200 )  ;
   }
   ftfCanvas->Modified();
   ftfCanvas->Update();
}
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
int FtfGraphic::plotFit ( FtfTrack *lTrack, float rMin, float rMax ) {

   int no_hit, j ;
   int n_steps = 500 ;

   float lambda, h ;
   float x0, y0, phi0l, rh, coslam ;
   float ss, xl, yl, zl, rl, phil, etal ;
   float distance = 500.F, step = distance / n_steps ;
//
//---->    Get tracks to be plotted
//
   bField = 0.5 ;
//
//--->     Loop over tracks
//
   lambda = (float)atan ( lTrack->tanl ) ;
   rh     =   (float)(lTrack->pt / ( 2.9979e-3 * bField ) );
   h      = - ( lTrack->q * bField ) / (float)fabs ( lTrack->q * bField ) ;

   x0     = lTrack->r0 * (float)cos ( lTrack->phi0 ) ;
   y0     = lTrack->r0 * (float)sin ( lTrack->phi0 ) ;
   phi0l  = (float)(lTrack->psi - h * pi / 2.F) ;
   coslam = (float)cos ( lambda )  / rh  ;
/*
 *--   Go forward
 */
   ss = -step ;
   no_hit = 0 ;
   TPolyLine3D  *hitPM = new TPolyLine3D( 2*n_steps+1 );
   hitPM->SetLineColor(fitColor);
   hitPM->SetLineWidth(fitWidth);
   for ( j=0 ; j<n_steps ; j++ )
   {
     ss = ss + step ;
     xl = (float)(x0 + rh * ( cos ( phi0l + h * ss * coslam ) - cos(phi0l) ) );
     yl = (float)(y0 + rh * ( sin ( phi0l + h * ss * coslam ) - sin(phi0l) ) );
     zl = lTrack->z0 + ss *  (float)sin ( lambda ) ;
     rl     = (float)sqrt(xl*xl+yl*yl) ;
     if ( rl > rMax ) {
        break ;
     }
     phil   = (float)atan2 ( yl, xl ) ;
     if ( phil < 0 ) phil = phil + (float)twoPi ;
     phil  = phil * (float)toDeg ;
     etal   = (float)(-1.*log(tan(atan2(rl,zl)/2.))) ;

     if ( etal > etaMin && etal < etaMax
       && phil > phiMin && phil < phiMax
       && rl   > rMin         && rl   < rMax       )
     {
        hitPM->SetPoint( no_hit, xl, yl, zl );
        no_hit++ ;
     }
/*  End track in required volume condition */
   }
   hitPM->Draw() ;
//
//--    Go backwards
///
   ss     = step ;
   no_hit = 0 ;
   rl     = 0.F ;
   TPolyLine3D  *hitNe = new TPolyLine3D( 2*n_steps+1 );
   hitNe->SetLineColor(fitColor);
   hitNe->SetLineWidth(fitWidth);
   for ( j=0 ; j<n_steps ; j++ )
   {
     ss = ss - step ;
     xl = (float)(x0 + rh * ( cos ( phi0l + h * ss * coslam ) - cos(phi0l) ) );
     yl = (float)(y0 + rh * ( sin ( phi0l + h * ss * coslam ) - sin(phi0l) ) );
     zl = lTrack->z0 + ss *  (float)sin ( lambda ) ;
     rl = (float)sqrt(xl*xl+yl*yl) ;
     
     if ( rl < rMin ) {
         break ;
     }

     phil   = (float)atan2 ( yl, xl ) ;
     if ( phil < 0 ) phil = phil + (float)twoPi ;
     phil  = phil * (float)toDeg ;
     etal   = (float)(-1.*log(tan(atan2(rl,zl)/2.))) ;

     if ( etal > etaMin && etal < etaMax && phil > phiMin && phil < phiMax
          && rl   > rMin     && rl   < rMax       ) {
        hitNe->SetPoint( no_hit, xl, yl, zl );
        no_hit++ ;
     }
/*   End track in required volume condition */
   }
   hitNe->Draw() ;
}


//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
long FtfGraphic::setDefaults ( ) {
   phiMin = 0 ;
   phiMax = 360. ;
   etaMin = -2. ;
   etaMax =  2. ;
   xMin   = -220. ;
   xMax   =  220. ;
   yMin   = -220. ;
   yMax   =  220. ;
   zMin   = -220. ;
   zMax   =  220. ;
   phi    = 180. ;
   theta  = 0.  ;
   psi    = 90. ;
 //
   hitColor    = 2  ;
   hitMarker   = 20 ;
   trackColor  = 4  ;
   trackWidth  = 1 ;
   fitColor    = 3  ;
   fitWidth    = 2 ;
}

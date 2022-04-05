/*!
 * \class StFgtPedPlotter 
 * \author S. Gliske, Sept 2011
 */

/***************************************************************************
 *
 * $Id: StFgtPedPlotter.cxx,v 1.7 2012/01/31 16:48:04 wwitzke Exp $
 * Author: S. Gliske, Sept 2011
 *
 ***************************************************************************
 *
 * Description: See header
 *
 ***************************************************************************
 *
 * $Log: StFgtPedPlotter.cxx,v $
 * Revision 1.7  2012/01/31 16:48:04  wwitzke
 * Changed for cosmic test stand.
 *
 * Revision 1.6  2011/09/30 19:09:07  sgliske
 * general update
 *
 * Revision 1.5  2011/09/29 18:39:43  sgliske
 * Update for geoId->elecCoord function now in StFgtCosmicTestStandGeom
 *
 * Revision 1.4  2011/09/27 00:49:00  sgliske
 * cosmic QA update
 *
 * Revision 1.3  2011/09/26 16:55:52  sgliske
 * Continued work on cosmic QA plots
 *
 * Revision 1.2  2011/09/24 02:14:10  sgliske
 * updated FGT cosmic QA
 *
 * Revision 1.1  2011/09/22 21:21:59  sgliske
 * creation
 *
 *
 **************************************************************************/

#include "StFgtPedPlotter.h"

#include <iostream>
#include <fstream>
using std::cerr;
using std::cout;
using std::endl;

#include <TGraphErrors.h>

#include "StRoot/StFgtPool/StFgtCosmicTestStandGeom/StFgtCosmicTestStandGeom.h"

// since this isn't defined elsewhere (yet)
// set to 8 so that it doesn't matter if bins are 0-6 or 1-7
const Int_t StFgtPedPlotter::mMaxNumTimeBins = 8;

Int_t StFgtPedPlotter::makePlots(){
   Int_t ierr = 0;

   // decide how many time bins are present
   mTimeBinMap.clear();

   for( Int_t i=0, k=-1; i<mMaxNumTimeBins; ++i )
      if( 1<<i & mTimeBinMask )
         mTimeBinMap[i] = ++k;

   UInt_t nTimeBins = mTimeBinMap.size();
   if( !nTimeBins )
      cerr << "No time bins in the time bin mask!" << endl;

   std::vector< std::vector< Float_t > > X, Y, E;
   X.reserve( nTimeBins );
   Y.reserve( nTimeBins );
   E.reserve( nTimeBins );

   // set up the vectors
   for( UInt_t i=0; i<nTimeBins; ++i ){
      X.push_back( std::vector< Float_t >() );
      Y.push_back( std::vector< Float_t >() );
      E.push_back( std::vector< Float_t >() );
   };

   ierr = fillData( X, Y, E );

   for( UInt_t i = 0; i<mGraphVec.size(); ++i ){
      if( mGraphVec[i] )
         delete mGraphVec[i];
   };
   mGraphVec.clear();
   mGraphVec.resize( nTimeBins, 0 );

   if( !ierr ){
      std::map< Int_t, Int_t >::iterator iter;
      for( iter = mTimeBinMap.begin(); iter != mTimeBinMap.end(); ++iter ){
         Int_t timebin =  iter->first;
         Int_t i = iter->second;

         TGraph* gr = makePlot( X[i], Y[i], E[i], timebin );
         cout << "Plot for time bin " << timebin << ' ' << gr << endl;

         if( gr && gr->GetN() )
            mGraphVec[i] = gr;
      };
   };

   return ierr;
};

// to load the data
Int_t StFgtPedPlotter::fillData( VecVec_t& X, VecVec_t& Y, VecVec_t& E ){
   Int_t ierr = 0;

   std::ifstream fin( mFileNameIn.data() );

   if( !fin ){
      cerr << "Error opening file '" << mFileNameIn << "'" << endl;
      ierr = 1;
   };

   if( !ierr ){
      Int_t timebin, geoId;
      Short_t disc, quad; //, strip;
      Char_t layer;
      Float_t ped, stdev, x = 0;

      while( !fin.eof() && !ierr ){
         fin >> geoId >> timebin >> ped >> stdev;
         //cout << geoId << ' ' << timebin << ' ' << ped << ' ' << stdev << endl;

         if( 1<<timebin & mTimeBinMask ){
            Double_t pos, high, low;

            //StFgtGeom::decodeGeoId( geoId, disc, quad, layer, strip );
            StFgtGeom::getPhysicalCoordinate( geoId, disc, quad, layer, pos, high, low );

            Bool_t pass = 1;

            if( disc == mDiscId && quad == mQuadId ){
               Int_t i = mTimeBinMap[ timebin ];

               if( mPlotVsStrip == 'r' || mPlotVsStrip == 'R' ){
                  pass = ( layer == 'R' );
                  pass &= ( mPlotVsStrip == 'R' ? (low > 0.785) : (low < 0.785) );
                  x = pos;
               } else if( mPlotVsStrip == 'P' ){
                  x = pos;
                  pass = ( layer == mPlotVsStrip );
               } else {
                  Int_t rdo, arm, apv, channel;
                  StFgtCosmicTestStandGeom::getNaiveElecCoordFromGeoId(geoId,rdo,arm,apv,channel);
                  x = 128*(apv%12) + channel;
               };

               if( x && pass ){
//                   cout << mPlotVsStrip << ' ' << x << ' ' << ped << ' ' << stdev << " | "
//                        << geoId << ' ' << disc << ' ' << quad << ' ' << layer << ' ' << high << ' ' << low << endl;

                  X[i].push_back( x );
                  Y[i].push_back( ped );
                  E[i].push_back( stdev );

                  Float_t maxY = stdev;
                  if( !mDoPlotStDev  )
                     maxY += ped;
                  if( maxY > mMaxY )
                     mMaxY = maxY;
                  if( x > mMaxX )
                     mMaxX = x;
               };
            };
         };

         // check to see if at the end of the file
         fin >> std::ws;
         fin.peek();
      };
   };

   if( mMaxY > 4096 )
      mMaxY = 4096;

   return ierr;
};

// for each time bin
TGraph* StFgtPedPlotter::makePlot( std::vector< Float_t >& x,
                                         std::vector< Float_t >& y,
                                         std::vector< Float_t >& e,
                                         Int_t timebin ){
   TGraph* gr = 0;
//    Float_t xlow = 0;
//    Float_t xhigh = 1280;
//    Int_t nbins = 1280;
//    if( mPlotVsStrip == 'R' ){
//       xlow = 100;
//       xhigh = 400;
//    } else if ( mPlotVsStrip == 'P' ){
//       xlow = 0;
//       xhigh = 6.28/4;
//    };

   cout << "sizes " << x.size() << ' ' << y.size() << ' ' << e.size() << endl;
   if( !x.empty() && x.size() == y.size() && y.size() == e.size() ){
      // need to copy to array
      Float_t *xArr = new Float_t[ x.size() ];
      Float_t *yArr = new Float_t[ x.size() ];
      Float_t *eArr = new Float_t[ x.size() ];

      for( UInt_t i=0; i<x.size(); ++i ){
         xArr[i] = x[i];
         yArr[i] = y[i];
         eArr[i] = e[i];
      };

      if( mDoPlotStDev )
         gr = new TGraph( x.size(), xArr, eArr );
      else
         gr = new TGraphErrors( x.size(), xArr, yArr, 0, eArr );
      std::stringstream ss;

      if( mPlotVsStrip == 'r' )
         ss << "r-strips, cham. 1: ";
      else if( mPlotVsStrip == 'R' )
         ss << "r-strips, cham. 2: ";
      else if( mPlotVsStrip == 'P' )
         ss << "#phi-strips: ";

      ss << "Pedestal ";
      if( mDoPlotStDev )
         ss << "St. Dev. ";
      ss << "vs. ";

      if( mPlotVsStrip == 'R' || mPlotVsStrip == 'r' || mPlotVsStrip == 'P' )
         ss << "Position";
      else
         ss << "Channel";

      if( !mQuadName.empty() )
         ss << ", Quad " << mQuadName;

      if( mPlotVsStrip == 'r' || mPlotVsStrip == 'R' )
         ss << "; r Strip Pos. [cm]";
      else if( mPlotVsStrip == 'P' )
         ss << "; #phi Strip Pos. [cm]";
      else
         ss << "; 128x(APV Num) + Channel Id.";

      ss << "; ADC Value";

      gr->SetTitle( ss.str().data() );

      if( mDoPlotStDev )
         gr->SetMarkerStyle(20);
   };

   return gr;
};

const TGraph* StFgtPedPlotter::getGraph( Int_t timebin ) const {
   std::map< Int_t, Int_t >::const_iterator iter = mTimeBinMap.find( timebin );
   return (iter == mTimeBinMap.end() ? 0 : mGraphVec[ iter->second ]);
};


ClassImp( StFgtPedPlotter );

/*!
 * \class StFgtPedPlotter 
 * \author S. Gliske, Sept 2011
 */

/***************************************************************************
 *
 * $Id: StFgtPedPlotter.cxx,v 1.2 2011/09/24 02:14:10 sgliske Exp $
 * Author: S. Gliske, Sept 2011
 *
 ***************************************************************************
 *
 * Description: See header
 *
 ***************************************************************************
 *
 * $Log: StFgtPedPlotter.cxx,v $
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

#include "StRoot/StFgtUtil/geometry/StFgtGeom.h"

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

         TGraphErrors* gr = makePlot( X[i], Y[i], E[i], timebin );
         //cout << "Plot for time bin " << timebin << ' ' << gr << endl;

         if( gr && gr->GetN() ){
            mGraphVec[i] = gr;
         };
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
      Short_t disc, quad, strip;
      Char_t layer;
      Float_t ped, stdev, x;

      while( !fin.eof() && !ierr ){
         fin >> geoId >> timebin >> ped >> stdev;

         if( 1<<timebin & mTimeBinMask ){

            StFgtGeom::decodeGeoId( geoId, disc, quad, layer, strip );

            if( disc == mDiscId && quad == mQuadId ){
               Int_t i = mTimeBinMap[ timebin ];

               if( mPlotVsStrip == 'R' ){
                  x = (strip == 'R') ? strip : -1;
               } else if( mPlotVsStrip == 'P' ){
                  x = (strip == 'P') ? strip : -1;
               } else {
                  x = strip + (layer == 'R')*kNumFgtStripsPerLayer;  // need to fix this
                  std::cerr << "WARNING: asked for vs. channel but getting vs. strip" << endl;
               };

               if( x >= 0 ){
                  X[i].push_back( x );
                  Y[i].push_back( ped );
                  E[i].push_back( stdev );

                  if( ped+stdev > mMaxY )
                     mMaxY = (ped+stdev);
                  if( x > mMaxX )
                     mMaxX = x;
               };
            };
         };

         // check to see if at the end of the file
         fin.peek();
      };
   };

   return ierr;
};

// for each time bin
TGraphErrors* StFgtPedPlotter::makePlot( std::vector< Float_t >& x,
                                         std::vector< Float_t >& y,
                                         std::vector< Float_t >& e,
                                         Int_t timebin ){
   TGraphErrors* gr = 0;
   Int_t width = 1280;
   if( mPlotVsStrip )
      width = 2*kNumFgtStripsPerLayer;

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

      gr = new TGraphErrors( x.size(), xArr, yArr, 0, eArr );
      std::stringstream ss;
      ss << "Pedistals vs. ";
      if( mPlotVsStrip )
         ss << "Strip Id";
      else
         ss << "Channel";

      if( mPlotVsStrip )
         ss << "; Strip Id. (#phi = 0-" << kNumFgtStripsPerLayer-1
            << ", r = " << kNumFgtStripsPerLayer << '-' << 2*kNumFgtStripsPerLayer-1 << ")";
      else
         ss << "; 128x(APV Num) + Channel Id.";

      ss << "; ADC Value";

      gr->SetTitle( ss.str().data() );
   };

   return gr;
};

const TGraphErrors* StFgtPedPlotter::getGraph( Int_t timebin ) const {
   std::map< Int_t, Int_t >::const_iterator iter = mTimeBinMap.find( timebin );
   return (iter == mTimeBinMap.end() ? 0 : mGraphVec[ iter->second ]);
};

ClassImp( StFgtPedPlotter );

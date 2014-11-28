/***************************************************************************
 *
 * $Id: StFgtLHTracking.cxx,v 1.4 2012/04/13 15:08:58 sgliske Exp $
 * Author: S. Gliske, March 2012
 *
 ***************************************************************************
 *
 * Description: see header.
 *
 ***************************************************************************
 *
 * $Log: StFgtLHTracking.cxx,v $
 * Revision 1.4  2012/04/13 15:08:58  sgliske
 * updates
 *
 * Revision 1.3  2012/04/11 22:13:30  sgliske
 * update
 *
 * Revision 1.2  2012/04/09 21:08:24  sgliske
 * many bugs fixed--seems to be working
 *
 * Revision 1.1  2012/03/16 21:51:22  sgliske
 * creation
 *
 *
 **************************************************************************/

#include "StFgtLHTracking.h"
#include "StRoot/StFgtUtil/geometry/StFgtGeom.h"

//#define DEBUG

#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StarClassLibrary/StThreeVectorF.hh"
#include <TVector3.h>

// constructor
StFgtLHTracking::StFgtLHTracking( const Char_t* name ) : StFgtTracking( name ), mPoints( 3 ), mFitThres( 1 ), mIncludeThres( 0.5 ), mNumAgreeThres( mPoints-1 ), mUseVertex(0) { /* */ };

// deconstructor
StFgtLHTracking::~StFgtLHTracking(){ /* */ };

void StFgtLHTracking::Clear( const Option_t *opt ){
   StFgtTracking::Clear();
   mLineVec.clear();
   mTrackVec.clear();
};

// find the tracks
Int_t StFgtLHTracking::findTracks(){

   // Make lines out of triplets of points.  Points must come from different discs.
   StFgtTrPointVec tempPoints;
   Int_t startDiscIdx = 0;
   UShort_t discBitArray = 0;
   TVector3 vertex;
   Bool_t vertexValid = 0;

   if( mUseVertex ){
      StMuDstMaker* muDstMkr = static_cast< StMuDstMaker* >( GetMaker( "MuDst" ) );
      if( muDstMkr ){
         StMuEvent *event = muDstMkr->muDst()->event();

         if( event ){
            const StThreeVectorF& v = event->primaryVertexPosition();
            tempPoints.push_back( StFgtTrPoint( v.x(), v.y(), v.z() ) );
            vertex.SetXYZ( v.x(), v.y(), v.z() );
            vertexValid = 1;
         } else {
            LOG_ERROR << "ERROR finding vertex from StMuEvent" << endl;
         };
      };
   };

   makePointTuples( tempPoints, startDiscIdx, discBitArray );

   Int_t nLines = mLineVec.size();

#ifdef DEBUG
   if( nLines ){
      LOG_INFO << "------------------------------------------------" << endm;
      LOG_INFO << "Event number " << GetEventNumber() << " has " << mPointsTot << " points and " << nLines << " line(s)" << endm;

//      StFgtLHLineVec::iterator lineIter;
//       for( lineIter = mLineVec.begin(); lineIter != mLineVec.end(); ++lineIter ){
//          LOG_INFO << "Line params "
//                   << lineIter->mx << ' ' << lineIter->bx << ' '
//                   << lineIter->my << ' ' << lineIter->by << ' '
//                   << lineIter->vertZ << ' ' << lineIter->res << ' ' << lineIter->res/sqrt(mPoints) << endm;
//       };
   };
#endif

   if( nLines ){
      // copy lines to track containers
      mTrackVec.clear();
      StFgtLHLineVec::iterator lineIter;
      for( lineIter = mLineVec.begin(); lineIter != mLineVec.end(); ++lineIter )
         mTrackVec.push_back( StFgtLHTrack( *lineIter ) );

      // prep stuff
      Double_t includeThresSq = mIncludeThres * mIncludeThres;
      StFgtLHTrackVec::iterator trackIter1, trackIter2;
      StFgtTrPointVec::const_iterator pointIter;

      // iterate over lines to collect the nearby (or nearest) point
      Int_t trackIdx = 0;
      for( trackIter1 = mTrackVec.begin(); trackIter1 != mTrackVec.end(); ++trackIter1, ++trackIdx ){
         for( Int_t disc = 0; disc < kFgtNumDiscs; ++disc ){
            StFgtTrPointVec &pointVec = mPointVecPerDisc[ disc ];

            Int_t pointIdx = 0;
            for( pointIter = pointVec.begin(); pointIter != pointVec.end(); ++pointIter, ++pointIdx ){
               Double_t thisResSq = perpDistSqLineToPoint( trackIter1->line, pointIter->pos );

               if( thisResSq < includeThresSq && thisResSq < trackIter1->resSqPerDisc[disc] ){
                  trackIter1->resSqPerDisc[disc] = thisResSq;
                  trackIter1->pointPerDisc[disc] = pointIdx;
               };
            };
         };
      };

#ifdef DEBUG2
      Int_t nTracks = mTrackVec.size();
      LOG_INFO << "Before prunning, event number " << GetEventNumber() << " has " << nTracks << " tracks" << endm;

      trackIdx = 0;
      for( trackIter1 = mTrackVec.begin(); trackIter1 != mTrackVec.end(); ++trackIter1, ++trackIdx ){
         cout << "\tPoints on track " << trackIdx << ": ";
         for( Int_t i = 0; i<kFgtNumDiscs; ++i )
            cout << trackIter1->pointPerDisc[i] << ' ';
         cout << endl;
      };

      cout << "mInclude Thres is " << mIncludeThres << endl;
#endif

      // now combine tracks that include almost the same points
      for( trackIter1 = mTrackVec.begin(); trackIter1 != mTrackVec.end(); ++trackIter1 ){
         if( trackIter1->pointPerDisc[0] > -2 ){
            trackIter1->effResSq = 0;
            Int_t nDiscs = 0;

            if( vertexValid ){
               ++nDiscs;
               trackIter1->effResSq += perpDistSqLineToPoint( trackIter1->line, vertex );
            };

            for( Int_t disc = 0; disc < kFgtNumDiscs; ++disc )
               if( trackIter1->pointPerDisc[disc] > -1 ){
                  trackIter1->effResSq += trackIter1->resSqPerDisc[disc];
                  ++nDiscs;
               };
            trackIter1->effResSq /= (nDiscs*nDiscs);

            if( nDiscs < mPoints ){
               trackIter1->pointPerDisc[0] = -10;
               continue;
            };

            for( trackIter2 = mTrackVec.begin(); trackIter2 != trackIter1; ++trackIter2 ){
               if( trackIter2->pointPerDisc[0] > -2 ){
                  Int_t nAgree = 0;
                  for( Int_t i = 0; i<kFgtNumDiscs; ++i )
                     nAgree += ( trackIter1->pointPerDisc[i] == trackIter2->pointPerDisc[i] && trackIter1->pointPerDisc[i] > -1 ) ;

                  //cout << "\tNum Agree " << nAgree << ' ' << trackIter1->effResSq << ' ' << trackIter2->effResSq << endl;
                  if( nAgree >= mNumAgreeThres ){
                     ( trackIter1->effResSq < trackIter2->effResSq ? trackIter2 : trackIter1 )->pointPerDisc[0] = -10;  // flag to delete
                  };
               };
            };
         };
      };
         
      // prune the removed lines
      StFgtLHTrackVec copyVec;
      for( trackIter1 = mTrackVec.begin(); trackIter1 != mTrackVec.end(); ++trackIter1 )
         if( trackIter1->pointPerDisc[0] > -2 )
            copyVec.push_back( *trackIter1 );
      copyVec.swap( mTrackVec );

#ifdef DEBUG
      Int_t nTracks = mTrackVec.size();
      LOG_INFO << "After prunning, event number " << GetEventNumber() << " has " << nTracks << " tracks" << endm;

      trackIdx = 0;
      for( trackIter1 = mTrackVec.begin(); trackIter1 != mTrackVec.end(); ++trackIter1, ++trackIdx ){
         cout << "\tPoints on track " << trackIdx << ": ";
         for( Int_t i = 0; i<kFgtNumDiscs; ++i )
            cout << trackIter1->pointPerDisc[i] << ' ';
         cout << ", old vertZ " << trackIter1->line.vertZ << ", effRes " << sqrt( trackIter1->effResSq ) << endl;
      };
#endif

      // refit the line
      mLineVec.clear();
      StFgtTrPointVec points;
      StFgtTrPoint vertex2( vertex.X(), vertex.Y(), vertex.Z() );
 
      for( trackIter1 = mTrackVec.begin(); trackIter1 != mTrackVec.end(); ++trackIter1, ++trackIdx ){
         points.clear();
         UShort_t bitArray = 0;

         if( vertexValid )
            points.push_back( vertex2 );

         for( Int_t i = 0; i<kFgtNumDiscs; ++i )
            if( trackIter1->pointPerDisc[i] > -1 ){
               points.push_back( mPointVecPerDisc[i][trackIter1->pointPerDisc[i]] );
               bitArray |= (1<<i);
            };

         // compute the new line
         makeLine( points, bitArray, &trackIter1->line );

         // copy the results
         trackIter1->effResSq = trackIter1->line.res / points.size();
         trackIter1->effResSq *= trackIter1->effResSq;

#ifdef DEBUG
         Double_t perp2 = trackIter1->line.bx*trackIter1->line.bx + trackIter1->line.by*trackIter1->line.by;
         Double_t x =  trackIter1->line.bx +  trackIter1->line.mx*120;
         Double_t y =  trackIter1->line.by +  trackIter1->line.my*120;
         Double_t perpB = sqrt(x*x+y*y);
         cout << "\tEvent " << GetEventNumber() << " Track new vertZ " << trackIter1->line.vertZ << ", effRes " << sqrt( trackIter1->effResSq )
              << ", bitVec 0x" << std::hex << bitArray << std::dec << ", perp at z=0 " << sqrt(perp2) << " perp at z=120 " << perpB;

         {
            StMuDstMaker* muDstMkr = static_cast< StMuDstMaker* >( GetMaker( "MuDst" ) );
            if( muDstMkr ){
               StMuEvent *event = muDstMkr->muDst()->event();

               if( event ){
                  const StThreeVectorF& v = event->primaryVertexPosition();
                  cout << " delta vertZ = " << trackIter1->line.vertZ - v.z();
               };
            };
         };
         cout << endl;
#endif
      };

#ifdef DEBUG
      StMuDstMaker* muDstMkr = static_cast< StMuDstMaker* >( GetMaker( "MuDst" ) );
      if( muDstMkr ){
         StMuEvent *event = muDstMkr->muDst()->event();

         if( event ){
            const StThreeVectorF& v = event->primaryVertexPosition();
            cout << "-----> Real vertex at " << v.x() << ' ' << v.y() << ' ' << v.z() << endl;
         };
      };
#endif

//       if( nTracks ){
//          StFgtLHTrackData *data = new StFgtLHTrackData( "LHTracks", mTrackVec );
//          AddData( data );
//          cout << "Event " << GetEventNumber() << " added data is at "
//               << GetData( "LHTracks" ) << endl;
//       };
   };
   
   return kStOk;
};


// recursive algo to get all combinations of mPoints points, each point beging on a different disc
void StFgtLHTracking::makePointTuples( StFgtTrPointVec& points, Int_t startDiscIdx, UShort_t discBitArray ){
   if( startDiscIdx < kFgtNumDiscs ){
      StFgtTrPointVec::const_iterator iter;

      //LOG_INFO << "Event Number " << GetEventNumber() << " ---------> Number of points " << points.size() << ", start disc is " << startDiscIdx+1 << endm;

      for( Int_t disc = startDiscIdx; disc < kFgtNumDiscs; ++disc ){
         StFgtTrPointVec &pointVec = mPointVecPerDisc[ disc ];
         UShort_t discBit = (1<<disc);

         for( iter = pointVec.begin(); iter != pointVec.end(); ++iter ){
            points.push_back( *iter );
            //cout << std::dec << GetEventNumber() << " 0x" << std::hex << (discBitArray | discBit) << std::dec << ' ' << points.size() << endl;
            ( (Int_t)points.size() < mPoints ) ? makePointTuples( points, disc+1, discBitArray | discBit ) : makeLine( points, discBitArray | discBit );
            points.pop_back();
         };
      };
   };
};

void StFgtLHTracking::makeLine( StFgtTrPointVec& points, UShort_t discBitArray, StFgtLHLine *linePtr ){
   StFgtTrPointVec::const_iterator iter = points.begin();
   Double_t A = 0, B = 0, Cx = 0, Cy = 0, D = 0, Ex = 0, Ey = 0;

   //LOG_INFO << " --------------------------> calling makeLine with " << points.size() << " 0x" << std::hex << discBitArray << std::dec << endm;

   for( ; iter != points.end(); ++iter ){
      Double_t x = iter->pos.X();
      Double_t y = iter->pos.Y();
      Double_t z = iter->pos.Z();

      A += z*z;
      B += z;
      Cx += x*z;
      Cy += y*z;
      Ex += x;
      Ey += y;

      //cout << "*** Point located at " << x << ' ' << y << ' ' << z << " <-> " << iter->pos.Perp() << ' ' << iter->pos.Phi() << endl;
   };
   D = points.size();
   //cout << "*** Consts " << A << ' ' << B << ' ' << Cx << ' ' << Cy << ' ' << D << ' ' << Ex << ' ' << Ey << endl;

   Double_t denom = D*A - B*B;
   if( denom ){
      Double_t bx = (-B*Cx + A*Ex)/denom;
      Double_t by = (-B*Cy + A*Ey)/denom;
      Double_t mx = ( D*Cx - B*Ex)/denom;
      Double_t my = ( D*Cy - B*Ey)/denom;

      mLineVec.push_back( StFgtLHLine( discBitArray, mx, my, bx, by ) );
      StFgtLHLine& line = mLineVec.back();

      //cout << "*** Line params " << mx << ' ' << bx << ' ' << my << ' ' << by << endl;

//       for( iter = points.begin(); iter != points.end(); ++iter ){
//          cout << "--- Location at each disc, "
//               << "X: " << mx*iter->pos.Z()+bx << " vs " << iter->pos.X() << ' '
//               << "Y: " << my*iter->pos.Z()+by << " vs " << iter->pos.Y() << endl;
//       };

      // push back points and compute dist
      Double_t dist = 0;
      for( iter = points.begin(); iter != points.end(); ++iter ){
         line.pointIdx.push_back( iter->ptIdx );
         dist += perpDistSqLineToPoint( line, iter->pos );
         //cout << "*** DistSq " << dist << endl;
      };
      line.res = sqrt(dist);

      // copy to pointer, if one provided
      if( linePtr )
         *linePtr = line;

      // #ifdef DEBUG
      //       cout << "*** Event " << GetEventNumber() << " final distSQ " << dist << endl;
      // #endif

      // add to vector, if thes OK
      if( line.res > mFitThres )
         mLineVec.pop_back();
   };
};

ClassImp( StFgtLHTracking );

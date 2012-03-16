/***************************************************************************
 *
 * $Id: StFgtLHTracking.cxx,v 1.1 2012/03/16 21:51:22 sgliske Exp $
 * Author: S. Gliske, March 2012
 *
 ***************************************************************************
 *
 * Description: see header.
 *
 ***************************************************************************
 *
 * $Log: StFgtLHTracking.cxx,v $
 * Revision 1.1  2012/03/16 21:51:22  sgliske
 * creation
 *
 *
 **************************************************************************/

#include "StFgtLHTracking.h"
#include "StRoot/StFgtUtil/geometry/StFgtGeom.h"

#define DEBUG

// constructor
StFgtLHTracking::StFgtLHTracking( const Char_t* name ) : StFgtTracking( name ), mPoints( 3 ), mFitThres( 1 ), mInclusionThres( 0.5 ) { /* */ };

// deconstructor
StFgtLHTracking::~StFgtLHTracking(){ /* */ };

void StFgtLHTracking::Clear( const Option_t *opt ){
   mLineVec.clear();
};

// find the tracks
Int_t StFgtLHTracking::findTracks(){

   // Make lines out of triplets of points.  Points must come from different discs.
   StFgtTrPointVec tempPoints;
   Int_t startDiscIdx = 0;
   UShort_t discBitArray = 0;
   makePointTuples( tempPoints, startDiscIdx, discBitArray );

   Int_t nLines = mLineVec.size();

#ifdef DEBUG
   if( nLines ){
      LOG_INFO << GetEventNumber() << " mPointsTot = " << mPointsTot << endm;
      LOG_INFO << GetEventNumber() << " have " << nLines << " number of lines" << endm;

      StFgtLHLineVec::iterator lineIter;

//       for( Int_t disc1 = 0, discIdx = 0; disc1 < kFgtNumDiscs-1; ++disc1 ){
//          for( Int_t disc2 = disc1+1; disc2 < kFgtNumDiscs; ++disc2, ++discIdx ){
//             for( lineIter = mLineVec.begin(); lineIter != mLineVec.end(); ++lineIter ){
//                LOG_INFO << lineIter->mx << ' ' << lineIter->bx << ' '
//                         << lineIter->my << ' ' << lineIter->by << ' '
//                         << lineIter->vertZ << endm;
//             };
//          };
//       };
   };
#endif

   return kStOk;
};


// recursive algo to get all combinations of mPoints points, each point beging on a different disc
void StFgtLHTracking::makePointTuples( StFgtTrPointVec& points, Int_t startDiscIdx, UShort_t discBitArray ){
   StFgtTrPointVec::const_iterator iter;

   cout << GetEventNumber() << "---------> " << points.size() << endl;

   for( Int_t disc = startDiscIdx; disc < kFgtNumDiscs-1; ++disc ){
      StFgtTrPointVec &pointVec = mPointVecPerDisc[ disc ];
      UShort_t discBit = (1<<disc);

      for( iter = pointVec.begin(); iter != pointVec.end(); ++iter ){
         points.push_back( *iter );
         cout << std::dec << GetEventNumber() << ' ' << std::hex << (discBitArray | discBit) << std::dec << ' ' << points.size() << endl;
         ( (Int_t)points.size() < mPoints ) ? makePointTuples( points, disc+1, discBitArray | discBit ) : makeLine( points, discBitArray | discBit );
         points.pop_back();
      };
   };
};

void StFgtLHTracking::makeLine( StFgtTrPointVec& points, UShort_t discBitArray ){
   StFgtTrPointVec::const_iterator iter = points.begin();
   Double_t A = 0, B = 0, Cx = 0, Cy = 0, D = 0, Ex = 0, Ey = 0;


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

      cout << "AAA " << x << ' ' << y << ' ' << z << endl;
   };
   D = points.size();
   cout << "AAAA " << A << ' ' << B << ' ' << Cx << ' ' << Cy << ' ' << D << ' ' << Ex << ' ' << Ey << endl;

   Double_t denom = D*A - B*B;
   if( denom ){
      Double_t bx = (B*Cx - A*Ex)/denom;
      Double_t by = (B*Cy - A*Ey)/denom;
      Double_t mx = (D*Cx - B*Ex)/denom;
      Double_t my = (D*Cy - B*Ey)/denom;

      mLineVec.push_back( StFgtLHLine( discBitArray, mx, my, bx, by ) );
      StFgtLHLine& line = mLineVec.back();

      cout << "aaa " << mx << ' ' << bx << ' ' << my << ' ' << by << endl;

      // push back points and compute dist
      Double_t dist = 0;
      for( iter = points.begin(); iter != points.end() && dist < mFitThres; ++iter ){
         line.pointIdx.push_back( iter->ptIdx );
         dist += perpDistSqLineToPoint( line, iter->pos );
         cout << "aaaa " << dist << endl;
      };


#ifdef DEBUG
      cout << GetEventNumber() << " dist " << dist << endl;
#endif

      if( dist > mFitThres )
         mLineVec.pop_back();
   };
};

ClassImp( StFgtLHTracking );

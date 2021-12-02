/*
 * This file is part of TPCCATracker package
 * Copyright (C) 2007-2020 FIAS Frankfurt Institute for Advanced Studies
 *               2007-2020 Goethe University of Frankfurt
 *               2007-2020 Ivan Kisel <I.Kisel@compeng.uni-frankfurt.de>
 *               2007-2019 Sergey Gorbunov
 *               2007-2019 Maksym Zyzak
 *               2007-2014 Igor Kulakov
 *               2014-2020 Grigory Kozlov
 *
 * TPCCATracker is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * TPCCATracker is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */


#include "AliHLTTPCCANeighboursCleaner.h"
#include "AliHLTTPCCAMath.h"
#include "AliHLTTPCCATracker.h"

// *
// * kill link to the neighbour if the neighbour is not pointed to the hit
// *
#ifndef V6
void AliHLTTPCCANeighboursCleaner::run( const int numberOfRows, SliceData &data, const AliHLTTPCCAParam &param )
#else
void AliHLTTPCCANeighboursCleaner::run( const int numberOfRows, AliHLTTPCCASliceData &data, const AliHLTTPCCAParam &param, int it, std::vector<hit_link>& save_up_links )
#endif
{
  float_v X,Y,Z,Xup,Yup,Zup,Xdown,Ydown,Zdown, X4,Y4,Z4;
  float_v Yx1, Yx2, Yxx1, Yxx2, Yxxx, Zx1, Zx2, Zxx1, Zxx2, Zxxx, iX;
  const int_v minusOne(-1);

  const int rowStep = AliHLTTPCCAParameters::RowStep;
  const int beginRowIndex = rowStep;
  const int endRowIndex = numberOfRows - rowStep;
  for ( int rowIndex = beginRowIndex; rowIndex < endRowIndex; ++rowIndex ) {
    const AliHLTTPCCARow &row = data.Row( rowIndex );
    const AliHLTTPCCARow &rowUp = data.Row( rowIndex + rowStep );
    const AliHLTTPCCARow &rowDown = data.Row( rowIndex - rowStep );
    const int numberOfHits = row.NHits();

    // - look at up link, if it's valid but the down link in the row above doesn't link to us remove
    //   the link
    // - look at down link, if it's valid but the up link in the row below doesn't link to us remove
    //   the link
    for ( int hitIndex = 0; hitIndex < numberOfHits; hitIndex += int_v::Size ) {

      const uint_v hitIndexes = uint_v( Vc::IndexesFromZero ) + hitIndex;
      int_m validHitsMask = hitIndexes < numberOfHits;
      assert( ( validHitsMask && ((hitIndexes   >= 0 ) && (hitIndexes   < row.NHits()   )) ) == validHitsMask );
//      validHitsMask &= ( int_v(data.HitDataIsUsed( row ), hitIndexes, validHitsMask ) == int_v( Vc::Zero ) ); // not-used hits can be connected only with not-used, so only one check is needed
      int_v hitDataTemp;
      for( unsigned int i = 0; i < float_v::Size; i++ ) {
	if( !validHitsMask[i] ) continue;
	hitDataTemp[i] = data.HitDataIsUsed( row )[(unsigned int)hitIndexes[i]];
      }
      validHitsMask &= hitDataTemp == int_v( Vc::Zero );

        // collect information
        // up part
      assert( ( validHitsMask && ((hitIndexes   >= 0 ) && (hitIndexes   < row.NHits()   )) ) == validHitsMask );
      int_v up;
      for( unsigned int i = 0; i < float_v::Size; i++ ) {
	if( !validHitsMask[i] ) continue;
	up[i] = data.HitLinkUpData( row )[(unsigned int)hitIndexes[i]];
      }
      VALGRIND_CHECK_VALUE_IS_DEFINED( up );
      const uint_v upIndexes = up.staticCast<uint_v>();
      assert ( (validHitsMask && (up >= minusOne) ) == validHitsMask );
      int_m upMask = validHitsMask && up >= int_v( Vc::Zero );
      assert( ( upMask && ((upIndexes   >= 0 ) && (upIndexes   < rowUp.NHits()   )) ) == upMask );
      int_v downFromUp;
      for( unsigned int i = 0; i < float_v::Size; i++ ) {
	if( !upMask[i] ) continue;
	downFromUp[i] = data.HitLinkDownData( rowUp )[(unsigned int)upIndexes[i]];
      }
        // down part
      int_v dn;
      for( unsigned int i = 0; i < float_v::Size; i++ ) {
      	if( !validHitsMask[i] ) continue;
      	dn[i] = data.HitLinkDownData( row )[(unsigned int)hitIndexes[i]];
      }
      assert ( ( validHitsMask && (dn >= minusOne) ) == validHitsMask );
      VALGRIND_CHECK_VALUE_IS_DEFINED( dn );
      const uint_v downIndexes = dn.staticCast<uint_v>();
      int_m dnMask = validHitsMask && dn >= int_v( Vc::Zero );
      assert( ( dnMask && ((downIndexes   >= 0 ) && (downIndexes   < rowDown.NHits()   )) ) == dnMask );
      int_v upFromDown;
      for( unsigned int i = 0; i < float_v::Size; i++ ) {
      	if( !dnMask[i] ) continue;
      	upFromDown[i] = data.HitLinkUpData( rowDown )[(unsigned int)downIndexes[i]];
      }
#ifdef V6	// Triplet saver
      if( it == 0 ) {
        int_m trs_mask( upMask && dnMask && int_m( up >= 0 && downFromUp < 0 ) && int_m( dn >= 0 && upFromDown < 0 ) );
        for( unsigned int i = 0; i < float_v::Size; i++ ) {
	  if( !trs_mask[i] ) continue;
	  save_up_links.push_back( hit_link( rowIndex-rowStep, dn[i], hitIndex+i ) );
	  save_up_links.push_back( hit_link( rowIndex, hitIndex+i, up[i] ) );
        }
      }
#endif
        // -- make clean --

           // check if some one-way links can be good
#define USE_EDGE_HITS // use edge links, which are not reciprocall
#ifdef USE_EDGE_HITS
      upMask &= (downFromUp == -1) && (upFromDown == static_cast<int_v>(hitIndexes));  // have mutual link only downwards. is   up-link good?
      dnMask &= (upFromDown == -1) && (downFromUp == static_cast<int_v>(hitIndexes)); // have mutual link only upwards.   is down-link good?
      
      dnMask &= int_m(rowIndex + 2*rowStep < numberOfRows);  // will use up & upup hits for check. If no one there then have to delete link
      upMask &= int_m(rowIndex - 2*rowStep >= 0);
      upMask &= dn >= int_v( Vc::Zero );
      dnMask &= up >= int_v( Vc::Zero );

        // #define SELECT_EDGE_HITS  // select edge links before use. otherwise all not reciprocall links will be used
#ifdef SELECT_EDGE_HITS
      const int_m upOrDnMask = upMask || dnMask;
      if(!upOrDnMask.isEmpty())  // TODO do we need edge links - investigate
      {
        X = data.RowX( rowIndex );
        assert( ( upOrDnMask && ((hitIndexes   >= 0 ) && (hitIndexes   < row.NHits()   )) ) == upOrDnMask );
        Y.gather( data.HitDataY( row ), hitIndexes, static_cast<float_m>(upOrDnMask) );//HitDataY( row, hitIndex );
        Z.gather( data.HitDataZ( row ), hitIndexes, static_cast<float_m>(upOrDnMask) );// data.HitDataZ( row, hitIndex );

        if(!dnMask.isEmpty())
        {
          Xdown = data.RowX( rowIndex - rowStep );
          assert( ( dnMask && ((downIndexes   >= 0 ) && (downIndexes   < rowDown.NHits()   )) ) == dnMask );
          Ydown.gather( data.HitDataY( rowDown ), downIndexes, static_cast<float_m>(dnMask) );
          Zdown.gather( data.HitDataZ( rowDown ), downIndexes, static_cast<float_m>(dnMask) );
          
          const AliHLTTPCCARow &rowUpUp = data.Row( rowIndex + 2*rowStep );
          assert( ( dnMask && ((upIndexes   >= 0 ) && (upIndexes   < rowUp.NHits()   )) ) == dnMask );
          const int_v upup = int_v(data.HitLinkUpData( rowUp ), upIndexes, dnMask );
          
          dnMask &= upup >= int_v( Vc::Zero ); // can't check, so can't save any other link
//          int_v downFromUpUp = int_v(data.HitLinkDownData( rowUpUp ), static_cast<uint_v>(upupIndexes), dnMask ); 
//          dnMask &= downFromUpUp != -1; 

          X4 = data.RowX( rowIndex + 2*rowStep );
          const uint_v upupIndexes = upup.staticCast<uint_v>();
          ASSERT( ( dnMask && (upupIndexes   < rowUpUp.NHits() ) ) == dnMask,
            " dnMask= " << dnMask << " upupIndexes= " << upupIndexes << " rowUpUp.NHits()= "<< rowUpUp.NHits() );
          Y4.gather( data.HitDataY( rowUpUp ), upupIndexes, static_cast<float_m>(dnMask) );
          Z4.gather( data.HitDataZ( rowUpUp ), upupIndexes, static_cast<float_m>(dnMask) );

          iX = Vc::One/(X - X4);
          Yx1 = (Y - Y4)*iX;
          Yx2 = Y - X*Yx1;
          Yxx2 = Yx2 + Yx1*Xdown;
          Zx1 = (Z - Z4)*iX;
          Zx2 = Z - X*Zx1;
          Zxx2 = Zx2 + Zx1*Xdown;

          float_v err2Y, err2Z;
          param.GetClusterErrors2(upIndexes,Xdown,Ydown,Zdown,err2Y, err2Z);

 //         float_v ch = CAMath::Abs((Yxx2 - Ydown)/CAMath::Sqrt(err2Y))+CAMath::Abs((Zxx2 - Zdown)/CAMath::Sqrt(err2Z));
 //         upMask &= static_cast<int_m>(ch < 50.f);
          dnMask &= static_cast<int_m>(CAMath::Abs((Yxx2 - Ydown)*CAMath::RSqrt(err2Y)) < 50.f);
//          dnMask &= static_cast<int_m>(CAMath::Abs((Zxx2 - Zdown)/CAMath::Sqrt(err2Z)) < 80.f);
        }

        if(!upMask.isEmpty())
        {
          Xup = data.RowX( rowIndex + rowStep );
          assert( ( upMask && ((upIndexes   >= 0 ) && (upIndexes   < rowUp.NHits()   )) ) == upMask );
          Yup.gather( data.HitDataY( rowUp ), upIndexes, static_cast<float_m>(upMask) );
          Zup.gather( data.HitDataZ( rowUp ), upIndexes, static_cast<float_m>(upMask) );
          
          const AliHLTTPCCARow &rowDownDown = data.Row( rowIndex - 2*rowStep );
          assert( ( upMask && ((downIndexes   >= 0 ) && (downIndexes   < rowDown.NHits()   )) ) == upMask );
          const int_v downdown = int_v(data.HitLinkDownData( rowDown ), downIndexes, upMask );
          upMask &= downdown >= int_v( Vc::Zero ); // can't check, so can't save any other link
//          int_v upFromDownDown = int_v(data.HitLinkUpData( rowDownDown ), static_cast<uint_v>(downdownIndexes), upMask ); 
//          upMask &= upFromDownDown != -1;
          
          ASSERT( ( upMask && (downdown   >= 0 ) && (downdown   < rowDownDown.NHits()) ) == upMask,
                   " upMask= " << upMask << " dn= " << dn <<  " downdown= "  << downdown << " row= " << rowIndex << " rowDownDown.NHits= " << rowDownDown.NHits());
          X4 = data.RowX( rowIndex - 2*rowStep );
          const uint_v downdownIndexes = downdown.staticCast<uint_v>();
          Y4.gather( data.HitDataY( rowDownDown ), downdownIndexes, static_cast<float_m>(upMask) );
          Z4.gather( data.HitDataZ( rowDownDown ), downdownIndexes, static_cast<float_m>(upMask) );

          iX = Vc::One/(X - X4);
          Yx1 = (Y - Y4)*iX;
          Yx2 = Y - X*Yx1;
          Yxx2 = Yx2 + Yx1*Xup;
          Zx1 = (Z - Z4)*iX;
          Zx2 = Z - X*Zx1;
          Zxx2 = Zx2 + Zx1*Xup;

          float_v err2Y, err2Z;
          param.GetClusterErrors2(upIndexes,Xup,Yup,Zup,err2Y, err2Z);

          const float_v ch = CAMath::Abs((Yxx2 - Yup)*CAMath::RSqrt(err2Y));
          upMask &= static_cast<int_m>(ch < 50.f);
//          upMask &= static_cast<int_m>(CAMath::Abs((Yxx2 - Yup)/CAMath::Sqrt(err2Y)) < 50.f);
//          upMask &= static_cast<int_m>(CAMath::Abs((Zxx2 - Zup)/CAMath::Sqrt(err2Z)) < 80.f);
//          std::cout << upMask << "     "<<std::endl;

          // Zx1 = (Zdown - Z4)/(Xdown - X4);
          // Zx2 = (Z - Zdown)/(X - Xdown);
          // Zxx2 = (Zx1 - Zx2)*2*iX;
        }
      } // if(!dnMask.isEmpty() || !upMask.isEmpty()) 
#else // SELECT_EDGE_HITS
      UNUSED_PARAM1( param );
#endif // SELECT_EDGE_HITS

      assert( ( upMask && ((upIndexes   >= 0 ) && (upIndexes   < rowUp.NHits()   )) ) == upMask );
      assert( ( dnMask && ((downIndexes >= 0 ) && (downIndexes < rowDown.NHits() )) ) == dnMask );      
        // make from good one-way links mutual links
      downFromUp( upMask ) = static_cast<int_v>(hitIndexes);
      upFromDown( dnMask ) = static_cast<int_v>(hitIndexes);
      data.SetHitLinkDownData( rowUp, upIndexes, downFromUp, upMask );
      data.SetHitLinkUpData( rowDown, downIndexes, upFromDown, dnMask );
      // data.SetHitLinkDownData( rowUp, upIndexes, static_cast<int_v>(hitIndexes), upMask );
      // data.SetHitLinkUpData( rowDown, downIndexes, static_cast<int_v>(hitIndexes), dnMask );
#endif // USE_EDGE_HITS
        // delete one-way links (all other one-way links)
      const int_m badUpMask = validHitsMask && (downFromUp != static_cast<int_v>(hitIndexes));
      VALGRIND_CHECK_VALUE_IS_DEFINED( up );
      assert( ( badUpMask && ((hitIndexes   >= 0 ) && (hitIndexes   < row.NHits()   )) )  == badUpMask );
      data.SetHitLinkUpData( row, hitIndexes, minusOne, badUpMask );

      const int_m badDnMask = validHitsMask && (upFromDown != static_cast<int_v>(hitIndexes));
      VALGRIND_CHECK_VALUE_IS_DEFINED( dn );
      assert( ( badDnMask && ((hitIndexes   >= 0 ) && (hitIndexes   < row.NHits()   )) )  == badDnMask );
      data.SetHitLinkDownData( row, hitIndexes, minusOne, badDnMask );
    } // for iHit
  } // for iRow
}

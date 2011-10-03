// @(#) $Id: AliHLTTPCCANeighboursCleaner.cxx,v 1.1.1.1 2010/07/26 20:55:38 ikulakov Exp $
// **************************************************************************
// This file is property of and copyright by the ALICE HLT Project          *
// ALICE Experiment at CERN, All rights reserved.                           *
//                                                                          *
// Primary Authors: Sergey Gorbunov <sergey.gorbunov@kip.uni-heidelberg.de> *
//                  Ivan Kisel <kisel@kip.uni-heidelberg.de>                *
//                  for The ALICE HLT Project.                              *
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


#include "AliHLTTPCCANeighboursCleaner.h"
#include "AliHLTTPCCAMath.h"
#include "AliHLTTPCCATracker.h"

#include <valgrind/memcheck.h>

// *
// * kill link to the neighbour if the neighbour is not pointed to the hit
// *

void AliHLTTPCCANeighboursCleaner::run( const int numberOfRows, SliceData &data, const AliHLTTPCCAParam &param )
{
  sfloat_v X,Y,Z,Xup,Yup,Zup,Xdown,Ydown,Zdown, X4,Y4,Z4;
  sfloat_v Yx1, Yx2, Yxx1, Yxx2, Yxxx, Zx1, Zx2, Zxx1, Zxx2, Zxxx;

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
    for ( int hitIndex = 0; hitIndex < numberOfHits; hitIndex += short_v::Size ) {
      const short_v hitIndexes = short_v( Vc::IndexesFromZero ) + hitIndex;
      const short_m &validHitsMask = hitIndexes < numberOfHits;

        // collect information
        // up part
      short_v up = data.HitLinkUpData( row, hitIndex );
      VALGRIND_CHECK_VALUE_IS_DEFINED( up );
      ushort_v upIndexes = CAMath::Max( up, short_v( Vc::Zero ) ).staticCast<unsigned short>();
      short_m upMask = validHitsMask && up >= short_v( Vc::Zero );
      short_v downFromUp = data.HitLinkDownData( rowUp, upIndexes );
        // down part
      short_v dn = data.HitLinkDownData( row, hitIndex );
      VALGRIND_CHECK_VALUE_IS_DEFINED( dn );
      ushort_v downIndexes = CAMath::Max( dn, short_v( Vc::Zero ) ).staticCast<unsigned short>();
      short_m dnMask = validHitsMask && dn >= short_v( Vc::Zero );
      short_v upFromDown = data.HitLinkUpData( rowDown, downIndexes );
      
        // make clean
           // check if some one-way links can be good


      short_v downFromUp0 = downFromUp;
//       std::cout << "downFromUp " << downFromUp   << " upFromDown " << upFromDown << " validHitsMask " << validHitsMask << " dnMask " << dnMask << " upMask " << upMask << std::endl; // IKu debug
      upMask &= downFromUp == -1 && upFromDown == hitIndexes;
      dnMask &= upFromDown == -1 && downFromUp0 == hitIndexes;

///mvz start 14.07.2010
      if(!dnMask.isEmpty() || !upMask.isEmpty())
      {
        X = data.RowX( rowIndex );
        Y.gather( data.HitDataY( row ), static_cast<ushort_v>(hitIndexes) );//HitDataY( row, hitIndex );
        Z.gather( data.HitDataZ( row ), static_cast<ushort_v>(hitIndexes) );// data.HitDataZ( row, hitIndex );

        Xup = data.RowX( rowIndex + rowStep );
        Yup.gather( data.HitDataY( rowUp ), static_cast<ushort_v>(upIndexes) );
        Zup.gather( data.HitDataZ( rowUp ), static_cast<ushort_v>(upIndexes) );

        Xdown = data.RowX( rowIndex - rowStep );
        Ydown.gather( data.HitDataY( rowDown ), static_cast<ushort_v>(downIndexes) );
        Zdown.gather( data.HitDataZ( rowDown ), static_cast<ushort_v>(downIndexes) );

/*        Yx1 = (Yup - Y)/(Xup - X);
        Yx2 = (Y - Ydown)/(X - Xdown);
        Yxx1 = (Yx1 - Yx2)*2/(Xup - Xdown);

        Zx1 = (Zup - Y)/(Xup - X);
        Zx2 = (Y - Ydown)/(X - Xdown);
        Zxx1 = (Zx1 - Zx2)*2/(Xup - Xdown);*/

        if(!dnMask.isEmpty())
        {
          const AliHLTTPCCARow &rowUpUp = data.Row( rowIndex + 2*rowStep );
          short_v upup = data.HitLinkUpData( rowUp, upIndexes );
          ushort_v upupIndexes = CAMath::Max( upup, short_v( Vc::Zero ) ).staticCast<unsigned short>();
          short_v downFromUpUp(Vc::Zero);
          downFromUpUp(dnMask) = data.HitLinkDownData( rowUpUp, upupIndexes );
//          dnMask &= downFromUpUp != -1; 

          X4 = data.RowX( rowIndex + 2*rowStep );
          Y4.gather( data.HitDataY( rowUpUp ), static_cast<ushort_v>(upupIndexes) );
          Z4.gather( data.HitDataZ( rowUpUp ), static_cast<ushort_v>(upupIndexes) );

          Yx1 = (Y - Y4)/(X - X4);
          Yx2 = Y - X*Yx1;
          Yxx2 = Yx2 + Yx1*Xdown;
          Zx1 = (Z - Z4)/(X - X4);
          Zx2 = Z - X*Zx1;
          Zxx2 = Zx2 + Zx1*Xdown;

          sfloat_v err2Y, err2Z;
          param.GetClusterErrors2(upIndexes,Xdown,Ydown,Zdown,err2Y, err2Z);

          sfloat_v ch = CAMath::Abs((Yxx2 - Ydown)/CAMath::Sqrt(err2Y))+CAMath::Abs((Zxx2 - Zdown)/CAMath::Sqrt(err2Z));
 //         upMask &= static_cast<short_m>(ch < 50.f);
          dnMask &= static_cast<short_m>(CAMath::Abs((Yxx2 - Ydown)/CAMath::Sqrt(err2Y)) < 50.f);
//          dnMask &= static_cast<short_m>(CAMath::Abs((Zxx2 - Zdown)/CAMath::Sqrt(err2Z)) < 80.f);
        }
        if(!upMask.isEmpty())
        {
          const AliHLTTPCCARow &rowDownDown = data.Row( rowIndex - 2*rowStep );
          short_v downdown = data.HitLinkDownData( rowDown, downIndexes );
          ushort_v downdownIndexes = CAMath::Max( downdown, short_v( Vc::Zero ) ).staticCast<unsigned short>();
          short_v upFromDownDown(Vc::Zero);
          upFromDownDown(upMask) = data.HitLinkUpData( rowDownDown, downdownIndexes );
//          upMask &= upFromDownDown != -1; 

          X4 = data.RowX( rowIndex - 2*rowStep );
          Y4.gather( data.HitDataY( rowDownDown ), static_cast<ushort_v>(downdownIndexes) );
          Z4.gather( data.HitDataZ( rowDownDown ), static_cast<ushort_v>(downdownIndexes) );

          Yx1 = (Y - Y4)/(X - X4);
          Yx2 = Y - X*Yx1;
          Yxx2 = Yx2 + Yx1*Xup;
          Zx1 = (Z - Z4)/(X - X4);
          Zx2 = Z - X*Zx1;
          Zxx2 = Zx2 + Zx1*Xup;

          sfloat_v err2Y, err2Z;
          param.GetClusterErrors2(upIndexes,Xup,Yup,Zup,err2Y, err2Z);

          //std::cout << upMask << "     "<<CAMath::Abs((Yxx2 - Yup)/CAMath::Sqrt(err2Y))<< "   " << (CAMath::Abs((Yxx2 - Yup)/CAMath::Sqrt(err2Y))+CAMath::Abs((Zxx2 - Zup)/CAMath::Sqrt(err2Z)))<<std::endl;
          sfloat_v ch = CAMath::Abs((Yxx2 - Yup)/CAMath::Sqrt(err2Y));
          upMask &= static_cast<short_m>(ch < 50.f);
//          upMask &= static_cast<short_m>(CAMath::Abs((Yxx2 - Yup)/CAMath::Sqrt(err2Y)) < 50.f);
//          upMask &= static_cast<short_m>(CAMath::Abs((Zxx2 - Zup)/CAMath::Sqrt(err2Z)) < 80.f);
//          std::cout << upMask << "     "<<std::endl;

          Zx1 = (Zdown - Z4)/(Xdown - X4);
          Zx2 = (Z - Zdown)/(X - Xdown);
          Zxx2 = (Zx1 - Zx2)*2/(X - X4);
        }
      }

///mvz end 14.07.2010

      downFromUp( upMask ) = hitIndexes;
      upFromDown( dnMask ) = hitIndexes;
      data.SetHitLinkDownData( rowUp, upIndexes, downFromUp, upMask );
      data.SetHitLinkUpData( rowDown, downIndexes, upFromDown, dnMask );
//       std::cout << " upIndexes " <<  upIndexes << " downIndexes " <<  downIndexes << std::endl;
//       std::cout << "downFromUp " << downFromUp   << " upFromDown " << upFromDown << std::endl; // IKu debug
      downFromUp = data.HitLinkDownData( rowUp, upIndexes );
      upFromDown = data.HitLinkUpData( rowDown, downIndexes );


//       std::cout << " downFromUp " << downFromUp
//           << " upFromDown " << upFromDown
//          << " dnMask " << dnMask << " upMask " << upMask  << std::endl; // IKu debug
           // delete else one-way links
      up( validHitsMask && downFromUp != hitIndexes ) = -1;
      VALGRIND_CHECK_VALUE_IS_DEFINED( up );
      data.SetHitLinkUpData( row, hitIndex, up );

      dn( validHitsMask && upFromDown != hitIndexes ) = -1;
      VALGRIND_CHECK_VALUE_IS_DEFINED( dn );
      data.SetHitLinkDownData( row, hitIndex, dn );
    }
  }
}

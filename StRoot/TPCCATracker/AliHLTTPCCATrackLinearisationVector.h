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


#ifndef ALIHLTTPCCATRACKLINEARISATIONVECTOR_H
#define ALIHLTTPCCATRACKLINEARISATIONVECTOR_H

#include "AliHLTTPCCATrackParamVector.h"
#include "AliHLTTPCCADef.h"

/**
 * @class AliHLTTPCCATrackLinearisationVector
 *
 * AliHLTTPCCATrackLinearisationVector class describes the parameters which are used
 * to linearise the transport equations for the track trajectory.
 *
 * The class is used during track (re)fit, when the AliHLTTPCTrackParam track is only
 * partially fitted, and there is some apriory knowledge about trajectory.
 * This apriory knowledge is used to linearise the transport equations.
 *
 * In case the track is fully fitted, the best linearisation point is
 * the track trajectory itself (AliHLTTPCCATrackLinearisationVector = AliHLTTPCTrackParamVector ).
 *
 */
class AliHLTTPCCATrackLinearisationVector
{
  public:

    AliHLTTPCCATrackLinearisationVector()
        : fSinPhi( Vc::Zero ), fCosPhi( 1 ), fDzDs( Vc::Zero ), fQPt( Vc::Zero ) {}

    AliHLTTPCCATrackLinearisationVector( float_v SinPhi1, float_v CosPhi1, float_v DzDs1, float_v QPt1 )
        : fSinPhi( SinPhi1 ), fCosPhi( CosPhi1 ), fDzDs( DzDs1 ), fQPt( QPt1 ) {}

    AliHLTTPCCATrackLinearisationVector( const AliHLTTPCCATrackParamVector &t );

    void Set( float_v SinPhi1, float_v CosPhi1, float_v DzDs1, float_v QPt1 );


    float_v SinPhi()const { return fSinPhi; }
    float_v CosPhi()const { return fCosPhi; }
    float_v DzDs()  const { return fDzDs; }
    float_v QPt()   const { return fQPt; }

    float_v GetSinPhi()const { return fSinPhi; }
    float_v GetCosPhi()const { return fCosPhi; }
    float_v GetDzDs()  const { return fDzDs; }
    float_v GetQPt()   const { return fQPt; }

    void SetSinPhi( float_v v ) {  fSinPhi = v; }
    void SetCosPhi( float_v v ) {  fCosPhi = v; }
    void SetDzDs( float_v v )  {  fDzDs   = v; }
    void SetQPt( float_v v )   {  fQPt = v; }

  private:

    float_v fSinPhi; // SinPhi
    float_v fCosPhi; // CosPhi
    float_v fDzDs;   // DzDs
    float_v fQPt;    // QPt
};


inline AliHLTTPCCATrackLinearisationVector::AliHLTTPCCATrackLinearisationVector( const AliHLTTPCCATrackParamVector &t )
    : fSinPhi( t.SinPhi() ), fCosPhi( Vc::Zero ), fDzDs( t.DzDs() ), fQPt( t.QPt() )
{
  fSinPhi = CAMath::Max( CAMath::Min( fSinPhi, float_v( .999f ) ), float_v( -.999f ) );
  fCosPhi = t.SignCosPhi() * CAMath::Sqrt( float_v( Vc::One ) - fSinPhi * fSinPhi );
}


inline void AliHLTTPCCATrackLinearisationVector::Set( float_v SinPhi1, float_v CosPhi1,
    float_v DzDs1, float_v QPt1 )
{
  SetSinPhi( SinPhi1 );
  SetCosPhi( CosPhi1 );
  SetDzDs( DzDs1 );
  SetQPt( QPt1 );
}

#endif

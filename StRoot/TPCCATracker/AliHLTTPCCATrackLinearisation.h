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


#ifndef ALIHLTTPCCATRACKLINEARISATION_H
#define ALIHLTTPCCATRACKLINEARISATION_H

#include "AliHLTTPCCATrackParam.h"


/**
 * @class AliHLTTPCCATrackLinearisation
 *
 * AliHLTTPCCATrackLinearisation class describes the parameters which are used
 * to linearise the transport equations for the track trajectory.
 *
 * The class is used during track (re)fit, when the AliHLTTPCTrackParam track is only
 * partially fitted, and there is some apriory knowledge about trajectory.
 * This apriory knowledge is used to linearise the transport equations.
 *
 * In case the track is fully fitted, the best linearisation point is
 * the track trajectory itself (AliHLTTPCCATrackLinearisation = AliHLTTPCTrackParam ).
 *
 */
class AliHLTTPCCATrackLinearisation
{
  public:

    AliHLTTPCCATrackLinearisation()
        : fSinPhi( 0 ), fCosPhi( 1 ), fDzDs( 0 ), fQPt( 0 ) {}

    AliHLTTPCCATrackLinearisation( float SinPhi1, float CosPhi1, float DzDs1, float QPt1 )
        : fSinPhi( SinPhi1 ), fCosPhi( CosPhi1 ), fDzDs( DzDs1 ), fQPt( QPt1 ) {}

    AliHLTTPCCATrackLinearisation( const AliHLTTPCCATrackParam &t );

    void Set( float SinPhi1, float CosPhi1, float DzDs1, float QPt1 );


    float SinPhi()const { return fSinPhi; }
    float CosPhi()const { return fCosPhi; }
    float DzDs()  const { return fDzDs; }
    float QPt()   const { return fQPt; }

    float GetSinPhi()const { return fSinPhi; }
    float GetCosPhi()const { return fCosPhi; }
    float GetDzDs()  const { return fDzDs; }
    float GetQPt()   const { return fQPt; }

    void SetSinPhi( float v ) {  fSinPhi = v; }
    void SetCosPhi( float v ) {  fCosPhi = v; }
    void SetDzDs( float v )  {  fDzDs   = v; }
    void SetQPt( float v )   {  fQPt = v; }

  private:

    float fSinPhi; // SinPhi
    float fCosPhi; // CosPhi
    float fDzDs;   // DzDs
    float fQPt;    // QPt
};


inline AliHLTTPCCATrackLinearisation::AliHLTTPCCATrackLinearisation( const AliHLTTPCCATrackParam &t )
    : fSinPhi( CAMath::Min( .999f, CAMath::Max( -.999f, t.SinPhi() ) ) ),
    fCosPhi( t.SignCosPhi() * CAMath::Sqrt( 1.f - fSinPhi * fSinPhi ) ),
    fDzDs( t.DzDs() ), fQPt( t.QPt() )
{
}


inline void AliHLTTPCCATrackLinearisation::Set( float SinPhi1, float CosPhi1,
    float DzDs1, float QPt1 )
{
  SetSinPhi( SinPhi1 );
  SetCosPhi( CosPhi1 );
  SetDzDs( DzDs1 );
  SetQPt( QPt1 );
}

#endif

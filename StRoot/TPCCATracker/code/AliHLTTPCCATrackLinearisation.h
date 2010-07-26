// ************************************************************************
// This file is property of and copyright by the ALICE HLT Project        *
// ALICE Experiment at CERN, All rights reserved.                         *
// See cxx source for full Copyright notice                               *
//                                                                        *
//*************************************************************************


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

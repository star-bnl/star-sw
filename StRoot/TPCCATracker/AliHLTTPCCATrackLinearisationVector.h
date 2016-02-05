// ************************************************************************
// This file is property of and copyright by the ALICE HLT Project        *
// ALICE Experiment at CERN, All rights reserved.                         *
// See cxx source for full Copyright notice                               *
//                                                                        *
//*************************************************************************


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

    AliHLTTPCCATrackLinearisationVector( sfloat_v SinPhi1, sfloat_v CosPhi1, sfloat_v DzDs1, sfloat_v QPt1 )
        : fSinPhi( SinPhi1 ), fCosPhi( CosPhi1 ), fDzDs( DzDs1 ), fQPt( QPt1 ) {}

    AliHLTTPCCATrackLinearisationVector( const AliHLTTPCCATrackParamVector &t );

    void Set( sfloat_v SinPhi1, sfloat_v CosPhi1, sfloat_v DzDs1, sfloat_v QPt1 );


    sfloat_v SinPhi()const { return fSinPhi; }
    sfloat_v CosPhi()const { return fCosPhi; }
    sfloat_v DzDs()  const { return fDzDs; }
    sfloat_v QPt()   const { return fQPt; }

    sfloat_v GetSinPhi()const { return fSinPhi; }
    sfloat_v GetCosPhi()const { return fCosPhi; }
    sfloat_v GetDzDs()  const { return fDzDs; }
    sfloat_v GetQPt()   const { return fQPt; }

    void SetSinPhi( sfloat_v v ) {  fSinPhi = v; }
    void SetCosPhi( sfloat_v v ) {  fCosPhi = v; }
    void SetDzDs( sfloat_v v )  {  fDzDs   = v; }
    void SetQPt( sfloat_v v )   {  fQPt = v; }

  private:

    sfloat_v fSinPhi; // SinPhi
    sfloat_v fCosPhi; // CosPhi
    sfloat_v fDzDs;   // DzDs
    sfloat_v fQPt;    // QPt
};


inline AliHLTTPCCATrackLinearisationVector::AliHLTTPCCATrackLinearisationVector( const AliHLTTPCCATrackParamVector &t )
    : fSinPhi( t.SinPhi() ), fCosPhi( Vc::Zero ), fDzDs( t.DzDs() ), fQPt( t.QPt() )
{
  fSinPhi = CAMath::Max( CAMath::Min( fSinPhi, sfloat_v( .999f ) ), sfloat_v( -.999f ) );
  fCosPhi = t.SignCosPhi() * CAMath::Sqrt( sfloat_v( Vc::One ) - fSinPhi * fSinPhi );
}


inline void AliHLTTPCCATrackLinearisationVector::Set( sfloat_v SinPhi1, sfloat_v CosPhi1,
    sfloat_v DzDs1, sfloat_v QPt1 )
{
  SetSinPhi( SinPhi1 );
  SetCosPhi( CosPhi1 );
  SetDzDs( DzDs1 );
  SetQPt( QPt1 );
}

#endif

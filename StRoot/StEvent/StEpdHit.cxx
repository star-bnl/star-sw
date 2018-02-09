/*!
 * \class StEpdHit
 * \author Mike Lisa
 * \date 29 Dec 2017
 * \brief Stores information for tiles in STAR Event Plane Detector
 */
/***************************************************************************
 *
 * $Id: StEpdHit.cxx,v 2.1 2018/02/08 17:35:02 ullrich Exp $
 *
 * Author: Mike Lisa, Jan 2018
 ***************************************************************************
 *
 * Description: see header file
 *
 ***************************************************************************
 *
 * $Log: StEpdHit.cxx,v $
 * Revision 2.1  2018/02/08 17:35:02  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include "StEpdHit.h"

ClassImp(StEpdHit);

StEpdHit::StEpdHit() : StEpdHit(0, 0, 0, 0, 0, 0, false, 0.0, false, 0)
{
  /* no-op */
}

StEpdHit::StEpdHit(int position, int tile,
		   short EW, int ADC, int TAC,
		   int TDC, bool hasTAC, float nMIP,
		   bool statusIsGood, int truthId) :
  mId( (100*position + tile)*EW ),
  mQTdata( (ADC & 0x0FFF) | (TAC & 0x0FFF) << 12 | (TDC & 0x001F) << 24 | hasTAC << 29 | statusIsGood << 30 ),
  mnMIP(nMIP),
  mTruthId(truthId)
{
  /* no-op */
}



// $Id: EEmcTTMatch.cxx,v 1.4 2004/05/05 22:11:08 zolnie Exp $
/*!
 *                                                                     
 * \class  EEmcTTMatch

 * \brief  EEmcTTMatch class contains results of TPC track to EEMC tower matching
 *
 * The contents of a EEmcTTMatch is a pointer to EEmcTower object and a list of StMuTrack 
 * objects that 
 * fulfilled certain matching criteria. 
 * 
 * \author Piotr A. Zolnierczuk
 * \date   2004/04/30
 *
 *
 * \section eemcttmachexample Example 
  \verbatim
  // assume that tmatch is of type EEmcTTMatch* 
  EEmcTower *tower =tmatch->Tower(); 
  StMuTrack *track =NULL;
  TIter nextTrack(tmatch->Tracks());
  while((track=(StMuTrack *)nextTrack())) { 
    // do something with tracks
  } 
  \endverbatim
 */                                                                      


#include <ostream>

#include "TList.h"

#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"

#include "EEmcTower.h"
#include "EEmcTTMatch.h"

#if !defined(ST_NO_NAMESPACES)
using std::ostream;
#endif


ClassImp(EEmcTTMatch);


EEmcTTMatch::EEmcTTMatch()
{
	mTower  = NULL;
	mTracks = new TList();
	mNTracks= 0;
}

EEmcTTMatch::~EEmcTTMatch()
{
	delete mTracks;
}

void
EEmcTTMatch::Clear(Option_t *opt)
{
	mTower = NULL;
	mTracks->Clear();
	mNTracks=0;
}


void 
EEmcTTMatch::Add(EEmcTower *t) 
{ 
  mTower = t; 
}

void 
EEmcTTMatch::Add(StMuTrack *t) 
{
  mTracks->Add(t);
  mNTracks++;  // do not trust ROOT :)
}


//_____________________________________________________________________________
//! given track and position z return TVector3 with a 
/// \param track a const pointer to current track
/// \param z     a z (along the beam) position where 
/// \param r     a TVector (returned)
/// \return boolean indicating if track crosses a plane
Bool_t
EEmcTTMatch::ExtrapolateToZ(const StMuTrack *track, const double   z, TVector3 &r)
{
  const double kMinDipAngle   = 1.0e-13;

  StPhysicalHelixD   helix  = track->helix();
  double             dipAng = helix.dipAngle();
  double             z0     = helix.origin().z();
  if(dipAng<kMinDipAngle) return kFALSE; 
  double s  = ( z - z0 ) / sin(dipAng)  ;
  StThreeVectorD hit = helix.at(s);
  r.SetXYZ(hit.x(),hit.y(),hit.z());
  return   kTRUE;
}



ostream& 
EEmcTTMatch::Out(ostream &out ) const
{
  out << *mTower ;
  StMuTrack *track;
  TIter nextTrack(mTracks);
  while((track=(StMuTrack *)nextTrack()))  out << *track;
  return out;
}




// ================================================================================================
ostream&  operator<<(ostream &out, const StMuTrack    &t  )  {
  out << "<StMuTrack";
  out << " ORIGIN=\""   << t.helix().origin() << "\"";
  out << " MOMENTUM=\"" << t.momentum()   << "\"";
  out << "/>\n"; 
  return out;
}
// ================================================================================================
ostream&  operator<<(ostream &out, const EEmcTTMatch  &m  )  {
  return m.Out(out);
}

#include <iostream>

#include "TList.h"

//#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
//#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
//#include "StMuDSTMaker/COMMON/StMuEmcCollection.h"


#include "EEmcTTMatch.h"

#if !defined(ST_NO_NAMESPACES)
using std::map;
using std::ostream;
#endif

ClassImp(EEmcTower);

//=============================================================================
EEmcTower::EEmcTower(const char* label, float ene)
{
	ParseLabel(label);
	mEdep = ene;
}


void 
EEmcTower::ParseLabel(const char *label)
{
	//const int kMaxLabelLen = 16;
	if(label==NULL) return;
	//int len = strnlen(label,kMaxLabelLen);
	// TO BE continued
	assert(0); // not yet implemented
}

void
EEmcTower::WriteLabel()
{
	// FIXME
	const int kMaxLabelLen = 16;
	if(mLabel) delete mLabel;
	mLabel = new char[kMaxLabelLen];
	sprintf(mLabel,"%02dT%1c%02d",SecLabel(),SubSecLabel(),EtaLabel());
}

  
//=============================================================================
ostream& 
EEmcTower::Out(ostream &out ) const
{
  out << "<EEmcTower TOWER=" << mLabel << " EDEP=" << mEdep << " />\n";
  return out;
}


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
static Bool_t
EEmcTTMatch::ExtrapolateToZ(const StMuTrack *track, const double   z, TVector3 &r)
{
  const double kMinDipAngle   = 1.0e-13;
  //const float kMinCurvature =  1e+00;

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
ostream&  operator<<(ostream &out, const EEmcTower    &t  )  {
  return t.Out(out);
}
// ================================================================================================
ostream&  operator<<(ostream &out, const EEmcTTMatch  &m  )  {
  return m.Out(out);
}

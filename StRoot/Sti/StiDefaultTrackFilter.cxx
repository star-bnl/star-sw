#include "StiDefaultTrackFilter.h"

#include <stdexcept>
#include <string>
using std::string;
#include "Sti/Base/Parameter.h"
#include "Sti/Base/EditableParameter.h"
#include "Sti/StiTrack.h"
#include "StiDefaultTrackFilter.h"

StiDefaultTrackFilter::StiDefaultTrackFilter()
  : EditableFilter<StiTrack>()
{}

StiDefaultTrackFilter::StiDefaultTrackFilter(const string & name, const string & description)
  : EditableFilter<StiTrack>(name,description)
{}


StiDefaultTrackFilter::~StiDefaultTrackFilter()
{}

void StiDefaultTrackFilter::initialize()
{
  cout << "StiDefaultTrackFilter::initialize() - INFO - Starting" << endl;
  parameterVector.clear();
  add("Chi2Used", "Use Chi2",     false, false, 0,1,1,Parameter::Boolean, StiTrack::kChi2);
  add("Chi2Min",  "Minimum Chi2", 0., 0., 0., 100.,1,Parameter::Double, StiTrack::kChi2);
  add("Chi2Max",  "Maximum Chi2", 20., 20., 0., 100.,1,Parameter::Double, StiTrack::kChi2);
  
  add("PhiUsed",  "Use Phi",     false, false, 0,1,1,Parameter::Boolean, StiTrack::kPhi);
  add("PhiMin",   "Minimum Phi", -3.2,    -3.2, -3.2, 3.2, 2,Parameter::Double, StiTrack::kPhi);
  add("PhiMax",   "Maximum Phi",  3.2,     3.2, -3.2, 3.2, 2,Parameter::Double, StiTrack::kPhi);
  
  add("PtUsed",   "Use Pt",     true, true, 0,1,1,Parameter::Boolean, StiTrack::kPt);
  add("PtMin",    "Minimum Pt", 0.1, 0.1, 0., 100.,2,Parameter::Double, StiTrack::kPt);
  add("PtMax",    "Maximum Pt", 10., 10., 0., 100.,2,Parameter::Double, StiTrack::kPt);
  
  add("PUsed",    "Use P",     false, false, 0,1,1,Parameter::Boolean, StiTrack::kP);
  add("PMin",     "Minimum P", 0., 0., 0., 100.,2,Parameter::Double, StiTrack::kP);
  add("PMax",     "Maximum P", 10., 10., 0., 100.,2,Parameter::Double, StiTrack::kP);
  
  add("EtaUsed",  "Use Eta",     true, true, 0,1,1,Parameter::Boolean, StiTrack::kPseudoRapidity);
  add("EtaMin",   "Minimum Eta", -0.5, -0.5, -10., 10.,2,Parameter::Double, StiTrack::kPseudoRapidity);
  add("EtaMax",   "Maximum Eta",  0.5,  0.5, -10., 10.,2,Parameter::Double, StiTrack::kPseudoRapidity);
  
  add("nPtsUsed", "Use nPts",     true, true, 0,1,1,Parameter::Boolean, StiTrack::kPointCount);
  add("nPtsMin",  "Minimum nPts", 10., 10., 0., 100.,1,Parameter::Integer, StiTrack::kPointCount);
  add("nPtsMax",  "Maximum nPts", 60., 60., 0., 100.,1,Parameter::Integer, StiTrack::kPointCount);

  add("nGapsUsed","Use nGaps",     true, true, 0,1,1,Parameter::Boolean, StiTrack::kGapCount);
  add("nGapsMin", "Minimum nGaps", 0., 0., 0., 100.,1,Parameter::Integer, StiTrack::kGapCount);
  add("nGapsMax", "Maximum nGaps", 3., 3., 0., 100.,1,Parameter::Integer, StiTrack::kGapCount);

  add("chargeUsed","Use Charge",     false, false, 0,1,1,Parameter::Boolean, StiTrack::kCharge);
  add("chargeMin", "Minimum Charge", -1., -1., -100.,   100.,1,Parameter::Integer, StiTrack::kCharge);
  add("chargeMax", "Maximum Charge",  1.,  1., -100.,   100.,1,Parameter::Integer, StiTrack::kCharge);


  //add("NToNmaxPtsUsed", "Use NToNmaxPts",false,false,0,1,1,Parameter::Boolean);
  //add("NToNmaxPtsMin","Minimum NToNmaxPts",0.25, 0.25, 0.,1.,1.,Parameter::Double);
  //add("NToNmaxPtsMax","Maximum NToNmaxPts",1.00, 1.00, 0.,1.,1.,Parameter::Double);
}

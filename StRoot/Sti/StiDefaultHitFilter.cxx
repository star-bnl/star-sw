#include "StiDefaultHitFilter.h"

#include <stdexcept>
#include <string>
using std::string;
#include "Sti/Base/Parameter.h"
#include "Sti/Base/EditableParameter.h"
#include "Sti/StiHit.h"
#include "StiDefaultHitFilter.h"

StiDefaultHitFilter::StiDefaultHitFilter()
  : EditableFilter<StiHit>()
{}

StiDefaultHitFilter::StiDefaultHitFilter(const string & name, const string & description)
  : EditableFilter<StiHit>(name,description)
{}


StiDefaultHitFilter::~StiDefaultHitFilter()
{}

void StiDefaultHitFilter::initialize()
{
  cout << "StiDefaultHitFilter::initialize() - INFO - Starting" << endl;
  parameterVector.clear();
  add(new EditableParameter("PhiUsed",  "Use Phi",     false, false, 0,1,1,Parameter::Boolean, StiHit::kPhi));
  add(new EditableParameter("PhiMin",   "Minimum Phi", -3.2,    -3.2, -3.2, 3.2, 2,Parameter::Double, StiHit::kPhi));
  add(new EditableParameter("PhiMax",   "Maximum Phi",  3.2,     3.2, -3.2, 3.2, 2,Parameter::Double, StiHit::kPhi));
  
  add(new EditableParameter("RUsed",    "Use R",     false, false, 0,1,1,Parameter::Boolean, StiHit::kR));
  add(new EditableParameter("RMin",     "Minimum R", 0.,     0., 0., 500.,2,Parameter::Double, StiHit::kR));
  add(new EditableParameter("RMax",     "Maximum R", 200., 200., 0., 500.,2,Parameter::Double, StiHit::kR));

  add(new EditableParameter("zUsed",    "Use z",     false, false, 0,1,1,Parameter::Boolean, StiHit::kZ));
  add(new EditableParameter("zMin",     "Minimum z", 200., 200., -500., 500.,2,Parameter::Double, StiHit::kZ));
  add(new EditableParameter("zMax",     "Maximum z", 200., 200., -500., 500.,2,Parameter::Double, StiHit::kZ));
  
  add(new EditableParameter("EtaUsed",  "Use Eta",     true, true, 0,1,1,Parameter::Boolean, StiHit::kPseudoRapidity));
  add(new EditableParameter("EtaMin",   "Minimum Eta", -0.5, -0.5, -10., 10.,2,Parameter::Double, StiHit::kPseudoRapidity));
  add(new EditableParameter("EtaMax",   "Maximum Eta",  0.5,  0.5, -10., 10.,2,Parameter::Double, StiHit::kPseudoRapidity));
  
}

#include "StiLocalTrackSeedFinderParameters.h"
#include "Sti/Base/EditableParameter.h"

StiLocalTrackSeedFinderParameters::StiLocalTrackSeedFinderParameters()
{
  initialize();
}

StiLocalTrackSeedFinderParameters::StiLocalTrackSeedFinderParameters(const StiLocalTrackSeedFinderParameters & pars)
{
  initialize();
}

StiLocalTrackSeedFinderParameters::~StiLocalTrackSeedFinderParameters()
{}

const StiLocalTrackSeedFinderParameters & StiLocalTrackSeedFinderParameters::operator=(const StiLocalTrackSeedFinderParameters & p)
{
  return *this;
}

void   StiLocalTrackSeedFinderParameters::initialize()
{
  cout << "StiLocalTrackSeedFinderParameters::initialize() -I- Started" << endl;
  _enabled  = true; 
  _editable = true; 
  add(new EditableParameter("DeltaY",    "Delta-Y",           &_deltaY,        5., 0.5, 20., 0.1, 0));
  add(new EditableParameter("DeltaZ",     "Delta-Z",          &_deltaZ,       15., 0.5, 20., 0.1, 0));
  add(new EditableParameter("SeedLength", "Seed Length",      &_seedLength,    2,  2, 6, 1, 0));
  add(new EditableParameter("extraDeltaY","extra-Delta-Y",    &_extrapDeltaY,  1., 0.5, 10., 0.1, 0));
  add(new EditableParameter("extraDeltaZ","extra-Delta-Z",    &_extrapDeltaZ,  2., 0.5, 10., 0.1, 0));
  add(new EditableParameter("MaxSkipped","Max Layers Skipped",&_maxSkipped, 4, 0, 5, 1, 0));
  add(new EditableParameter("ExtrapMinLength","Min Length of Extrapolation", &_extrapMinLength , 2, 1, 10, 1, 0));
  add(new EditableParameter("ExtrapMaxLength","Max Length of Extrapolation", &_extrapMaxLength,  5, 1, 10, 1, 0));
  add(new EditableParameter("UseOrigin","Use Origin in Fit", &_useOrigin, true, 0));
  add(new EditableParameter("DoHelixFit","Do Helix Fit",  &_doHelixFit, true, 0));
  cout << "StiLocalTrackSeedFinderParameters::initialize() -I- Done" << endl;
}

void StiLocalTrackSeedFinderParameters::loadDS(TDataSet&)
{
  cout << "StiLocalTrackSeedFinderParameters::loadDS(TDataSet&) -I- Started" << endl;
  /*
  St_LocalTrackSeedFinderParameters * a = static_cast<St_LocalTrackSeedFinderParameters*>(ds.Find("LocalTrackSeedFinderParameters" ));
  if (!a) throw runtime_error("StiLocalTrackSeedFinderParameters::load(TDataSet&ds) -E- a==0");
  LocalTrackSeedFinderParameters_st * b = a->GetTable();
  if (!b) throw runtime_error("StiLocalTrackSeedFinderParameters::load(TDataSet&ds) -E- b==0"); 
  _deltaY = b->deltaY;
  _deltaZ = b->deltaZ;
  _seedLength = b->seedLength;
  _extrapDeltaY = b->extrapDeltaY;
  _extrapDeltaZ = b->extrapDeltaZ;
  _maxSkipped  = b->maxSkipped;
  _extrapMinLength = b->extrapMinLength;
  _extrapLength = b->extrapMaxLength;
  _useOrigin = b->useOrigin;
  _doHelixFit = b->doHelixFit;
  */
  cout << *this<<endl;
  cout << "StiLocalTrackSeedFinderParameters::loadDS(TDataSet&) -I- Started" << endl;
}

void StiLocalTrackSeedFinderParameters::loadFS(ifstream& inFile)
{
  cout << "StiLocalTrackSeedFinderParameters::loadFS(ifstream& inFile) -I- Started" << endl;
  inFile >> _deltaY
	 >> _deltaZ
	 >> _seedLength
	 >> _extrapDeltaY
	 >> _extrapDeltaZ
	 >> _maxSkipped
	 >> _extrapMinLength
	 >> _extrapMaxLength
	 >> _useOrigin
	 >> _doHelixFit;    
  cout << *this<<endl;
  cout << "StiLocalTrackSeedFinderParameters::loadFS(ifstream& inFile) -I- Started" << endl;
}


ostream& operator<<(ostream& os, const StiLocalTrackSeedFinderParameters& p)
{
  os << p.getName() << endl
     << "            deltaY : " << p._deltaY << endl
     << "            deltaZ : " << p._deltaZ << endl
     << "       _seedLength : " << p._seedLength << endl
     << "     _extrapDeltaY : " << p._extrapDeltaY << endl
     << "     _extrapDeltaZ : " << p._extrapDeltaZ << endl
     << "       _maxSkipped : " << p._maxSkipped << endl
     << "  _extrapMinLength : " << p._extrapMinLength << endl
     << "  _extrapMaxLength : " << p._extrapMaxLength << endl
     << "        _useOrigin : " << p._useOrigin << endl
     << "       _doHelixFit : " << p._doHelixFit << endl << endl;
  return os;
}

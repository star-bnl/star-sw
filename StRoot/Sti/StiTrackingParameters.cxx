/*!
 * $Id: StiTrackingParameters.cxx,v 2.4 2004/01/30 21:29:42 pruneau Exp $  
 *
 * $Log: StiTrackingParameters.cxx,v $
 * Revision 2.4  2004/01/30 21:29:42  pruneau
 * Added load function to load values from db
 *
 * Revision 2.3  2003/10/28 15:55:15  andrewar
 * Added method to set parameters from a txt input file.
 *
 * Revision 2.2  2003/07/30 17:04:20  andrewar
 * Added log and version id bars.
 *
 *
 */


#include "tables/St_TrackingParameters_Table.h"
#include "StiTrackingParameters.h"
#include "Sti/Base/EditableParameter.h"

StiTrackingParameters::StiTrackingParameters()
  : EditableParameters("noName","noName"),
    _minSearchWindow(1.),
    _maxSearchWindow(5.),
    _searchWindowScaling(4.),
    _maxChi2ForSelection(5.)
{
	initialize();
}

StiTrackingParameters::StiTrackingParameters(const string & name,
					     const string & description)
  : EditableParameters(name,description),
    _minSearchWindow(1.),
    _maxSearchWindow(5.),
    _searchWindowScaling(4.),
    _maxChi2ForSelection(5.)
{
  initialize();
}

StiTrackingParameters::StiTrackingParameters(const string & name,
					     const string & description,
					     double minSearchWindow,
					     double maxSearchWindow,
					     double searchWindowScaling,
					     double maxChi2ForSelection)
  : EditableParameters(name,description),
    _minSearchWindow(minSearchWindow),
    _maxSearchWindow(maxSearchWindow),
    _searchWindowScaling(searchWindowScaling),
    _maxChi2ForSelection(maxChi2ForSelection)
{
  initialize();
}

StiTrackingParameters::StiTrackingParameters(const StiTrackingParameters & pars)
  : _minSearchWindow(pars._minSearchWindow),
    _maxSearchWindow(pars._maxSearchWindow),
    _searchWindowScaling(pars._searchWindowScaling),
    _maxChi2ForSelection(pars._maxChi2ForSelection)
{}

StiTrackingParameters::~StiTrackingParameters()
{}

const StiTrackingParameters & StiTrackingParameters::operator=(const StiTrackingParameters & pars)
{
  _minSearchWindow = pars._minSearchWindow;
  _maxSearchWindow = pars._maxSearchWindow;
  _searchWindowScaling = pars._searchWindowScaling;
  _maxChi2ForSelection = pars._maxChi2ForSelection;
  return *this;
}

const StiTrackingParameters & StiTrackingParameters::operator=(const TrackingParameters_st & pars)
{
  _minSearchWindow = pars.minSearch;
  _maxSearchWindow = pars.maxSearch;
  _searchWindowScaling = pars.scaling;
  _maxChi2ForSelection = pars.maxChi2;
  return *this;
}

void StiTrackingParameters::initialize()
{ 
  add(new EditableParameter("MinSearch", "Minimum Search Window",&_minSearchWindow,_minSearchWindow,0.,10.,0.1,0));
  add(new EditableParameter("MaxSearch", "Maximum Search Window",&_maxSearchWindow,_maxSearchWindow,0.,10.,0.1,0));
  add(new EditableParameter("Scaling",   "Search Window Scaling",&_searchWindowScaling,_searchWindowScaling,0.,10.,0.1,0));
  add(new EditableParameter("Max Chi2",  "Max Chi2",&_maxChi2ForSelection,_maxChi2ForSelection,0.,20.,0.1,0));

}

void StiTrackingParameters::setPar(ifstream& inFile)
{
  inFile >> _used;
  inFile >> _active;
  inFile >> _maxChi2ForSelection;
  inFile >> _minSearchWindow;
  inFile >> _maxSearchWindow;
  inFile >> _searchWindowScaling;


  return;
}

void StiTrackingParameters::load(TDataSet *ds)
{
	// validate source
	if (!ds) throw runtime_error("StiTrackingParameters::load(TDataSet * ds)");
	St_TrackingParameters * a = dynamic_cast<St_TrackingParameters*>(ds->Find(getName().c_str() ));
  if (!a) throw runtime_error("StiKalmanTrackFitterParameters::load(TDataSet * ds) -E- a==0");
	TrackingParameters_st * b = a->GetTable();
	if (!b) throw runtime_error("StiKalmanTrackFitterParameters::load(TDataSet * ds) -E- b==0");
	*this = *b;
}


ostream& operator<<(ostream& os, const StiTrackingParameters& par)
{
  return os  <<"StiTracking Parameters - " << par.getName() << endl
       <<"\tActive:\t"                 << par.active() <<endl
       <<"\tMaxChi2ForSelection:\t "   << par.getMaxChi2ForSelection() <<endl
       <<"\tMinSearchWindow:\t "       << par.getMinSearchWindow()     <<endl
       <<"\tMaxSearchWindow:\t "       << par.getMaxSearchWindow()     <<endl
       <<"\tSearchWindowScaling:\t "   << par.getSearchWindowScale() <<endl;
}

/*!
 * $Id: StiTrackingParameters.cxx,v 2.2 2003/07/30 17:04:20 andrewar Exp $  
 *
 * $Log: StiTrackingParameters.cxx,v $
 * Revision 2.2  2003/07/30 17:04:20  andrewar
 * Added log and version id bars.
 *
 *
 */


#include "StiTrackingParameters.h"
#include "Sti/Base/EditableParameter.h"

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

void StiTrackingParameters::initialize()
{ 
  add(new EditableParameter("MinSearch", "Minimum Search Window",&_minSearchWindow,_minSearchWindow,0.,10.,0.1,0));
  add(new EditableParameter("MaxSearch", "Maximum Search Window",&_maxSearchWindow,_maxSearchWindow,0.,10.,0.1,0));
  add(new EditableParameter("Scaling",   "Search Window Scaling",&_searchWindowScaling,_searchWindowScaling,0.,10.,0.1,0));
  add(new EditableParameter("Max Chi2",  "Max Chi2",&_maxChi2ForSelection,_maxChi2ForSelection,0.,20.,0.1,0));

}

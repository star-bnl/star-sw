#include "StiKalmanTrackFitterParameters.h"
#include "Sti/StiToolkit.h" 
#include "Sti/Base/Factory.h" 
#include "Sti/Base/EditableParameter.h" 
 
StiKalmanTrackFitterParameters::StiKalmanTrackFitterParameters() 
  : EditableParameters("KalmanTrackFitterParameters","KalmanTrackFitterParameters") 
{ 
  initialize(); 
} 

StiKalmanTrackFitterParameters::StiKalmanTrackFitterParameters(const string & name, const string & description)
 : EditableParameters(name,description)
{
  initialize(); 
} 
  
StiKalmanTrackFitterParameters::~StiKalmanTrackFitterParameters() 
{}   
 
 
StiKalmanTrackFitterParameters::StiKalmanTrackFitterParameters(const StiKalmanTrackFitterParameters & p)
{} 
 
const StiKalmanTrackFitterParameters & StiKalmanTrackFitterParameters::operator=(const StiKalmanTrackFitterParameters & p) 
{ 
  clear(); 
  _enabled  = p._enabled; 
  _editable = p._editable; 
  return *this; 
} 
 
void StiKalmanTrackFitterParameters::initialize() 
{ 
  _enabled  = true; 
  _editable = true; 
  add( new EditableParameter("maxChi2", "Maximum Chi2", &_maxChi2, 3., 0., 1000., 0.1, 0) );
} 

#include "StiKalmanTrackFitterParameters.h"
#include "Sti/StiToolkit.h" 
#include "Sti/Base/Factory.h" 
#include "Sti/Base/EditableParameter.h" 
#include "tables/St_KalmanTrackFitterParameters_Table.h"
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

const StiKalmanTrackFitterParameters & StiKalmanTrackFitterParameters::operator=(const KalmanTrackFitterParameters_st & p) 
{ 
	_maxChi2 = p.maxChi2;
  return *this; 
} 


 
void StiKalmanTrackFitterParameters::initialize() 
{ 
  _enabled  = true; 
  _editable = true; 
  add( new EditableParameter("maxChi2", "Maximum Chi2", &_maxChi2, 3., 0., 1000., 0.1, 0) );
} 


void StiKalmanTrackFitterParameters::load(TDataSet * ds)
{
	// validate source
	if (!ds) throw runtime_error("StiKalmanTrackFitterParameters::load(TDataSet * ds)");
	// 	
	St_KalmanTrackFitterParameters * a = dynamic_cast<St_KalmanTrackFitterParameters*>(ds->Find(getName().c_str() ));
  if (!a) throw runtime_error("StiKalmanTrackFitterParameters::load(TDataSet * ds) -E- a==0");
	KalmanTrackFitterParameters_st * b = a->GetTable();
	if (!b) throw runtime_error("StiKalmanTrackFitterParameters::load(TDataSet * ds) -E- b==0");
	*this = *b;
}

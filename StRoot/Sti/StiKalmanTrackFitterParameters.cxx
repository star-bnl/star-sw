#include "StiKalmanTrackFitterParameters.h"
#include "Sti/StiToolkit.h" 
#include "Sti/Base/Factory.h" 
#include "Sti/Base/EditableParameter.h" 
#include "tables/St_KalmanTrackFitterParameters_Table.h"

StiKalmanTrackFitterParameters::StiKalmanTrackFitterParameters() 
  : EditableParameters("KalmanTrackFitterParameters","KalmanTrackFitterParameters"),
    _maxChi2(3.)
{ 
  initialize(); 
} 

StiKalmanTrackFitterParameters::StiKalmanTrackFitterParameters(const string & name, const string & description)
  : EditableParameters(name,description),
    _maxChi2(3.)
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
  add( new EditableParameter("maxChi2", "Maximum Chi2", &_maxChi2, _maxChi2, 0., 1000., 0.1, 0) );
} 


void StiKalmanTrackFitterParameters::loadDS(TDataSet&ds)
{
  cout << "StiKalmanTrackFitterParameters::load(TDataSet&ds) -I- Started"<<endl;
  St_KalmanTrackFitterParameters * a = dynamic_cast<St_KalmanTrackFitterParameters*>(ds.Find(getName().c_str() ));
  if (!a) throw runtime_error("StiKalmanTrackFitterParameters::load(TDataSet&ds) -E- a==0");
  KalmanTrackFitterParameters_st * b = a->GetTable();
  if (!b) throw runtime_error("StiKalmanTrackFitterParameters::load(TDataSet&ds) -E- b==0");
  _maxChi2 = b->maxChi2;
  cout << *this;
  cout << "StiKalmanTrackFitterParameters::load(TDataSet&ds) -I- Done"<<endl;
}

void StiKalmanTrackFitterParameters::loadFS(ifstream& inFile)
{
  cout << "StiKalmanTrackFitterParameters::load(ifstream& inFile) -I- Started"<<endl;
  inFile >> _maxChi2;
  cout << *this;
  cout << "StiKalmanTrackFitterParameters::load(ifstream& inFile) -I- Done"<<endl;
}

ostream& operator<<(ostream& os, const StiKalmanTrackFitterParameters& p)
{
  return os << "StiKalmanTrackFitterParameters" 
	    << "    " <<  p.getName() << endl
	    << "   _maxChi2: " << p._maxChi2 << endl << endl;
}


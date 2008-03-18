#include "StiKalmanTrackFinderParameters.h"
#include "tables/St_KalmanTrackFinderParameters_Table.h"
#include "Sti/StiToolkit.h" 
#include "Sti/Base/Factory.h" 
#include "Sti/Base/EditableParameter.h" 
 
StiKalmanTrackFinderParameters::StiKalmanTrackFinderParameters() 
  : EditableParameters("KalmanTrackFinderParameters","KalmanTrackFinderParameters") 
{ 
  initialize(); 
} 
   
StiKalmanTrackFinderParameters::~StiKalmanTrackFinderParameters() 
{}   
 
 
StiKalmanTrackFinderParameters::StiKalmanTrackFinderParameters(const StiKalmanTrackFinderParameters & p) 
{ 
} 
 
const StiKalmanTrackFinderParameters & StiKalmanTrackFinderParameters::operator=(const StiKalmanTrackFinderParameters & p) 
{ 
  clear(); 
  _enabled  = p._enabled; 
  _editable = p._editable; 
  return *this; 
} 


void StiKalmanTrackFinderParameters::initialize() 
{ 
  _enabled  = true; 
  _editable = true; 
  Factory<EditableParameter> * f = StiToolkit::instance()->getParameterFactory(); 
  if (!f) 
    { 
      cout << "StiLocalTrackSeedFinder::initialize() -F- Parameter factory is null" << endl; 
      throw logic_error("StiKalmanTrackFinderParameters::initialize() -F- Parameter factory is null"); 
    } 
  add(f->getInstance()->set("mcsCalculated",       
                            "mcsCalculated",    
                            &mcsCalculated, 
                            true,  
                            0)); 
  add(f->getInstance()->set("field",     
                            "field",  
                            &field, 
                            0.5,  
                            -2.,  
                            2.,  
                            0.1,  
                            0)); 
  add(f->getInstance()->set("maxNullCount",     
                            "maxNullCount", 
                            &maxNullCount, 
                            13, 
                            0, 
                            30, 
                            1, 
                            0)); 
  add(f->getInstance()->set("maxContiguousNullCount",  
                            "maxContiguousNullCount",  
                            &maxContiguousNullCount,   
                            8, 
                            0, 
                            20, 
                            1, 
                            0)); 
  add(f->getInstance()->set("minContiguousHitCountForNullReset",     
                            "minContiguousHitCountForNullReset",  
                            &minContiguousHitCountForNullReset, 
                            2, 
                            1, 
                            10, 
                            1, 
                            0));
  add(f->getInstance()->set("maxChi2Vertex",  
                            "maxChi2Vertex",   
                            &maxChi2Vertex, 
                            1000., 0., 20000., 0.1, 0)); 
  add(f->getInstance()->set("massHypothesis", 
                            "massHypothesis",  
                            &massHypothesis,  
                            0.139, 0.1, 20., 0.01, 0)); 
} 


void StiKalmanTrackFinderParameters::loadDS(TDataSet &ds)
{
  cout << "StiKalmanTrackFinder::load(TDataSet&ds) -I- Starting" << endl;
  St_KalmanTrackFinderParameters *a = static_cast<St_KalmanTrackFinderParameters*>(ds.Find("KalmanTrackFinderParameters" ));
  if (!a) throw runtime_error("StiKalmanTrackFinderParameters::load(TDataSet&ds) -E- a==0");
  KalmanTrackFinderParameters_st *b = a->GetTable();
  if (!b) throw runtime_error("StiKalmanTrackFinderParameters::load(TDataSet&ds) -E- b==0");
  useMcAsRec      = b->useMcAsRec;               
  elossCalculated = b->elossCalculated;
  mcsCalculated   = b->mcsCalculated; 
  field           = b->field; 
  maxNullCount    = b->maxNullCount;
  maxContiguousNullCount            = b->maxContigNullCount; 
  minContiguousHitCountForNullReset = b->minCountForReset;   
  maxChi2Vertex   = b->maxChi2Vertex;
  massHypothesis  = b->massHypothesis;	
  maxDca3dVertex  = b->maxDca3dVertex;
  maxDca2dZeroXY  = b->maxDca2dZeroXY;
cout << *this;
  cout << "StiKalmanTrackFinder::load(TDataSet*ds) -I- Done" << endl;
}


void StiKalmanTrackFinderParameters::loadFS(ifstream& inFile)
{
  cout << "StiKalmanTrackFinderParameters::load(ifstream& inFile) -I- Starting" << endl;
  inFile >> useMcAsRec;
  inFile >> elossCalculated;
  inFile >> mcsCalculated;
  inFile >> field;
  inFile >> maxNullCount;
  inFile >> maxContiguousNullCount;
  inFile >> minContiguousHitCountForNullReset;
  inFile >> maxChi2Vertex;
  inFile >> massHypothesis;
  cout << *this;
  cout << "StiKalmanTrackFinderParameters::load(ifstream& inFile) -I- Done" << endl;
}

ostream& operator<<(ostream& os, const StiKalmanTrackFinderParameters& p)
{
  os << p.getName() << endl
     << "                         useMcAsRec: " << p.useMcAsRec << endl
     << "                    elossCalculated: " << p.elossCalculated << endl
     << "                      mcsCalculated: " << p.mcsCalculated << endl
     << "                              field: " << p.field << endl
     << "                       maxNullCount: " << p.maxNullCount << endl
     << "             maxContiguousNullCount: " << p.maxContiguousNullCount << endl
     << "  minContiguousHitCountForNullReset: " << p.minContiguousHitCountForNullReset << endl
     << "                      maxChi2Vertex: " << p.maxChi2Vertex << endl
     << "                     massHypothesis: " << p.massHypothesis << endl;
  return os;
}

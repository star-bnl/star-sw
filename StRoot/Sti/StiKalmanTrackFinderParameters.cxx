#include "StiKalmanTrackFinderParameters.h" 
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
  add(f->getInstance()->set("useMcAsRec",
                            "useMcAsRec",
                            &useMcAsRec, 
                            false,  
                            0)); 
  add(f->getInstance()->set("mcsCalculated",       
                            "mcsCalculated",    
                            &mcsCalculated, 
                            true,  
                            0)); 
  add(f->getInstance()->set("field",     
                            "field",  
                            &field, 
                            0.5,  
                            0.1,  
                            2.,  
                            0.1,  
                            0)); 
  add(f->getInstance()->set("maxNullCount",     
                            "maxNullCount", 
                            &maxNullCount, 
                            15, 
                            0, 
                            30, 
                            1, 
                            0)); 
  add(f->getInstance()->set("maxContiguousNullCount",  
                            "maxContiguousNullCount",  
                            &maxContiguousNullCount,   
                            13, 
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
  add(f->getInstance()->set("minSearchWindow",     
                            "minSearchWindow",   
                            &minSearchWindow,  
                            1.5, 0.5, 20., 0.1, 0)); 
  add(f->getInstance()->set("maxSearchWindow", 
                            "maxSearchWindow", 
                            &maxSearchWindow,         
                            4.5, 0.5, 20., 0.1, 0)); 
  add(f->getInstance()->set("searchWindowScale", 
                            "searchWindowScale",  
                            &searchWindowScale, 
                            2.5, 0.5, 20., 0.1, 0)); 
  add(f->getInstance()->set("maxChi2ForSelection",  
                            "maxChi2ForSelection",   
                            &maxChi2ForSelection, 
                            2., 0.5, 200., 0.1, 0)); 
  add(f->getInstance()->set("maxChi2Vertex",  
                            "maxChi2Vertex",   
                            &maxChi2Vertex, 
                            100., 0., 20000., 0.1, 0)); 
  add(f->getInstance()->set("massHypothesis", 
                            "massHypothesis",  
                            &massHypothesis,  
                            0.139, 0.1, 20., 0.1, 0)); 
} 

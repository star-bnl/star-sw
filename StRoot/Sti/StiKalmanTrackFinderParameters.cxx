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

/// Set the parameter values according to the given databse object
const StiKalmanTrackFinderParameters & StiKalmanTrackFinderParameters::operator=(const KalmanTrackFinderParameters_st & p) 
{ 
  useMcAsRec      = p.useMcAsRec;
  elossCalculated = p.elossCalculated;
  mcsCalculated   = p.mcsCalculated; 
  field           = p.field; 
  maxNullCount    = p.maxNullCount;
  maxContiguousNullCount            = p.maxContigNullCount; //maxContiguousNullCount; 
  minContiguousHitCountForNullReset = p.minCountForReset;   //minContiguousHitCountForNullReset;
  maxChi2Vertex   = p.maxChi2Vertex;
  massHypothesis  = p.massHypothesis;
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
  /* 
  add(f->getInstance()->set("minSearchWindow",     
                            "minSearchWindow",   
                            &minSearchWindow,  
                            1.6, 0.5, 20., 0.1, 0)); 
  add(f->getInstance()->set("maxSearchWindow", 
                            "maxSearchWindow", 
                            &maxSearchWindow,         
                            7., 0.5, 20., 0.1, 0)); 
  add(f->getInstance()->set("searchWindowScale", 
                            "searchWindowScale",  
                            &searchWindowScale, 
                            10.0, 0.5, 20., 0.1, 0)); 
  add(f->getInstance()->set("maxChi2ForSelection",  
                            "maxChi2ForSelection",   
                            &maxChi2ForSelection, 
                            10., 0.5, 200., 0.1, 0)); 
  */
  add(f->getInstance()->set("maxChi2Vertex",  
                            "maxChi2Vertex",   
                            &maxChi2Vertex, 
                            1000., 0., 20000., 0.1, 0)); 
  add(f->getInstance()->set("massHypothesis", 
                            "massHypothesis",  
                            &massHypothesis,  
                            0.139, 0.1, 20., 0.01, 0)); 
  
  //add(f->getInstance()->set("OuterScaling",
  //                          "OuterScaling",
  //                          &outerScaling,
  //                          1., 0., 20., 0.1, 0));
  //add(f->getInstance()->set("InnerScaling",
  //                          "InnerScaling",
  //                          &innerScaling,
  //                          1., 0., 20., 0.1, 0)); 
  //
} 


void StiKalmanTrackFinderParameters::load(TDataSet * ds)
{
	// validate source
	if (!ds) throw runtime_error("StiKalmanTrackFinderParameters::load(TDataSet * ds)");
	cout << " <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<"<<endl;
	cout << " <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<"<<endl;
	cout << " <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<"<<endl;
	cout << " <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<"<<endl;
	cout << " <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<"<<endl;
	ds->ls(3);
	// 	
	St_KalmanTrackFinderParameters * a = static_cast<St_KalmanTrackFinderParameters*>(ds->Find("KalmanTrackFinderParameters" ));
  if (!a) throw runtime_error("StiKalmanTrackFinderParameters::load(TDataSet * ds) -E- a==0");
	KalmanTrackFinderParameters_st * b = a->GetTable();
	if (!b) throw runtime_error("StiKalmanTrackFinderParameters::load(TDataSet * ds) -E- b==0");
	*this = *b;
}

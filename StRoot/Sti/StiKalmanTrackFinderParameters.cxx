#include "StiKalmanTrackFinderParameters.h"
#include "tables/St_KalmanTrackFinderParameters_Table.h"
#include "Sti/StiToolkit.h" 
#include "Sti/Base/Factory.h" 
#include "Sti/Base/EditableParameter.h" 
 
//______________________________________________________________________________
StiKalmanTrackFinderParameters::StiKalmanTrackFinderParameters() 
  : EditableParameters("KalmanTrackFinderParameters","KalmanTrackFinderParameters") 
{ 
  initialize(); 
} 
   
//______________________________________________________________________________
StiKalmanTrackFinderParameters::~StiKalmanTrackFinderParameters() 
{}   
 
 
//______________________________________________________________________________
StiKalmanTrackFinderParameters::StiKalmanTrackFinderParameters(const StiKalmanTrackFinderParameters & p) 
{ 
} 
 
//______________________________________________________________________________
const StiKalmanTrackFinderParameters & StiKalmanTrackFinderParameters::operator=(const StiKalmanTrackFinderParameters & p) 
{ 
  clear(); 
  _enabled  = p._enabled; 
  _editable = p._editable; 
  return *this; 
} 


//______________________________________________________________________________
void StiKalmanTrackFinderParameters::initialize() 
{ 
  _enabled  = true; 
  _editable = true; 
  memset(mBeg,0,mEnd-mBeg+1);
} 


//______________________________________________________________________________
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
  setHitRegions(b->mHitRegions);
  setHitWeights(b->mHitWeights);


cout << *this;
  cout << "StiKalmanTrackFinder::load(TDataSet*ds) -I- Done" << endl;
}
// //______________________________________________________________________________
// void StiKalmanTrackFinderParameters::loadFS(ifstream& inFile)
// {
//   cout << "StiKalmanTrackFinderParameters::load(ifstream& inFile) -I- Starting" << endl;
//   inFile >> useMcAsRec;
//   inFile >> elossCalculated;
//   inFile >> mcsCalculated;
//   inFile >> field;
//   inFile >> maxNullCount;
//   inFile >> maxContiguousNullCount;
//   inFile >> minContiguousHitCountForNullReset;
//   inFile >> maxChi2Vertex;
//   inFile >> massHypothesis;
//   cout << *this;
//   cout << "StiKalmanTrackFinderParameters::load(ifstream& inFile) -I- Done" << endl;
// }
// 
//______________________________________________________________________________
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
//______________________________________________________________________________
void StiKalmanTrackFinderParameters::setHitRegions(int rs)
{
  int i=0;
  for (i=0;rs;i++) {mHitRegions[i]=rs%100; rs/=100;}
  mHitRegions[i]=10000;
}
//______________________________________________________________________________
void StiKalmanTrackFinderParameters::setHitWeights(int ws)
{
  int i=0;
  for (i=0;ws;i++) {mHitWeights[i]=ws%100; ws/=100;}
  mHitWeights[i]=0;
}
//______________________________________________________________________________
int StiKalmanTrackFinderParameters::hitWeight(int rxy) const
{
  if (rxy>50) return 0;
  int i=0; for (i=0;rxy>mHitRegions[i];i++) {}
  return mHitWeights[i];
}


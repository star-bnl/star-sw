#ifndef StLuminosityHolder_hh
#define StLuminosityHolder_hh

#include "TObject.h"
#include <vector>
using namespace std;

class StLuminosityHolder : public TObject
{
 private:
  int mRunNumber;
  vector<unsigned int> mTriggers;
  vector<float> mLumTotal;
  vector<float> mLumCuts;
  vector<float>mLumVertex;
  vector<float>mLumSoftTrig;
  vector<float> mPrescales;
  vector<unsigned int> mNTotal;
  vector<unsigned int> mNCuts;
  vector<unsigned int> mNVertex;
  vector<unsigned int> mNSoftTrig;
  float mXsec;
  float mVertexCut;

 public:
  StLuminosityHolder(int run = 0);
  ~StLuminosityHolder(){}
  void ClearVectors();
  void setRunNumber(int run){mRunNumber = run;}
  void setCrossSectionNB(float xsec){mXsec = xsec;}
  void setVertexCutcm(float vcut){mVertexCut = vcut;}
  void setTriggers(vector<unsigned int> triggers){mTriggers = triggers;}
  void setLumTotal(vector<float> lumtot){mLumTotal = lumtot;}
  void setLumCuts(vector<float> lumcut){mLumCuts = lumcut;}
  void setLumSoftTrig(vector<float> lumtrig){mLumSoftTrig = lumtrig;}
  void setLumVertex(vector<float> lumvert){mLumVertex = lumvert;}
  void setNTotal(vector<unsigned int> ntot){mNTotal = ntot;}
  void setNCuts(vector<unsigned int> ncuts){mNCuts = ncuts;}
  void setNSoftTrig(vector<unsigned int> ntrig){mNSoftTrig = ntrig;}
  void setNVertex(vector<unsigned int> nvert){mNVertex = nvert;}
  void setPrescales(vector<float> prescales){mPrescales = prescales;}

  const int getRunNumber() const {return mRunNumber;}
  const float getCrossSectionNB() const {return mXsec;}
  const float getVertexCutcm() const {return mVertexCut;}
  const vector<unsigned int> getTriggers() const {return mTriggers;}
  const vector<unsigned int> getNTotal() const {return mNTotal;}
  const vector<unsigned int> getNCuts() const {return mNCuts;}
  const vector<unsigned int> getNVertex() const {return mNVertex;}
  const vector<unsigned int> getNSoftTrig() const {return mNSoftTrig;}
  const vector<float> getLumTotal() const {return mLumTotal;}
  const vector<float> getLumCuts() const {return mLumCuts;}
  const vector<float> getLumVertex() const {return mLumVertex;}
  const vector<float> getLumSoftTrig() const {return mLumSoftTrig;}
  const vector<float> getPrescales() const {return mPrescales;}


  ClassDef(StLuminosityHolder,1)
};


#endif

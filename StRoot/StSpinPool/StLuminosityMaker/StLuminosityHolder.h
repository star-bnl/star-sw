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
  vector<float> mPrescales;
  vector<unsigned int> mNTotal;
  vector<unsigned int> mNCuts;
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
  void setNTotal(vector<unsigned int> ntot){mNTotal = ntot;}
  void setNCuts(vector<unsigned int> ncuts){mNCuts = ncuts;}
  void setPrescales(vector<float> prescales){mPrescales = prescales;}

  const int getRunNumber() const {return mRunNumber;}
  const float getCrossSectionNB() const {return mXsec;}
  const float getVertexCutcm() const {return mVertexCut;}
  const vector<unsigned int> getTriggers() const {return mTriggers;}
  const vector<unsigned int> getNTotal() const {return mNTotal;}
  const vector<unsigned int> getNCuts() const {return mNCuts;}
  const vector<float> getLumTotal() const {return mLumTotal;}
  const vector<float> getLumCuts() const {return mLumCuts;}
  const vector<float> getPrescales() const {return mPrescales;}


  ClassDef(StLuminosityHolder,1)
};


#endif

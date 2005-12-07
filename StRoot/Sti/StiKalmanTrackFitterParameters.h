#ifndef StiKalmanTrackFitterParameters_H
#define StiKalmanTrackFitterParameters_H
#include "Sti/Base/EditableParameters.h"
#include "TDataSet.h"
class KalmanTrackFitterParameters_st;

class StiKalmanTrackFitterParameters : public EditableParameters
{
public: 
  StiKalmanTrackFitterParameters();
  StiKalmanTrackFitterParameters(const string & name, const string & description);
  StiKalmanTrackFitterParameters(const StiKalmanTrackFitterParameters & pars);
	const StiKalmanTrackFitterParameters & operator=(const StiKalmanTrackFitterParameters & p);
  virtual ~StiKalmanTrackFitterParameters();
  void setMaxChi2(double maxChi2)	{_maxChi2 = maxChi2;}
  double getMaxChi2()    const		{return _maxChi2   ;}
  double getMaxChi2Vtx() const 		{return _maxChi2Vtx;}
  void initialize(); 
	virtual void loadDS(TDataSet&ds);
	virtual void loadFS(ifstream& inFile); 
	friend ostream& operator<<(ostream& os, const StiKalmanTrackFitterParameters& par);
 protected:
  double _maxChi2; 
  double _maxChi2Vtx; 
};




#endif

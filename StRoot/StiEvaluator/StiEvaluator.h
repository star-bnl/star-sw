//StiEvaluator.h
// $Id: StiEvaluator.h,v 1.23 2003/06/19 01:37:03 calderon Exp $
//
// Evaluation histograms for STAR Integrated Tracker
// Manuel Calderon de la Barca Sanchez
// Oct 2002
//
// $Log: StiEvaluator.h,v $
// Revision 1.23  2003/06/19 01:37:03  calderon
// Adding all the histograms from Zbigniew.
//
// Revision 1.22  2002/11/27 00:08:55  calderon
// New version of evaluator using the minimctrees
//

#ifndef StiEvaluator_HH
#define StiEvaluator_HH
#include <vector>

#include "TObject.h"

#include "EfficiencyAnalysis.h"

class TH2D;
class TChain;
class StMiniMcEvent;
class StMiniMcPair;

class StiEvaluator {
public:
    StiEvaluator();
    StiEvaluator(const StiEvaluator&);
    virtual ~StiEvaluator();
    void setChain(TChain* ch);
    void setFitPtsLimit(double);
    void setDcaLimit(double);
    void setGeantId(int);
    void setEtaRange(double,double);
    void setFileName(char*);
    void setPullType(bool);

    size_t getIndex(size_t);
    int initialize();
    void resethistograms();
    void makehistograms();
    void writehistograms();
    
private:
    StMiniMcEvent* minimcevent; //!
    double mFitPtsLimit; //!
    double mDcaLimit; //!
    int mGeantId;     //!
    double  mEtaMinimum; //!
    double  mEtaMaximum; //!
    bool    mPullType;   //!
    char* mFileName;  //!
    TChain* mChain;      //! 

    vector<EfficiencyAnalysis> mEfficiencyAnalysisVector; //!
    
    ClassDef(StiEvaluator,1)	
};
#endif

//  TrMean.h
//  M.L. Miller
//  5/00

#ifndef TrMean_HH
#define TrMean_HH

#include "DeDxPreparation.h"

//Define a functor for STL algorithms
struct Truncator {
    void clear();
    void Truncator::operator() (const double );
    double chargeAccumulated;
    double chargeSquaredAccumulated; // For error on mean
    bool debug;
    unsigned int nPoints;
    vector<double> truncatedVec; //!
};

//Reproduce Ogilvies Truncated Mean calculation
class TrMean : public DeDxPreparation {
public:    
    //constructor-destructor
    TrMean();
    TrMean( StTrack* , int, int, int, double);
    virtual ~TrMean();

    //Access------------------------------------------
    virtual double mean() const;
    virtual double errorOnMean() const;
    virtual double numberOfPoints() const;
    virtual bool flag() const;
    void setTruncationFactor(double);
    double truncationFactor() const;
    const vector<double>& truncatedVector() const; //!

    //Methods---------------------------------------
    virtual void compute();
    virtual void truncate();
    void clear();
    void clearAll();
    
    //Utilities---------------------------------------
    void printTruncatedVec();
    
protected:
    //Members--------------------------------------
    
    vector<double> m_TruncatedVec; //!     Vector of Charge after Truncation
    double m_TruncatedMean;     //The truncated Arithmetic Mean.
    double m_TruncatedMeanError;     //The error on TM
    double m_NPointsAfterTruncation;     //The number of points after truncation.
    double m_truncationFactor;     //The truncation factor
    bool m_flag;     //Status flag (m_flag == flase -> disregard)    
};



#endif

//  TrMean.cxx
//  M.L. Miller
//  5/00

#include <cmath>
#include "TrMean.h"
// StEvent
#include "StEventTypes.h"
#include "StThreeVectorD.hh"

// STL
#include <algorithm>
#include <numeric>
using std::for_each;
using std::copy;

//constructor-------------------------------------------
TrMean::TrMean() {}

TrMean::TrMean(StTrack* track, int tnum,int minprow, int maxprow, double trFact)
    : DeDxPreparation(track, tnum, minprow, maxprow)
{
    clear();
    m_truncationFactor=trFact;
}

TrMean::~TrMean() {}

//clear TrMean info, save base class info
void TrMean::clear()
{
    m_TruncatedVec.clear();
    m_TruncatedMean = 0.;
    m_TruncatedMeanError = 0.;
    m_NPointsAfterTruncation = 0.;
    m_truncationFactor = 0.;
    m_flag = false;
    m_debug = false;
    return;
}

//clear base classes too
void TrMean::clearAll()
{
    clear();
    DeDxPreparation::clearAll();
    return;
}

//Accessors-----------------------------------------------------------------
double TrMean::mean() const
{
    return m_TruncatedMean;
}

double TrMean::errorOnMean() const
{
    return m_TruncatedMeanError;
}

double TrMean::numberOfPoints() const
{
    return m_NPointsAfterTruncation;
}

void TrMean::setTruncationFactor(double tfac)
{
    m_truncationFactor = tfac;
    return;
}

double TrMean::truncationFactor() const
{
    return m_truncationFactor  ;
}

bool TrMean::flag() const
{
    return m_flag;
}

const vector<double>&TrMean::truncatedVector() const
{
    return m_TruncatedVec;
}

//Methods---------------------------------------------------------
void TrMean::compute()
{
    truncate();

    //Debugging patch
    if (0) {
	if (!m_flag) {
	    cout <<"\n\n!!!Error, no truncation point found!!"<<endl;
	    cout <<"Track:\t"<<m_TrackNumber<<"\tTpcPoints:\t"<<m_tpcHitVec.size()<<"\tNormTpcPoints:\t"<<m_normChargeVec.size()<<endl;
	    cout<<m_StTrack->geometry()->helix()<<endl;
	    cout<<"Track Flag:\t"<<m_StTrack->flag()<<endl;
	    m_normChargeVec.clear();
	    m_debug = true;
	    cout <<"\n ------Begin Normalization Loop"<<endl;
	    dxNorm();
	    truncate();
	}
    }
    
    return;
}

//Do truncated mean
void TrMean::truncate()
{
    Truncator truncator;
    truncator.clear();
    truncator.debug = m_debug;

    int npoints = static_cast<int>(m_normChargeVec.size()) - static_cast<int>((1.-m_truncationFactor)*m_normChargeVec.size());

    if (npoints>1) {
	truncator = for_each(m_normChargeVec.begin(), m_normChargeVec.begin()+npoints, truncator);
    }
    if (truncator.nPoints==m_normChargeVec.size() || truncator.nPoints==0) {
	m_flag = false;}
    else {m_flag=true;}

    //Store the arithmetic mean
    m_NPointsAfterTruncation = truncator.nPoints;
    m_TruncatedMean = truncator.chargeAccumulated / static_cast<double>(m_NPointsAfterTruncation);
    //Store the normalized charge vector after truncation
    m_TruncatedVec.resize(truncator.truncatedVec.size());
    copy(truncator.truncatedVec.begin(), truncator.truncatedVec.end(), m_TruncatedVec.begin());
    //Calculate the error on the mean
    double diff = truncator.chargeSquaredAccumulated/m_NPointsAfterTruncation - m_TruncatedMean*m_TruncatedMean;
    m_TruncatedMeanError = sqrt(diff) / sqrt(m_NPointsAfterTruncation);

    return;
}

//Utilities-----------------------------
void TrMean::printTruncatedVec()
{
    vector<double>::const_iterator it;
    for (it = m_TruncatedVec.begin(); it != m_TruncatedVec.end(); it++) {
	cout <<(*it)<<endl;
    }
    return;
}

//Define truncator functor
void Truncator::clear()
{
    chargeAccumulated=0.;
    chargeSquaredAccumulated=0.;;
    nPoints=0;
    return;
}

void Truncator::operator() (const double q)
{
    chargeAccumulated = chargeAccumulated + q;
    chargeSquaredAccumulated = chargeSquaredAccumulated + (q*q);
    truncatedVec.push_back(q);
    nPoints++;
	
    return;
}

#ifndef _STARBEMCGAMMAFILTER_
#define _STARBEMCGAMMAFILTER_

/// \author Michael Betancourt
/// Massachusetts Institute of Technology
/// 
/// StMCFilter implementation designed to filter
/// for events with photon like electromagnetic
/// calorimeter depositions in the barrel.

#include "StMCFilter/StMCFilter.h"
#include <string>

// Forward declarations
class StGenParticleMaster;

class StBemcGammaFilter : public StMCFilter 
{

    public:

        StBemcGammaFilter();
        virtual ~StBemcGammaFilter() {};

        void parseConfig(std::string, float);

        // No decision immediately after PYTHIA event generation
        // int RejectEG(const StGenParticleMaster &ptl) const; 

        // Reject after vertex sampling
        int RejectGT(const StGenParticleMaster &ptl) const;

        // No decision after GEANT reconstruction
        //int RejectGE(const StGenParticleMaster &ptl) const;

public:

        double mConeRadius;        ///< Cone radius of cluster
        double mSeedThreshold;     ///< Seed energy threshold for cluster finding
        double mClusterThreshold;  ///< Cluster E_{T} threshold for clustering finding
        double mEtaLow;            ///< Minimum detector eta
        double mEtaHigh;           ///< Maximum detector eta
        double mMaxVertex;         ///< Maximum vertex magnitude in cm

};


/// Sparse class to hold track kinematics

class kinematics
{

    public:

        kinematics(): Et(0), eta(0), phi() {};
       	kinematics(double a, double b, double c): Et(a), eta(b), phi(c) {};

        double Et;
        double eta;
        double phi;

};

#endif


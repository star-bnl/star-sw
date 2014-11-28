#ifndef _STAREBEMCGAMMAFILTER_
#define _STAREBEMCGAMMAFILTER_

/// \author Michael Betancourt
/// Massachusetts Institute of Technology
/// 
/// StMCFilter implementation designed to filter
/// for events with photon like electromagnetic
/// calorimeter depositions in the barrel.
///
/// \author Jan Balewski, MIT,
///  modification of original StBemcGammaFilter
/// *changed hardcoded parameters to cover the Barrel and 2/3 of the Endcap
/// *relaxed hadron fraction from oryginal 0.85 to 0.5 to speed it up (and make it less accurate - no free lunch)
/// *added few static counters to trace rejection event by event
///
/// Few additional modifications by J. Webb
/// * Changed name to StMCCaloFilter, as this is a fairly generic filter
/// * 

#include "StMCFilter/StMCFilter.h"
#include <string>

// Forward declarations
class StGenParticleMaster;

class StMCCaloFilter : public StMCFilter 
{

    public:

        StMCCaloFilter();
        virtual ~StMCCaloFilter() {};

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
	double mHadronEfract;      ///< Attenuation factor for hadron energy

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


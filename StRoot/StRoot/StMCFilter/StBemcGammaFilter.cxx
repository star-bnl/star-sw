#include "stdlib.h"
#include "math.h"
#include <iostream>
#include <vector>
#include <map>

#include "StMCFilter/StGenParticle.h"
#include "StMCFilter/StBemcGammaFilter.h"


// IMPORTANT IMPORTANT IMPORTANT
// Defining the static instance of user filter provides creating this
// class during the loading of library. Afterward GEANT could select 
// the needed filter by name. 

static StBemcGammaFilter bemcGammaFilter;

using namespace std;

/// Utility class for sorting tracks
bool comparePhi(kinematics A, kinematics B) { return A.phi < B.phi ? true : false; }

/// Constructor

StBemcGammaFilter::StBemcGammaFilter(): StMCFilter("bemcGammaFilter") 
{

    // 2009 Defaults
    mConeRadius = 0.12;       // Circumscribe a 3x3 BEMC tower cluster
    mSeedThreshold = 5.0;
    mClusterThreshold = 6.55;
    mEtaLow = -1.05;
    mEtaHigh = 1.05;
    mMaxVertex = 80;

}

/// Modify filter settings
/// \param attr String of the parameter to be modified
/// \param val New parameter value

void StBemcGammaFilter::parseConfig(string attr, float val)
{

    if(attr == "ConeRadius")
    {
        cout << "StBemcGammaFilter::parseConfig() - Setting mConeRadius to " << val << endl;
        mConeRadius = val;
    }
    else if(attr == "SeedThreshold")
    {
        cout << "StBemcGammaFilter::parseConfig() - Setting mSeedThreshold to " << val << endl;
        mSeedThreshold = val;
    }
    else if(attr == "ClusterThreshold")
    {
        cout << "StBemcGammaFilter::parseConfig() - Setting mClusterThreshold to " << val << endl;
        mClusterThreshold = val;
    }
    else if(attr == "EtaLow")
    {
        cout << "StBemcGammaFilter::parseConfig() - Setting mEtaLow to " << val << endl;
        mEtaLow = val;
    }
    else if(attr == "EtaHigh")
    {
        cout << "StBemcGammaFilter::parseConfig() - Setting mEtaHigh to " << val << endl;
        mEtaHigh = val;
    }
    else if(attr == "MaxVertex")
    {
        cout << "StBemcGammaFilter::parseConfig() - Setting mMaxVertex to " << val << endl;
        mMaxVertex = val;
    }
    else
    {
        cout << "StBemcGammaFilter::parseConfig() - " << attr << " is not an existing parameter!" << endl;        
    }

    return;

}

/// Filter events immediately after vertex
/// \param plt List of particles in the Pythia event
/// \return Filtering decision, 1 for rejection and 0 for acceptance

int StBemcGammaFilter::RejectGT(const StGenParticleMaster &ptl) const
{

    // Instantiate variables
    const StGenParticle* track = 0;

    double p[4] = {0, 0, 0, 0};
    double v[3] = {0, 0, 0};

    // Immediately abort events with extreme vertices
    track = ptl(0);
    track->Vertex(v);    
    if(fabs(v[2]) > mMaxVertex) return 1;

    // Resume instantiations
    double E = 0;
    double particleEt = 0;
    double detectorEt = 0;
    double detectorEta = 0;
    double detectorPhi = 0;

    int id = 0;
    bool hadronFlag = 0;

    double rSmd = 230.705;
    double rSmd2 = rSmd * rSmd;

    double pT2 = 0;
    double pdotv = 0;
    double pcrossv = 0;
    double N = 0;
    double rho2 = 0;

    double pi = 3.141592653589793;

    map<double, kinematics> seedTracks;
    map<double, kinematics> eventTracks;

    // Loop over particles
    for(int i = 0; i < ptl.Size(); ++i)
    {
    
        track = ptl(i);
        if(!track) continue;

        // Skip any intermediate particles
        if(track->GetStatusCode() != 1) continue;

        id = track->GetPdgCode();
        E = track->Energy();

        // To mimic the response of the calorimeters to hadrons,
        // reduce the energy of all particles except for
        // photons (22), neutral pions (111), eta (221), electrons (11)
        // antiprotons (-2212), and antineutrons (-2112)

        hadronFlag = abs(id) != 22 && abs(id) != 111 && abs(id) != 221 && abs(id) != 11;
        hadronFlag &= id != -2212 && id != -2112;

        if(hadronFlag) E *= 0.85;

        // Compute track kinematics from the vertex
        track->Vertex(v);
        track->Momentum(p);

        pT2 = p[0] * p[0] + p[1] * p[1];
        particleEt = E * sqrt( pT2 / (pT2 + p[2] * p[2]) );

        // Compute track kinematics corrected for vertex
        pdotv = p[0] * v[0] + p[1] + v[1];
        pcrossv = p[0] * v[1] - p[1] * v[0];
        N = ( - pdotv + sqrt( pT2 * rSmd2 - pcrossv ) ) / pT2;

        for(unsigned int j = 0; j < 3; ++j) p[j] = N * p[j] + v[j];

        rho2 = p[0] * p[0] + p[1] * p[1];

        detectorEt = E * sqrt( rho2 / (rho2 + p[2] * p[2] ) );
        detectorEta = - log( sqrt(rho2) / ( sqrt( rho2 + p[2] * p[2] ) + p[2] ) );
        detectorPhi = atan2(p[1], p[0]);

        // Ignore tracks outside of the fiducial volume
        if(detectorEta < mEtaLow) continue;
        if(detectorEta > mEtaHigh) continue;

        // Store seed tracks
        if(E > mSeedThreshold) seedTracks[detectorPhi] = kinematics(E, detectorEta, detectorPhi);

        // Store all tracks
        eventTracks[detectorPhi] = kinematics(particleEt, detectorEta, detectorPhi);

    }

    // Search for clusters around each seed,
    // being careful with the discontinuity in phi

    map<double, kinematics>::iterator itSeed;
    map<double, kinematics>::iterator itEvent;

    double phiLeft = 0;
    double phiRight = 0;

    double dEta = 0;
    double dPhi = 0;
    double R = 0;

    double EtSum = 0;

    itEvent = eventTracks.begin();

    for(itSeed = seedTracks.begin(); itSeed != seedTracks.end(); ++itSeed)
    {

        EtSum = 0;
        dPhi = 0;        

        // Calculate phi boundaries of seed cone
        phiLeft = (itSeed->first) - mConeRadius;
        phiRight = (itSeed->first) + mConeRadius;

        // Slide back in the list if the previous cone overlaps with current cone
        while(itEvent != eventTracks.begin())
        {
            if(itEvent->first > phiLeft) --itEvent;
            else break; 
        }

        // Slide to the end of the list if the cone crosses the discontinuity in phi
        if(phiLeft < - pi)
        {

            // Reset the track iterator only if at least one track is within the cone overflow
            map<double, kinematics>::reverse_iterator rit = eventTracks.rbegin();
            if(rit->first > phiLeft + 2 * pi)
            {

                // Remap phi domain
                phiLeft = phiLeft + 2 * pi;
                phiRight = phiRight + 2 * pi;

                // Reverse iterate to the first track that falls outside the cone
                while(rit->first > phiLeft) ++rit;

                // Set event track iterator to this last track
                // Note the increment operator to take into account the decrement
                // implicit in reverse_iterator::base()
                itEvent = (++rit).base();

            }

        }

        // Loop past tracks before the cone
        while(itEvent->first < phiLeft) ++itEvent;

        // Loop over tracks in the cone
        dPhi = 0;
    
        while(itEvent->first < phiRight)
        {

            dEta = itSeed->second.eta - itEvent->second.eta;
            dPhi = acos( cos( itSeed->first - itEvent->first) );
            R = sqrt( dEta * dEta + dPhi * dPhi );

            if(R < mConeRadius) EtSum += itEvent->second.Et;

            // Increment event track, 
            ++itEvent;

            // Slide to the front of the list if the cone crosses the discontinuity in phi
            if(itEvent == eventTracks.end()) 
            {

                // Remap phi
                phiLeft = phiLeft - 2 * pi;
                phiRight = phiRight - 2 * pi;

                itEvent = eventTracks.begin();

            }

        }

        // If a cluster was found above threshold, 
        // let the event through the filter
        if(EtSum > mClusterThreshold) return 0; // 0 FOR BOTH TESTS

    }

    // If no clusters were found, abort the event
    return 1;

}

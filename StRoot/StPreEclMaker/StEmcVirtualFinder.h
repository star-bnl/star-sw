/*!\class StEmcVirtualFinder
\author Alexandre A. P. Suaide
 
This is the basic class for cluster finder
algorithm developement. All the cluster
algorithms should inherit from this class.
It already creates the basic 4 BEMC collections,
one for each detector and have basic QA histograms.
 
 
*/
#ifndef STAR_StEmcVirtualFinder
#define STAR_StEmcVirtualFinder

#include "TObject.h"
#include "TList.h"
#include "StEmcPreClusterCollection.h"
#include "StEmcUtil/others/emcInternalDef.h"
#include "StEmcRawMaker/defines.h"
#include "TH2.h"
#include "StMessMgr.h"

#define NHIST1 5
#define NHIST2 1

class StEvent;

class StEmcVirtualFinder : public TObject
{
private:

protected:
    StEmcPreClusterCollection*  mColl[MAXDETBARREL];
    TH1F*                       mHist1D[NHIST1][MAXDETBARREL];
    TH2F*                       mHist2D[NHIST2][MAXDETBARREL];
    Bool_t                      mPrint;

public:
    StEmcVirtualFinder();
    virtual        ~StEmcVirtualFinder();

    virtual Bool_t findClusters(StEvent*); ///< finds clusters in a StEvent object
    virtual Bool_t fillStEvent(StEvent*); ///< fills the StEvent object with the StEmcPreCluster objects in the collections
    virtual Bool_t fillHistograms(StEvent*); ///< fills the QA histograms
    virtual Bool_t clear(StEvent*); ///< removes clusters and points from the StEvent object
    virtual Bool_t clear(); ///< clear the pre cluster collections
    void    setPrint(Bool_t a)
    {
		LOG_INFO << "::setPrint() is obsolete.  Use logger config file to set verbosity instead." << endm;
    }///< Obsolete function; users can control messages with logger config file.

    ClassDef(StEmcVirtualFinder,1)
};

#endif

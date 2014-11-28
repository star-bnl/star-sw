/*!\class StEmcPreClusterCollection
\author Alexandre A. P. Suaide
 
This is a placeholder for the StEmcPreClusters. The
collection owns the clusters added to them so be careful
about deleting. If you want a cluster to be part of the
collection, create and delete clusters using the methods
newCluster() and removeCluster() instead of new and
delete.
 
*/
#include "TList.h"
#include "StEmcPreCluster.h"

#ifndef StEmcPreClusterCollection_HH
#define StEmcPreClusterCollection_HH
#include "StEmcRawMaker/defines.h"
class StEmcPreClusterCollection: public TList
{
private:

protected:
    Int_t              mDetector;
public:
    StEmcPreClusterCollection(Int_t);
    virtual            ~StEmcPreClusterCollection();

    Int_t              getDetector()
    {
        return mDetector;
    } ///< returns the detector id for the collection

    void               addCluster(StEmcPreCluster*); ///< add a previously created cluster to the collection
    StEmcPreCluster*   newCluster(); ///< creates a new cluster in the collection. Returns its pointer
    StEmcPreCluster*   getCluster(Int_t); ///< gets a cluster in the collection by its index
    StEmcPreCluster*   removeCluster(Int_t); ///< removes a cluster from the collection. DOES NOT delete it. Returns its pointer
    StEmcPreCluster*   removeCluster(StEmcPreCluster*); ///< removes a cluster from the collection. DOES NOT delete it. Returns its pointer
    void               deleteCluster(Int_t); ///< removes and deletes a cluster from the collection
    void               deleteCluster(StEmcPreCluster*); ///< removes and deletes a cluster from the collection
    Int_t              getNClusters(); ///< gets the number of clusters in the collection
    void               empty()
    {
        Delete();
        Clear();
    } ///< deletes all the cluster from the collection.

    ClassDef(StEmcPreClusterCollection,1)
};
#endif

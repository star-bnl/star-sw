#include "StEmcPreClusterCollection.h"

ClassImp(StEmcPreClusterCollection)

StEmcPreClusterCollection::StEmcPreClusterCollection(Int_t detector):TList()
{
    mDetector = detector;
}
StEmcPreClusterCollection::~StEmcPreClusterCollection()
{
    Delete();
    Clear();
}
void StEmcPreClusterCollection::addCluster(StEmcPreCluster* cluster)
{
    Add(cluster);
    return;
}
StEmcPreCluster* StEmcPreClusterCollection::newCluster()
{
    StEmcPreCluster* cl = new StEmcPreCluster(mDetector);
    Add(cl);
    return cl;
}
StEmcPreCluster* StEmcPreClusterCollection::getCluster(Int_t clId)
{
    return (StEmcPreCluster*)At(clId);
}
Int_t StEmcPreClusterCollection::getNClusters()
{
    return GetSize();
}
StEmcPreCluster* StEmcPreClusterCollection::removeCluster(Int_t clId)
{
    StEmcPreCluster *cluster = getCluster(clId);
    if(!cluster)
        return NULL;
    Remove(cluster);
    return cluster;
}
StEmcPreCluster* StEmcPreClusterCollection::removeCluster(StEmcPreCluster* cl)
{
    if(!cl)
        return NULL;
    Remove(cl);
    return cl;
}
void StEmcPreClusterCollection::deleteCluster(Int_t clId)
{
    StEmcPreCluster *cluster = getCluster(clId);
    if(!cluster)
        return;
    Remove(cluster);
    delete cluster;
    return;
}
void StEmcPreClusterCollection::deleteCluster(StEmcPreCluster* cl)
{
    if(!cl)
        return;
    Remove(cl);
    delete cl;
    return;
}


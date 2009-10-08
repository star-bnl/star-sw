/****************************************************************
 *
 * $Id: St_pp2pp_ClusterCollection.cxx,v 1.1 2009/10/08 18:12:57 yipkin Exp $
 *
 * Author: Kin Yip
 *
 *****************************************************************
 *
 * Description:
 * Description: pp2pp Cluster collection
 *
 *****************************************************************
 *
 * $Log: St_pp2pp_ClusterCollection.cxx,v $
 * Revision 1.1  2009/10/08 18:12:57  yipkin
 * StEvent stuff ..
 *
 * Revision 1.0  2009/10/08 16:57:21  yipkin
 * first release
 *
 *
 ****************************************************************/
#include "St_pp2pp_ClusterCollection.h"
#include "St_pp2pp_Cluster.h"

St_pp2pp_ClusterCollection::St_pp2pp_ClusterCollection() {/* nope */}
St_pp2pp_ClusterCollection::~St_pp2pp_ClusterCollection(){/* nope */}

void St_pp2pp_ClusterCollection::clear() {mClusterVector.clear();}

bool St_pp2pp_ClusterCollection::push_back(St_pp2pp_Cluster* cluster) {
        mClusterVector.push_back(cluster);
        return true;
}

St_pp2pp_Cluster* St_pp2pp_ClusterCollection::front() const { return mClusterVector.front(); }

St_pp2pp_Cluster* St_pp2pp_ClusterCollection::getCluster(size_t index) const { return mClusterVector[index]; }

St_pp2pp_Cluster* St_pp2pp_ClusterCollection::back() const { return mClusterVector.back(); }

size_t St_pp2pp_ClusterCollection::size() const { return mClusterVector.size(); }

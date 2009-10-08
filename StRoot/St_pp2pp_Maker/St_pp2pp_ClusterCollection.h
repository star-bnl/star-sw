/****************************************************************  
 * $Id: St_pp2pp_ClusterCollection.h,v 1.1 2009/10/08 18:12:57 yipkin Exp $
 *****************************************************************
 * Author: Kin Yip
 * Description: pp2pp Cluster collection
 *****************************************************************
 * $Log: St_pp2pp_ClusterCollection.h,v $
 * Revision 1.1  2009/10/08 18:12:57  yipkin
 * StEvent stuff ..
 *
 * Revision 1.0  2009/10/08 16:57:21  yipkin
 * first release
 *
 *
 ****************************************************************/
#ifndef St_PP2PP_CLUSTERCOLLECTION_H
#define St_PP2PP_CLUSTERCOLLECTION_H

#include <vector>
#ifndef ST_NO_NAMESPACES
using std::vector;
using std::copy;
#endif

//#include "St_pp2pp_Cluster.h"
class St_pp2pp_Cluster;

#ifndef ST_NO_DEF_TEMPLATE_ARGS
typedef vector<St_pp2pp_Cluster*> clusterVector;
#else
typedef vector<St_pp2pp_Cluster*, allocator<St_pp2pp_Cluster*> > clusterVector;
#endif

/**
   \class St_Pp2pp_ClusterCollection
   Class used as a collection for BTofRawHit in StBTofCollection.
 */
class St_pp2pp_ClusterCollection {
public:
    /// Default constructor
    St_pp2pp_ClusterCollection();
    virtual ~St_pp2pp_ClusterCollection();
    
    /// Add a St_pp2pp_Cluster into the vector
    bool        push_back(St_pp2pp_Cluster* cluster);
    /// Returns the size of the collection vector
    size_t      size()  const;
    /// Returns the first element of the vector
    St_pp2pp_Cluster*  front() const;
    /// Returns the last element of the vector
    St_pp2pp_Cluster*  back()  const;
    /// Returns a St_pp2pp_Cluster at index in the vector
    St_pp2pp_Cluster*  getCluster(size_t index) const;
    void clear();    
    
private:
    /// St_pp2pp_Cluster vector
    clusterVector         mClusterVector;
};
#endif

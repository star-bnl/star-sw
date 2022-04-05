/*!\class StEmcAssociationMaker
\author Marcia Maria de Moura

First version - 09/15/00<br><br>

This is the basic EMC association Maker. It has two methods of association:<br><br>

1. Cluster association:<br><br>

This maker takes tracks in StMcEvent object and EMC clusters in
StEvent object and compare their hits to check if they match. This is done
for the EMC Tower (bemc),Pre-Shower(bprs), Eta and Phi Shower Max (bsmde
and bsmdp) detectors.<br><br>

It builds the following association matrices:<br><br>

    - Simple Association (association): matrix elements are 0 for negative association and 1 for positive association.

    - Track Energy Fraction (trackFraction): matrix elements are fraction of track energy in a cluster.

    - Cluster Energy Fraction (clusterFraction): matrix elements are fraction of cluster energy from a track.

The elements of the cluster energy fraction matrix are
normalized. Initially, the energy information of former hits to be in a
cluster is the deposited energy. Before making the clusters, the deposited
energy is converted to ADC signal by St_ems_Maker and then is converted to
energy by St_emc_Maker that applies calibration constants. The elements of
the cluster energy fraction matrix are the ratio of the summed hit energy
over the track hits that are associated to that cluster to the cluster
energy. The track hits energies that are taken in StEmcAssociationMaker
are  obtained converting directilly the deposited energy into energy by
the method StEmcAssociationMaker::deToEnergy(). So the energy associated
to a track hit by each method is a little bit different leading to a
normalization of the elemtents of the cluster energy fraction matrix.<br><br>

2. Point association:<br><br>

The Maker also does association for EMC points. It compares
the tracks hits to the clusters hits from the clusters of the 4 detectors
that composes an EMC point (particle). It builds a simple association
matrix are related to the type of EMC cluster that compose the point and
is associated to a MC track.The matrix element is a bit map in which:<br><br>

    - bit 0 = 1 - Tower cluster is associated in that point
    - bit 1 = 1 - Pre shower is associated
    - bit 2 = 1 - SMD-eta is associated
    - bit 3 = 1 - SMD-phi is associated
    
3. EMC Association multimaps<br><br>

In order to make the association more powerfull and similar to TPC
association, a set of multimaps were created. Multimaps are:<br><br>

    - multiEmcTrackCluster - correlates MC tracks with clusters for each EMC detecor
    - multiEmcClusterTrack - correlates clusters with MC tracks
    - multiEmcTrackPoint - correlates MC tracks with EMC points
    - multiEmcPointTrack - correlates points with MC tracks

The use of these multimaps are similar to the TPC multimaps.

Each multimap correlates a MC track, EMC cluster or point to an EMC
association object. There are two types of association objects

    - StEmcClusterAssociation - used to associate MC tracks and EMC clusters
    - StEmcPointAssociation - used to associate MC tracks and EMC points
    
See description for these classes for more details

*/

/*!\class StEmcAssociation
\author Alexandre Suaide

Basic class for EMC association. 
*/

/*!\class StEmcClusterAssociation
\author Alexandre Suaide

This class hold association information between MC tracks and
EMC clusters. There are two Association information that
can be obtained:

  - getFractionTrack() - returns the fraction of the energy deposited
  by the track on the EMC that ended in the reconstructed cluster
  - getFractionCluster() - returns the fraction of the energy of the
  cluster that was deposited by the MC track

*/

/*!\class StEmcPointAssociation
\author Alexandre Suaide

This class hold association information between MC tracks and
EMC Points. The Association information is a bit map that corresponds
the the clusters that compose the point that could be associated to 
the MC track. The association information is a bit map in which:<br><br>

    - bit 0 = 1 - Tower cluster is associated in that point
    - bit 1 = 1 - Pre shower is associated
    - bit 2 = 1 - SMD-eta is associated
    - bit 3 = 1 - SMD-phi is associated

*/
#ifndef STAR_StEmcAssociationMaker_HH
#define STAR_StEmcAssociationMaker_HH

#define NDETECTORS 4

#ifndef StMaker_H
#include "StMaker.h"
#include "TMatrix.h"
#endif

#include <map>
#include <utility>
#if !defined(ST_NO_NAMESPACES)
using std::multimap;
using std::pair;
#endif

class StEmcRawHit;
class StEmcCluster;
class StEmcPoint;
class StMcTrack;
class StMcCalorimeterHit;

class StEmcAssociation
{
  private:
    StMcTrack        *mTrack;
  public:
                     StEmcAssociation(StMcTrack *t);
    virtual          ~StEmcAssociation();
    const StMcTrack* getTrack() const           { return mTrack; }   ///< returns pointer to the MC track
    ClassDef(StEmcAssociation, 1)
};

class StEmcClusterAssociation:public StEmcAssociation
{
  private:
    StEmcCluster     *mCluster;
    float            mFTrack;
    float            mFEmc;
  public:
                     StEmcClusterAssociation(StMcTrack*, StEmcCluster*, float,float);
    virtual          ~StEmcClusterAssociation();
    const StEmcCluster* getCluster() const      { return mCluster; }  ///< returns pointer to the EMC cluster
    float            getFractionTrack() const   { return mFTrack; }   ///< returns the fraction of the energy deposited by the track on the EMC that ended in the reconstructed cluster
    float            getFractionCluster() const { return mFEmc; }     ///< returns the fraction of the energy of the cluster that was deposited by the MC track
    ClassDef(StEmcClusterAssociation, 1)
};
class StEmcPointAssociation:public StEmcAssociation
{
  private:
    StEmcPoint       *mPoint;
    int              mAssocType;
  public:
                     StEmcPointAssociation(StMcTrack*, StEmcPoint*, int);
    virtual          ~StEmcPointAssociation();
    const StEmcPoint*getPoint() const           { return mPoint; }      ///< returns pointer to the EMC point
    int              getAssociation() const     { return mAssocType; }  ///< returns Association information between MC track and EMC point
    int              getAssociation(int det)    { return (det>0 && det<=NDETECTORS) ? ((mAssocType&(1<<(det-1)))!=0): 0; } ///<returns bit information (0,1) for each EMC subdetector
    ClassDef(StEmcPointAssociation, 1)
};

// define basic multimap structures

#ifndef __CINT__
struct compEmcTrack   { bool operator()(const StMcTrack* a, const StMcTrack* b) const {return a<b; } };
struct compEmcHit     { bool operator()(const StEmcRawHit* a, const StEmcRawHit* b) const {return a<b; }};
struct compEmcCluster { bool operator()(const StEmcCluster* a, const StEmcCluster* b) const {return a<b; }};
struct compEmcPoint   { bool operator()(const StEmcPoint* a, const StEmcPoint* b) const {return a<b; }};
// define maps and iterators
typedef  multimap<const StMcTrack*, const StEmcClusterAssociation*, compEmcTrack>      multiEmcTrackCluster;//!
typedef  multimap<const StEmcCluster*, const StEmcClusterAssociation*, compEmcCluster> multiEmcClusterTrack;//!
typedef  multimap<const StMcTrack*, const StEmcPointAssociation*, compEmcTrack>        multiEmcTrackPoint;//!
typedef  multimap<const StEmcPoint*, const StEmcPointAssociation*, compEmcPoint>       multiEmcPointTrack;//!
//defining iterators
typedef  multiEmcTrackCluster::iterator      multiEmcTrackClusterIter; //!
typedef  multiEmcClusterTrack::iterator      multiEmcClusterTrackIter; //!
typedef  multiEmcTrackPoint::iterator        multiEmcTrackPointIter;   //!
typedef  multiEmcPointTrack::iterator        multiEmcPointTrackIter;   //!
typedef  multiEmcTrackCluster::value_type    multiEmcTrackClusterValue; //!
typedef  multiEmcClusterTrack::value_type    multiEmcClusterTrackValue; //!
typedef  multiEmcTrackPoint::value_type      multiEmcTrackPointValue;   //!
typedef  multiEmcPointTrack::value_type      multiEmcPointTrackValue;   //!
#else
class multiEmcTrackCluster;
class multiEmcClusterTrack;
class multiEmcTrackPoint;
class multiEmcPointTrack;
class multiEmcTrackClusterIter;
class multiEmcClusterTrackIter;
class multiEmcTrackPointIter;
class multiEmcPointTrackIter;
class multiEmcTrackClusterValue;
class multiEmcClusterTrackValue;
class multiEmcTrackPointValue;
class multiEmcPointTrackValue;
#endif


class StEmcAssociationMaker : public StMaker
{
  public:
                                StEmcAssociationMaker(const char* name="EmcAssoc");
         virtual                ~StEmcAssociationMaker();
                 Int_t          Init();
                 Int_t          Make();
                 Int_t          Finish();
                 void           Clear(const char*);
                 
                 void           printMaps();
                 void           printTracks();
         
         multiEmcTrackCluster*  getTrackClusterMap(Int_t i)      { if(i>0 && i<=NDETECTORS) return mTrackCluster[i-1]; else return NULL; } ///< returns multimap for association betwwen MC tracks and clusters 
         multiEmcTrackCluster*  getTrackClusterMap(const char*);  ///< returns multimap for association betwwen MC tracks and clusters 
         multiEmcClusterTrack*  getClusterTrackMap(Int_t i)      { if(i>0 && i<=NDETECTORS) return mClusterTrack[i-1]; else return NULL; } ///< returns multimap for association betwwen clusters and MC tracks 
         multiEmcClusterTrack*  getClusterTrackMap(const char*);  ///< returns multimap for association betwwen clusters and MC tracks     
         multiEmcTrackPoint*    getTrackPointMap()               { return mTrackPoint;} ///< returns multimap for association betwwen MC tracks and points 
         multiEmcPointTrack*    getPointTrackMap()               { return mPointTrack;} ///< returns multimap for association betwwen points and MC tracks 
                 Int_t          getDetNum(const char*);          ///< returns detector number for each EMC sub detector
                 
                 void           setPrint(Bool_t a)               {mPrint = a;} ///< Set print log
         
         
  protected: 
         virtual Float_t        dEToEnergy(StMcCalorimeterHit*,Int_t);
                 Bool_t         mPrint;
                 
         multiEmcTrackCluster*  mTrackCluster[NDETECTORS];
         multiEmcClusterTrack*  mClusterTrack[NDETECTORS];
         multiEmcTrackPoint*    mTrackPoint;
         multiEmcPointTrack*    mPointTrack;
   ClassDef(StEmcAssociationMaker, 1)
};

#endif

/*******************************************************************************/

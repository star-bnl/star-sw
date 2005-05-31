/*!\class StEEmcAssociationMaker
Modified from StEmcAssociationMaker of Marcia Maria de Moura/Alexandre Suaide
                                         Wei-Ming Zhang   KSU
First version - 03/15/05<br><br>

This is the basic EEMC association Maker. It has two methods of association:<br><br>

1. Cluster association:<br><br>

This maker takes tracks in StMcEvent object and EMC(EEMC) clusters in
StEvent object and compare their hits to check if they match. This is done
for the EMC Tower (eemc),Pre-Shower(eprs), U and V layers of Shower Max (esmdu
and esmdv) detectors.<br><br>

It builds the following association matrices:<br><br>

    - Simple Association (association): matrix elements are 0 for negative association and 1 for positive association.

    - Track Energy Fraction (trackFraction): matrix elements are fraction of track energy in a cluster.

    - Cluster Energy Fraction (clusterFraction): matrix elements are fraction of cluster energy from a track.

  The elements of the cluster energy fraction matrix are normalized. 
Initially, the energy information of former MC hits to be in a cluster is 
the deposited energy. Before making the clusters, the deposited energy is 
converted to ADC signal by StEEmcSimulatorMaker and then is converted to 
energy by the method adcToEnergy() of StEEmcMixerMaker. The elements of
the cluster energy fraction matrix are the ratio of the summed hit energy
over the track hits that are associated to that cluster to the cluster
energy. The track hits energies that are taken in StEEmcAssociationMaker
are  obtained converting directilly the deposited energy into energy by
the method StEEmcAssociationMaker::dEToEnergy(). (de = Energy for now.
The relation will be replaced by a correct function later) So the energy 
associated to a track hit by each method is a little bit different 
leading to a normalization of the elemtents of the cluster energy fraction 
matrix.<br><br>

2. Point association:<br><br>

The Maker also does association for EMC points. It compares
the tracks hits to the clusters hits from the clusters of the 4 detectors
that composes an EEM(EEMC) point (particle). It builds a simple association
matrix are related to the type of EMC(EEMC) cluster that compose the point and
is associated to a MC track.The matrix element is a bit map in which:<br><br>

    - bit 0 = 1 - Tower cluster is associated in that point
    - bit 1 = 1 - Pre shower is associated
    - bit 2 = 1 - SMD-U is associated
    - bit 3 = 1 - SMD-V is associated
    
3. EEMC Association multimaps<br><br>

In order to make the association more powerfull and similar to TPC
association, a set of multimaps were created. Multimaps are:<br><br>

    - multiEEmcTrackCluster - correlates MC tracks with clusters for each EEMC detecor
    - multiEEmcClusterTrack - correlates clusters with MC tracks
    - multiEEmcTrackPoint - correlates MC tracks with EMC(EEMC) points
    - multiEEmcPointTrack - correlates points with MC tracks

The use of these multimaps are similar to the TPC multimaps.

Each multimap correlates a MC track, EMC(EEMC) cluster or point to an EEMC
association object. There are two types of association objects

    - StEEmcClusterAssociation - used to associate MC tracks and EMC(EEMC) clusters
    - StEEmcPointAssociation - used to associate MC tracks and EMC(EEMC) points
    
See description for these classes for more details

*/

/*!\class StEEmcAssociation

Basic class for EEMC association. 
*/

/*!\class StEEmcClusterAssociation

This class hold association information between MC tracks and
EMC(EEMC) clusters. There are two Association information that
can be obtained:

  - getFractionTrack() - returns the fraction of the energy deposited
  by the track on the EMC that ended in the reconstructed cluster
  - getFractionCluster() - returns the fraction of the energy of the
  cluster that was deposited by the MC track

*/

/*!\class StEEmcPointAssociation

This class hold association information between MC tracks and
EMC(EEMC) Points. The Association information is a bit map that corresponds
the the clusters that compose the point that could be associated to 
the MC track. The association information is a bit map in which:<br><br>

    - bit 0 = 1 - Tower cluster is associated in that point
    - bit 1 = 1 - Pre shower is associated
    - bit 2 = 1 - SMD-U is associated
    - bit 3 = 1 - SMD-V is associated

*/
#ifndef STAR_StEEmcAssociationMaker_HH
#define STAR_StEEmcAssociationMaker_HH

#define NDETECTORS 4

#ifndef StMaker_H
#include "StMaker.h"
#include "StMcEvent/StMcCalorimeterHit.hh"
#include "TMatrix.h"
#endif

#include <map>
#include <utility>
#if !defined(ST_NO_NAMESPACES)
using std::multimap;
using std::pair;
#endif

class StEvent;
class StEmcRawHit;
class StEmcCluster;
class StEmcPoint;
class StMcTrack;

class StEEmcAssociation
{
  private:
    StMcTrack        *mTrack;
  public:
                     StEEmcAssociation(StMcTrack *t);
    virtual          ~StEEmcAssociation();
    const StMcTrack* getTrack() const           { return mTrack; }   ///< returns pointer to the MC track
    ClassDef(StEEmcAssociation, 1)
};

class StEEmcClusterAssociation:public StEEmcAssociation
{
  private:
    StEmcCluster     *mCluster;
    float            mFTrack;
    float            mFEmc;
  public:
                     StEEmcClusterAssociation(StMcTrack*, StEmcCluster*, float,float);
    virtual          ~StEEmcClusterAssociation();
    const StEmcCluster* getCluster() const      { return mCluster; }  ///< returns pointer to the EMC cluster
    float            getFractionTrack() const   { return mFTrack; }   ///< returns the fraction of the energy deposited by the track on the EMC that ended in the reconstructed cluster
    float            getFractionCluster() const { return mFEmc; }     ///< returns the fraction of the energy of the cluster that was deposited by the MC track
    ClassDef(StEEmcClusterAssociation, 1)
};
class StEEmcPointAssociation:public StEEmcAssociation
{
  private:
    StEmcPoint       *mPoint;
    int              mAssocType;
  public:
                     StEEmcPointAssociation(StMcTrack*, StEmcPoint*, int);
    virtual          ~StEEmcPointAssociation();
    const StEmcPoint*getPoint() const           { return mPoint; }      ///< returns pointer to the EMC point
    int              getAssociation() const     { return mAssocType; }  ///< returns Association information between MC track and EMC point
    int              getAssociation(int det)    { return (det>0 && det<=NDETECTORS) ? ((mAssocType&(1<<(det-1)))!=0): 0; } ///<returns bit information (0,1) for each EMC subdetector
    ClassDef(StEEmcPointAssociation, 1)
};

// define basic multimap structures

#ifndef __CINT__
struct compEEmcTrack   { bool operator()(const StMcTrack* a, const StMcTrack* b) const {return a<b; } };
struct compEEmcHit     { bool operator()(const StEmcRawHit* a, const StEmcRawHit* b) const {return a<b; }};
struct compEEmcCluster { bool operator()(const StEmcCluster* a, const StEmcCluster* b) const {return a<b; }};
struct compEEmcPoint   { bool operator()(const StEmcPoint* a, const StEmcPoint* b) const {return a<b; }};
// define maps and iterators
typedef  multimap<const StMcTrack*, const StEEmcClusterAssociation*, compEEmcTrack>      multiEEmcTrackCluster;//!
typedef  multimap<const StEmcCluster*, const StEEmcClusterAssociation*, compEEmcCluster> multiEEmcClusterTrack;//!
typedef  multimap<const StMcTrack*, const StEEmcPointAssociation*, compEEmcTrack>        multiEEmcTrackPoint;//!
typedef  multimap<const StEmcPoint*, const StEEmcPointAssociation*, compEEmcPoint>       multiEEmcPointTrack;//!
//defining iterators
typedef  multiEEmcTrackCluster::iterator      multiEEmcTrackClusterIter; //!
typedef  multiEEmcClusterTrack::iterator      multiEEmcClusterTrackIter; //!
typedef  multiEEmcTrackPoint::iterator        multiEEmcTrackPointIter;   //!
typedef  multiEEmcPointTrack::iterator        multiEEmcPointTrackIter;   //!
typedef  multiEEmcTrackCluster::value_type    multiEEmcTrackClusterValue; //!
typedef  multiEEmcClusterTrack::value_type    multiEEmcClusterTrackValue; //!
typedef  multiEEmcTrackPoint::value_type      multiEEmcTrackPointValue;   //!
typedef  multiEEmcPointTrack::value_type      multiEEmcPointTrackValue;   //!
#else
class multiEEmcTrackCluster;
class multiEEmcClusterTrack;
class multiEEmcTrackPoint;
class multiEEmcPointTrack;
class multiEEmcTrackClusterIter;
class multiEEmcClusterTrackIter;
class multiEEmcTrackPointIter;
class multiEEmcPointTrackIter;
class multiEEmcTrackClusterValue;
class multiEEmcClusterTrackValue;
class multiEEmcTrackPointValue;
class multiEEmcPointTrackValue;
#endif


class StEEmcAssociationMaker : public StMaker
{
  public:
                                StEEmcAssociationMaker(const char* name="EEmcAssoc");
         virtual                ~StEEmcAssociationMaker();
                 Int_t          Init();
                 Int_t          Make();
                 Int_t          Finish();
                 void           Clear(const char*);
                 
                 void           printMaps();
         
         multiEEmcTrackCluster*  getTrackClusterMap(Int_t i)      { if(i>0 && i<=NDETECTORS) return mTrackCluster[i-1]; else return NULL; } ///< returns multimap for association betwwen MC tracks and clusters 
         multiEEmcTrackCluster*  getTrackClusterMap(const char*);  ///< returns multimap for association betwwen MC tracks and clusters 
         multiEEmcClusterTrack*  getClusterTrackMap(Int_t i)      { if(i>0 && i<=NDETECTORS) return mClusterTrack[i-1]; else return NULL; } ///< returns multimap for association betwwen clusters and MC tracks 
         multiEEmcClusterTrack*  getClusterTrackMap(const char*);  ///< returns multimap for association betwwen clusters and MC tracks     
         multiEEmcTrackPoint*    getTrackPointMap()               { return mTrackPoint;} ///< returns multimap for association betwwen MC tracks and points 
         multiEEmcPointTrack*    getPointTrackMap()               { return mPointTrack;} ///< returns multimap for association betwwen points and MC tracks 
                 Int_t          getDetNum(const char*);          ///< returns detector number for each EMC sub detector
                 
                 void           setPrint(Bool_t a)               {mPrint = a;}; ///< Set print log
     void          printHits(StEvent*); ///< Prints all EEMC hits in the StEvent object
 
         
         
  protected: 
                 Bool_t         mPrint;
                 
         multiEEmcTrackCluster*  mTrackCluster[NDETECTORS];
         multiEEmcClusterTrack*  mClusterTrack[NDETECTORS];
         multiEEmcTrackPoint*    mTrackPoint;
         multiEEmcPointTrack*    mPointTrack;
   ClassDef(StEEmcAssociationMaker, 1)
};

#endif

/*******************************************************************************/
///////////////////////////////////////////////////////////////////////////
//
// $Id: StEEmcAssociationMaker.h,v 1.1.1.1 2005/05/31 18:54:14 wzhang Exp $
// $Log: StEEmcAssociationMaker.h,v $
// Revision 1.1.1.1  2005/05/31 18:54:14  wzhang
// First version
//
//
///////////////////////////////////////////////////////////////////////////

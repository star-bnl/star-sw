#ifndef StMuFcsUtil_h
#define StMuFcsUtil_h
#include "TObject.h"

#include <map>

class StMuFcsCollection;
class StFcsCollection;
class StTriggerData;
class StMuDst;
class StFcsHit;
class StFcsCluster;
class StFcsPoint;


class StMuFcsUtil : public TObject
{
public:
  StMuFcsUtil();
  ~StMuFcsUtil();
  StMuFcsCollection* getMuFcs(StFcsCollection*);
  StFcsCollection*   getFcs(StMuFcsCollection*);
  void               fillMuFcs(StMuFcsCollection*,StFcsCollection*);
  void               fillFcs(StFcsCollection*,StMuFcsCollection*);
  
  std::map< const StFcsCluster*, StMuFcsCluster* > & getClusterMap() {return mMapClusters; }

private:

  /** Create StMuFcsHits from StFcsHits and fill StMuFcsCollection */
  void fillMuFcsHits(StMuFcsCollection*, StFcsCollection*);
  /** Create StMuFcsClusters from StFcsClusters and fill StMuFcsCollection */
  void fillMuFcsClusters(StMuFcsCollection*, StFcsCollection*);
  /** Create StMuFcsPoints from StFcsPoints and fill StMuFcsCollection */
  void fillMuFcsPoints(StMuFcsCollection*, StFcsCollection*);
  /** Create StMuFcsInfo from reconstruction flag and det indices */
  void fillMuFcsInfo(StMuFcsCollection*, StFcsCollection*);
  /** Set the relationships in StMu */
  void rebuildRelationships(StFcsCollection*, StMuFcsCollection*);
  /** fill the StFcsHits from MuDst */
  void fillFcsHits(StFcsCollection*, StMuFcsCollection*);

  /** holds relation between StEvent and StMuDst types (in memory)**/
  std::map< const StFcsHit*, StMuFcsHit* > mMapHits;
  std::map< const StFcsCluster*, StMuFcsCluster* > mMapClusters;
  std::map< const StFcsPoint*, StMuFcsPoint* > mMapPoints;

  ClassDef(StMuFcsUtil,1)
};

#endif

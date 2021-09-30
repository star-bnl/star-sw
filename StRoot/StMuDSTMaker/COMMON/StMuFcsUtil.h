#ifndef StMuFcsUtil_h
#define StMuFcsUtil_h
#include "TObject.h"

#include <map>

class StMuFcsCollection;
class StFcsCollection;
// class StFcsDbMaker;
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
  

private:

  /** Create StMuFcsHits from StFcsHits and fill StMuFcsCollection */
  void fillMuFcsHits(StMuFcsCollection*, StFcsCollection*);
  /** Create StMuFcsClusters from StFcsClusters and fill StMuFcsCollection */
  void fillMuFcsClusters(StMuFcsCollection*, StFcsCollection*);
  /** Create StMuFcsPoints from StFcsPoints and fill StMuFcsCollection */
  void fillMuFcsPoints(StMuFcsCollection*, StFcsCollection*);
  /** Set the relationships in StMu */
  void rebuildRelationships(StFcsCollection*, StMuFcsCollection*);
  /** fill the StFcsHits from MuDst */
  void fillFcsHits(StFcsCollection*, StMuFcsCollection*);

  /** holds relation between StEvent and StMuDst types (in memory)**/
  map< const StFcsHit*, TObject* > mMapHits;
  map< const StFcsCluster*, TObject* > mMapClusters;
  map< const StFcsPoint*, TObject* > mMapPoints;

  ClassDef(StMuFcsUtil,1)
};

#endif

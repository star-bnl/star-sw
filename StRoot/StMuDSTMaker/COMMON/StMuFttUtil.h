#ifndef StMuFttUtil_h
#define StMuFttUtil_h

#include <TObject.h>
#include <map>

class StMuFttCollection;
class StFttCollection;
class StMuDst;
class StFttRawHit;
class StFttCluster;
class StFttPoint;


class StMuFttUtil : public TObject
{
public:
  StMuFttUtil();
  ~StMuFttUtil();
  StMuFttCollection* getMuFtt(StFttCollection*);
  StFttCollection*   getFtt(StMuFttCollection*);
  void               fillMuFtt(StMuFttCollection*,StFttCollection*);
  void               fillFtt(StFttCollection*,StMuFttCollection*);
  

private:

  /** Create StMuFttRawHits from StFttRawHits and fill StMuFttCollection */
  void fillMuFttRawHits(StMuFttCollection*, StFttCollection*);
  /** Create StMuFttClusters from StFttClusters and fill StMuFttCollection */
  void fillMuFttClusters(StMuFttCollection*, StFttCollection*);
  /** Create StMuFttPoints from StFttPoints and fill StMuFttCollection */
  void fillMuFttPoints(StMuFttCollection*, StFttCollection*);
  /** Set the relationships in StMu */
  void rebuildRelationships(StFttCollection*, StMuFttCollection*);
  /** fill the StFttRawHits from MuDst */
  void fillFttRawHits(StFttCollection*, StMuFttCollection*);

  /** holds relation between StEvent and StMuDst types (in memory)**/
  map< const StFttRawHit*, TObject* > mMapHits;
  map< const StFttCluster*, TObject* > mMapClusters;
  map< const StFttPoint*, TObject* > mMapPoints;

  ClassDef(StMuFttUtil,1)
};

#endif

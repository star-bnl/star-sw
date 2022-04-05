 /***************************************************************************
  *
  * $Id: StMuFmsUtil.h,v 1.4 2018/05/24 01:13:37 jdb Exp $
  *
  * Author: Jingguo Ma, Jan 2010
  ***************************************************************************
  *
  * Description: FMS Util to convert between StEvent and MuDst
  *
  ***************************************************************************
  *
  * $Log: StMuFmsUtil.h,v $
  * Revision 1.4  2018/05/24 01:13:37  jdb
  * Move fillFmsHits to public
  *
  * Revision 1.3  2017/08/14 16:22:36  smirnovd
  * Recover FMS hits using StTriggerData
  *
  * commit 6d7358f4c86a15edd0671326580d291a9843aec9
  * Date:   Tue Aug 8 23:42:41 2017 -0400
  *
  *     StMuFmsUtil: Recover FMS hits using StTriggerData
  *
  * commit 556d07cb8fd87cb62e4ac674226423671c94917d
  * Date:   Tue Aug 8 23:42:34 2017 -0400
  *
  *     StMuFmsUtil: Added func to fill StMuFmsCollection with FMS hits from StTriggerData in StMuEvent
  *
  * commit c355529c1ee401849b2b81d74df8d452886593d1
  * Date:   Tue Aug 8 23:42:19 2017 -0400
  *
  *     [Cosmetic] Changes in whitespace
  *
  * commit 67fdc1b348bebbfbfb137b726ee9c455a7d8be37
  * Date:   Mon Jun 5 12:00:24 2017 -0400
  *
  *     StMuFmsCollection::addHit() Return pointer to just added default FMS hit object
  *
  * Revision 1.2  2015/08/28 18:36:04  jdb
  * Added Akios FMS codes
  *
  * Revision 1.1  2010/01/25 03:57:39  tone421
  * Added FMS and Roman pot arrays
  *
  **************************************************************************/
 #ifndef StMuFmsUtil_h
 #define StMuFmsUtil_h
 #include "TObject.h"

 class StMuFmsCollection;
 class StFmsCollection;
 class StFmsDbMaker;
 class StTriggerData;
 class StMuDst;


class StMuFmsUtil : public TObject
{
public:
  StMuFmsUtil();
  ~StMuFmsUtil();
  StMuFmsCollection* getMuFms(StFmsCollection*);
  StFmsCollection*   getFms(StMuFmsCollection*);
  void               fillMuFms(StMuFmsCollection*,StFmsCollection*);
  void               fillFms(StFmsCollection*,StMuFmsCollection*);
  void               fillFmsHits(StFmsCollection*, StMuFmsCollection*);

  /**
   * Creates `StMuFmsHit`s from the StTriggerData object and appends them to the
   * StMuFmsCollection.
   */
  static void fillMuFmsHits(StMuFmsCollection&, const StTriggerData&, const StFmsDbMaker* = nullptr);

  /**
   * In the provided `muDst` object fills StMuFmsCollection with FMS hits
   * extracted from the StTriggerData block in the same `muDst`.
   */
  static void recoverMuFmsCollection(StMuDst& muDst, const StFmsDbMaker* = nullptr);

private:

  /** Create StMuFmsHits from StFmsHits and fill StMuFmsCollection */
  void fillMuFmsHits(StMuFmsCollection*, StFmsCollection*);
  /** Create StMuFmsClusters from StFmsClusters and fill StMuFmsCollection */
  void fillMuFmsClusters(StMuFmsCollection*, StFmsCollection*);
  /** Create StMuFmsPoints from StFmsPoints and fill StMuFmsCollection */
  void fillMuFmsPoints(StMuFmsCollection*, StFmsCollection*);
  /** Set the parent clusters of StMuFmsPoints */
  void setMuFmsPointParentClusters(StMuFmsCollection*, StFmsCollection*);

  /** Create StFmsClusters from StMuFmsClusters and fill StFmsCollection */
  void fillFmsClusters(StFmsCollection*, StMuFmsCollection*);
  /** Create StFmsPoints from StMuFmsPoints and fill StFmsCollection */
  void fillFmsPoints(StFmsCollection*, StMuFmsCollection*);
  /** Set the parent clusters of StFmsPoints */
  void setFmsPointParentClusters(StFmsCollection*, StMuFmsCollection*);

  ClassDef(StMuFmsUtil,1)
};

#endif

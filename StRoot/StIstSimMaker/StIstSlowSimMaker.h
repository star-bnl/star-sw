/***************************************************************************
*
* $Id: StIstSlowSimMaker.h,v 1.2 2018/03/15 21:38:24 dongx Exp $
*
* Author: Leszek Kosarzewski, March 2014
****************************************************************************
* Description: 
* Slow simulation maker for IST.
****************************************************************************
* StIstSlowSimMaker.h,v 1.1
* Revision 1.1 2014/08/05 10:54:12 ypwang
* Make it compatible with other IST software
* Update after tests in the BFC chain
****************************************************************************
* StIstSlowSimMaker.h,v 1.0
* Revision 1.0 2014/03/07 11:25:30 lkosarz
* Initial version
****************************************************************************/

#ifndef STAR_StIstSlowSimMaker
#define STAR_StIstSlowSimMaker
#include <StMaker.h>
#include <StThreeVectorF.hh>
#include <StThreeVectorD.hh>
#include <vector>


class TRandom3;
class StEvent;
class StMcEvent;
class StMcIstHitCollection;
class St_g2t_ist_hit;
class StIstDb;
class StIstRawHit;
class StMcIstHit;
class StIstCollection;

class TString;
class TF1;

/**
 * Slow simulation maker for IST. IST GEANT hit is transformed to either ideal
 * or misaligned geometry of realistic detector, raw adc is calculated by the dE of GEANT hit deposited in IST sensor. Control table and timebins are applied to hits for embedding purpose. 
 *
 * \author: Leszek Kosarzewski 
 * \date Mar 2014 
 */

class StIstSlowSimMaker : public StMaker {
 public:

  StIstSlowSimMaker(const char *name="istSlowSim");
  Int_t Make();
  Int_t Init();
  Int_t InitRun( Int_t runNumber );
  void Clear( Option_t *opts = "" );

  virtual const char *GetCVS() const
  {
    static const char cvs[]="Tag $Name:  $ $Id: StIstSlowSimMaker.h,v 1.2 2018/03/15 21:38:24 dongx Exp $ built " __DATE__ " " __TIME__ ;
    return cvs;
  }
  
  void buildIdealGeom(const Bool_t isIdealGeom) {mBuildIdealGeom = isIdealGeom;}

 protected:
  StIstDb *mIstDb;
  Bool_t mBuildIdealGeom;

  StIstCollection *mIstCollectionPtr;
  TF1 *fAdc;

  //! control paramters
  UChar_t mDefaultTimeBin, mCurrentTimeBinNum;
  //! mapping
  typedef std::vector< Int_t > MappingGeomVec_t; //!Geometry ID -> channel elec. index
  MappingGeomVec_t mMappingGeomVec;
  
  //! single hit efficiency - tunable parameter
  short mHitEffMode;
  float mMomCut;
  float mHitEff;
  TRandom3* mRndGen;
      
  void getMCHitRowAndColumn(const StMcIstHit *istMChit, UShort_t &meanColumn, UShort_t &meanRow) const;
  void checkPadCrossing(const StThreeVectorD inPos, const StThreeVectorD outPos, StThreeVectorD mcLocalDir, Double_t dS, vector<StThreeVectorD> &cross_vec) const;
  void transformToSensor(StThreeVectorD &hitPos) const;
  Double_t distanceToBorder(const StThreeVectorD hitPos, const StThreeVectorD dir, const StThreeVectorD mean, StThreeVectorD &dist) const;
  void findPad(const StThreeVectorD hitPos, UShort_t &column, UShort_t &row, Double_t &rPhiPos_mean, Double_t &zPos_mean) const;

  private:
  void generateRawHits(const StMcIstHit *istMChit) const;
  Double_t direction(const Double_t x) const;
  Double_t scaleFromYvsX(const StThreeVectorD vec, const Double_t a) const;
  Double_t scaleFromYvsZ(const StThreeVectorD vec, const Double_t a) const;

  ClassDef(StIstSlowSimMaker,0)   //StAF chain virtual base class for Makers
};

#endif

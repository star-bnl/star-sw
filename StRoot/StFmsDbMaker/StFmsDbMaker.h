/***************************************************************************
 * $Id: StFmsDbMaker.h,v 1.13 2016/06/08 19:58:03 akio Exp $
 * \author: akio ogawa
 ***************************************************************************
 *
 * Description: FMS DB access Maker
 * Please visit http://drupal.star.bnl.gov/STAR/subsys/fms/database/stfmsdbmaker for more information
 *
 ***************************************************************************
 *
 * $Log: StFmsDbMaker.h,v $
 * Revision 1.13  2016/06/08 19:58:03  akio
 * Applying Coverity report
 *
 * Revision 1.12  2015/11/10 19:06:02  akio
 * Adding TimeDepCorr for LED gain correction based on event#
 *
 * Revision 1.11  2015/10/20 19:49:28  akio
 * Fixing distanceFromEdge()
 * Adding readRecParamFromFile()
 *
 * Revision 1.10  2015/09/23 17:34:01  akio
 * Adding distanceFromEdge() for fiducial volume cut
 *
 * Revision 1.9  2015/09/18 18:34:35  akio
 * Adding getStarXYZfromColumnRow() to convert from local grid space [cell width unit, not cm]
 * Adding protection for fmsGain and fmsGainCorrection when table length get shorter and can
 * overwritten by old values.
 * Removing some error log
 *
 * Revision 1.8  2015/09/02 14:45:14  akio
 * Adding new functions for un-uniform grid cell positions, switched based on DB fmsPositionModel
 *
 * Revision 1.4  2014/08/06 11:43:15  jeromel
 * Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
 *
 * Revision 1.3  2014/04/24 21:15:04  tpb
 * Change default name to fmsDb to match BFC
 *
 * Revision 1.2  2010/01/11 20:35:30  jgma
 * Added reversed map and some other minor updates
 *
 * Revision 1.1  2009/10/28 16:11:15  jgma
 * This is the first check in of the code.
 *
 **************************************************************************/

#ifndef STFMSDBMAKER_H
#define STFMSDBMAKER_H

#ifndef StMaker_H
#include "StMaker.h"
#endif
#include "StThreeVectorF.hh"
#include "StRoot/StFmsUtil/StFmsDbConfig.h"

struct fmsDetectorPosition_st;
struct fmsChannelGeometry_st;
struct fmsMap_st;
struct fmsPatchPanelMap_st;
struct fmsQTMap_st;
struct fmsGain_st;
struct fmsGainCorrection_st;
struct fmsTimeDepCorr_st;
struct fmsRec_st;
struct fpsConstant_st;
struct fpsChannelGeometry_st;
struct fpsSlatId_st;
struct fpsPosition_st;
struct fpsMap_st;
struct fpsGain_st;
struct fpsStatus_st;

class StFmsHit;
class StFmsPoint;

class StFmsDbMaker : public StMaker {
 public: 
  StFmsDbMaker(const Char_t *name="fmsDb");
  virtual       ~StFmsDbMaker();
  virtual Int_t  Init();
  virtual Int_t  InitRun(Int_t runNumber);
  virtual Int_t  Make();
  virtual Int_t  Finish();
  virtual void   Clear(const Char_t *opt);
  
  void setDebug(Int_t debug); ///< debug mode, 0 for minimal message, >0 for more debug messages

  //! getting the whole table
  fmsChannelGeometry_st*  ChannelGeometry();
  fmsDetectorPosition_st* DetectorPosition();
  fmsMap_st*              Map();
  fmsPatchPanelMap_st*    PatchPanelMap();
  fmsQTMap_st*            QTMap();
  fmsGain_st*             Gain();
  fmsGainCorrection_st*   GainCorrection();
  fmsTimeDepCorr_st*      TimeDepCorr();
  fmsRec_st*              RecPar(); //reconstruction related parameters
  fpsConstant_st*         FpsConstant();
  fpsChannelGeometry_st** FpsChannelGeometry();
  fpsSlatId_st*           FpsSlatId();
  fpsPosition_st*         FpsPosition();
  fpsMap_st*              FpsMap();
  fpsGain_st*             FpsGain();
  fpsStatus_st*           FpsStatus();

  //! Utility functions related to FMS ChannelGeometry
  UShort_t maxDetectorId(); //! maximum value of detector Id
  Int_t detectorId(Int_t ew, Int_t ns, Int_t type);  //! convert to detector Id
  Int_t eastWest(Int_t detectorId); //! east or west to the STAR IP
  Int_t northSouth(Int_t detectorId); //! north or south side
  Int_t largeSmall(Int_t detectorId); //! large or small cells for FMS
  Int_t type(Int_t detectorId); //! type of the detector
  Int_t nRow(Int_t detectorId); //! number of rows
  Int_t nColumn(Int_t detectorId); //! number of column
  UShort_t maxChannel(Int_t detectorId); //! maximum number of channels
  Int_t getRowNumber(Int_t detectorId, Int_t ch); //! get the row number for the channel
  Int_t getColumnNumber(Int_t detectorId, Int_t ch); //! get the column number for the channel
  Int_t getChannelNumber(Int_t detectorId, Int_t row, Int_t column); //! get the channel number

  //! Utility functions related to DetectorPosition
  StThreeVectorF getDetectorOffset(Int_t detectorId);  //! get the offset of the detector
  Float_t getXWidth(Int_t detectorId); //! get the X width of the cell
  Float_t getYWidth(Int_t detectorId); //! get the Y width of the cell
  StThreeVectorF getStarXYZ(Int_t detectorId,Float_t FmsX, Float_t FmsY);   //! get the STAR frame coordinates from local X/Y [cm]
  StThreeVectorF getStarXYZ(Int_t detectorId,Double_t FmsX, Double_t FmsY); //! get the STAR frame coordinates from local X/Y [cm]
  StThreeVectorF getStarXYZ(Int_t detectorId,Int_t column, int row);        //! get the STAR frame coordinates from column/row
  StThreeVectorF getStarXYZfromColumnRow(Int_t detectorId,Float_t column, Float_t row); 
                                                                            //! get the STAR frame coordinates from column/row grid space [unit is cell size]
  StThreeVectorF getStarXYZ(StFmsHit*);                                     //! get the STAR frame coordinates from StFmsHit
  StThreeVectorF getStarXYZ(Int_t detectorId,Int_t ch);                     //! get the STAR frame cooridnates for center of the cell
  Float_t getPhi(Int_t detectorId,Float_t FmsX, Float_t FmsY);              //! get the STAR frame phi angle from from local X/Y [cm]
  Float_t getEta(Int_t detectorId,Float_t FmsX, Float_t FmsY, Float_t Vertex); //! get the STAR frame pseudo rapidity from the vertex from local X/Y [cm]
  
  // Distance(unit is incell space) from edge for given local X/Y [cm] for fiducial volume cut                        
  // return negative distance if inside, positive outside
  // edge: 0=well inside (more than 1 cell)
  //       1=inner edge
  //       2=outer edge
  //       3=between north and south
  //       4=between small and large
  //       4=large cell corner
  Float_t distanceFromEdge(Int_t det,Float_t x, Float_t y, int& edge);
  Float_t distanceFromEdge(StFmsPoint* point, int& edge);
  Int_t nCellHole(Int_t det);
  Int_t nCellCorner(Int_t det);

  //! fmsMap related
  Int_t maxMap();
  void getMap(Int_t detectorId, Int_t ch, Int_t* qtCrate, Int_t* qtSlot, Int_t* qtChannel);
  void getReverseMap(Int_t qtCrate, Int_t qtSlot, Int_t qtChannel, Int_t* detectorId, Int_t* ch);

  //! fmsPatchPanelMap related
  Int_t maxModule();

  //! fmsQTMap related
  Int_t maxNS();

  //! fmsGain/GainCorrection related
  Int_t maxGain();
  Int_t maxGainCorrection();
  Float_t getGain(Int_t detectorId, Int_t ch); //! get the gain for the channel
  Float_t getGainCorrection(Int_t detectorId, Int_t ch); //! get the gain correction for the channel
    
  void forceUniformGain(float v)           {mForceUniformGain=v;          } //! force gain to be specified value               
  void forceUniformGainCorrection(float v) {mForceUniformGainCorrection=v;} //! force gaincorr to be specified value
  void readGainFromText(int v=1)           {mReadGainFile=v;}               //! force gain to be read from FmsGain.txt

  //! fmsTimeDepCorr relayed
  float getTimeDepCorr(int event, int det, int ch); //det = detectorId - 8 (0=largeNoth, 1=largeSouth, 2=smallNorth, 3=smallSouth)
                                                    //ch=1 to 578

  //reference to StFmsDbConfig
  StFmsDbConfig& getRecConfig();  
  void readRecParamFromFile(int v=1){mReadRecParam=v;} // Read fmsrecpar.txt for reconstuction parameters

  //! FPS related
  Int_t   fpsNQuad();
  Int_t   fpsNLayer();
  Int_t   fpsMaxSlat();
  Int_t   fpsMaxQTaddr();
  Int_t   fpsMaxQTch();
  Int_t   fpsMaxSlatId();
  Int_t   fpsNSlat(int quad, int layer);
  Int_t   fpsSlatId(int quad, int layer, int slat);
  void    fpsQLSfromSlatId(int slatid, int* quad, int* layer, int* slat);
  void    fpsPosition(int slatid, float xyz[3], float dxyz[3]);
  void    fpsPosition(int quad, int layer, int slat, float xyz[3], float dxyz[3]);
  void    fpsQTMap(int slatid, int* QTaddr, int* QTch);
  Int_t   fpsSlatidFromQT(int QTaddr, int QTch);
  void    fpsQLSFromQT(int QTaddr, int QTch, int* quad, int* layer, int* slat);
  Float_t fpsGain(int slatid);
  Float_t fpsGain(int quad, int layer, int slat);
  UShort_t fpsStatus(int slatid);
  UShort_t fpsStatus(int quad, int layer, int slat);
  Int_t   fpsSlatIdFromG2t(int g2tvolid);

  //! text dump for debugging
  void dumpFmsChannelGeometry (const Char_t* filename="dumpFmsChannelGeometry.txt");
  void dumpFmsDetectorPosition(const Char_t* filename="dumpFmsDetectorPosition.txt");
  void dumpFmsMap             (const Char_t* filename="dumpFmsMap.txt");
  void dumpFmsPatchPanelMap   (const Char_t* filename="dumpFmsPatchPanelMap.txt");
  void dumpFmsQTMap           (const Char_t* filename="dumpFmsQTMap.txt");
  void dumpFmsGain            (const Char_t* filename="dumpFmsGain.txt");
  void dumpFmsGainCorrection  (const Char_t* filename="dumpFmsGainCorrection.txt");
  void dumpFmsTimeDepCorr     (const Char_t* filename="dumpFmsTimeDepCorr.txt");
  void dumpFpsConstant        (const Char_t* filename="dumpFpsConstant.txt");
  void dumpFpsChannelGeometry (const Char_t* filename="dumpFpsChannelGeometry.txt");
  void dumpFpsSlatId          (const Char_t* filename="dumpSlatId.txt"); 
  void dumpFpsPosition        (const Char_t* filename="dumpFpsPosition.txt");
  void dumpFpsMap             (const Char_t* filename="dumpFpsMap.txt");
  void dumpFpsGain            (const Char_t* filename="dumpFpsGain.txt");
  void dumpFpsStatus          (const Char_t* filename="dumpFpsStatus.txt");
  void dumpFmsRec             (const Char_t* filename="dumpFmsRec.txt");

 private:
  void                  deleteArrays();
  Int_t                 mDebug=0; //! >0 dump tables to text files

  fmsChannelGeometry_st *mChannelGeometry=0;  //! channel configuration for each detector
  UShort_t              mMaxDetectorId=0;     //! max detector Id

  fmsDetectorPosition_st *mDetectorPosition=0;  //! position (in STAR frame) of each detector

  unsigned int          mPositionModel=0; //! Position model (0=uniform, 1=run15pp, 2=run15pA)

  fmsMap_st             *mMap=0;    //! detector map
  fmsMap_st             **mmMap=0;
  Int_t                  mMaxMap=0;
  enum {mMaxCrate=8, mMaxSlot=17, mMaxCh=32};
  Int_t                  mReverseMapDetectorId[mMaxCrate][mMaxSlot][mMaxCh]; //!
  Int_t                  mReverseMapChannel[mMaxCrate][mMaxSlot][mMaxCh];    //!

  fmsPatchPanelMap_st   *mPatchPanelMap=0; //! patch panel map
  Int_t                  mMaxModule=0;

  fmsQTMap_st           *mQTMap=0;  //! Qt map
  Int_t                  mMaxNS=0;

  fmsGain_st            *mGain=0;   //! gain table
  fmsGain_st            **mmGain=0; 
  Int_t                  mMaxGain=0;

  fmsGainCorrection_st  *mGainCorrection=0; //! gain correction table
  fmsGainCorrection_st  **mmGainCorrection=0;
  Int_t                   mMaxGainCorrection=0;

    enum {mFmsTimeDepMaxData=20000,mFmsTimeDepMaxTimeSlice=200,mFmsTimeDepMaxDet=4,mFmsTimeDepMaxCh=578};
  fmsTimeDepCorr_st     *mTimeDepCorr=0;
  int mMaxTimeSlice=0;
  int mTimeDepEvt[mFmsTimeDepMaxTimeSlice];    
  float mTimeDep[mFmsTimeDepMaxTimeSlice][mFmsTimeDepMaxDet][mFmsTimeDepMaxCh];

  fmsRec_st             *mRecPar=0; //! rec. parameters table
  Int_t                 mMaxRecPar=0;
  StFmsDbConfig&        mRecConfig; //reference to StFmsDbConfig singleton, for accessing rec. parameter values by name

  Float_t                 mForceUniformGain=0.0; //!
  Float_t                 mForceUniformGainCorrection=0.0; //!
  Int_t                   mReadGainFile=0; //!             

  Int_t                   mReadRecParam=0; //!

  fpsConstant_st*         mFpsConstant=0;
  Int_t                   mMaxSlatId=0;
  fpsChannelGeometry_st** mFpsChannelGeometry=0;
  fpsSlatId_st*           mFpsSlatId=0;
  Int_t***                mFpsReverseSlatId=0;
  fpsPosition_st*         mFpsPosition=0;
  fpsMap_st*              mFpsMap=0;
  Int_t**                 mFpsReverseMap=0;
  fpsGain_st*             mFpsGain=0;
  fpsStatus_st*           mFpsStatus=0;
  
  virtual const Char_t *GetCVS() const {static const Char_t cvs[]="Tag " __DATE__ " " __TIME__ ; return cvs;}
  ClassDef(StFmsDbMaker,2)   //StAF chain virtual base class for Makers
};

#endif
  


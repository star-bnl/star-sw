#ifndef STAR_StSvtAlignmentMaker
#define STAR_StSvtAlignmentMaker

#ifndef StMaker_H
#include "StMaker.h"
#endif

class StSvtConfig;
class StSsdAlign;
class StSvtCoordinateTransform;
class srs_srspar_st;
class svg_shape_st;
class svg_geom_st;
class TString;

class TH1F;
class TH2F;

class StSvtAlignmentMaker : public StMaker
{
 public:
  StSvtAlignmentMaker(const char* name = "SvtAligmentMaker");
  ~StSvtAlignmentMaker();

  virtual Int_t Init();
  virtual Int_t Make();
  virtual Int_t Finish();

  Int_t setConfig(const char* config);
  Int_t setConfig(StSvtConfig* config);

private:
 
  StSvtConfig                  *mConfig;             //!
  StSvtCoordinateTransform     *mCoordTransform;     //!
  srs_srspar_st                *mSvtSrsParT;         //!
  svg_shape_st                 *mSvtShapeT;          //!
  svg_geom_st                  *mSvtGeomT;           //!     
  TString                      mConfigString;  

  int NumberEvents;
  int NumberOfEventsSoFar;
  int DataType;

  double unalign[536][6];  //! What align params should be

  StSsdAlign    *work; //!

  TH2F *hParams2d[6];
  TH1F *dxError;
  TH1F *dyError;
  TH1F *dzError;
  TH1F *alphaError;
  TH1F *betaError;
  TH1F *gammaError;

  ClassDef(StSvtAlignmentMaker,1)

};

#endif

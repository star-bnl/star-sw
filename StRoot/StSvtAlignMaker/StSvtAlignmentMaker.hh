/***************************************************************************
 *
 * $Id: StSvtAlignmentMaker.hh,v 1.3 2004/07/29 14:59:36 caines Exp $
 *
 * Author: Helen Caines
 ***************************************************************************
 *
 * Description: Interface to SVT & SSD alignment code
 *
 ***************************************************************************
 *
 * $Log: StSvtAlignmentMaker.hh,v $
 * Revision 1.3  2004/07/29 14:59:36  caines
 * Fix Alignmentmaker with new param settings
 *
 * Revision 1.2  2001/05/09 16:33:02  gaudiche
 * bug on Solaris fixed - cleanup
 *
 *
 ***************************************************************************/

#ifndef STAR_StSvtAlignmentMaker
#define STAR_StSvtAlignmentMaker

#ifndef StMaker_H
#include "StMaker.h"
#endif

class StSvtConfig;
class StSsdAlign;
class StSvtCoordinateTransform;
class StSvtHybridCollection;
class StSvtT0;
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
  Int_t setDataType(int val);
  Int_t setNumberOfEvents(int val);

private:
 
  StSvtConfig                  *mConfig;             //!
  StSvtCoordinateTransform     *mCoordTransform;     //!
  srs_srspar_st                *mSvtSrsParT;         //!
  svg_shape_st                 *mSvtShapeT;          //!
  svg_geom_st                  *mSvtGeomT;           //!     
  TString                      mConfigString;  
  StSvtHybridCollection *m_driftVeloc; //!
  StSvtHybridCollection *m_driftCurve; //!
  StSvtT0 *m_t0; //!
  int NumberEvents;
  int NumberOfEventsSoFar;
  int DataType;
  int alignGroup;
  int cosmicInAlignGroup;

  double unalign[536][6];  //! What align params should be

  StSsdAlign    *work; //!

  TH2F *hParams2d[6];
  TH1F *dxError;
  TH1F *dyError;
  TH1F *dzError;
  TH1F *alphaError;
  TH1F *betaError;
  TH1F *gammaError;
  TH1F *tetaDistribution;
  TH1F *nHitsPerTrackDistribution;
  TH1F *chi2Distribution;

  ClassDef(StSvtAlignmentMaker,1)

};

#endif

// $Id: StFtpcDbReader.hh,v 1.1 2001/03/06 23:34:06 jcs Exp $
//
// $Log: StFtpcDbReader.hh,v $
// Revision 1.1  2001/03/06 23:34:06  jcs
// use database instead of params
//
//

#ifndef STAR_StFtpcDbReader
#define STAR_StFtpcDbReader

#include "TObject.h"

#include "tables/St_ftpcPadrowZ_Table.h"
#include "tables/St_ftpcEField_Table.h"
#include "tables/St_ftpcVDrift_Table.h"
#include "tables/St_ftpcDeflection_Table.h"
#include "tables/St_ftpcdVDriftdP_Table.h"
#include "tables/St_ftpcdDeflectiondP_Table.h"
#include "tables/St_ftpcAmpSlope_Table.h"
#include "tables/St_ftpcAmpOffset_Table.h"
#include "tables/St_ftpcTimeOffset_Table.h"

class StFtpcParamReader;

class StFtpcDbReader : public TObject 
{
  
protected:
  //ClusterFinder parameters (also used by other classes)
  Float_t *mPadrowZPosition;
  Float_t *mMagboltzEField;
  Float_t *mMagboltzVDrift;
  Float_t *mMagboltzDeflection;
  Float_t *mMagboltzdVDriftdP;
  Float_t *mMagboltzdDeflectiondP;

  ftpcAmpSlope_st   *ampslopeTable;
  ftpcAmpOffset_st  *ampoffsetTable;
  ftpcTimeOffset_st *timeoffsetTable;

private:
  StFtpcParamReader *mParam;
  
public:
  // constructor used by StFtpcClusterMaker:
  StFtpcDbReader(StFtpcParamReader    *paramReader,
                 St_ftpcPadrowZ       *zrow,
                 St_ftpcEField        *efield,
                 St_ftpcVDrift        *vdrift,
                 St_ftpcDeflection    *deflection,
                 St_ftpcdVDriftdP     *dvdriftdp,
                 St_ftpcdDeflectiondP *ddeflectiondp,
                 St_ftpcAmpSlope      *ampslope,
                 St_ftpcAmpOffset     *ampoffset,
                 St_ftpcTimeOffset    *timeoffset);
  // constructor used by StFtpcSlowSimMaker and StFtpcDriftMapMaker:
  StFtpcDbReader(StFtpcParamReader    *paramReader,
                 St_ftpcPadrowZ       *zrow,
                 St_ftpcEField        *efield,
                 St_ftpcVDrift        *vdrift,
                 St_ftpcDeflection    *deflection,
                 St_ftpcdVDriftdP     *dvdriftdp,
                 St_ftpcdDeflectiondP *ddeflectiondp);
  ~StFtpcDbReader();
  Float_t padrowZPosition(Int_t i); 
  Float_t magboltzEField(Int_t i);
  Float_t magboltzVDrift(Int_t i,Int_t padrow);
  Float_t magboltzDeflection(Int_t i, Int_t padrow);
  Float_t magboltzdVDriftdP(Int_t i, Int_t padrow);
  Float_t magboltzdDeflectiondP(Int_t i, Int_t padrow);
  Float_t amplitudeSlope(Int_t i, Int_t padrow);
  Float_t amplitudeOffset(Int_t i, Int_t padrow);
  Float_t timeOffset(Int_t i, Int_t padrow);

  // parameter set functions
  Int_t setMagboltzEField(Int_t i, Float_t newvalue);
  Int_t setMagboltzVDrift(Int_t i, Int_t padrow, Float_t  newvalue);
  Int_t setMagboltzDeflection(Int_t i, Int_t padrow, Float_t  newvalue);
  Int_t setMagboltzdVDriftdP(Int_t i, Int_t padrow, Float_t  newvalue);
  Int_t setMagboltzdDeflectiondP(Int_t i, Int_t padrow, Float_t  newvalue);
};

#endif

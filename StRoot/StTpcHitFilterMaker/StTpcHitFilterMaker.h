// $Id: StTpcHitFilterMaker.h,v 1.1 2001/03/22 19:55:30 hardtke Exp $
// $Log: StTpcHitFilterMaker.h,v $
// Revision 1.1  2001/03/22 19:55:30  hardtke
// Initial version of hit filtering maker
//
#ifndef STAR_StTpcHitFilterMaker
#define STAR_StTpcHitFilterMaker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StTpcHitFilterMaker virtual base class for Maker                            //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifndef StMaker_H
#include "StMaker.h"
#endif


class StTpcHitFilterMaker : public StMaker {
private:
//static Char_t m_VersionCVS = "$Id: StTpcHitFilterMaker.h,v 1.1 2001/03/22 19:55:30 hardtke Exp $";
  Bool_t SectorOn[24];
  Bool_t RowOn[45];
  float z_min;
  float z_max;
  float membrane_cut;
  int minrow;
  int maxrow;

protected:


public: 
  StTpcHitFilterMaker(const char *name="tpc_hit_filter");
  virtual       ~StTpcHitFilterMaker();

  virtual Int_t  Init();
  virtual Int_t  Make();
  virtual Int_t  Finish();
  virtual void   SetZrange(float min, float max);
  virtual void   SetZmax(float max);
  virtual void   SetZmin(float min);
  virtual void   SetMembraneCut(float z_membrane); //cut near central membrane.
  virtual void   DisableInner();
  virtual void   EnableInner();
  virtual void   DisableOuter();
  virtual void   EnableOuter();
  virtual void   SetMinRow(int row);
  virtual void   SetMaxRow(int row);
  virtual void   SetRowRange(int row_inner, int row_outer);
  virtual void   DisableRow(int row);
  virtual void   EnableRow(int row);
  virtual void   DisableSector(int sector);
  virtual void   EnableSector(int sector);
  virtual void   EastOn(); 
  virtual void   EastOff();
  virtual void   WestOn(); 
  virtual void   WestOff();

  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StTpcHitFilterMaker.h,v 1.1 2001/03/22 19:55:30 hardtke Exp $ built "__DATE__" "__TIME__ ; return cvs;}

 ClassDef(StTpcHitFilterMaker, 1)   //StAF chain virtual base class for Makers
};

#endif

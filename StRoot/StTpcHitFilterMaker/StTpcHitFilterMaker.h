// $Id: StTpcHitFilterMaker.h,v 1.4 2003/09/10 19:47:40 perev Exp $
// $Log: StTpcHitFilterMaker.h,v $
// Revision 1.4  2003/09/10 19:47:40  perev
// ansi corrs
//
// Revision 1.3  2001/04/13 21:34:46  hardtke
// Add option to disable hit deletion
//
// Revision 1.2  2001/04/12 22:04:57  hardtke
// Add option for setting large hit errors
//
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
//static Char_t m_VersionCVS = "$Id: StTpcHitFilterMaker.h,v 1.4 2003/09/10 19:47:40 perev Exp $";
  Bool_t SectorOn[24];
  Bool_t RowOn[45];
  Bool_t BigErrorsInner;
  Bool_t BigErrorsOuter;
  Bool_t DeleteHits;
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
  virtual void   RidiculousErrorsInner();
  virtual void   RidiculousErrorsOuter();
  virtual void   CanDeleteHits();
  virtual void   DoNotDeleteHits();

  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StTpcHitFilterMaker.h,v 1.4 2003/09/10 19:47:40 perev Exp $ built "__DATE__" "__TIME__ ; return cvs;}

 ClassDef(StTpcHitFilterMaker,0)   //StAF chain virtual base class for Makers
};

#endif

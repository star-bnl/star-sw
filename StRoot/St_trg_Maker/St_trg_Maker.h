// $Id: St_trg_Maker.h,v 1.8 2000/06/25 23:51:03 fisyak Exp $
// $Log: St_trg_Maker.h,v $
// Revision 1.8  2000/06/25 23:51:03  fisyak
// Replace assert by return of kStErr
//
// Revision 1.7  2000/05/04 22:25:22  ward
// New 3d DST tables, and some support for sim.
//
// Revision 1.5  2000/02/04 18:57:18  ward
// Added dst_L1_Trigger and dst_L2_Trigger to output.
//
// Revision 1.4  2000/01/24 20:35:39  ward
// Access trigger data.
//
// Revision 1.3  1999/07/15 13:58:32  perev
// cleanup
//
// Revision 1.2  1999/03/14 00:25:40  perev
// New makers
//
// Revision 1.1  1999/02/06 01:51:24  yepes
// Add trg maker
//
// Revision 1.7  1998/10/31 00:25:45  fisyak
// Makers take care about branches
//
// Revision 1.6  1998/10/06 18:00:31  perev
// cleanup
//
// Revision 1.5  1998/08/26 12:15:13  fisyak
// Remove asu & dsl libraries
//
// Revision 1.4  1998/08/14 15:25:58  fisyak
// add options
//
// Revision 1.3  1998/08/10 02:32:07  fisyak
// Clean up
//
// Revision 1.2  1998/07/20 15:08:15  fisyak
// Add tcl and tpt
//
#ifndef STAR_St_trg_Maker
#define STAR_St_trg_Maker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_trg_Maker virtual base class for Maker                            //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif
class St_ctu_cor;
class St_mwc_raw;
class St_dst_L0_Trigger;
class St_dst_L1_Trigger;
class St_dst_L2_Trigger;
class St_dst_TrgDet;
class StDAQReader;
class StTRGReader;

class St_trg_Maker : public StMaker {
 private:
   Bool_t drawinit;
   StDAQReader *fVictorPrelim; //!
   StTRGReader *fVictor;       //!
// static Char_t  m_VersionCVS = "$Id: St_trg_Maker.h,v 1.8 2000/06/25 23:51:03 fisyak Exp $";
// Int_t          m_mode;        // mode 1 = primaries;
// St_stk_stkpar *m_stk_stkpar;  //! pointer to stk parameters
   void dumpDataToScreenAndExit();
   void InitMwcArrays();
   void InitCtbArrays();
   Int_t SanityCheck();
   int auxctbmap[16],ctbmap[120][2];  // Hardcoded from ctb_dsm.map.
   int auxmwcmap[32],mwcmap[24][4];  // Hardcoded from mwc_dsm.map.
 
 protected:
 public: 
   int HandleCtu(St_ctu_cor *ctu_cor,St_dst_TrgDet *dst1);
   int HandleMwc(St_mwc_raw *mwc_raw,St_dst_TrgDet *dst1);
   void Vladimir2Herbert(int,int*,int*);
   void CtbMwcDaq(St_dst_TrgDet *dst1);
   void SecondDstSim(St_dst_L0_Trigger *dst2);
   void TakeCareOfL1andL2Daq(St_dst_L1_Trigger*,St_dst_L2_Trigger*);
   void TakeCareOfL1andL2Sim(St_dst_L1_Trigger*,St_dst_L2_Trigger*);
   void SecondDstDaq(St_dst_L0_Trigger *dst2);
   void VpdSim(St_dst_TrgDet *dst);
   void ZdcSim(St_dst_TrgDet *dst);
   void VpdDaq(St_dst_TrgDet *dst);
   void ZdcDaq(St_dst_TrgDet *dst);
   int Daq(St_DataSet*,St_dst_TrgDet*,St_dst_L0_Trigger*,St_dst_L1_Trigger*,St_dst_L2_Trigger*);
   int Sim(            St_dst_TrgDet*,St_dst_L0_Trigger*,St_dst_L1_Trigger*,St_dst_L2_Trigger*);
                  St_trg_Maker(const char *name="trg");
   virtual       ~St_trg_Maker();
   virtual Int_t Init();
   virtual Int_t  Make();
// virtual void Set_mode       (Int_t   m =      2){m_mode       = m;} // *MENU*
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: St_trg_Maker.h,v 1.8 2000/06/25 23:51:03 fisyak Exp $ built "__DATE__" "__TIME__ ; return cvs;}

   ClassDef(St_trg_Maker, 1)   //StAF chain virtual base class for Makers
};

#endif

// $Id: St_trg_Maker.h,v 1.17 2003/01/16 13:32:32 ward Exp $
// $Log: St_trg_Maker.h,v $
// Revision 1.17  2003/01/16 13:32:32  ward
// Accomodation of the new trgStructures.h.
//
// Revision 1.14  2002/02/19 18:34:44  ward
// Changes from Jenn Klay: EMC unpacker rewritten, updated dsm-to-patch conversion to match offline software.
//
// Revision 1.13  2001/10/16 20:26:03  ward
// New code from Jennifer Klay for unpacking EMC data.
//
// Revision 1.12  2001/09/03 19:09:40  ward
// Runtime selection of 2000 or 2001 trigger data format.
//
// Revision 1.11  2001/07/25 19:10:53  ward
// New function InitCtbArrays2001 for ctb_dsm_2001.map.
//
// Revision 1.10  2001/04/26 22:37:56  perev
// HPcorrs
//
// Revision 1.9  2001/01/02 18:10:44  ward
// Pablo Yepes' modifications in support of CTU simulations.
//
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
class St_ctu_raw;
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
// static Char_t  m_VersionCVS = "$Id: St_trg_Maker.h,v 1.17 2003/01/16 13:32:32 ward Exp $";
// Int_t          m_mode;        // mode 1 = primaries;
// St_stk_stkpar *m_stk_stkpar;  //! pointer to stk parameters
   void dumpDataToScreenAndExit    ();
   void dumpDataToScreenAndExit2000();
   void dumpDataToScreenAndExit2003();
   void InitMwcArrays();
   void InitCtbArrays();
   int St_trg_Maker::YearOfData(St_DataSet *herb);
   void InitCtbArrays2001(); // For data taken 2001 and (?) after. 
   Int_t SanityCheck    ();
   Int_t SanityCheck2000();
   Int_t SanityCheck2003();
   int auxctbmap[16],ctbmap[120][2];  // Hardcoded from ctb_dsm.map.
   int auxmwcmap[32],mwcmap[24][4];  // Hardcoded from mwc_dsm.map.
 
 protected:
 public: 
   int getTrayCtb ( float phi, float z ) ;
   int HandleCtu(St_ctu_raw *ctu_raw,St_dst_TrgDet *dst1);
   int HandleMwc(St_mwc_raw *mwc_raw,St_dst_TrgDet *dst1);
   void Vladimir2Herbert(int,int*,int*);
   void Emc    (St_dst_TrgDet *dst1);
   void Emc2000(St_dst_TrgDet *dst1);
   void Emc2003(St_dst_TrgDet *dst1);
   void CtbMwcDaq    (St_dst_TrgDet *dst1);
   void CtbMwcDaq2000(St_dst_TrgDet *dst1);
   void CtbMwcDaq2003(St_dst_TrgDet *dst1);
   void SecondDstSim(St_dst_L0_Trigger *dst2);
   void TakeCareOfL1andL2Daq    (St_dst_L1_Trigger*,St_dst_L2_Trigger*);
   void TakeCareOfL1andL2Daq2000(St_dst_L1_Trigger*,St_dst_L2_Trigger*);
   void TakeCareOfL1andL2Daq2003(St_dst_L1_Trigger*,St_dst_L2_Trigger*);
   void TakeCareOfL1andL2Sim(St_dst_L1_Trigger*,St_dst_L2_Trigger*);
   void SecondDstDaq    (St_dst_L0_Trigger *dst2);
   void SecondDstDaq2000(St_dst_L0_Trigger *dst2);
   void SecondDstDaq2003(St_dst_L0_Trigger *dst2);
   void VpdSim(St_dst_TrgDet *dst);
   void ZdcSim(St_dst_TrgDet *dst);
   void VpdDaq    (St_dst_TrgDet *dst);
   void VpdDaq2000(St_dst_TrgDet *dst);
   void VpdDaq2003(St_dst_TrgDet *dst);
   void ZdcDaq    (St_dst_TrgDet *dst);
   void ZdcDaq2000(St_dst_TrgDet *dst);
   void ZdcDaq2003(St_dst_TrgDet *dst);
   int Daq    (St_DataSet*,St_dst_TrgDet*,St_dst_L0_Trigger*,St_dst_L1_Trigger*,St_dst_L2_Trigger*);
   int Daq2000(St_DataSet*,St_dst_TrgDet*,St_dst_L0_Trigger*,St_dst_L1_Trigger*,St_dst_L2_Trigger*);
   int Daq2003(St_DataSet*,St_dst_TrgDet*,St_dst_L0_Trigger*,St_dst_L1_Trigger*,St_dst_L2_Trigger*);
   int Sim(            St_dst_TrgDet*,St_dst_L0_Trigger*,St_dst_L1_Trigger*,St_dst_L2_Trigger*);
                  St_trg_Maker(const char *name="trg");
   virtual       ~St_trg_Maker();
   virtual Int_t Init();
   virtual Int_t  Make();
// virtual void Set_mode       (Int_t   m =      2){m_mode       = m;} // *MENU*
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: St_trg_Maker.h,v 1.17 2003/01/16 13:32:32 ward Exp $ built "__DATE__" "__TIME__ ; return cvs;}

   ClassDef(St_trg_Maker, 1)   //StAF chain virtual base class for Makers
};

#endif

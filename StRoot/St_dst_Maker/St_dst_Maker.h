// $Id: St_dst_Maker.h,v 1.9 1999/07/15 13:57:56 perev Exp $
// $Log: St_dst_Maker.h,v $
// Revision 1.9  1999/07/15 13:57:56  perev
// cleanup
//
// Revision 1.8  1999/07/12 23:04:17  fisyak
// Remove glob2
//
// Revision 1.7  1999/07/01 17:27:42  fisyak
// New global chain from  Wensheng Deng
//
// Revision 1.6  1999/05/04 21:00:44  fisyak
// Step back to MDC2 version
//
// Revision 1.5  1999/05/01 01:49:15  fisyak
// Add StRootEvent fill
//
// Revision 1.4  1999/03/11 03:12:18  perev
// new schema
//
// Revision 1.3  1999/02/19 16:28:55  fisyak
// Reactivate dst Maker
//
// Revision 1.2  1999/01/20 23:58:03  fisyak
// Tree 2 GetTree
//
// Revision 1.1  1999/01/02 19:09:23  fisyak
// Add Clones
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
#ifndef STAR_St_dst_Maker
#define STAR_St_dst_Maker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_dst_Maker virtual base class for Maker                            //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "StMaker.h"

class St_dst_summary_param;

class St_dst_Maker : public StMaker {
 private:
  const Char_t **fSelect;
  Char_t collectionName[256];

  St_dst_summary_param *m_dst_summary_param; //!

 protected:
 public: 
                  St_dst_Maker(const char *name="dst");
   virtual       ~St_dst_Maker();
   virtual Int_t  Init();
   virtual Int_t  Make();
   virtual Int_t  Filler();
   virtual void   SetSelection(const Char_t **sel ){fSelect=sel;};
   
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: St_dst_Maker.h,v 1.9 1999/07/15 13:57:56 perev Exp $ built "__DATE__" "__TIME__ ; return cvs;}

   ClassDef(St_dst_Maker, 1)   //StAF chain virtual base class for Makers
};

#endif

// $Id: St_dst_Maker.h,v 1.3 1999/02/19 16:28:55 fisyak Exp $
// $Log: St_dst_Maker.h,v $
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
#include "TClonesArray.h"

//class St_stk_stkpar;
class St_dst_Maker : public StMaker {
 private:
   Bool_t drawinit;
   // CloneArrays
   Int_t  fNvertex;
   Int_t  fNtrack;
   Int_t  fNpoint;
   static const Int_t maxNoVertex;
   static const Int_t maxNoTrack;
   static const Int_t maxNoPoint;
   TClonesArray *m_Vertex;  //!
   TClonesArray *m_Track;   //!
   //   TClonesArray *m_Point;
 protected:
 public: 
                  St_dst_Maker(const char *name="dst", const char *title="dst");
   virtual       ~St_dst_Maker();
   virtual Int_t  Init();
   virtual Int_t  Make();
   virtual void   PrintInfo();
   
   ClassDef(St_dst_Maker, 1)   //StAF chain virtual base class for Makers
};

#endif

// $Id: St_geom_Maker.h,v 1.4 1998/12/25 21:02:14 nevski Exp $
// $Log: St_geom_Maker.h,v $
// Revision 1.4  1998/12/25 21:02:14  nevski
// Add Set/Get method
//
// Revision 1.3  1998/12/16 20:56:24  fisyak
// Add gstar to ROOT
//
// Revision 1.2  1998/12/04 19:36:48  fisyak
// Add Pavel/Ruben gstar interface
//
// Revision 1.1  1998/10/31 00:28:31  fisyak
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
#ifndef STAR_St_geom_Maker
#define STAR_St_geom_Maker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_geom_Maker virtual base class for Maker                            //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
class TNode;

#endif

class St_geom_Maker : public StMaker {
 private:
  Bool_t drawinit;
  Int_t  nwgeant;     // No. of words in GCBANK common block
  Int_t  nwpaw;       // No. of words in PAWC  common block
  Int_t  iwtype;      // HIGZ interface (=0 no HIGZ)
  TNode*   fNode;
 
 protected:
 public: 
                  St_geom_Maker(const char *name="geom", const char *title="run/geant/Run");
   virtual       ~St_geom_Maker();
   virtual Int_t  Finish(){SafeDelete(m_DataSet); return kStOK;}
   virtual Int_t  Init();
   virtual void   Clear(Option_t *option){}; // No clearance for parameters
   void Draw();  
   void Do  (const char*);  
   virtual Int_t  Make();
   virtual void   PrintInfo();
   virtual void   SetNwGEANT (Int_t n=100000) {nwgeant = n;} // *MENU
   virtual void   SetNwPAW   (Int_t n=     0) {nwpaw   = n;} // *MENU
   virtual void   SetIwtype  (Int_t n=     0) {iwtype  = n;} // *MENU
   void Work();  
   TNode* GetNode() { return fNode; }

   ClassDef(St_geom_Maker, 1)   //StAF chain virtual base class for Makers
};

#endif


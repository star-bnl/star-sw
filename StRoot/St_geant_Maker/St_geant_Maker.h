// $Id: St_geant_Maker.h,v 1.4 1999/01/05 01:37:02 fisyak Exp $
// $Log: St_geant_Maker.h,v $
// Revision 1.4  1999/01/05 01:37:02  fisyak
// Intermeidate version with St_Node
//
// Revision 1.3  1999/01/03 20:56:36  fisyak
// Remove St_geom_Maker
//
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
#ifndef STAR_St_geant_Maker
#define STAR_St_geant_Maker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_geant_Maker virtual base class for Maker                            //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
class St_Node;

#endif

class St_geant_Maker : public StMaker {
 private:
  Bool_t drawinit;
  Int_t  nwgeant;     // No. of words in GCBANK common block
  Int_t  nwpaw;       // No. of words in PAWC  common block
  Int_t  iwtype;      // HIGZ interface (=0 no HIGZ)
  St_Node*   fNode;   //!
 
 protected:
 public: 
                  St_geant_Maker(const char *name="geant", const char *title="run/geant/Run");
   virtual       ~St_geant_Maker();
   virtual Int_t  Finish(){SafeDelete(m_DataSet); return kStOK;}
   virtual Int_t  Init();
   virtual void   Clear(Option_t *option){}; // No clearance for parameters
   virtual void   Do(const Char_t *option = "dcut cave x 0.1 10 10 0.03 0.03"); // *MENU 
   virtual void   Draw();  
   virtual Int_t  Make();
   virtual void   PrintInfo();
   virtual void   LoadGeometry (Char_t *option = "detp geometry field_only");  // *MENU
   virtual void   SetNwGEANT (Int_t n=2000000) {nwgeant = n;} // *MENU
   virtual void   SetNwPAW   (Int_t n=      0) {nwpaw   = n;} // *MENU
   virtual void   SetIwtype  (Int_t n=      0) {iwtype  = n;} // *MENU

   virtual void   Work();
   St_Node* GetNode() { return fNode; }

   ClassDef(St_geant_Maker, 1)   //StAF chain virtual base class for Makers
};

#endif


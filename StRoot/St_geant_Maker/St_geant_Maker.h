// $Id: St_geant_Maker.h,v 1.9 1999/02/22 19:27:23 fisyak Exp $

#ifndef STAR_St_geant_Maker
#define STAR_St_geant_Maker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_geant_Maker virtual base class for Maker                            //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#ifndef __CINT__
#include "fortranc.h"
#define rootmaptable_ F77_NAME(rootmaptable,ROOTMAPTABLE)
extern "C" {
R__EXTERN  void type_of_call rootmaptable_(Char_t *,Char_t *, Char_t*, Int_t *, Char_t *);
}
#endif

class St_Node;

#endif
class TRotMatrix;
class St_geant_Maker : public StMaker {
 private:
  Bool_t drawinit;
  Int_t  nwgeant;     // No. of words in GCBANK common block
  Int_t  nwpaw;       // No. of words in PAWC  common block
  Int_t  iwtype;      // HIGZ interface (=0 no HIGZ)
  St_Node*   fNode;   //!
  Char_t   mGnam[20]; //!
 protected:
  virtual TShape  *MakeShape(TString *name, Int_t ivo);
  virtual St_Node *MakeNode(TString *name, Int_t ivo, Int_t Nlevel, Int_t *Names, Int_t *Numbers);
 public: 
                  St_geant_Maker(const char *name="geant", const char *title="run/geant/Run");
   virtual       ~St_geant_Maker();
   virtual Int_t  Finish(){SafeDelete(m_DataSet); return kStOK;}
   virtual Int_t  Init();
   virtual void   Do(const Char_t *option = "dcut cave x 0.1 10 10 0.03 0.03"); // *MENU 
   virtual void   Draw();
   virtual void   G2root();
   virtual Int_t  Make();
   virtual void   LoadGeometry (Char_t *option = "detp geometry field_only");  // *MENU
   virtual void   PrintInfo();
   virtual void   SetNwGEANT (Int_t n=2000000) {nwgeant = n;} // *MENU
   virtual void   SetNwPAW   (Int_t n=      0) {nwpaw   = n;} // *MENU
   virtual void   SetIwtype  (Int_t n=      0) {iwtype  = n;} // *MENU

   virtual void   Work();
   virtual void   Call(const Char_t *name); // *MENU 
   virtual TRotMatrix *GetMatrix(float theta1, float phi1,
                                 float theta2, float phi2,
                                 float theta3, float phi3);

   St_Node* GetNode() { return fNode; }
   //----------------------------------------------------------------------
   ClassDef(St_geant_Maker, 1)   //StAF chain virtual base class for Makers
};

#endif


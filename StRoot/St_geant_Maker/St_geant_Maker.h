// $Id: St_geant_Maker.h,v 1.14 1999/04/29 19:29:28 nevski Exp $

#ifndef STAR_St_geant_Maker
#define STAR_St_geant_Maker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_geant_Maker virtual base class for Maker                          //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
 class St_Node;

#ifndef __CINT__
#include "fortranc.h"
#define rootmaptable_ F77_NAME(rootmaptable,ROOTMAPTABLE)
#define agvolume_     F77_NAME(agvolume,AGVOLUME)
extern "C" {
R__EXTERN  void type_of_call rootmaptable_(Char_t *,Char_t *, Char_t*, Int_t *, Char_t *);
R__EXTERN Int_t type_of_call agvolume_(St_Node**,Float_t**,Float_t**,Float_t**,
                                       Int_t*,Int_t*,Float_t**,Int_t*);
}
#endif

#endif
class TRotMatrix;
class St_geant_Maker : public StMaker {
protected:
  Int_t  nwgeant;     // No. of words in GCBANK common block
  Int_t  nwpaw;       // No. of words in PAWC  common block
  Int_t  iwtype;      // HIGZ interface (=0 no HIGZ)
  St_Node*   fNode;   //!
  TString fInputFile; // 
  StEvtHddr *fEvtHddr;//! pointer to Event Header
  virtual TShape  *MakeShape(TString *name, Int_t ivo);
  virtual St_Node *MakeNode(TString *name, Int_t ivo, Int_t Nlevel, Int_t *Names, Int_t *Numbers);
public: 
                  St_geant_Maker(const char *name="geant");
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

   virtual St_Node *Work();
   virtual void   Call(const Char_t *name); // *MENU 
   virtual TRotMatrix *GetMatrix(float theta1, float phi1,
                                 float theta2, float phi2,
                                 float theta3, float phi3);

   virtual void  SetDebug(EDebugLevel dbl=kDebug); 
           Int_t SetInputFile(const char* file);

   St_Node* GetNode() { return fNode; }
   //----------------------------------------------------------------------
static void RootMapTable(Char_t *Cdest,Char_t *Table, Char_t* Spec, Int_t *k, Char_t *iq);

protected:
   static St_DataSet *fgGeom;
ClassDef(St_geant_Maker, 1)   //StAF chain virtual base class for Makers
};

#endif


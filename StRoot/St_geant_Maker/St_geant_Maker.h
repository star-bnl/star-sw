// $Id: St_geant_Maker.h,v 1.18 1999/11/11 05:16:30 fine Exp $

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
R__EXTERN  void type_of_call rootmaptable_(const Char_t *,const Char_t *,const Char_t*, Int_t *, Char_t *,const int ,const int, const int);
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
   virtual void   SetNwGEANT (Int_t n=2000000) {nwgeant = n;} // *MENU
   virtual void   SetNwPAW   (Int_t n=      0) {nwpaw   = n;} // *MENU
   virtual void   SetIwtype  (Int_t n=      0) {iwtype  = n;} // *MENU
   virtual Int_t  Skip(Int_t Nskip=1);                        // *MENU*
   virtual St_Node *Work();
   virtual void   Call(const Char_t *name); // *MENU 
   virtual TRotMatrix *GetMatrix(float theta1, float phi1,
                                 float theta2, float phi2,
                                 float theta3, float phi3);

   virtual St_DataSet  *GetDataSet (const char* logInput,
                                    const StMaker *uppMk=0,
                                    const StMaker *dowMk=0) const ;

   virtual void  SetDebug(EDebugLevel dbl=kDebug); 
           Int_t SetInputFile(const char* file);

   St_Node* GetNode() { return fNode; }
   //----------------------------------------------------------------------
static void RootMapTable(Char_t *Cdest,Char_t *Table, Char_t* Spec, Int_t *k, Char_t *iq);

protected:
   static St_DataSet *fgGeom;
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: St_geant_Maker.h,v 1.18 1999/11/11 05:16:30 fine Exp $ built "__DATE__" "__TIME__ ; return cvs;}

ClassDef(St_geant_Maker, 1)   //StAF chain virtual base class for Makers
};

#endif


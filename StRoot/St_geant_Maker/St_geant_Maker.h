// $Id: St_geant_Maker.h,v 1.20 2000/01/04 21:51:11 fisyak Exp $

#ifndef STAR_St_geant_Maker
#define STAR_St_geant_Maker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_geant_Maker virtual base class for Maker                          //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include "StMaker.h"
class St_Node;
class TGeant3;
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
   static void RootMapTable(Char_t *Cdest,Char_t *Table, Char_t* Spec, 
			    Int_t &k, Char_t *iq);
   virtual void     Geometry();
   virtual Int_t    Agstroot();
   virtual Int_t    G2t_volume_id(const Char_t *name, Int_t *numbv);
   virtual Int_t    Agvolume(St_Node *&node,Float_t *&par,Float_t *&pos,Float_t *&mot,
			     Int_t &who, Int_t &copy,Float_t *&par1,Int_t &npar);
   virtual void     Agnzgete (Int_t &ILK, Int_t &IDE,
			      Int_t &NPART, Int_t &IRUN,
			      Int_t &IEVT, const Char_t *CGNAM,
			      Float_t *VERT,Int_t &IWTFL,Float_t &WEIGH);
   
   virtual void     Gfxzrm(Int_t & Nlevel, 
		     Float_t &x, Float_t &y, Float_t &z,
		     Float_t &Theta1, Float_t & Phi1,
		     Float_t &Theta2, Float_t & Phi2,
		     Float_t &Theta3, Float_t & Phi3,
		     Float_t &Type);  
   virtual void     Dzddiv(Int_t& idiv ,Int_t &Ldummy,
			   const Char_t* path,const Char_t* opt,
			   Int_t& one,Int_t &two,Int_t &three,Int_t& iw);
   
 protected:
   static St_DataSet *fgGeom; //!
   static TGeant3    *geant3; //!
   virtual const char *GetCVS() const
   {static const char cvs[]="Tag $Name:  $ $Id: St_geant_Maker.h,v 1.20 2000/01/04 21:51:11 fisyak Exp $ built "__DATE__" "__TIME__ ; return cvs;}
ClassDef(St_geant_Maker, 1)   //StAF chain virtual base class for Makers
};

#endif


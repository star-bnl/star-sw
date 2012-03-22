#ifndef __MagpGeo__ 
#define __MagpGeo__ 
 
#include "StarVMC/StarAgmlLib/AgModule.h" 
 
namespace MAGPGEO // $NMSPC 
{ 
   class Magg_t : public AgStructure 
   { 
      ClassDef(Magg_t,1); 
      public: 
      Float_t version; 
      Float_t rmax; 
      Float_t length; 
      Float_t test; 
      Magg_t() : AgStructure("Magg_t","User-defined AgML structure") 
      { 
         version=0; 
         rmax=0; 
         length=0; 
         test=0; 
         _index=0; 
      } 
      ~ Magg_t(){ /* nada */ }; 
   }; 
   class Mbar_t : public AgStructure 
   { 
      ClassDef(Mbar_t,1); 
      public: 
      Float_t coilrmn; 
      Float_t coilrmx; 
      Float_t coillen; 
      Float_t retyrmn; 
      Float_t retylen; 
      Float_t barwidin; 
      Float_t barwidou; 
      Float_t barheigh; 
      Float_t ringrmn; 
      Float_t ncoil; 
      Array_t<Float_t> zcoil; 
      Array_t<Float_t> dzcoil; 
      Mbar_t() : AgStructure("Mbar_t","User-defined AgML structure") 
      { 
         coilrmn=0; 
         coilrmx=0; 
         coillen=0; 
         retyrmn=0; 
         retylen=0; 
         barwidin=0; 
         barwidou=0; 
         barheigh=0; 
         ringrmn=0; 
         ncoil=0; 
         zcoil = Array_t<Float_t>(6); 
         dzcoil = Array_t<Float_t>(6); 
         _index=0; 
      } 
      ~ Mbar_t(){ /* nada */ }; 
   }; 
   class Mend_t : public AgStructure 
   { 
      ClassDef(Mend_t,1); 
      public: 
      Float_t polermn; 
      Float_t polez; 
      Float_t polermx; 
      Float_t tcoilrmn; 
      Float_t tcoilrmx; 
      Float_t polecavr; 
      Float_t polecavd; 
      Float_t tcoildz; 
      Float_t etacut; 
      Mend_t() : AgStructure("Mend_t","User-defined AgML structure") 
      { 
         polermn=0; 
         polez=0; 
         polermx=0; 
         tcoilrmn=0; 
         tcoilrmx=0; 
         polecavr=0; 
         polecavd=0; 
         tcoildz=0; 
         etacut=0; 
         _index=0; 
      } 
      ~ Mend_t(){ /* nada */ }; 
   }; 
   // ---------------------------------------------------------------------- MAGP -- 
   ///@defgroup MAGP_doc 
   ///@class MAGP 
   ///@brief is the magnet mother 
   class MAGP : public AgBlock 
   {  public: 
      MAGP() : AgBlock("MAGP","is the magnet mother"){ }; 
      ~MAGP(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(MAGP,1); 
   }; 
   // ---------------------------------------------------------------------- COIL -- 
   ///@defgroup COIL_doc 
   ///@class COIL 
   ///@brief is the main coil mother 
   class COIL : public AgBlock 
   {  public: 
      COIL() : AgBlock("COIL","is the main coil mother"){ }; 
      ~COIL(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(COIL,1); 
   }; 
   // ---------------------------------------------------------------------- MCSE -- 
   ///@defgroup MCSE_doc 
   ///@class MCSE 
   ///@brief is a single barrel coil 
   class MCSE : public AgBlock 
   {  public: 
      MCSE() : AgBlock("MCSE","is a single barrel coil"){ }; 
      ~MCSE(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(MCSE,1); 
   }; 
   // ---------------------------------------------------------------------- MRET -- 
   ///@defgroup MRET_doc 
   ///@class MRET 
   ///@brief is Magnet RETurn Yoke 
   class MRET : public AgBlock 
   {  public: 
      MRET() : AgBlock("MRET","is Magnet RETurn Yoke"){ }; 
      ~MRET(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(MRET,1); 
   }; 
   // ---------------------------------------------------------------------- MSEC -- 
   ///@defgroup MSEC_doc 
   ///@class MSEC 
   ///@brief is a sector containing a single retun bar 
   class MSEC : public AgBlock 
   {  public: 
      MSEC() : AgBlock("MSEC","is a sector containing a single retun bar"){ }; 
      ~MSEC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(MSEC,1); 
   }; 
   // ---------------------------------------------------------------------- MBAR -- 
   ///@defgroup MBAR_doc 
   ///@class MBAR 
   ///@brief is a single return yoke bar 
   class MBAR : public AgBlock 
   {  public: 
      MBAR() : AgBlock("MBAR","is a single return yoke bar"){ }; 
      ~MBAR(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(MBAR,1); 
   }; 
   // ---------------------------------------------------------------------- MRGV -- 
   ///@defgroup MRGV_doc 
   ///@class MRGV 
   ///@brief is the Magnet Return rinG 
   class MRGV : public AgBlock 
   {  public: 
      MRGV() : AgBlock("MRGV","is the Magnet Return rinG"){ }; 
      ~MRGV(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(MRGV,1); 
   }; 
   // ---------------------------------------------------------------------- MPTV -- 
   ///@defgroup MPTV_doc 
   ///@class MPTV 
   ///@brief is the magnet pole-tip volume 
   class MPTV : public AgBlock 
   {  public: 
      MPTV() : AgBlock("MPTV","is the magnet pole-tip volume"){ }; 
      ~MPTV(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(MPTV,1); 
   }; 
   // ---------------------------------------------------------------------- MPCV -- 
   ///@defgroup MPCV_doc 
   ///@class MPCV 
   ///@brief is the coil cavity in the pole-tip (filled with cables ?) 
   class MPCV : public AgBlock 
   {  public: 
      MPCV() : AgBlock("MPCV","is the coil cavity in the pole-tip (filled with cables ?)"){ }; 
      ~MPCV(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(MPCV,1); 
   }; 
   // ---------------------------------------------------------------------- MTCL -- 
   ///@defgroup MTCL_doc 
   ///@class MTCL 
   ///@brief is TRIM COIL Volume (filled with aluminum) 
   class MTCL : public AgBlock 
   {  public: 
      MTCL() : AgBlock("MTCL","is TRIM COIL Volume (filled with aluminum)"){ }; 
      ~MTCL(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(MTCL,1); 
   }; 
   /// \class MagpGeo 
   /// \brief  is the geometry of the STAR magnet  
   class MagpGeo : public AgModule 
   { 
      public: 
      MagpGeo(); 
      virtual void ConstructGeometry( const Char_t *dummy="" ); 
      ~MagpGeo(){ }; 
      ClassDef(MagpGeo,1); 
      public: 
   }; 
   // endElement in class Module 
}; // namespace MagpGeo 
#endif // __MagpGeo__ 

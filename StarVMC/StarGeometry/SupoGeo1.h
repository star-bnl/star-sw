#ifndef __SupoGeo1__ 
#define __SupoGeo1__ 
 
#include "StarVMC/StarAgmlLib/AgModule.h" 
 
namespace SUPOGEO1 // $NMSPC 
{ 
   class Smai_t : public AgStructure 
   { 
      ClassDef(Smai_t,1); 
      public: 
      Float_t version; 
      Float_t rinner; 
      Float_t router; 
      Float_t zmin; 
      Float_t zmax; 
      Float_t phimid; 
      Float_t fixhei; 
      Float_t fixwid; 
      Float_t fixthk; 
      Smai_t() : AgStructure("Smai_t","User-defined AgML structure") 
      { 
         version=0; 
         rinner=0; 
         router=0; 
         zmin=0; 
         zmax=0; 
         phimid=0; 
         fixhei=0; 
         fixwid=0; 
         fixthk=0; 
         _index=0; 
      } 
      ~ Smai_t(){ /* nada */ }; 
   }; 
   class Sslo_t : public AgStructure 
   { 
      ClassDef(Sslo_t,1); 
      public: 
      Float_t version; 
      Float_t phimin; 
      Float_t phimax; 
      Float_t raillen; 
      Float_t railwin; 
      Float_t railwout; 
      Float_t railhei; 
      Float_t wallwid; 
      Float_t wallhei; 
      Float_t headthk; 
      Float_t headhei; 
      Float_t xwalthk; 
      Float_t xwal1pos; 
      Float_t xwal2pos; 
      Float_t endthk; 
      Float_t endhei; 
      Float_t boltpos; 
      Float_t boltoff; 
      Float_t boltrad; 
      Sslo_t() : AgStructure("Sslo_t","User-defined AgML structure") 
      { 
         version=0; 
         phimin=0; 
         phimax=0; 
         raillen=0; 
         railwin=0; 
         railwout=0; 
         railhei=0; 
         wallwid=0; 
         wallhei=0; 
         headthk=0; 
         headhei=0; 
         xwalthk=0; 
         xwal1pos=0; 
         xwal2pos=0; 
         endthk=0; 
         endhei=0; 
         boltpos=0; 
         boltoff=0; 
         boltrad=0; 
         _index=0; 
      } 
      ~ Sslo_t(){ /* nada */ }; 
   }; 
   class Sshi_t : public AgStructure 
   { 
      ClassDef(Sshi_t,1); 
      public: 
      Float_t version; 
      Float_t phimin; 
      Float_t phimax; 
      Float_t xdist; 
      Float_t ydist; 
      Float_t railwid; 
      Float_t railhei; 
      Float_t platthk; 
      Float_t plathei; 
      Float_t barlen; 
      Float_t baroffz; 
      Float_t barhei; 
      Float_t topwid; 
      Float_t blochei; 
      Float_t bloclen; 
      Float_t boltoff; 
      Float_t stabwid; 
      Float_t stabthk; 
      Float_t stab1z; 
      Float_t stab2z; 
      Sshi_t() : AgStructure("Sshi_t","User-defined AgML structure") 
      { 
         version=0; 
         phimin=0; 
         phimax=0; 
         xdist=0; 
         ydist=0; 
         railwid=0; 
         railhei=0; 
         platthk=0; 
         plathei=0; 
         barlen=0; 
         baroffz=0; 
         barhei=0; 
         topwid=0; 
         blochei=0; 
         bloclen=0; 
         boltoff=0; 
         stabwid=0; 
         stabthk=0; 
         stab1z=0; 
         stab2z=0; 
         _index=0; 
      } 
      ~ Sshi_t(){ /* nada */ }; 
   }; 
   // ---------------------------------------------------------------------- SUPO -- 
   ///@defgroup SUPO_doc 
   ///@class SUPO 
   ///@brief is the FTPC support mother volume 
   class SUPO : public AgBlock 
   {  public: 
      SUPO() : AgBlock("SUPO","is the FTPC support mother volume"){ }; 
      ~SUPO(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SUPO,1); 
   }; 
   // ---------------------------------------------------------------------- SUPL -- 
   ///@defgroup SUPL_doc 
   ///@class SUPL 
   ///@brief is the lower FTPC support mother volume 
   class SUPL : public AgBlock 
   {  public: 
      SUPL() : AgBlock("SUPL","is the lower FTPC support mother volume"){ }; 
      ~SUPL(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SUPL,1); 
   }; 
   // ---------------------------------------------------------------------- SLRL -- 
   ///@defgroup SLRL_doc 
   ///@class SLRL 
   ///@brief is the lower FTPC support rail 
   class SLRL : public AgBlock 
   {  public: 
      SLRL() : AgBlock("SLRL","is the lower FTPC support rail"){ }; 
      ~SLRL(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SLRL,1); 
   }; 
   // ---------------------------------------------------------------------- SLWL -- 
   ///@defgroup SLWL_doc 
   ///@class SLWL 
   ///@brief is the lower FTPC support side wall 
   class SLWL : public AgBlock 
   {  public: 
      SLWL() : AgBlock("SLWL","is the lower FTPC support side wall"){ }; 
      ~SLWL(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SLWL,1); 
   }; 
   // ---------------------------------------------------------------------- SLHD -- 
   ///@defgroup SLHD_doc 
   ///@class SLHD 
   ///@brief is the lower FTPC support head plate (mounted to TPC) 
   class SLHD : public AgBlock 
   {  public: 
      SLHD() : AgBlock("SLHD","is the lower FTPC support head plate (mounted to TPC)"){ }; 
      ~SLHD(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SLHD,1); 
   }; 
   // ---------------------------------------------------------------------- SLXW -- 
   ///@defgroup SLXW_doc 
   ///@class SLXW 
   ///@brief is the lower FTPC support cross wall 
   class SLXW : public AgBlock 
   {  public: 
      SLXW() : AgBlock("SLXW","is the lower FTPC support cross wall"){ }; 
      ~SLXW(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SLXW,1); 
   }; 
   // ---------------------------------------------------------------------- SLEN -- 
   ///@defgroup SLEN_doc 
   ///@class SLEN 
   ///@brief is the lower FTPC support end block 
   class SLEN : public AgBlock 
   {  public: 
      SLEN() : AgBlock("SLEN","is the lower FTPC support end block"){ }; 
      ~SLEN(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SLEN,1); 
   }; 
   // ---------------------------------------------------------------------- SLFX -- 
   ///@defgroup SLFX_doc 
   ///@class SLFX 
   ///@brief is the lower FTPC support fixture plate 
   class SLFX : public AgBlock 
   {  public: 
      SLFX() : AgBlock("SLFX","is the lower FTPC support fixture plate"){ }; 
      ~SLFX(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SLFX,1); 
   }; 
   // ---------------------------------------------------------------------- SLBL -- 
   ///@defgroup SLBL_doc 
   ///@class SLBL 
   ///@brief is the lower FTPC support bolt 
   class SLBL : public AgBlock 
   {  public: 
      SLBL() : AgBlock("SLBL","is the lower FTPC support bolt"){ }; 
      ~SLBL(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SLBL,1); 
   }; 
   // ---------------------------------------------------------------------- SUPH -- 
   ///@defgroup SUPH_doc 
   ///@class SUPH 
   ///@brief is the upper FTPC support mother volume 
   class SUPH : public AgBlock 
   {  public: 
      SUPH() : AgBlock("SUPH","is the upper FTPC support mother volume"){ }; 
      ~SUPH(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SUPH,1); 
   }; 
   // ---------------------------------------------------------------------- SHRL -- 
   ///@defgroup SHRL_doc 
   ///@class SHRL 
   ///@brief is the upper FTPC support rail 
   class SHRL : public AgBlock 
   {  public: 
      SHRL() : AgBlock("SHRL","is the upper FTPC support rail"){ }; 
      ~SHRL(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SHRL,1); 
   }; 
   // ---------------------------------------------------------------------- SHPT -- 
   ///@defgroup SHPT_doc 
   ///@class SHPT 
   ///@brief is the upper FTPC support main plate 
   class SHPT : public AgBlock 
   {  public: 
      SHPT() : AgBlock("SHPT","is the upper FTPC support main plate"){ }; 
      ~SHPT(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SHPT,1); 
   }; 
   // ---------------------------------------------------------------------- SHBR -- 
   ///@defgroup SHBR_doc 
   ///@class SHBR 
   ///@brief is the upper FTPC support top bar 
   class SHBR : public AgBlock 
   {  public: 
      SHBR() : AgBlock("SHBR","is the upper FTPC support top bar"){ }; 
      ~SHBR(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SHBR,1); 
   }; 
   // ---------------------------------------------------------------------- SHBK -- 
   ///@defgroup SHBK_doc 
   ///@class SHBK 
   ///@brief is the upper FTPC support top block 
   class SHBK : public AgBlock 
   {  public: 
      SHBK() : AgBlock("SHBK","is the upper FTPC support top block"){ }; 
      ~SHBK(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SHBK,1); 
   }; 
   // ---------------------------------------------------------------------- SHFX -- 
   ///@defgroup SHFX_doc 
   ///@class SHFX 
   ///@brief is the upper FTPC support fixture plate 
   class SHFX : public AgBlock 
   {  public: 
      SHFX() : AgBlock("SHFX","is the upper FTPC support fixture plate"){ }; 
      ~SHFX(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SHFX,1); 
   }; 
   // ---------------------------------------------------------------------- SHST -- 
   ///@defgroup SHST_doc 
   ///@class SHST 
   ///@brief are the upper FTPC support stabilizers 
   class SHST : public AgBlock 
   {  public: 
      SHST() : AgBlock("SHST","are the upper FTPC support stabilizers"){ }; 
      ~SHST(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SHST,1); 
   }; 
   /// \class SupoGeo1 
   /// \brief   is the geometry of the Forward TPC supports in STAR  
   class SupoGeo1 : public AgModule 
   { 
      public: 
      SupoGeo1(); 
      virtual void ConstructGeometry( const Char_t *dummy="" ); 
      ~SupoGeo1(){ }; 
      ClassDef(SupoGeo1,1); 
      public: 
   }; 
   // endElement in class Module 
}; // namespace SupoGeo1 
#endif // __SupoGeo1__ 

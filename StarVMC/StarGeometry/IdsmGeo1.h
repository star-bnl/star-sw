#ifndef __IdsmGeo1__ 
#define __IdsmGeo1__ 
 
#include "StarVMC/StarAgmlLib/AgModule.h" 
 
namespace IDSMGEO1 // $NMSPC 
{ 
   class Idsc_t : public AgStructure 
   { 
      ClassDef(Idsc_t,1); 
      public: 
      Float_t version; 
      Idsc_t() : AgStructure("Idsc_t","User-defined AgML structure") 
      { 
         version=0; 
         _index=0; 
      } 
      ~ Idsc_t(){ /* nada */ }; 
   }; 
   class Idsg_t : public AgStructure 
   { 
      ClassDef(Idsg_t,1); 
      public: 
      Float_t version; 
      Float_t rf; 
      Float_t angflat; 
      Float_t r1res; 
      Float_t r2res; 
      Float_t rrres; 
      Float_t dangres; 
      Float_t dxres; 
      Float_t dyres; 
      Float_t fgtstartz; 
      Float_t fgtdiskstepz; 
      Int_t fgtndisk; 
      Idsg_t() : AgStructure("Idsg_t","User-defined AgML structure") 
      { 
         version=0; 
         rf=0; 
         angflat=0; 
         r1res=0; 
         r2res=0; 
         rrres=0; 
         dangres=0; 
         dxres=0; 
         dyres=0; 
         fgtstartz=0; 
         fgtdiskstepz=0; 
         fgtndisk=0; 
         _index=0; 
      } 
      ~ Idsg_t(){ /* nada */ }; 
   }; 
   class Idsa_t : public AgStructure 
   { 
      ClassDef(Idsa_t,1); 
      public: 
      Float_t version; 
      Float_t x; 
      Float_t y; 
      Float_t z; 
      Float_t thetax; 
      Float_t thetay; 
      Float_t thetaz; 
      Float_t phix; 
      Float_t phiy; 
      Float_t phiz; 
      Idsa_t() : AgStructure("Idsa_t","User-defined AgML structure") 
      { 
         version=0; 
         x=0; 
         y=0; 
         z=0; 
         thetax=0; 
         thetay=0; 
         thetaz=0; 
         phix=0; 
         phiy=0; 
         phiz=0; 
         _index=0; 
      } 
      ~ Idsa_t(){ /* nada */ }; 
   }; 
   // ---------------------------------------------------------------------- IDSM -- 
   ///@defgroup IDSM_doc 
   ///@class IDSM 
   ///@brief mother volume for beam support cone 
   class IDSM : public AgBlock 
   {  public: 
      IDSM() : AgBlock("IDSM","mother volume for beam support cone"){ }; 
      ~IDSM(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(IDSM,1); 
   }; 
   // ---------------------------------------------------------------------- SUCA -- 
   ///@defgroup SUCA_doc 
   ///@class SUCA 
   ///@brief central CFiber tube 
   class SUCA : public AgBlock 
   {  public: 
      SUCA() : AgBlock("SUCA","central CFiber tube"){ }; 
      ~SUCA(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SUCA,1); 
   }; 
   // ---------------------------------------------------------------------- SUCB -- 
   ///@defgroup SUCB_doc 
   ///@class SUCB 
   ///@brief small Alu ring at central tube 
   class SUCB : public AgBlock 
   {  public: 
      SUCB() : AgBlock("SUCB","small Alu ring at central tube"){ }; 
      ~SUCB(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SUCB,1); 
   }; 
   // ---------------------------------------------------------------------- SUCC -- 
   ///@defgroup SUCC_doc 
   ///@class SUCC 
   ///@brief CFiber cone section 
   class SUCC : public AgBlock 
   {  public: 
      SUCC() : AgBlock("SUCC","CFiber cone section"){ }; 
      ~SUCC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SUCC,1); 
   }; 
   // ---------------------------------------------------------------------- SUCD -- 
   ///@defgroup SUCD_doc 
   ///@class SUCD 
   ///@brief large Alu ring at cone 
   class SUCD : public AgBlock 
   {  public: 
      SUCD() : AgBlock("SUCD","large Alu ring at cone"){ }; 
      ~SUCD(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SUCD,1); 
   }; 
   // ---------------------------------------------------------------------- SUCE -- 
   ///@defgroup SUCE_doc 
   ///@class SUCE 
   ///@brief East or West CFiber tube 
   class SUCE : public AgBlock 
   {  public: 
      SUCE() : AgBlock("SUCE","East or West CFiber tube"){ }; 
      ~SUCE(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SUCE,1); 
   }; 
   // ---------------------------------------------------------------------- SUCF -- 
   ///@defgroup SUCF_doc 
   ///@class SUCF 
   ///@brief large Alu ring at the end of west cylinder 
   class SUCF : public AgBlock 
   {  public: 
      SUCF() : AgBlock("SUCF","large Alu ring at the end of west cylinder"){ }; 
      ~SUCF(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SUCF,1); 
   }; 
   // ---------------------------------------------------------------------- SUCG -- 
   ///@defgroup SUCG_doc 
   ///@class SUCG 
   ///@brief large Alu end disk 
   class SUCG : public AgBlock 
   {  public: 
      SUCG() : AgBlock("SUCG","large Alu end disk"){ }; 
      ~SUCG(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SUCG,1); 
   }; 
   // ---------------------------------------------------------------------- TPRR -- 
   ///@defgroup TPRR_doc 
   ///@class TPRR 
   ///@brief  TPC resistor 
   class TPRR : public AgBlock 
   {  public: 
      TPRR() : AgBlock("TPRR"," TPC resistor"){ }; 
      ~TPRR(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TPRR,1); 
   }; 
   // ---------------------------------------------------------------------- TPRT -- 
   ///@defgroup TPRT_doc 
   ///@class TPRT 
   ///@brief resistor protection,  carbon fiber 
   class TPRT : public AgBlock 
   {  public: 
      TPRT() : AgBlock("TPRT","resistor protection,  carbon fiber"){ }; 
      ~TPRT(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TPRT,1); 
   }; 
   // ---------------------------------------------------------------------- FGRL -- 
   ///@defgroup FGRL_doc 
   ///@class FGRL 
   ///@brief FGT rail  
   class FGRL : public AgBlock 
   {  public: 
      FGRL() : AgBlock("FGRL","FGT rail "){ }; 
      ~FGRL(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FGRL,1); 
   }; 
   // ---------------------------------------------------------------------- FGHV -- 
   ///@defgroup FGHV_doc 
   ///@class FGHV 
   ///@brief FGT cables mixture  
   class FGHV : public AgBlock 
   {  public: 
      FGHV() : AgBlock("FGHV","FGT cables mixture "){ }; 
      ~FGHV(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FGHV,1); 
   }; 
   /// \class IdsmGeo1 
   /// \brief  simplified  beam support cone for 2012  
   class IdsmGeo1 : public AgModule 
   { 
      public: 
      IdsmGeo1(); 
      virtual void ConstructGeometry( const Char_t *dummy="" ); 
      ~IdsmGeo1(){ }; 
      ClassDef(IdsmGeo1,1); 
      public: 
   }; 
   // endElement in class Module 
}; // namespace IdsmGeo1 
#endif // __IdsmGeo1__ 

#ifndef __IdsmGeo1__ 
#define __IdsmGeo1__ 
 
#include "StarVMC/StarAgmlLib/AgModule.h" 
 
namespace IDSMGEO1 // $NMSPC 
{ 
   class Idsg_t : public AgStructure 
   { 
      ClassDef(Idsg_t,1); 
      public: 
      Float_t version; 
      Float_t lenm; 
      Float_t rm; 
      Float_t lenw; 
      Float_t rw; 
      Float_t thick; 
      Float_t zstart; 
      Float_t angdel; 
      Float_t angflat; 
      Float_t r1res; 
      Float_t r2res; 
      Float_t rrres; 
      Float_t dangres; 
      Float_t dxres; 
      Float_t dyres; 
      Idsg_t() : AgStructure("Idsg_t","User-defined AgML structure") 
      { 
         version=0; 
         lenm=0; 
         rm=0; 
         lenw=0; 
         rw=0; 
         thick=0; 
         zstart=0; 
         angdel=0; 
         angflat=0; 
         r1res=0; 
         r2res=0; 
         rrres=0; 
         dangres=0; 
         dxres=0; 
         dyres=0; 
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
   // ---------------------------------------------------------------------- OSCA -- 
   ///@defgroup OSCA_doc 
   ///@class OSCA 
   ///@brief middle cylinder, carbon fiber 
   class OSCA : public AgBlock 
   {  public: 
      OSCA() : AgBlock("OSCA","middle cylinder, carbon fiber"){ }; 
      ~OSCA(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(OSCA,1); 
   }; 
   // ---------------------------------------------------------------------- WSCC -- 
   ///@defgroup WSCC_doc 
   ///@class WSCC 
   ///@brief  east/west cylinder, carbon fiber 
   class WSCC : public AgBlock 
   {  public: 
      WSCC() : AgBlock("WSCC"," east/west cylinder, carbon fiber"){ }; 
      ~WSCC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(WSCC,1); 
   }; 
   // ---------------------------------------------------------------------- WSCO -- 
   ///@defgroup WSCO_doc 
   ///@class WSCO 
   ///@brief  circle carbon fiber 
   class WSCO : public AgBlock 
   {  public: 
      WSCO() : AgBlock("WSCO"," circle carbon fiber"){ }; 
      ~WSCO(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(WSCO,1); 
   }; 
   // ---------------------------------------------------------------------- WSCD -- 
   ///@defgroup WSCD_doc 
   ///@class WSCD 
   ///@brief  east/west disk, carbon fiber 
   class WSCD : public AgBlock 
   {  public: 
      WSCD() : AgBlock("WSCD"," east/west disk, carbon fiber"){ }; 
      ~WSCD(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(WSCD,1); 
   }; 
   // ---------------------------------------------------------------------- WSCP -- 
   ///@defgroup WSCP_doc 
   ///@class WSCP 
   ///@brief  circle carbon fiber 
   class WSCP : public AgBlock 
   {  public: 
      WSCP() : AgBlock("WSCP"," circle carbon fiber"){ }; 
      ~WSCP(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(WSCP,1); 
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
   /// \class IdsmGeo1 
   /// \brief  simplified  beam support cone for 2012  
   class IdsmGeo1 : public AgModule 
   { 
      public: 
      IdsmGeo1(); 
      virtual void ConstructGeometry(); 
      ~IdsmGeo1(){ }; 
      ClassDef(IdsmGeo1,1); 
      public: 
   }; 
   // endElement in class Module 
}; // namespace IdsmGeo1 
#endif // __IdsmGeo1__ 

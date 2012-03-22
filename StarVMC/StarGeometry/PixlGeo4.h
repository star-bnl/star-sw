#ifndef __PixlGeo4__ 
#define __PixlGeo4__ 
 
#include "StarVMC/StarAgmlLib/AgModule.h" 
 
namespace PIXLGEO4 // $NMSPC 
{ 
   // ---------------------------------------------------------------------- PXMO -- 
   ///@defgroup PXMO_doc 
   ///@class PXMO 
   ///@brief Main volume in the AGML tutorial geometry 
   class PXMO : public AgBlock 
   {  public: 
      PXMO() : AgBlock("PXMO","Main volume in the AGML tutorial geometry"){ }; 
      ~PXMO(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PXMO,1); 
   }; 
   // ---------------------------------------------------------------------- PXLA -- 
   ///@defgroup PXLA_doc 
   ///@class PXLA 
   ///@brief pixel sector 
   class PXLA : public AgBlock 
   {  public: 
      PXLA() : AgBlock("PXLA","pixel sector"){ }; 
      ~PXLA(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PXLA,1); 
   }; 
   // ---------------------------------------------------------------------- PXRB -- 
   ///@defgroup PXRB_doc 
   ///@class PXRB 
   ///@brief sector right side 
   class PXRB : public AgBlock 
   {  public: 
      PXRB() : AgBlock("PXRB","sector right side"){ }; 
      ~PXRB(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PXRB,1); 
   }; 
   // ---------------------------------------------------------------------- PXLB -- 
   ///@defgroup PXLB_doc 
   ///@class PXLB 
   ///@brief sector left side 
   class PXLB : public AgBlock 
   {  public: 
      PXLB() : AgBlock("PXLB","sector left side"){ }; 
      ~PXLB(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PXLB,1); 
   }; 
   // ---------------------------------------------------------------------- PXIB -- 
   ///@defgroup PXIB_doc 
   ///@class PXIB 
   ///@brief sector bottom side 
   class PXIB : public AgBlock 
   {  public: 
      PXIB() : AgBlock("PXIB","sector bottom side"){ }; 
      ~PXIB(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PXIB,1); 
   }; 
   // ---------------------------------------------------------------------- PXTR -- 
   ///@defgroup PXTR_doc 
   ///@class PXTR 
   ///@brief part of sector right 
   class PXTR : public AgBlock 
   {  public: 
      PXTR() : AgBlock("PXTR","part of sector right"){ }; 
      ~PXTR(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PXTR,1); 
   }; 
   // ---------------------------------------------------------------------- PXTM -- 
   ///@defgroup PXTM_doc 
   ///@class PXTM 
   ///@brief part of sector middle 
   class PXTM : public AgBlock 
   {  public: 
      PXTM() : AgBlock("PXTM","part of sector middle"){ }; 
      ~PXTM(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PXTM,1); 
   }; 
   // ---------------------------------------------------------------------- PXTL -- 
   ///@defgroup PXTL_doc 
   ///@class PXTL 
   ///@brief part of sector right 
   class PXTL : public AgBlock 
   {  public: 
      PXTL() : AgBlock("PXTL","part of sector right"){ }; 
      ~PXTL(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PXTL,1); 
   }; 
   // ---------------------------------------------------------------------- PXTJ -- 
   ///@defgroup PXTJ_doc 
   ///@class PXTJ 
   ///@brief part joining 2 parts of top sector 
   class PXTJ : public AgBlock 
   {  public: 
      PXTJ() : AgBlock("PXTJ","part joining 2 parts of top sector"){ }; 
      ~PXTJ(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PXTJ,1); 
   }; 
   // ---------------------------------------------------------------------- PXCA -- 
   ///@defgroup PXCA_doc 
   ///@class PXCA 
   ///@brief arc bottom right 
   class PXCA : public AgBlock 
   {  public: 
      PXCA() : AgBlock("PXCA","arc bottom right"){ }; 
      ~PXCA(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PXCA,1); 
   }; 
   // ---------------------------------------------------------------------- PXCC -- 
   ///@defgroup PXCC_doc 
   ///@class PXCC 
   ///@brief arc top right 
   class PXCC : public AgBlock 
   {  public: 
      PXCC() : AgBlock("PXCC","arc top right"){ }; 
      ~PXCC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PXCC,1); 
   }; 
   // ---------------------------------------------------------------------- PXCD -- 
   ///@defgroup PXCD_doc 
   ///@class PXCD 
   ///@brief arc top 23 
   class PXCD : public AgBlock 
   {  public: 
      PXCD() : AgBlock("PXCD","arc top 23"){ }; 
      ~PXCD(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PXCD,1); 
   }; 
   // ---------------------------------------------------------------------- PXCE -- 
   ///@defgroup PXCE_doc 
   ///@class PXCE 
   ///@brief arc bottom 23 
   class PXCE : public AgBlock 
   {  public: 
      PXCE() : AgBlock("PXCE","arc bottom 23"){ }; 
      ~PXCE(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PXCE,1); 
   }; 
   // ---------------------------------------------------------------------- PXCF -- 
   ///@defgroup PXCF_doc 
   ///@class PXCF 
   ///@brief arc top 12 
   class PXCF : public AgBlock 
   {  public: 
      PXCF() : AgBlock("PXCF","arc top 12"){ }; 
      ~PXCF(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PXCF,1); 
   }; 
   // ---------------------------------------------------------------------- PXCG -- 
   ///@defgroup PXCG_doc 
   ///@class PXCG 
   ///@brief arc bottom 12 
   class PXCG : public AgBlock 
   {  public: 
      PXCG() : AgBlock("PXCG","arc bottom 12"){ }; 
      ~PXCG(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PXCG,1); 
   }; 
   // ---------------------------------------------------------------------- PXCH -- 
   ///@defgroup PXCH_doc 
   ///@class PXCH 
   ///@brief arc top left 
   class PXCH : public AgBlock 
   {  public: 
      PXCH() : AgBlock("PXCH","arc top left"){ }; 
      ~PXCH(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PXCH,1); 
   }; 
   // ---------------------------------------------------------------------- PXCB -- 
   ///@defgroup PXCB_doc 
   ///@class PXCB 
   ///@brief arc bottom left 
   class PXCB : public AgBlock 
   {  public: 
      PXCB() : AgBlock("PXCB","arc bottom left"){ }; 
      ~PXCB(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PXCB,1); 
   }; 
   // ---------------------------------------------------------------------- PLAC -- 
   ///@defgroup PLAC_doc 
   ///@class PLAC 
   ///@brief active silicon top sector 
   class PLAC : public AgBlock 
   {  public: 
      PLAC() : AgBlock("PLAC","active silicon top sector"){ }; 
      ~PLAC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PLAC,1); 
   }; 
   /// \class PixlGeo4 
   /// \brief Pixel Detector Geometry   
   class PixlGeo4 : public AgModule 
   { 
      public: 
      PixlGeo4(); 
      virtual void ConstructGeometry( const Char_t *dummy="" ); 
      ~PixlGeo4(){ }; 
      ClassDef(PixlGeo4,1); 
      public: 
   }; 
   // endElement in class Module 
}; // namespace PixlGeo4 
#endif // __PixlGeo4__ 

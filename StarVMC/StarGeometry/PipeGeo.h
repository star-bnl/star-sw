#ifndef __PipeGeo__ 
#define __PipeGeo__ 
 
#include "StarVMC/StarAgmlLib/AgModule.h" 
 
namespace PIPEGEO // $NMSPC 
{ 
   class Pipv_t : public AgStructure 
   { 
      ClassDef(Pipv_t,1); 
      public: 
      Float_t version; 
      Float_t pipeconfig; 
      Int_t pipeflag; 
      Pipv_t() : AgStructure("Pipv_t","User-defined AgML structure") 
      { 
         version=0; 
         pipeconfig=0; 
         pipeflag=0; 
         _index=0; 
      } 
      ~ Pipv_t(){ /* nada */ }; 
   }; 
   class Pipg_t : public AgStructure 
   { 
      ClassDef(Pipg_t,1); 
      public: 
      Float_t config; 
      Float_t beinnr; 
      Float_t beoutr; 
      Float_t beleng; 
      TString material; 
      Float_t s1innr; 
      Float_t s1outr; 
      Float_t s1leng; 
      Float_t s2innr; 
      Float_t s2outr; 
      Float_t s2leng; 
      Float_t s3innr; 
      Float_t s3outr; 
      Float_t s3leng; 
      Float_t s4innr; 
      Float_t s4outr; 
      Float_t s4leng; 
      Float_t flange1t; 
      Float_t flange1r; 
      Float_t conelen; 
      Float_t ribnum; 
      Float_t ribspa; 
      Float_t ribthk; 
      Float_t riboutr; 
      Float_t ribcent; 
      Float_t wrpinnr; 
      Float_t wrpoutr; 
      Float_t wrpleng; 
      Float_t sldinnr; 
      Float_t sldoutr; 
      Float_t sldleng; 
      Pipg_t() : AgStructure("Pipg_t","User-defined AgML structure") 
      { 
         config=0; 
         beinnr=0; 
         beoutr=0; 
         beleng=0; 
         material=""; 
         s1innr=0; 
         s1outr=0; 
         s1leng=0; 
         s2innr=0; 
         s2outr=0; 
         s2leng=0; 
         s3innr=0; 
         s3outr=0; 
         s3leng=0; 
         s4innr=0; 
         s4outr=0; 
         s4leng=0; 
         flange1t=0; 
         flange1r=0; 
         conelen=0; 
         ribnum=0; 
         ribspa=0; 
         ribthk=0; 
         riboutr=0; 
         ribcent=0; 
         wrpinnr=0; 
         wrpoutr=0; 
         wrpleng=0; 
         sldinnr=0; 
         sldoutr=0; 
         sldleng=0; 
         _index=0; 
      } 
      ~ Pipg_t(){ /* nada */ }; 
   }; 
   // ---------------------------------------------------------------------- PIPE -- 
   ///@defgroup PIPE_doc 
   ///@class PIPE 
   ///@brief is the STAR beam pipe mother volume 
   class PIPE : public AgBlock 
   {  public: 
      PIPE() : AgBlock("PIPE","is the STAR beam pipe mother volume"){ }; 
      ~PIPE(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PIPE,1); 
   }; 
   // ---------------------------------------------------------------------- PIPC -- 
   ///@defgroup PIPC_doc 
   ///@class PIPC 
   ///@brief is the Central Beam PIPe Volume 
   class PIPC : public AgBlock 
   {  public: 
      PIPC() : AgBlock("PIPC","is the Central Beam PIPe Volume"){ }; 
      ~PIPC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PIPC,1); 
   }; 
   // ---------------------------------------------------------------------- PVAC -- 
   ///@defgroup PVAC_doc 
   ///@class PVAC 
   ///@brief is the Vacuum Volume of Be section of pipe 
   class PVAC : public AgBlock 
   {  public: 
      PVAC() : AgBlock("PVAC","is the Vacuum Volume of Be section of pipe"){ }; 
      ~PVAC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PVAC,1); 
   }; 
   // ---------------------------------------------------------------------- PIPO -- 
   ///@defgroup PIPO_doc 
   ///@class PIPO 
   ///@brief is Steel pipe from Be to 1st flanges 
   class PIPO : public AgBlock 
   {  public: 
      PIPO() : AgBlock("PIPO","is Steel pipe from Be to 1st flanges"){ }; 
      ~PIPO(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PIPO,1); 
   }; 
   // ---------------------------------------------------------------------- PVAO -- 
   ///@defgroup PVAO_doc 
   ///@class PVAO 
   ///@brief is its cavity 
   class PVAO : public AgBlock 
   {  public: 
      PVAO() : AgBlock("PVAO","is its cavity"){ }; 
      ~PVAO(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PVAO,1); 
   }; 
   // ---------------------------------------------------------------------- PIPI -- 
   ///@defgroup PIPI_doc 
   ///@class PIPI 
   ///@brief is Steel pipe of the Bellow section 
   class PIPI : public AgBlock 
   {  public: 
      PIPI() : AgBlock("PIPI","is Steel pipe of the Bellow section"){ }; 
      ~PIPI(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PIPI,1); 
   }; 
   // ---------------------------------------------------------------------- PVAI -- 
   ///@defgroup PVAI_doc 
   ///@class PVAI 
   ///@brief is its cavity 
   class PVAI : public AgBlock 
   {  public: 
      PVAI() : AgBlock("PVAI","is its cavity"){ }; 
      ~PVAI(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PVAI,1); 
   }; 
   // ---------------------------------------------------------------------- PIPT -- 
   ///@defgroup PIPT_doc 
   ///@class PIPT 
   ///@brief is short Steel pipe of the transition section 
   class PIPT : public AgBlock 
   {  public: 
      PIPT() : AgBlock("PIPT","is short Steel pipe of the transition section"){ }; 
      ~PIPT(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PIPT,1); 
   }; 
   // ---------------------------------------------------------------------- PVAT -- 
   ///@defgroup PVAT_doc 
   ///@class PVAT 
   ///@brief is its cavity 
   class PVAT : public AgBlock 
   {  public: 
      PVAT() : AgBlock("PVAT","is its cavity"){ }; 
      ~PVAT(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PVAT,1); 
   }; 
   // ---------------------------------------------------------------------- PIPB -- 
   ///@defgroup PIPB_doc 
   ///@class PIPB 
   ///@brief is the beam pipe Bell reducing section 
   class PIPB : public AgBlock 
   {  public: 
      PIPB() : AgBlock("PIPB","is the beam pipe Bell reducing section"){ }; 
      ~PIPB(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PIPB,1); 
   }; 
   // ---------------------------------------------------------------------- PVAB -- 
   ///@defgroup PVAB_doc 
   ///@class PVAB 
   ///@brief is its cavity 
   class PVAB : public AgBlock 
   {  public: 
      PVAB() : AgBlock("PVAB","is its cavity"){ }; 
      ~PVAB(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PVAB,1); 
   }; 
   // ---------------------------------------------------------------------- PIPS -- 
   ///@defgroup PIPS_doc 
   ///@class PIPS 
   ///@brief 5 inch OD steel beam pipe starting ~4.5 m from IR 
   class PIPS : public AgBlock 
   {  public: 
      PIPS() : AgBlock("PIPS","5 inch OD steel beam pipe starting ~4.5 m from IR"){ }; 
      ~PIPS(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PIPS,1); 
   }; 
   // ---------------------------------------------------------------------- PVAS -- 
   ///@defgroup PVAS_doc 
   ///@class PVAS 
   ///@brief is its cavity 
   class PVAS : public AgBlock 
   {  public: 
      PVAS() : AgBlock("PVAS","is its cavity"){ }; 
      ~PVAS(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PVAS,1); 
   }; 
   // ---------------------------------------------------------------------- PFLO -- 
   ///@defgroup PFLO_doc 
   ///@class PFLO 
   ///@brief is the 1st set of flanges at ~3.9 m from IR 
   class PFLO : public AgBlock 
   {  public: 
      PFLO() : AgBlock("PFLO","is the 1st set of flanges at ~3.9 m from IR"){ }; 
      ~PFLO(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PFLO,1); 
   }; 
   // ---------------------------------------------------------------------- PFLT -- 
   ///@defgroup PFLT_doc 
   ///@class PFLT 
   ///@brief is the 2nd set of flanges at ~4.2 m from IR 
   class PFLT : public AgBlock 
   {  public: 
      PFLT() : AgBlock("PFLT","is the 2nd set of flanges at ~4.2 m from IR"){ }; 
      ~PFLT(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PFLT,1); 
   }; 
   // ---------------------------------------------------------------------- PRIS -- 
   ///@defgroup PRIS_doc 
   ///@class PRIS 
   ///@brief is the Bellow Steel Rib Set 
   class PRIS : public AgBlock 
   {  public: 
      PRIS() : AgBlock("PRIS","is the Bellow Steel Rib Set"){ }; 
      ~PRIS(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PRIS,1); 
   }; 
   // ---------------------------------------------------------------------- PRID -- 
   ///@defgroup PRID_doc 
   ///@class PRID 
   ///@brief is a Rib section 
   class PRID : public AgBlock 
   {  public: 
      PRID() : AgBlock("PRID","is a Rib section"){ }; 
      ~PRID(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PRID,1); 
   }; 
   // ---------------------------------------------------------------------- PRIB -- 
   ///@defgroup PRIB_doc 
   ///@class PRIB 
   ///@brief is a Rib of Steel Bellows 
   class PRIB : public AgBlock 
   {  public: 
      PRIB() : AgBlock("PRIB","is a Rib of Steel Bellows"){ }; 
      ~PRIB(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PRIB,1); 
   }; 
   // ---------------------------------------------------------------------- PWRP -- 
   ///@defgroup PWRP_doc 
   ///@class PWRP 
   ///@brief is the beampipe wrap of Kapton and aluminum 
   class PWRP : public AgBlock 
   {  public: 
      PWRP() : AgBlock("PWRP","is the beampipe wrap of Kapton and aluminum"){ }; 
      ~PWRP(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PWRP,1); 
   }; 
   // ---------------------------------------------------------------------- PSLD -- 
   ///@defgroup PSLD_doc 
   ///@class PSLD 
   ///@brief is the svt beampipe shield 
   class PSLD : public AgBlock 
   {  public: 
      PSLD() : AgBlock("PSLD","is the svt beampipe shield"){ }; 
      ~PSLD(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PSLD,1); 
   }; 
   /// \class PipeGeo 
   /// \brief  is the geometry  of the STAR beam pipe.  
   class PipeGeo : public AgModule 
   { 
      public: 
      PipeGeo(); 
      virtual void ConstructGeometry( const Char_t *dummy="" ); 
      ~PipeGeo(){ }; 
      ClassDef(PipeGeo,1); 
      public: 
   }; 
   // endElement in class Module 
}; // namespace PipeGeo 
#endif // __PipeGeo__ 

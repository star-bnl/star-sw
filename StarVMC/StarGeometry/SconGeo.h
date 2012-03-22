#ifndef __SconGeo__ 
#define __SconGeo__ 
 
#include "StarVMC/StarAgmlLib/AgModule.h" 
 
namespace SCONGEO // $NMSPC 
{ 
   class Svtg_t : public AgStructure 
   { 
      ClassDef(Svtg_t,1); 
      public: 
      Float_t version; 
      Float_t nlayer; 
      Float_t rsizemin; 
      Float_t rsizemax; 
      Float_t zsizemax; 
      Float_t angoff; 
      Float_t supportver; 
      Float_t conever; 
      Float_t ifmany; 
      Float_t nmin; 
      Svtg_t() : AgStructure("Svtg_t","User-defined AgML structure") 
      { 
         version=0; 
         nlayer=0; 
         rsizemin=0; 
         rsizemax=0; 
         zsizemax=0; 
         angoff=0; 
         supportver=0; 
         conever=0; 
         ifmany=0; 
         nmin=0; 
         _index=0; 
      } 
      ~ Svtg_t(){ /* nada */ }; 
   }; 
   class Ssup_t : public AgStructure 
   { 
      ClassDef(Ssup_t,1); 
      public: 
      Float_t version; 
      Float_t cabthk; 
      Float_t hosrmn; 
      Float_t hosrmx; 
      Float_t nhoses; 
      Float_t wrpmythk; 
      Float_t wrpalthk; 
      Float_t grphthk; 
      Float_t cone1zmn; 
      Float_t rodlen; 
      Float_t roddist; 
      Float_t rodid; 
      Float_t rodod; 
      Float_t rodidx; 
      Float_t rododx; 
      Float_t carbonshell; 
      Float_t carbondens; 
      Float_t nomexdens; 
      Float_t con1idmn; 
      Float_t con3idmn; 
      Float_t con4idmn; 
      Float_t con4idmx; 
      Float_t cone3zmx; 
      Float_t cone4zmx; 
      Float_t brathk; 
      Float_t erjthk; 
      Float_t erjwid; 
      Float_t erjlen; 
      Float_t erjzdis; 
      Float_t erj1x; 
      Float_t erj2x; 
      Float_t erj2y; 
      Float_t erjrad; 
      Float_t erjdia; 
      Ssup_t() : AgStructure("Ssup_t","User-defined AgML structure") 
      { 
         version=0; 
         cabthk=0; 
         hosrmn=0; 
         hosrmx=0; 
         nhoses=0; 
         wrpmythk=0; 
         wrpalthk=0; 
         grphthk=0; 
         cone1zmn=0; 
         rodlen=0; 
         roddist=0; 
         rodid=0; 
         rodod=0; 
         rodidx=0; 
         rododx=0; 
         carbonshell=0; 
         carbondens=0; 
         nomexdens=0; 
         con1idmn=0; 
         con3idmn=0; 
         con4idmn=0; 
         con4idmx=0; 
         cone3zmx=0; 
         cone4zmx=0; 
         brathk=0; 
         erjthk=0; 
         erjwid=0; 
         erjlen=0; 
         erjzdis=0; 
         erj1x=0; 
         erj2x=0; 
         erj2y=0; 
         erjrad=0; 
         erjdia=0; 
         _index=0; 
      } 
      ~ Ssup_t(){ /* nada */ }; 
   }; 
   class Ssub_t : public AgStructure 
   { 
      ClassDef(Ssub_t,1); 
      public: 
      Float_t version; 
      Float_t kmountid; 
      Float_t kmountod; 
      Float_t kmntthk; 
      Float_t kmcutod; 
      Float_t kmcutid; 
      Float_t kmcutoa; 
      Float_t kmcutoff; 
      Float_t sringid; 
      Float_t sringod; 
      Float_t sringthk; 
      Float_t srcutphi; 
      Float_t srcutwid; 
      Float_t srcutout; 
      Float_t srcutin; 
      Float_t srollid; 
      Float_t srollod; 
      Float_t srolllen; 
      Float_t swirelen; 
      Float_t mblkhgh; 
      Float_t mblkowid; 
      Float_t mblkolen; 
      Float_t mblkiwid; 
      Float_t mblkilen; 
      Float_t mblkorad; 
      Float_t mblkirad; 
      Float_t mroddia; 
      Ssub_t() : AgStructure("Ssub_t","User-defined AgML structure") 
      { 
         version=0; 
         kmountid=0; 
         kmountod=0; 
         kmntthk=0; 
         kmcutod=0; 
         kmcutid=0; 
         kmcutoa=0; 
         kmcutoff=0; 
         sringid=0; 
         sringod=0; 
         sringthk=0; 
         srcutphi=0; 
         srcutwid=0; 
         srcutout=0; 
         srcutin=0; 
         srollid=0; 
         srollod=0; 
         srolllen=0; 
         swirelen=0; 
         mblkhgh=0; 
         mblkowid=0; 
         mblkolen=0; 
         mblkiwid=0; 
         mblkilen=0; 
         mblkorad=0; 
         mblkirad=0; 
         mroddia=0; 
         _index=0; 
      } 
      ~ Ssub_t(){ /* nada */ }; 
   }; 
   // ---------------------------------------------------------------------- SCON -- 
   ///@defgroup SCON_doc 
   ///@class SCON 
   ///@brief is the Silicon tracker supporting cone mother volume 
   class SCON : public AgBlock 
   {  public: 
      SCON() : AgBlock("SCON","is the Silicon tracker supporting cone mother volume"){ }; 
      ~SCON(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SCON,1); 
   }; 
   // ---------------------------------------------------------------------- SGRA -- 
   ///@defgroup SGRA_doc 
   ///@class SGRA 
   ///@brief is the graphite/epoxy support cone 
   class SGRA : public AgBlock 
   {  public: 
      SGRA() : AgBlock("SGRA","is the graphite/epoxy support cone"){ }; 
      ~SGRA(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SGRA,1); 
   }; 
   // ---------------------------------------------------------------------- SBSP -- 
   ///@defgroup SBSP_doc 
   ///@class SBSP 
   ///@brief is the beampipe support mother volume 
   class SBSP : public AgBlock 
   {  public: 
      SBSP() : AgBlock("SBSP","is the beampipe support mother volume"){ }; 
      ~SBSP(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SBSP,1); 
   }; 
   // ---------------------------------------------------------------------- SBMM -- 
   ///@defgroup SBMM_doc 
   ///@class SBMM 
   ///@brief is the beampipe support mounting mother volume 
   class SBMM : public AgBlock 
   {  public: 
      SBMM() : AgBlock("SBMM","is the beampipe support mounting mother volume"){ }; 
      ~SBMM(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SBMM,1); 
   }; 
   // ---------------------------------------------------------------------- SMRD -- 
   ///@defgroup SMRD_doc 
   ///@class SMRD 
   ///@brief is the aluminum rod carrying the beampipe support 
   class SMRD : public AgBlock 
   {  public: 
      SMRD() : AgBlock("SMRD","is the aluminum rod carrying the beampipe support"){ }; 
      ~SMRD(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SMRD,1); 
   }; 
   // ---------------------------------------------------------------------- SBMO -- 
   ///@defgroup SBMO_doc 
   ///@class SBMO 
   ///@brief is the outer beampipe support mounting block 
   class SBMO : public AgBlock 
   {  public: 
      SBMO() : AgBlock("SBMO","is the outer beampipe support mounting block"){ }; 
      ~SBMO(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SBMO,1); 
   }; 
   // ---------------------------------------------------------------------- SBMI -- 
   ///@defgroup SBMI_doc 
   ///@class SBMI 
   ///@brief is the inner beampipe support mounting block 
   class SBMI : public AgBlock 
   {  public: 
      SBMI() : AgBlock("SBMI","is the inner beampipe support mounting block"){ }; 
      ~SBMI(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SBMI,1); 
   }; 
   // ---------------------------------------------------------------------- SAKM -- 
   ///@defgroup SAKM_doc 
   ///@class SAKM 
   ///@brief is the beampipe support aluminum kinematic mount 
   class SAKM : public AgBlock 
   {  public: 
      SAKM() : AgBlock("SAKM","is the beampipe support aluminum kinematic mount"){ }; 
      ~SAKM(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SAKM,1); 
   }; 
   // ---------------------------------------------------------------------- SCKM -- 
   ///@defgroup SCKM_doc 
   ///@class SCKM 
   ///@brief is the cutout in the aluminum kinematic mount 
   class SCKM : public AgBlock 
   {  public: 
      SCKM() : AgBlock("SCKM","is the cutout in the aluminum kinematic mount"){ }; 
      ~SCKM(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SCKM,1); 
   }; 
   // ---------------------------------------------------------------------- SBRL -- 
   ///@defgroup SBRL_doc 
   ///@class SBRL 
   ///@brief is the ceramic roller supporting the beampipe 
   class SBRL : public AgBlock 
   {  public: 
      SBRL() : AgBlock("SBRL","is the ceramic roller supporting the beampipe"){ }; 
      ~SBRL(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SBRL,1); 
   }; 
   // ---------------------------------------------------------------------- SBRX -- 
   ///@defgroup SBRX_doc 
   ///@class SBRX 
   ///@brief is the stainless steel roller axis 
   class SBRX : public AgBlock 
   {  public: 
      SBRX() : AgBlock("SBRX","is the stainless steel roller axis"){ }; 
      ~SBRX(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SBRX,1); 
   }; 
   // ---------------------------------------------------------------------- SBSR -- 
   ///@defgroup SBSR_doc 
   ///@class SBSR 
   ///@brief is the beampipe support G10 ring 
   class SBSR : public AgBlock 
   {  public: 
      SBSR() : AgBlock("SBSR","is the beampipe support G10 ring"){ }; 
      ~SBSR(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SBSR,1); 
   }; 
   // ---------------------------------------------------------------------- SBCR -- 
   ///@defgroup SBCR_doc 
   ///@class SBCR 
   ///@brief is the cutout in the beampipe support G10 ring 
   class SBCR : public AgBlock 
   {  public: 
      SBCR() : AgBlock("SBCR","is the cutout in the beampipe support G10 ring"){ }; 
      ~SBCR(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SBCR,1); 
   }; 
   // ---------------------------------------------------------------------- SCMY -- 
   ///@defgroup SCMY_doc 
   ///@class SCMY 
   ///@brief is a mylar wrap around the support cone 
   class SCMY : public AgBlock 
   {  public: 
      SCMY() : AgBlock("SCMY","is a mylar wrap around the support cone"){ }; 
      ~SCMY(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SCMY,1); 
   }; 
   // ---------------------------------------------------------------------- SROD -- 
   ///@defgroup SROD_doc 
   ///@class SROD 
   ///@brief is the SVT Carbon composite support rod 
   class SROD : public AgBlock 
   {  public: 
      SROD() : AgBlock("SROD","is the SVT Carbon composite support rod"){ }; 
      ~SROD(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SROD,1); 
   }; 
   // ---------------------------------------------------------------------- SRON -- 
   ///@defgroup SRON_doc 
   ///@class SRON 
   ///@brief Is the creamy nomex filling 
   class SRON : public AgBlock 
   {  public: 
      SRON() : AgBlock("SRON","Is the creamy nomex filling"){ }; 
      ~SRON(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SRON,1); 
   }; 
   // ---------------------------------------------------------------------- SROI -- 
   ///@defgroup SROI_doc 
   ///@class SROI 
   ///@brief Is the inner carbon fiber shell 
   class SROI : public AgBlock 
   {  public: 
      SROI() : AgBlock("SROI","Is the inner carbon fiber shell"){ }; 
      ~SROI(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SROI,1); 
   }; 
   // ---------------------------------------------------------------------- SROH -- 
   ///@defgroup SROH_doc 
   ///@class SROH 
   ///@brief is the hole in SROD 
   class SROH : public AgBlock 
   {  public: 
      SROH() : AgBlock("SROH","is the hole in SROD"){ }; 
      ~SROH(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SROH,1); 
   }; 
   /// \class SconGeo 
   /// \brief  is Support structures from SVTT moved into CAVE:  
   class SconGeo : public AgModule 
   { 
      public: 
      SconGeo(); 
      virtual void ConstructGeometry( const Char_t *dummy="" ); 
      ~SconGeo(){ }; 
      ClassDef(SconGeo,1); 
      public: 
   }; 
   // endElement in class Module 
}; // namespace SconGeo 
#endif // __SconGeo__ 

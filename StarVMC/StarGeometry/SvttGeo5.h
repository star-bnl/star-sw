#ifndef __SvttGeo5__ 
#define __SvttGeo5__ 
 
#include "StarVMC/StarAgmlLib/AgModule.h" 
 
namespace SVTTGEO5 // $NMSPC 
{ 
   class Svtg_t : public AgStructure 
   { 
      ClassDef(Svtg_t,1); 
      public: 
      Float_t version; 
      Int_t nlayer; 
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
   class Swca_t : public AgStructure 
   { 
      ClassDef(Swca_t,1); 
      public: 
      Float_t version; 
      Float_t length; 
      Float_t waferwid; 
      Float_t waferlen; 
      Float_t waferthk; 
      Float_t rohathk; 
      Float_t wafcarwd; 
      Float_t wafcarth; 
      Float_t wafergap; 
      Float_t drift; 
      Float_t strutlen; 
      Swca_t() : AgStructure("Swca_t","User-defined AgML structure") 
      { 
         version=0; 
         length=0; 
         waferwid=0; 
         waferlen=0; 
         waferthk=0; 
         rohathk=0; 
         wafcarwd=0; 
         wafcarth=0; 
         wafergap=0; 
         drift=0; 
         strutlen=0; 
         _index=0; 
      } 
      ~ Swca_t(){ /* nada */ }; 
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
   class Swam_t : public AgStructure 
   { 
      ClassDef(Swam_t,1); 
      public: 
      Float_t version; 
      Float_t zmin; 
      Float_t len; 
      Float_t rmin; 
      Float_t rmax; 
      Float_t tbrdthk; 
      Float_t wallthk; 
      Swam_t() : AgStructure("Swam_t","User-defined AgML structure") 
      { 
         version=0; 
         zmin=0; 
         len=0; 
         rmin=0; 
         rmax=0; 
         tbrdthk=0; 
         wallthk=0; 
         _index=0; 
      } 
      ~ Swam_t(){ /* nada */ }; 
   }; 
   class Serg_t : public AgStructure 
   { 
      ClassDef(Serg_t,1); 
      public: 
      Float_t version; 
      Float_t irngtrmx; 
      Float_t irngprmn; 
      Float_t orngrmin; 
      Float_t orngrmax; 
      Float_t endrngth; 
      Float_t endrngzm; 
      Serg_t() : AgStructure("Serg_t","User-defined AgML structure") 
      { 
         version=0; 
         irngtrmx=0; 
         irngprmn=0; 
         orngrmin=0; 
         orngrmax=0; 
         endrngth=0; 
         endrngzm=0; 
         _index=0; 
      } 
      ~ Serg_t(){ /* nada */ }; 
   }; 
   class Selc_t : public AgStructure 
   { 
      ClassDef(Selc_t,1); 
      public: 
      Float_t version; 
      Float_t bethk; 
      Float_t watthk; 
      Float_t beothk; 
      Float_t dyethk; 
      Float_t dyewid; 
      Float_t dyespc; 
      Float_t elcawid; 
      Float_t agpdthk; 
      Float_t glassthk; 
      Float_t cabthk; 
      Float_t cabwid; 
      Selc_t() : AgStructure("Selc_t","User-defined AgML structure") 
      { 
         version=0; 
         bethk=0; 
         watthk=0; 
         beothk=0; 
         dyethk=0; 
         dyewid=0; 
         dyespc=0; 
         elcawid=0; 
         agpdthk=0; 
         glassthk=0; 
         cabthk=0; 
         cabwid=0; 
         _index=0; 
      } 
      ~ Selc_t(){ /* nada */ }; 
   }; 
   class Svtl_t : public AgStructure 
   { 
      ClassDef(Svtl_t,1); 
      public: 
      Int_t layer; 
      Float_t nladder; 
      Float_t nwafer; 
      Float_t radius; 
      Float_t bareedge; 
      Float_t pcblen; 
      Float_t pcbwidth; 
      Float_t pcbthk; 
      Float_t pcbgap; 
      Svtl_t() : AgStructure("Svtl_t","User-defined AgML structure") 
      { 
         layer=0; 
         nladder=0; 
         nwafer=0; 
         radius=0; 
         bareedge=0; 
         pcblen=0; 
         pcbwidth=0; 
         pcbthk=0; 
         pcbgap=0; 
         _index=0; 
      } 
      ~ Svtl_t(){ /* nada */ }; 
   }; 
   class Ssld_t : public AgStructure 
   { 
      ClassDef(Ssld_t,1); 
      public: 
      Float_t version; 
      Float_t sinrinn; 
      Float_t sinrout; 
      Float_t sinlen; 
      Float_t sseprinn; 
      Float_t sseprout; 
      Float_t sseplen; 
      Float_t soutrinn; 
      Float_t soutrout; 
      Float_t soutlen; 
      Float_t almeshid; 
      Float_t almeshod; 
      Float_t almshthk; 
      Float_t almshpos; 
      Ssld_t() : AgStructure("Ssld_t","User-defined AgML structure") 
      { 
         version=0; 
         sinrinn=0; 
         sinrout=0; 
         sinlen=0; 
         sseprinn=0; 
         sseprout=0; 
         sseplen=0; 
         soutrinn=0; 
         soutrout=0; 
         soutlen=0; 
         almeshid=0; 
         almeshod=0; 
         almshthk=0; 
         almshpos=0; 
         _index=0; 
      } 
      ~ Ssld_t(){ /* nada */ }; 
   }; 
   class Scbp_t : public AgStructure 
   { 
      ClassDef(Scbp_t,1); 
      public: 
      Int_t layer; 
      Float_t len; 
      Float_t rmin1; 
      Float_t rmax1; 
      Float_t rmin2; 
      Float_t rmax2; 
      Float_t vol; 
      Scbp_t() : AgStructure("Scbp_t","User-defined AgML structure") 
      { 
         layer=0; 
         len=0; 
         rmin1=0; 
         rmax1=0; 
         rmin2=0; 
         rmax2=0; 
         vol=0; 
         _index=0; 
      } 
      ~ Scbp_t(){ /* nada */ }; 
   }; 
   class Sfep_t : public AgStructure 
   { 
      ClassDef(Sfep_t,1); 
      public: 
      Int_t layer; 
      Float_t len; 
      Float_t rmin1; 
      Float_t rmax1; 
      Float_t rmin2; 
      Float_t rmax2; 
      Float_t vol; 
      Float_t volplast; 
      Sfep_t() : AgStructure("Sfep_t","User-defined AgML structure") 
      { 
         layer=0; 
         len=0; 
         rmin1=0; 
         rmax1=0; 
         rmin2=0; 
         rmax2=0; 
         vol=0; 
         volplast=0; 
         _index=0; 
      } 
      ~ Sfep_t(){ /* nada */ }; 
   }; 
   class Swcx_t : public AgStructure 
   { 
      ClassDef(Swcx_t,1); 
      public: 
      Int_t layer; 
      Float_t length; 
      Float_t dr; 
      Float_t offset; 
      Float_t rad; 
      Float_t wall; 
      Float_t roffset; 
      Swcx_t() : AgStructure("Swcx_t","User-defined AgML structure") 
      { 
         layer=0; 
         length=0; 
         dr=0; 
         offset=0; 
         rad=0; 
         wall=0; 
         roffset=0; 
         _index=0; 
      } 
      ~ Swcx_t(){ /* nada */ }; 
   }; 
   class Soup_t : public AgStructure 
   { 
      ClassDef(Soup_t,1); 
      public: 
      Float_t version; 
      Float_t length; 
      Float_t rout; 
      Float_t dr; 
      Float_t phi1; 
      Float_t phi2; 
      Float_t diamout; 
      Float_t diamin; 
      Soup_t() : AgStructure("Soup_t","User-defined AgML structure") 
      { 
         version=0; 
         length=0; 
         rout=0; 
         dr=0; 
         phi1=0; 
         phi2=0; 
         diamout=0; 
         diamin=0; 
         _index=0; 
      } 
      ~ Soup_t(){ /* nada */ }; 
   }; 
   // ---------------------------------------------------------------------- SVTT -- 
   ///@defgroup SVTT_doc 
   ///@class SVTT 
   ///@brief is the mother of all SVT volumes 
   class SVTT : public AgBlock 
   {  public: 
      SVTT() : AgBlock("SVTT","is the mother of all SVT volumes"){ }; 
      ~SVTT(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SVTT,1); 
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
   // ---------------------------------------------------------------------- STAP -- 
   ///@defgroup STAP_doc 
   ///@class STAP 
   ///@brief is the plastic part of the twin-ax cable layer (guess polyethylene) 
   class STAP : public AgBlock 
   {  public: 
      STAP() : AgBlock("STAP","is the plastic part of the twin-ax cable layer (guess polyethylene)"){ }; 
      ~STAP(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(STAP,1); 
   }; 
   // ---------------------------------------------------------------------- STAC -- 
   ///@defgroup STAC_doc 
   ///@class STAC 
   ///@brief is the copper part of the twin-ax cable layer 
   class STAC : public AgBlock 
   {  public: 
      STAC() : AgBlock("STAC","is the copper part of the twin-ax cable layer"){ }; 
      ~STAC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(STAC,1); 
   }; 
   // ---------------------------------------------------------------------- SHLA -- 
   ///@defgroup SHLA_doc 
   ///@class SHLA 
   ///@brief is the water hose layer for cone 3 (closer to vertex) 
   class SHLA : public AgBlock 
   {  public: 
      SHLA() : AgBlock("SHLA","is the water hose layer for cone 3 (closer to vertex)"){ }; 
      ~SHLA(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SHLA,1); 
   }; 
   // ---------------------------------------------------------------------- SHLB -- 
   ///@defgroup SHLB_doc 
   ///@class SHLB 
   ///@brief is the water hose layer cone 4 (further from vertex) 
   class SHLB : public AgBlock 
   {  public: 
      SHLB() : AgBlock("SHLB","is the water hose layer cone 4 (further from vertex)"){ }; 
      ~SHLB(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SHLB,1); 
   }; 
   // ---------------------------------------------------------------------- SBRG -- 
   ///@defgroup SBRG_doc 
   ///@class SBRG 
   ///@brief is the bracket joining the end rings 
   class SBRG : public AgBlock 
   {  public: 
      SBRG() : AgBlock("SBRG","is the bracket joining the end rings"){ }; 
      ~SBRG(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SBRG,1); 
   }; 
   // ---------------------------------------------------------------------- SOES -- 
   ///@defgroup SOES_doc 
   ///@class SOES 
   ///@brief is the volume to hold outer endring screws 
   class SOES : public AgBlock 
   {  public: 
      SOES() : AgBlock("SOES","is the volume to hold outer endring screws"){ }; 
      ~SOES(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SOES,1); 
   }; 
   // ---------------------------------------------------------------------- SIES -- 
   ///@defgroup SIES_doc 
   ///@class SIES 
   ///@brief is the volume to hold inner endring screws 
   class SIES : public AgBlock 
   {  public: 
      SIES() : AgBlock("SIES","is the volume to hold inner endring screws"){ }; 
      ~SIES(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SIES,1); 
   }; 
   // ---------------------------------------------------------------------- SISM -- 
   ///@defgroup SISM_doc 
   ///@class SISM 
   ///@brief is the mother volume division for the inner end ring screws 
   class SISM : public AgBlock 
   {  public: 
      SISM() : AgBlock("SISM","is the mother volume division for the inner end ring screws"){ }; 
      ~SISM(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SISM,1); 
   }; 
   // ---------------------------------------------------------------------- SOSM -- 
   ///@defgroup SOSM_doc 
   ///@class SOSM 
   ///@brief is the mother volume division for the outer end ring screws 
   class SOSM : public AgBlock 
   {  public: 
      SOSM() : AgBlock("SOSM","is the mother volume division for the outer end ring screws"){ }; 
      ~SOSM(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SOSM,1); 
   }; 
   // ---------------------------------------------------------------------- SCRW -- 
   ///@defgroup SCRW_doc 
   ///@class SCRW 
   ///@brief is the screw which attaches the end ring to the end ring bracket 
   class SCRW : public AgBlock 
   {  public: 
      SCRW() : AgBlock("SCRW","is the screw which attaches the end ring to the end ring bracket"){ }; 
      ~SCRW(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SCRW,1); 
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
   /// \class SvttGeo5 
   /// \brief   is the SVT geometry for STAR: without the central part  
   class SvttGeo5 : public AgModule 
   { 
      public: 
      SvttGeo5(); 
      virtual void ConstructGeometry( const Char_t *dummy="" ); 
      ~SvttGeo5(){ }; 
      ClassDef(SvttGeo5,1); 
      public: 
   }; 
   // endElement in class Module 
}; // namespace SvttGeo5 
#endif // __SvttGeo5__ 

#ifndef __SvttGeo2__ 
#define __SvttGeo2__ 
 
#include "StarVMC/StarAgmlLib/AgModule.h" 
 
namespace SVTTGEO2 // $NMSPC 
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
      Svtg_t() : AgStructure("Svtg_t","User-defined AgML structure") 
      { 
         version=0; 
         nlayer=0; 
         rsizemin=0; 
         rsizemax=0; 
         zsizemax=0; 
         angoff=0; 
         supportver=0; 
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
   class Sfpa_t : public AgStructure 
   { 
      ClassDef(Sfpa_t,1); 
      public: 
      Float_t version; 
      Float_t rmin; 
      Float_t rmax; 
      Float_t len; 
      Float_t rad; 
      Float_t nssd; 
      Float_t dmwid; 
      Float_t dmthk; 
      Float_t dmlen; 
      Float_t smwid; 
      Float_t smthk; 
      Float_t smlen; 
      Float_t sslen; 
      Float_t wplen; 
      Float_t sdlen; 
      Float_t tilt; 
      Float_t cprad; 
      Float_t cpral; 
      Float_t cfrad; 
      Float_t gpthk; 
      Sfpa_t() : AgStructure("Sfpa_t","User-defined AgML structure") 
      { 
         version=0; 
         rmin=0; 
         rmax=0; 
         len=0; 
         rad=0; 
         nssd=0; 
         dmwid=0; 
         dmthk=0; 
         dmlen=0; 
         smwid=0; 
         smthk=0; 
         smlen=0; 
         sslen=0; 
         wplen=0; 
         sdlen=0; 
         tilt=0; 
         cprad=0; 
         cpral=0; 
         cfrad=0; 
         gpthk=0; 
         _index=0; 
      } 
      ~ Sfpa_t(){ /* nada */ }; 
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
   // ---------------------------------------------------------------------- SXRL -- 
   ///@defgroup SXRL_doc 
   ///@class SXRL 
   ///@brief is the mother of the circular water pipes 
   class SXRL : public AgBlock 
   {  public: 
      SXRL() : AgBlock("SXRL","is the mother of the circular water pipes"){ }; 
      ~SXRL(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SXRL,1); 
   }; 
   // ---------------------------------------------------------------------- SWRP -- 
   ///@defgroup SWRP_doc 
   ///@class SWRP 
   ///@brief is an approximation of water in the circular pipe, a rectangular one 
   class SWRP : public AgBlock 
   {  public: 
      SWRP() : AgBlock("SWRP","is an approximation of water in the circular pipe, a rectangular one"){ }; 
      ~SWRP(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SWRP,1); 
   }; 
   // ---------------------------------------------------------------------- SYRU -- 
   ///@defgroup SYRU_doc 
   ///@class SYRU 
   ///@brief is the wall of the water pipe 
   class SYRU : public AgBlock 
   {  public: 
      SYRU() : AgBlock("SYRU","is the wall of the water pipe"){ }; 
      ~SYRU(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SYRU,1); 
   }; 
   // ---------------------------------------------------------------------- SOUM -- 
   ///@defgroup SOUM_doc 
   ///@class SOUM 
   ///@brief is the mother of the array of the outer shileding support tubes 
   class SOUM : public AgBlock 
   {  public: 
      SOUM() : AgBlock("SOUM","is the mother of the array of the outer shileding support tubes"){ }; 
      ~SOUM(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SOUM,1); 
   }; 
   // ---------------------------------------------------------------------- SOUR -- 
   ///@defgroup SOUR_doc 
   ///@class SOUR 
   ///@brief is the outer shileding support tubes (rods) 
   class SOUR : public AgBlock 
   {  public: 
      SOUR() : AgBlock("SOUR","is the outer shileding support tubes (rods)"){ }; 
      ~SOUR(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SOUR,1); 
   }; 
   // ---------------------------------------------------------------------- SLYD -- 
   ///@defgroup SLYD_doc 
   ///@class SLYD 
   ///@brief is a single SVT layer 
   class SLYD : public AgBlock 
   {  public: 
      SLYD() : AgBlock("SLYD","is a single SVT layer"){ }; 
      ~SLYD(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SLYD,1); 
   }; 
   // ---------------------------------------------------------------------- SLSD -- 
   ///@defgroup SLSD_doc 
   ///@class SLSD 
   ///@brief is a single ladder mother (sector of tube) 
   class SLSD : public AgBlock 
   {  public: 
      SLSD() : AgBlock("SLSD","is a single ladder mother (sector of tube)"){ }; 
      ~SLSD(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SLSD,1); 
   }; 
   // ---------------------------------------------------------------------- SLDI -- 
   ///@defgroup SLDI_doc 
   ///@class SLDI 
   ///@brief is a ladder volume 
   class SLDI : public AgBlock 
   {  public: 
      SLDI() : AgBlock("SLDI","is a ladder volume"){ }; 
      ~SLDI(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SLDI,1); 
   }; 
   // ---------------------------------------------------------------------- SRHC -- 
   ///@defgroup SRHC_doc 
   ///@class SRHC 
   ///@brief is the roha cell wafer support 
   class SRHC : public AgBlock 
   {  public: 
      SRHC() : AgBlock("SRHC","is the roha cell wafer support"){ }; 
      ~SRHC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SRHC,1); 
   }; 
   // ---------------------------------------------------------------------- STLI -- 
   ///@defgroup STLI_doc 
   ///@class STLI 
   ///@brief is the waver pack container 
   class STLI : public AgBlock 
   {  public: 
      STLI() : AgBlock("STLI","is the waver pack container"){ }; 
      ~STLI(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(STLI,1); 
   }; 
   // ---------------------------------------------------------------------- STSI -- 
   ///@defgroup STSI_doc 
   ///@class STSI 
   ///@brief is a single waver container 
   class STSI : public AgBlock 
   {  public: 
      STSI() : AgBlock("STSI","is a single waver container"){ }; 
      ~STSI(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(STSI,1); 
   }; 
   // ---------------------------------------------------------------------- SVTD -- 
   ///@defgroup SVTD_doc 
   ///@class SVTD 
   ///@brief is an active wafer volume 
   class SVTD : public AgBlock 
   {  public: 
      SVTD() : AgBlock("SVTD","is an active wafer volume"){ }; 
      ~SVTD(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SVTD,1); 
   }; 
   // ---------------------------------------------------------------------- SBER -- 
   ///@defgroup SBER_doc 
   ///@class SBER 
   ///@brief are the Berillium wafer carrier rails 
   class SBER : public AgBlock 
   {  public: 
      SBER() : AgBlock("SBER","are the Berillium wafer carrier rails"){ }; 
      ~SBER(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SBER,1); 
   }; 
   // ---------------------------------------------------------------------- STAB -- 
   ///@defgroup STAB_doc 
   ///@class STAB 
   ///@brief are the Berillium wafer carrier end tabs 
   class STAB : public AgBlock 
   {  public: 
      STAB() : AgBlock("STAB","are the Berillium wafer carrier end tabs"){ }; 
      ~STAB(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(STAB,1); 
   }; 
   // ---------------------------------------------------------------------- STRU -- 
   ///@defgroup STRU_doc 
   ///@class STRU 
   ///@brief are the Berillium struts between the wafer carrier rails 
   class STRU : public AgBlock 
   {  public: 
      STRU() : AgBlock("STRU","are the Berillium struts between the wafer carrier rails"){ }; 
      ~STRU(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(STRU,1); 
   }; 
   // ---------------------------------------------------------------------- SPCB -- 
   ///@defgroup SPCB_doc 
   ///@class SPCB 
   ///@brief is the G10 PCB 
   class SPCB : public AgBlock 
   {  public: 
      SPCB() : AgBlock("SPCB","is the G10 PCB"){ }; 
      ~SPCB(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SPCB,1); 
   }; 
   // ---------------------------------------------------------------------- SCBM -- 
   ///@defgroup SCBM_doc 
   ///@class SCBM 
   ///@brief is the mother for the bundle of cables going from PCBs 
   class SCBM : public AgBlock 
   {  public: 
      SCBM() : AgBlock("SCBM","is the mother for the bundle of cables going from PCBs"){ }; 
      ~SCBM(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SCBM,1); 
   }; 
   // ---------------------------------------------------------------------- SCBL -- 
   ///@defgroup SCBL_doc 
   ///@class SCBL 
   ///@brief is the bundle of cables going from PCBs to manifolds 
   class SCBL : public AgBlock 
   {  public: 
      SCBL() : AgBlock("SCBL","is the bundle of cables going from PCBs to manifolds"){ }; 
      ~SCBL(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SCBL,1); 
   }; 
   // ---------------------------------------------------------------------- SFED -- 
   ///@defgroup SFED_doc 
   ///@class SFED 
   ///@brief is the watrer in the bundle of pipes going to manifolds 
   class SFED : public AgBlock 
   {  public: 
      SFED() : AgBlock("SFED","is the watrer in the bundle of pipes going to manifolds"){ }; 
      ~SFED(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SFED,1); 
   }; 
   // ---------------------------------------------------------------------- SPLS -- 
   ///@defgroup SPLS_doc 
   ///@class SPLS 
   ///@brief is the plastic walls of the bundle of pipes going to manifolds 
   class SPLS : public AgBlock 
   {  public: 
      SPLS() : AgBlock("SPLS","is the plastic walls of the bundle of pipes going to manifolds"){ }; 
      ~SPLS(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SPLS,1); 
   }; 
   // ---------------------------------------------------------------------- SELE -- 
   ///@defgroup SELE_doc 
   ///@class SELE 
   ///@brief is the electronics mother volume 
   class SELE : public AgBlock 
   {  public: 
      SELE() : AgBlock("SELE","is the electronics mother volume"){ }; 
      ~SELE(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SELE,1); 
   }; 
   // ---------------------------------------------------------------------- SDYE -- 
   ///@defgroup SDYE_doc 
   ///@class SDYE 
   ///@brief is the ic chip on the hybrid 
   class SDYE : public AgBlock 
   {  public: 
      SDYE() : AgBlock("SDYE","is the ic chip on the hybrid"){ }; 
      ~SDYE(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SDYE,1); 
   }; 
   // ---------------------------------------------------------------------- SECA -- 
   ///@defgroup SECA_doc 
   ///@class SECA 
   ///@brief is the cable on the electronics carrier 
   class SECA : public AgBlock 
   {  public: 
      SECA() : AgBlock("SECA","is the cable on the electronics carrier"){ }; 
      ~SECA(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SECA,1); 
   }; 
   // ---------------------------------------------------------------------- SBOI -- 
   ///@defgroup SBOI_doc 
   ///@class SBOI 
   ///@brief is the Berillia layer 
   class SBOI : public AgBlock 
   {  public: 
      SBOI() : AgBlock("SBOI","is the Berillia layer"){ }; 
      ~SBOI(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SBOI,1); 
   }; 
   // ---------------------------------------------------------------------- SAGP -- 
   ///@defgroup SAGP_doc 
   ///@class SAGP 
   ///@brief is the Silver-Palladium layer 
   class SAGP : public AgBlock 
   {  public: 
      SAGP() : AgBlock("SAGP","is the Silver-Palladium layer"){ }; 
      ~SAGP(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SAGP,1); 
   }; 
   // ---------------------------------------------------------------------- SGLA -- 
   ///@defgroup SGLA_doc 
   ///@class SGLA 
   ///@brief is the insulating glass layer 
   class SGLA : public AgBlock 
   {  public: 
      SGLA() : AgBlock("SGLA","is the insulating glass layer"){ }; 
      ~SGLA(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SGLA,1); 
   }; 
   // ---------------------------------------------------------------------- SWCH -- 
   ///@defgroup SWCH_doc 
   ///@class SWCH 
   ///@brief is the Be top and bottom of the water channel 
   class SWCH : public AgBlock 
   {  public: 
      SWCH() : AgBlock("SWCH","is the Be top and bottom of the water channel"){ }; 
      ~SWCH(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SWCH,1); 
   }; 
   // ---------------------------------------------------------------------- SWCS -- 
   ///@defgroup SWCS_doc 
   ///@class SWCS 
   ///@brief is the Be side of the water channel 
   class SWCS : public AgBlock 
   {  public: 
      SWCS() : AgBlock("SWCS","is the Be side of the water channel"){ }; 
      ~SWCS(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SWCS,1); 
   }; 
   // ---------------------------------------------------------------------- SWCW -- 
   ///@defgroup SWCW_doc 
   ///@class SWCW 
   ///@brief is the water channel water (probably Evian?) 
   class SWCW : public AgBlock 
   {  public: 
      SWCW() : AgBlock("SWCW","is the water channel water (probably Evian?)"){ }; 
      ~SWCW(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SWCW,1); 
   }; 
   // ---------------------------------------------------------------------- SIRT -- 
   ///@defgroup SIRT_doc 
   ///@class SIRT 
   ///@brief is the SVT inner end ring tube 
   class SIRT : public AgBlock 
   {  public: 
      SIRT() : AgBlock("SIRT","is the SVT inner end ring tube"){ }; 
      ~SIRT(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SIRT,1); 
   }; 
   // ---------------------------------------------------------------------- SIRP -- 
   ///@defgroup SIRP_doc 
   ///@class SIRP 
   ///@brief is the SVT inner end ring polycone (overlaps tube) 
   class SIRP : public AgBlock 
   {  public: 
      SIRP() : AgBlock("SIRP","is the SVT inner end ring polycone (overlaps tube)"){ }; 
      ~SIRP(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SIRP,1); 
   }; 
   // ---------------------------------------------------------------------- SOER -- 
   ///@defgroup SOER_doc 
   ///@class SOER 
   ///@brief is the SVT outer end ring 
   class SOER : public AgBlock 
   {  public: 
      SOER() : AgBlock("SOER","is the SVT outer end ring"){ }; 
      ~SOER(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SOER,1); 
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
   // ---------------------------------------------------------------------- SBRM -- 
   ///@defgroup SBRM_doc 
   ///@class SBRM 
   ///@brief is a the mother of a single bracket joining the end rings 
   class SBRM : public AgBlock 
   {  public: 
      SBRM() : AgBlock("SBRM","is a the mother of a single bracket joining the end rings"){ }; 
      ~SBRM(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SBRM,1); 
   }; 
   // ---------------------------------------------------------------------- SBRI -- 
   ///@defgroup SBRI_doc 
   ///@class SBRI 
   ///@brief is the bracket which joins the rings 
   class SBRI : public AgBlock 
   {  public: 
      SBRI() : AgBlock("SBRI","is the bracket which joins the rings"){ }; 
      ~SBRI(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SBRI,1); 
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
   // ---------------------------------------------------------------------- SHMA -- 
   ///@defgroup SHMA_doc 
   ///@class SHMA 
   ///@brief is a single mother volume for a water hose on the cone 3 
   class SHMA : public AgBlock 
   {  public: 
      SHMA() : AgBlock("SHMA","is a single mother volume for a water hose on the cone 3"){ }; 
      ~SHMA(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SHMA,1); 
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
   // ---------------------------------------------------------------------- SHMB -- 
   ///@defgroup SHMB_doc 
   ///@class SHMB 
   ///@brief is a single mother volume for a water hose on the cone 4 
   class SHMB : public AgBlock 
   {  public: 
      SHMB() : AgBlock("SHMB","is a single mother volume for a water hose on the cone 4"){ }; 
      ~SHMB(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SHMB,1); 
   }; 
   // ---------------------------------------------------------------------- SWHO -- 
   ///@defgroup SWHO_doc 
   ///@class SWHO 
   ///@brief is a water hose 
   class SWHO : public AgBlock 
   {  public: 
      SWHO() : AgBlock("SWHO","is a water hose"){ }; 
      ~SWHO(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SWHO,1); 
   }; 
   // ---------------------------------------------------------------------- SHWA -- 
   ///@defgroup SHWA_doc 
   ///@class SHWA 
   ///@brief is the water in the hose 
   class SHWA : public AgBlock 
   {  public: 
      SHWA() : AgBlock("SHWA","is the water in the hose"){ }; 
      ~SHWA(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SHWA,1); 
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
   // ---------------------------------------------------------------------- SCAL -- 
   ///@defgroup SCAL_doc 
   ///@class SCAL 
   ///@brief is the aluminization on the mylar wrap around the support cone 
   class SCAL : public AgBlock 
   {  public: 
      SCAL() : AgBlock("SCAL","is the aluminization on the mylar wrap around the support cone"){ }; 
      ~SCAL(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SCAL,1); 
   }; 
   // ---------------------------------------------------------------------- SWMM -- 
   ///@defgroup SWMM_doc 
   ///@class SWMM 
   ///@brief is the water manifold mother 
   class SWMM : public AgBlock 
   {  public: 
      SWMM() : AgBlock("SWMM","is the water manifold mother"){ }; 
      ~SWMM(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SWMM,1); 
   }; 
   // ---------------------------------------------------------------------- SWMB -- 
   ///@defgroup SWMB_doc 
   ///@class SWMB 
   ///@brief is the water manifold bottom piece (small r) 
   class SWMB : public AgBlock 
   {  public: 
      SWMB() : AgBlock("SWMB","is the water manifold bottom piece (small r)"){ }; 
      ~SWMB(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SWMB,1); 
   }; 
   // ---------------------------------------------------------------------- SWMT -- 
   ///@defgroup SWMT_doc 
   ///@class SWMT 
   ///@brief is the water manifold top piece (big r) 
   class SWMT : public AgBlock 
   {  public: 
      SWMT() : AgBlock("SWMT","is the water manifold top piece (big r)"){ }; 
      ~SWMT(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SWMT,1); 
   }; 
   // ---------------------------------------------------------------------- SWMS -- 
   ///@defgroup SWMS_doc 
   ///@class SWMS 
   ///@brief is the water manifold side pieces 
   class SWMS : public AgBlock 
   {  public: 
      SWMS() : AgBlock("SWMS","is the water manifold side pieces"){ }; 
      ~SWMS(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SWMS,1); 
   }; 
   // ---------------------------------------------------------------------- SWMW -- 
   ///@defgroup SWMW_doc 
   ///@class SWMW 
   ///@brief is the water in the water manifold 
   class SWMW : public AgBlock 
   {  public: 
      SWMW() : AgBlock("SWMW","is the water in the water manifold"){ }; 
      ~SWMW(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SWMW,1); 
   }; 
   // ---------------------------------------------------------------------- SOTB -- 
   ///@defgroup SOTB_doc 
   ///@class SOTB 
   ///@brief is the outer transition board (large r) 
   class SOTB : public AgBlock 
   {  public: 
      SOTB() : AgBlock("SOTB","is the outer transition board (large r)"){ }; 
      ~SOTB(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SOTB,1); 
   }; 
   // ---------------------------------------------------------------------- SITB -- 
   ///@defgroup SITB_doc 
   ///@class SITB 
   ///@brief is the inner transition board (small r) 
   class SITB : public AgBlock 
   {  public: 
      SITB() : AgBlock("SITB","is the inner transition board (small r)"){ }; 
      ~SITB(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SITB,1); 
   }; 
   // ---------------------------------------------------------------------- SBWC -- 
   ///@defgroup SBWC_doc 
   ///@class SBWC 
   ///@brief is the bracket connecting the water manifold to the cone 
   class SBWC : public AgBlock 
   {  public: 
      SBWC() : AgBlock("SBWC","is the bracket connecting the water manifold to the cone"){ }; 
      ~SBWC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SBWC,1); 
   }; 
   // ---------------------------------------------------------------------- SWCM -- 
   ///@defgroup SWCM_doc 
   ///@class SWCM 
   ///@brief is a single bracket mother between mani and cone 
   class SWCM : public AgBlock 
   {  public: 
      SWCM() : AgBlock("SWCM","is a single bracket mother between mani and cone"){ }; 
      ~SWCM(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SWCM,1); 
   }; 
   // ---------------------------------------------------------------------- SXAI -- 
   ///@defgroup SXAI_doc 
   ///@class SXAI 
   ///@brief is a first piece (A) of the bracket between mani and cone (X) 
   class SXAI : public AgBlock 
   {  public: 
      SXAI() : AgBlock("SXAI","is a first piece (A) of the bracket between mani and cone (X)"){ }; 
      ~SXAI(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SXAI,1); 
   }; 
   // ---------------------------------------------------------------------- SXBI -- 
   ///@defgroup SXBI_doc 
   ///@class SXBI 
   ///@brief is a second piece (B) of the bracket between mani and cone (X) 
   class SXBI : public AgBlock 
   {  public: 
      SXBI() : AgBlock("SXBI","is a second piece (B) of the bracket between mani and cone (X)"){ }; 
      ~SXBI(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SXBI,1); 
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
   // ---------------------------------------------------------------------- SFMO -- 
   ///@defgroup SFMO_doc 
   ///@class SFMO 
   ///@brief is the mother of the fourth layer (strip detectors) 
   class SFMO : public AgBlock 
   {  public: 
      SFMO() : AgBlock("SFMO","is the mother of the fourth layer (strip detectors)"){ }; 
      ~SFMO(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SFMO,1); 
   }; 
   // ---------------------------------------------------------------------- SFLM -- 
   ///@defgroup SFLM_doc 
   ///@class SFLM 
   ///@brief is the mother of the 4th layer ladder 
   class SFLM : public AgBlock 
   {  public: 
      SFLM() : AgBlock("SFLM","is the mother of the 4th layer ladder"){ }; 
      ~SFLM(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SFLM,1); 
   }; 
   // ---------------------------------------------------------------------- SFDM -- 
   ///@defgroup SFDM_doc 
   ///@class SFDM 
   ///@brief is the mother of the detectors 
   class SFDM : public AgBlock 
   {  public: 
      SFDM() : AgBlock("SFDM","is the mother of the detectors"){ }; 
      ~SFDM(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SFDM,1); 
   }; 
   // ---------------------------------------------------------------------- SFSW -- 
   ///@defgroup SFSW_doc 
   ///@class SFSW 
   ///@brief is a single wafer container 
   class SFSW : public AgBlock 
   {  public: 
      SFSW() : AgBlock("SFSW","is a single wafer container"){ }; 
      ~SFSW(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SFSW,1); 
   }; 
   // ---------------------------------------------------------------------- SFSD -- 
   ///@defgroup SFSD_doc 
   ///@class SFSD 
   ///@brief is the strip detector 
   class SFSD : public AgBlock 
   {  public: 
      SFSD() : AgBlock("SFSD","is the strip detector"){ }; 
      ~SFSD(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SFSD,1); 
   }; 
   // ---------------------------------------------------------------------- SFSM -- 
   ///@defgroup SFSM_doc 
   ///@class SFSM 
   ///@brief is the mother of the ladder struct. 
   class SFSM : public AgBlock 
   {  public: 
      SFSM() : AgBlock("SFSM","is the mother of the ladder struct."){ }; 
      ~SFSM(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SFSM,1); 
   }; 
   // ---------------------------------------------------------------------- SFSS -- 
   ///@defgroup SFSS_doc 
   ///@class SFSS 
   ///@brief is the subvolume of the mother struct. 
   class SFSS : public AgBlock 
   {  public: 
      SFSS() : AgBlock("SFSS","is the subvolume of the mother struct."){ }; 
      ~SFSS(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SFSS,1); 
   }; 
   // ---------------------------------------------------------------------- SFCP -- 
   ///@defgroup SFCP_doc 
   ///@class SFCP 
   ///@brief is the cooling pipe 
   class SFCP : public AgBlock 
   {  public: 
      SFCP() : AgBlock("SFCP","is the cooling pipe"){ }; 
      ~SFCP(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SFCP,1); 
   }; 
   // ---------------------------------------------------------------------- SFCW -- 
   ///@defgroup SFCW_doc 
   ///@class SFCW 
   ///@brief is the water cylinder in the cooling pipe 
   class SFCW : public AgBlock 
   {  public: 
      SFCW() : AgBlock("SFCW","is the water cylinder in the cooling pipe"){ }; 
      ~SFCW(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SFCW,1); 
   }; 
   // ---------------------------------------------------------------------- SFCF -- 
   ///@defgroup SFCF_doc 
   ///@class SFCF 
   ///@brief is the carbon fiber structure container 
   class SFCF : public AgBlock 
   {  public: 
      SFCF() : AgBlock("SFCF","is the carbon fiber structure container"){ }; 
      ~SFCF(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SFCF,1); 
   }; 
   // ---------------------------------------------------------------------- SFCT -- 
   ///@defgroup SFCT_doc 
   ///@class SFCT 
   ///@brief is the carbon fiber tube 
   class SFCT : public AgBlock 
   {  public: 
      SFCT() : AgBlock("SFCT","is the carbon fiber tube"){ }; 
      ~SFCT(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SFCT,1); 
   }; 
   // ---------------------------------------------------------------------- SFCX -- 
   ///@defgroup SFCX_doc 
   ///@class SFCX 
   ///@brief is the carbon fiber tube 
   class SFCX : public AgBlock 
   {  public: 
      SFCX() : AgBlock("SFCX","is the carbon fiber tube"){ }; 
      ~SFCX(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SFCX,1); 
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
   // ---------------------------------------------------------------------- SALM -- 
   ///@defgroup SALM_doc 
   ///@class SALM 
   ///@brief is the aluminum shield mesh 
   class SALM : public AgBlock 
   {  public: 
      SALM() : AgBlock("SALM","is the aluminum shield mesh"){ }; 
      ~SALM(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SALM,1); 
   }; 
   // ---------------------------------------------------------------------- SISH -- 
   ///@defgroup SISH_doc 
   ///@class SISH 
   ///@brief is the inner shield cylinder 
   class SISH : public AgBlock 
   {  public: 
      SISH() : AgBlock("SISH","is the inner shield cylinder"){ }; 
      ~SISH(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SISH,1); 
   }; 
   // ---------------------------------------------------------------------- SSSH -- 
   ///@defgroup SSSH_doc 
   ///@class SSSH 
   ///@brief is the separation shield cylinder 
   class SSSH : public AgBlock 
   {  public: 
      SSSH() : AgBlock("SSSH","is the separation shield cylinder"){ }; 
      ~SSSH(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SSSH,1); 
   }; 
   // ---------------------------------------------------------------------- SOSH -- 
   ///@defgroup SOSH_doc 
   ///@class SOSH 
   ///@brief is the separation shield cylinder 
   class SOSH : public AgBlock 
   {  public: 
      SOSH() : AgBlock("SOSH","is the separation shield cylinder"){ }; 
      ~SOSH(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SOSH,1); 
   }; 
   /// \class SvttGeo2 
   /// \brief   is the SVT geometry for STAR: corrected and augmented  
   class SvttGeo2 : public AgModule 
   { 
      public: 
      SvttGeo2(); 
      virtual void ConstructGeometry( const Char_t *dummy="" ); 
      ~SvttGeo2(){ }; 
      ClassDef(SvttGeo2,1); 
      public: 
   }; 
   // endElement in class Module 
}; // namespace SvttGeo2 
#endif // __SvttGeo2__ 

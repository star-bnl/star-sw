#ifndef __TpceGeo3a__ 
#define __TpceGeo3a__ 
 
#include "StarVMC/StarAgmlLib/AgModule.h" 
 
namespace TPCEGEO3A // $NMSPC 
{ 
   class Tpcg_t : public AgStructure 
   { 
      ClassDef(Tpcg_t,1); 
      public: 
      Float_t version; 
      Float_t rmin; 
      Float_t rmax; 
      Float_t rminifc; 
      Float_t lengtht; 
      Float_t length; 
      Float_t lengthw; 
      Float_t lengthv; 
      Float_t dzenvelop; 
      Float_t wheelir; 
      Float_t wheelr0; 
      Float_t wheelr1; 
      Float_t wheelr2; 
      Float_t wheeltotalribwidth; 
      Float_t wheelribwidth; 
      Float_t wheelribheight; 
      Float_t wheelboxdx; 
      Float_t wheelboxdy; 
      Float_t wheelor1; 
      Float_t wheelor; 
      Float_t wheelthk1; 
      Float_t wheelthk; 
      Float_t sengasor; 
      Float_t membthk; 
      Float_t tocsdr; 
      Float_t tokadr; 
      Float_t tonxdr; 
      Float_t toaddr; 
      Float_t toigdr; 
      Float_t toaldr; 
      Float_t tohadr; 
      Float_t tiaddr; 
      Float_t tinxdr; 
      Float_t tikadr; 
      Float_t tialdr; 
      Float_t tifcrf; 
      Float_t tifcdrt; 
      Float_t dzyf1; 
      Float_t dzyf2; 
      Float_t dzyf3; 
      Float_t padplanethickness; 
      Float_t dgateground; 
      Float_t wiremountwidth; 
      Float_t wiremountheight; 
      Float_t dxrdo; 
      Float_t dyrdo; 
      Float_t dzrdo; 
      Float_t zrdo; 
      Float_t heigtube; 
      Float_t widtube; 
      Float_t rdocoolingdx; 
      Float_t rdocoolingdy; 
      Float_t rdocoolingdz; 
      Float_t zgatinggrid; 
      Float_t zgroundgrid; 
      Float_t deadzone; 
      Array_t<Float_t> danode; 
      Tpcg_t() : AgStructure("Tpcg_t","User-defined AgML structure") 
      { 
         version=0; 
         rmin=0; 
         rmax=0; 
         rminifc=0; 
         lengtht=0; 
         length=0; 
         lengthw=0; 
         lengthv=0; 
         dzenvelop=0; 
         wheelir=0; 
         wheelr0=0; 
         wheelr1=0; 
         wheelr2=0; 
         wheeltotalribwidth=0; 
         wheelribwidth=0; 
         wheelribheight=0; 
         wheelboxdx=0; 
         wheelboxdy=0; 
         wheelor1=0; 
         wheelor=0; 
         wheelthk1=0; 
         wheelthk=0; 
         sengasor=0; 
         membthk=0; 
         tocsdr=0; 
         tokadr=0; 
         tonxdr=0; 
         toaddr=0; 
         toigdr=0; 
         toaldr=0; 
         tohadr=0; 
         tiaddr=0; 
         tinxdr=0; 
         tikadr=0; 
         tialdr=0; 
         tifcrf=0; 
         tifcdrt=0; 
         dzyf1=0; 
         dzyf2=0; 
         dzyf3=0; 
         padplanethickness=0; 
         dgateground=0; 
         wiremountwidth=0; 
         wiremountheight=0; 
         dxrdo=0; 
         dyrdo=0; 
         dzrdo=0; 
         zrdo=0; 
         heigtube=0; 
         widtube=0; 
         rdocoolingdx=0; 
         rdocoolingdy=0; 
         rdocoolingdz=0; 
         zgatinggrid=0; 
         zgroundgrid=0; 
         deadzone=0; 
         danode = Array_t<Float_t>(2); 
         _index=0; 
      } 
      ~ Tpcg_t(){ /* nada */ }; 
   }; 
   class Tprs_t : public AgStructure 
   { 
      ClassDef(Tprs_t,1); 
      public: 
      Float_t sec; 
      Float_t nrow; 
      Float_t pitch; 
      Float_t width; 
      Float_t super; 
      Float_t danode; 
      Array_t<Float_t> rpads; 
      Array_t<Float_t> npads; 
      Tprs_t() : AgStructure("Tprs_t","User-defined AgML structure") 
      { 
         sec=0; 
         nrow=0; 
         pitch=0; 
         width=0; 
         super=0; 
         danode=0; 
         rpads = Array_t<Float_t>(40); 
         npads = Array_t<Float_t>(40); 
         _index=0; 
      } 
      ~ Tprs_t(){ /* nada */ }; 
   }; 
   class Tecw_t : public AgStructure 
   { 
      ClassDef(Tecw_t,1); 
      public: 
      Float_t sec; 
      Float_t gapwidi; 
      Float_t gapwido; 
      Float_t gapheit; 
      Float_t gaprad; 
      Float_t inwidth; 
      Float_t ouwidth; 
      Float_t widthlip; 
      Float_t noribs; 
      Float_t zsteprib; 
      Float_t widthrib; 
      Float_t height; 
      Float_t thick; 
      Float_t thickal; 
      Float_t rmin; 
      Float_t rcenter; 
      Float_t holedx; 
      Float_t holedy; 
      Tecw_t() : AgStructure("Tecw_t","User-defined AgML structure") 
      { 
         sec=0; 
         gapwidi=0; 
         gapwido=0; 
         gapheit=0; 
         gaprad=0; 
         inwidth=0; 
         ouwidth=0; 
         widthlip=0; 
         noribs=0; 
         zsteprib=0; 
         widthrib=0; 
         height=0; 
         thick=0; 
         thickal=0; 
         rmin=0; 
         rcenter=0; 
         holedx=0; 
         holedy=0; 
         _index=0; 
      } 
      ~ Tecw_t(){ /* nada */ }; 
   }; 
   class Tpcr_t : public AgStructure 
   { 
      ClassDef(Tpcr_t,1); 
      public: 
      Float_t rdovthk; 
      Float_t rdothk; 
      Float_t rdolen; 
      Float_t nrdobrd; 
      Array_t<Float_t> rdoht; 
      Tpcr_t() : AgStructure("Tpcr_t","User-defined AgML structure") 
      { 
         rdovthk=0; 
         rdothk=0; 
         rdolen=0; 
         nrdobrd=0; 
         rdoht = Array_t<Float_t>(9); 
         _index=0; 
      } 
      ~ Tpcr_t(){ /* nada */ }; 
   }; 
   class Tfee_t : public AgStructure 
   { 
      ClassDef(Tfee_t,1); 
      public: 
      Float_t vers; 
      Float_t carddx; 
      Float_t carddy; 
      Float_t carddz; 
      Float_t platedx; 
      Float_t platedy; 
      Float_t platedz; 
      Float_t assemblythickness; 
      Float_t ribdx; 
      Float_t ribdy; 
      Float_t ribdz; 
      Array_t<Float_t> ass; 
      Tfee_t() : AgStructure("Tfee_t","User-defined AgML structure") 
      { 
         vers=0; 
         carddx=0; 
         carddy=0; 
         carddz=0; 
         platedx=0; 
         platedy=0; 
         platedz=0; 
         assemblythickness=0; 
         ribdx=0; 
         ribdy=0; 
         ribdz=0; 
         ass = Array_t<Float_t>(3); 
         _index=0; 
      } 
      ~ Tfee_t(){ /* nada */ }; 
   }; 
   class Ribs_t : public AgStructure 
   { 
      ClassDef(Ribs_t,1); 
      public: 
      Float_t version; 
      Float_t inout; 
      Float_t num; 
      Array_t<Float_t> x; 
      Array_t<Float_t> dx; 
      Array_t<Float_t> y; 
      Array_t<Float_t> dy; 
      Ribs_t() : AgStructure("Ribs_t","User-defined AgML structure") 
      { 
         version=0; 
         inout=0; 
         num=0; 
         x = Array_t<Float_t>(16); 
         dx = Array_t<Float_t>(16); 
         y = Array_t<Float_t>(16); 
         dy = Array_t<Float_t>(16); 
         _index=0; 
      } 
      ~ Ribs_t(){ /* nada */ }; 
   }; 
   class Holz_t : public AgStructure 
   { 
      ClassDef(Holz_t,1); 
      public: 
      Int_t inout; 
      Int_t numberofrows; 
      Array_t<Int_t> numberperrow; 
      Array_t<Float_t> yholes; 
      Holz_t() : AgStructure("Holz_t","User-defined AgML structure") 
      { 
         inout=0; 
         numberofrows=0; 
         numberperrow = Array_t<Int_t>(16); 
         yholes = Array_t<Float_t>(160); 
         _index=0; 
      } 
      ~ Holz_t(){ /* nada */ }; 
   }; 
   class Cool_t : public AgStructure 
   { 
      ClassDef(Cool_t,1); 
      public: 
      Int_t inout; 
      Array_t<Float_t> tubelength; 
      Cool_t() : AgStructure("Cool_t","User-defined AgML structure") 
      { 
         inout=0; 
         tubelength = Array_t<Float_t>(16); 
         _index=0; 
      } 
      ~ Cool_t(){ /* nada */ }; 
   }; 
   // ---------------------------------------------------------------------- TPCE -- 
   ///@defgroup TPCE_doc 
   ///@class TPCE 
   ///@brief is the TPC envelope 
   class TPCE : public AgBlock 
   {  public: 
      TPCE() : AgBlock("TPCE","is the TPC envelope"){ }; 
      ~TPCE(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TPCE,1); 
   }; 
   // ---------------------------------------------------------------------- TPCM -- 
   ///@defgroup TPCM_doc 
   ///@class TPCM 
   ///@brief is the Central Membrane placed in TPC 
   class TPCM : public AgBlock 
   {  public: 
      TPCM() : AgBlock("TPCM","is the Central Membrane placed in TPC"){ }; 
      ~TPCM(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TPCM,1); 
   }; 
   // ---------------------------------------------------------------------- TIFC -- 
   ///@defgroup TIFC_doc 
   ///@class TIFC 
   ///@brief defines the Inner Field Cage placed in TPC 
   class TIFC : public AgBlock 
   {  public: 
      TIFC() : AgBlock("TIFC","defines the Inner Field Cage placed in TPC"){ }; 
      ~TIFC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TIFC,1); 
   }; 
   // ---------------------------------------------------------------------- TIKA -- 
   ///@defgroup TIKA_doc 
   ///@class TIKA 
   ///@brief is the kapton film of the inner field cage 
   class TIKA : public AgBlock 
   {  public: 
      TIKA() : AgBlock("TIKA","is the kapton film of the inner field cage"){ }; 
      ~TIKA(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TIKA,1); 
   }; 
   // ---------------------------------------------------------------------- TINX -- 
   ///@defgroup TINX_doc 
   ///@class TINX 
   ///@brief is the inner nomex structureure 
   class TINX : public AgBlock 
   {  public: 
      TINX() : AgBlock("TINX","is the inner nomex structureure"){ }; 
      ~TINX(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TINX,1); 
   }; 
   // ---------------------------------------------------------------------- TIAD -- 
   ///@defgroup TIAD_doc 
   ///@class TIAD 
   ///@brief the inner adhesive structureure 
   class TIAD : public AgBlock 
   {  public: 
      TIAD() : AgBlock("TIAD","the inner adhesive structureure"){ }; 
      ~TIAD(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TIAD,1); 
   }; 
   // ---------------------------------------------------------------------- TOFC -- 
   ///@defgroup TOFC_doc 
   ///@class TOFC 
   ///@brief defines outer field cage - fill it WITH insulating gas already 
   class TOFC : public AgBlock 
   {  public: 
      TOFC() : AgBlock("TOFC","defines outer field cage - fill it WITH insulating gas already"){ }; 
      ~TOFC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TOFC,1); 
   }; 
   // ---------------------------------------------------------------------- TOFS -- 
   ///@defgroup TOFS_doc 
   ///@class TOFS 
   ///@brief is the Outer Field Cage structureure 
   class TOFS : public AgBlock 
   {  public: 
      TOFS() : AgBlock("TOFS","is the Outer Field Cage structureure"){ }; 
      ~TOFS(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TOFS,1); 
   }; 
   // ---------------------------------------------------------------------- TOKA -- 
   ///@defgroup TOKA_doc 
   ///@class TOKA 
   ///@brief is MYLAR layer 
   class TOKA : public AgBlock 
   {  public: 
      TOKA() : AgBlock("TOKA","is MYLAR layer"){ }; 
      ~TOKA(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TOKA,1); 
   }; 
   // ---------------------------------------------------------------------- TONX -- 
   ///@defgroup TONX_doc 
   ///@class TONX 
   ///@brief is Nomex support 
   class TONX : public AgBlock 
   {  public: 
      TONX() : AgBlock("TONX","is Nomex support"){ }; 
      ~TONX(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TONX,1); 
   }; 
   // ---------------------------------------------------------------------- TOAD -- 
   ///@defgroup TOAD_doc 
   ///@class TOAD 
   ///@brief is Adhesive layer 
   class TOAD : public AgBlock 
   {  public: 
      TOAD() : AgBlock("TOAD","is Adhesive layer"){ }; 
      ~TOAD(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TOAD,1); 
   }; 
   // ---------------------------------------------------------------------- TOIG -- 
   ///@defgroup TOIG_doc 
   ///@class TOIG 
   ///@brief is Insulating Gas (Nitrogen) 
   class TOIG : public AgBlock 
   {  public: 
      TOIG() : AgBlock("TOIG","is Insulating Gas (Nitrogen)"){ }; 
      ~TOIG(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TOIG,1); 
   }; 
   // ---------------------------------------------------------------------- TOHA -- 
   ///@defgroup TOHA_doc 
   ///@class TOHA 
   ///@brief Gas Containment Vessel (Al) + HA 
   class TOHA : public AgBlock 
   {  public: 
      TOHA() : AgBlock("TOHA","Gas Containment Vessel (Al) + HA"){ }; 
      ~TOHA(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TOHA,1); 
   }; 
   // ---------------------------------------------------------------------- TSWH -- 
   ///@defgroup TSWH_doc 
   ///@class TSWH 
   ///@brief TpcSectorWhole is Sector as Whole 
   class TSWH : public AgBlock 
   {  public: 
      TSWH() : AgBlock("TSWH","TpcSectorWhole is Sector as Whole"){ }; 
      ~TSWH(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TSWH,1); 
   }; 
   // ---------------------------------------------------------------------- TSAW -- 
   ///@defgroup TSAW_doc 
   ///@class TSAW 
   ///@brief TpcSectorAndWheel 
   class TSAW : public AgBlock 
   {  public: 
      TSAW() : AgBlock("TSAW","TpcSectorAndWheel"){ }; 
      ~TSAW(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TSAW,1); 
   }; 
   // ---------------------------------------------------------------------- TSAS -- 
   ///@defgroup TSAS_doc 
   ///@class TSAS 
   ///@brief TpcInnerSectorAssembly & TpcOuterSectorAssembly TSAS and TSA1 
   class TSAS : public AgBlock 
   {  public: 
      TSAS() : AgBlock("TSAS","TpcInnerSectorAssembly & TpcOuterSectorAssembly TSAS and TSA1"){ }; 
      ~TSAS(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TSAS,1); 
   }; 
   // ---------------------------------------------------------------------- TWAS -- 
   ///@defgroup TWAS_doc 
   ///@class TWAS 
   ///@brief TpcWheelInnerAssembly & TpcWheelOuterAssembly TWAS and TWA1 
   class TWAS : public AgBlock 
   {  public: 
      TWAS() : AgBlock("TWAS","TpcWheelInnerAssembly & TpcWheelOuterAssembly TWAS and TWA1"){ }; 
      ~TWAS(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TWAS,1); 
   }; 
   // ---------------------------------------------------------------------- FEEA -- 
   ///@defgroup FEEA_doc 
   ///@class FEEA 
   ///@brief TGeoVolumeAssembly(FEE) 
   class FEEA : public AgBlock 
   {  public: 
      FEEA() : AgBlock("FEEA","TGeoVolumeAssembly(FEE)"){ }; 
      ~FEEA(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FEEA,1); 
   }; 
   // ---------------------------------------------------------------------- FEEP -- 
   ///@defgroup FEEP_doc 
   ///@class FEEP 
   ///@brief FEEplate 
   class FEEP : public AgBlock 
   {  public: 
      FEEP() : AgBlock("FEEP","FEEplate"){ }; 
      ~FEEP(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FEEP,1); 
   }; 
   // ---------------------------------------------------------------------- FEER -- 
   ///@defgroup FEER_doc 
   ///@class FEER 
   ///@brief FEERib 
   class FEER : public AgBlock 
   {  public: 
      FEER() : AgBlock("FEER","FEERib"){ }; 
      ~FEER(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FEER,1); 
   }; 
   // ---------------------------------------------------------------------- FEEI -- 
   ///@defgroup FEEI_doc 
   ///@class FEEI 
   ///@brief FEEitself 
   class FEEI : public AgBlock 
   {  public: 
      FEEI() : AgBlock("FEEI","FEEitself"){ }; 
      ~FEEI(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FEEI,1); 
   }; 
   // ---------------------------------------------------------------------- TALS -- 
   ///@defgroup TALS_doc 
   ///@class TALS 
   ///@brief TpcSectorAlSupport the Sector G10 alliminium support 
   class TALS : public AgBlock 
   {  public: 
      TALS() : AgBlock("TALS","TpcSectorAlSupport the Sector G10 alliminium support"){ }; 
      ~TALS(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TALS,1); 
   }; 
   // ---------------------------------------------------------------------- TSGT -- 
   ///@defgroup TSGT_doc 
   ///@class TSGT 
   ///@brief TpcSectorG10 the Sector G10 
   class TSGT : public AgBlock 
   {  public: 
      TSGT() : AgBlock("TSGT","TpcSectorG10 the Sector G10"){ }; 
      ~TSGT(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TSGT,1); 
   }; 
   // ---------------------------------------------------------------------- TWBT -- 
   ///@defgroup TWBT_doc 
   ///@class TWBT 
   ///@brief //WheelBottom 
   class TWBT : public AgBlock 
   {  public: 
      TWBT() : AgBlock("TWBT","//WheelBottom"){ }; 
      ~TWBT(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TWBT,1); 
   }; 
   // ---------------------------------------------------------------------- TWRI -- 
   ///@defgroup TWRI_doc 
   ///@class TWRI 
   ///@brief WheelOuterRing 
   class TWRI : public AgBlock 
   {  public: 
      TWRI() : AgBlock("TWRI","WheelOuterRing"){ }; 
      ~TWRI(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TWRI,1); 
   }; 
   // ---------------------------------------------------------------------- TWRG -- 
   ///@defgroup TWRG_doc 
   ///@class TWRG 
   ///@brief WheelOuterRing PGON part 
   class TWRG : public AgBlock 
   {  public: 
      TWRG() : AgBlock("TWRG","WheelOuterRing PGON part"){ }; 
      ~TWRG(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TWRG,1); 
   }; 
   // ---------------------------------------------------------------------- TWRC -- 
   ///@defgroup TWRC_doc 
   ///@class TWRC 
   ///@brief WheelOuterRing PCON part 
   class TWRC : public AgBlock 
   {  public: 
      TWRC() : AgBlock("TWRC","WheelOuterRing PCON part"){ }; 
      ~TWRC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TWRC,1); 
   }; 
   // ---------------------------------------------------------------------- TWTR -- 
   ///@defgroup TWTR_doc 
   ///@class TWTR 
   ///@brief TpcWheelTopRib 
   class TWTR : public AgBlock 
   {  public: 
      TWTR() : AgBlock("TWTR","TpcWheelTopRib"){ }; 
      ~TWTR(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TWTR,1); 
   }; 
   // ---------------------------------------------------------------------- TWMR -- 
   ///@defgroup TWMR_doc 
   ///@class TWMR 
   ///@brief TpcWheelMiddleRib 
   class TWMR : public AgBlock 
   {  public: 
      TWMR() : AgBlock("TWMR","TpcWheelMiddleRib"){ }; 
      ~TWMR(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TWMR,1); 
   }; 
   // ---------------------------------------------------------------------- TRDO -- 
   ///@defgroup TRDO_doc 
   ///@class TRDO 
   ///@brief TpcRDOAssembly 
   class TRDO : public AgBlock 
   {  public: 
      TRDO() : AgBlock("TRDO","TpcRDOAssembly"){ }; 
      ~TRDO(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TRDO,1); 
   }; 
   // ---------------------------------------------------------------------- TBRW -- 
   ///@defgroup TBRW_doc 
   ///@class TBRW 
   ///@brief WheelRibBox 
   class TBRW : public AgBlock 
   {  public: 
      TBRW() : AgBlock("TBRW","WheelRibBox"){ }; 
      ~TBRW(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TBRW,1); 
   }; 
   // ---------------------------------------------------------------------- TWRB -- 
   ///@defgroup TWRB_doc 
   ///@class TWRB 
   ///@brief TpcWheelRib 
   class TWRB : public AgBlock 
   {  public: 
      TWRB() : AgBlock("TWRB","TpcWheelRib"){ }; 
      ~TWRB(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TWRB,1); 
   }; 
   // ---------------------------------------------------------------------- TCOO -- 
   ///@defgroup TCOO_doc 
   ///@class TCOO 
   ///@brief CoolingTube 
   class TCOO : public AgBlock 
   {  public: 
      TCOO() : AgBlock("TCOO","CoolingTube"){ }; 
      ~TCOO(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TCOO,1); 
   }; 
   // ---------------------------------------------------------------------- TCAB -- 
   ///@defgroup TCAB_doc 
   ///@class TCAB 
   ///@brief Cables 
   class TCAB : public AgBlock 
   {  public: 
      TCAB() : AgBlock("TCAB","Cables"){ }; 
      ~TCAB(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TCAB,1); 
   }; 
   // ---------------------------------------------------------------------- TRDC -- 
   ///@defgroup TRDC_doc 
   ///@class TRDC 
   ///@brief RDOCard 
   class TRDC : public AgBlock 
   {  public: 
      TRDC() : AgBlock("TRDC","RDOCard"){ }; 
      ~TRDC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TRDC,1); 
   }; 
   // ---------------------------------------------------------------------- TRIB -- 
   ///@defgroup TRIB_doc 
   ///@class TRIB 
   ///@brief Tpc Ribs 
   class TRIB : public AgBlock 
   {  public: 
      TRIB() : AgBlock("TRIB","Tpc Ribs"){ }; 
      ~TRIB(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TRIB,1); 
   }; 
   // ---------------------------------------------------------------------- TWIR -- 
   ///@defgroup TWIR_doc 
   ///@class TWIR 
   ///@brief WireMount 
   class TWIR : public AgBlock 
   {  public: 
      TWIR() : AgBlock("TWIR","WireMount"){ }; 
      ~TWIR(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TWIR,1); 
   }; 
   // ---------------------------------------------------------------------- THOL -- 
   ///@defgroup THOL_doc 
   ///@class THOL 
   ///@brief the Sector holes for FEE 
   class THOL : public AgBlock 
   {  public: 
      THOL() : AgBlock("THOL","the Sector holes for FEE"){ }; 
      ~THOL(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(THOL,1); 
   }; 
   // ---------------------------------------------------------------------- FEES -- 
   ///@defgroup FEES_doc 
   ///@class FEES 
   ///@brief TFEESocket 
   class FEES : public AgBlock 
   {  public: 
      FEES() : AgBlock("FEES","TFEESocket"){ }; 
      ~FEES(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FEES,1); 
   }; 
   // ---------------------------------------------------------------------- TPGV -- 
   ///@defgroup TPGV_doc 
   ///@class TPGV 
   ///@brief is the Gas Volume placed in TPC 
   class TPGV : public AgBlock 
   {  public: 
      TPGV() : AgBlock("TPGV","is the Gas Volume placed in TPC"){ }; 
      ~TPGV(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TPGV,1); 
   }; 
   // ---------------------------------------------------------------------- TPSS -- 
   ///@defgroup TPSS_doc 
   ///@class TPSS 
   ///@brief is a division of gas volume corresponding to a supersectors 
   class TPSS : public AgBlock 
   {  public: 
      TPSS() : AgBlock("TPSS","is a division of gas volume corresponding to a supersectors"){ }; 
      ~TPSS(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TPSS,1); 
   }; 
   // ---------------------------------------------------------------------- TPAD -- 
   ///@defgroup TPAD_doc 
   ///@class TPAD 
   ///@brief is a REAL padrow WITH dimensions defined at positioning time 
   class TPAD : public AgBlock 
   {  public: 
      TPAD() : AgBlock("TPAD","is a REAL padrow WITH dimensions defined at positioning time"){ }; 
      ~TPAD(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TPAD,1); 
   }; 
   /// \class TpceGeo3a 
   /// \brief   is the updated TPC  
   class TpceGeo3a : public AgModule 
   { 
      public: 
      TpceGeo3a(); 
      virtual void ConstructGeometry( const Char_t *dummy="" ); 
      ~TpceGeo3a(){ }; 
      ClassDef(TpceGeo3a,1); 
      public: 
   }; 
   // endElement in class Module 
}; // namespace TpceGeo3a 
#endif // __TpceGeo3a__ 

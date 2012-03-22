#ifndef __VpddGeo1__ 
#define __VpddGeo1__ 
 
#include "StarVMC/StarAgmlLib/AgModule.h" 
 
namespace VPDDGEO1 // $NMSPC 
{ 
   class Vpdv_t : public AgStructure 
   { 
      ClassDef(Vpdv_t,1); 
      public: 
      Float_t version; 
      Int_t vpdconfig; 
      Vpdv_t() : AgStructure("Vpdv_t","User-defined AgML structure") 
      { 
         version=0; 
         vpdconfig=0; 
         _index=0; 
      } 
      ~ Vpdv_t(){ /* nada */ }; 
   }; 
   class Vpdg_t : public AgStructure 
   { 
      ClassDef(Vpdg_t,1); 
      public: 
      Float_t version; 
      Float_t zposeast; 
      Float_t zposwest; 
      Float_t rmin; 
      Float_t rmax; 
      Float_t bpwidth; 
      Float_t bplength; 
      Float_t bpthick; 
      Float_t bxheight; 
      Float_t bxlength; 
      Float_t bxwidth; 
      Float_t bxthick; 
      Float_t bxzposc; 
      Float_t fpwidth; 
      Float_t fpheight; 
      Float_t fpthick; 
      Float_t fpawidth; 
      Float_t fpahght; 
      Float_t fpchght; 
      Float_t fphhght; 
      Float_t fphwidth; 
      Float_t fphthick; 
      Float_t stthick; 
      Float_t stheight; 
      Float_t stangle; 
      Float_t stdiagsz; 
      Float_t scwidth; 
      Float_t scheight; 
      Float_t sclength; 
      Float_t scthick; 
      Float_t clheight; 
      Float_t clwidth; 
      Float_t cllength; 
      Float_t clthick; 
      Float_t detlen; 
      Float_t detrad; 
      Float_t detfront; 
      Float_t convthk; 
      Float_t radithk; 
      Float_t eleleng; 
      Float_t drlayer; 
      Float_t numpmt; 
      Float_t pmtwall; 
      Float_t pmtrad; 
      Float_t pmtlen; 
      Float_t ibchoice; 
      Float_t ibposyc; 
      Float_t ibposzc; 
      Float_t ibleng; 
      Float_t ibthickh; 
      Float_t ibthickv; 
      Float_t ibheight; 
      Float_t ibwidth; 
      Float_t ibwlen; 
      Float_t ibwhghtf; 
      Float_t ibwhghtb; 
      Vpdg_t() : AgStructure("Vpdg_t","User-defined AgML structure") 
      { 
         version=0; 
         zposeast=0; 
         zposwest=0; 
         rmin=0; 
         rmax=0; 
         bpwidth=0; 
         bplength=0; 
         bpthick=0; 
         bxheight=0; 
         bxlength=0; 
         bxwidth=0; 
         bxthick=0; 
         bxzposc=0; 
         fpwidth=0; 
         fpheight=0; 
         fpthick=0; 
         fpawidth=0; 
         fpahght=0; 
         fpchght=0; 
         fphhght=0; 
         fphwidth=0; 
         fphthick=0; 
         stthick=0; 
         stheight=0; 
         stangle=0; 
         stdiagsz=0; 
         scwidth=0; 
         scheight=0; 
         sclength=0; 
         scthick=0; 
         clheight=0; 
         clwidth=0; 
         cllength=0; 
         clthick=0; 
         detlen=0; 
         detrad=0; 
         detfront=0; 
         convthk=0; 
         radithk=0; 
         eleleng=0; 
         drlayer=0; 
         numpmt=0; 
         pmtwall=0; 
         pmtrad=0; 
         pmtlen=0; 
         ibchoice=0; 
         ibposyc=0; 
         ibposzc=0; 
         ibleng=0; 
         ibthickh=0; 
         ibthickv=0; 
         ibheight=0; 
         ibwidth=0; 
         ibwlen=0; 
         ibwhghtf=0; 
         ibwhghtb=0; 
         _index=0; 
      } 
      ~ Vpdg_t(){ /* nada */ }; 
   }; 
   // ---------------------------------------------------------------------- VPDD -- 
   ///@defgroup VPDD_doc 
   ///@class VPDD 
   ///@brief is the whole VPPD assembly 
   class VPDD : public AgBlock 
   {  public: 
      VPDD() : AgBlock("VPDD","is the whole VPPD assembly"){ }; 
      ~VPDD(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VPDD,1); 
   }; 
   // ---------------------------------------------------------------------- VPBP -- 
   ///@defgroup VPBP_doc 
   ///@class VPBP 
   ///@brief is the Base Plate 
   class VPBP : public AgBlock 
   {  public: 
      VPBP() : AgBlock("VPBP","is the Base Plate"){ }; 
      ~VPBP(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VPBP,1); 
   }; 
   // ---------------------------------------------------------------------- VPBO -- 
   ///@defgroup VPBO_doc 
   ///@class VPBO 
   ///@brief is container for the hook 
   class VPBO : public AgBlock 
   {  public: 
      VPBO() : AgBlock("VPBO","is container for the hook"){ }; 
      ~VPBO(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VPBO,1); 
   }; 
   // ---------------------------------------------------------------------- VPBA -- 
   ///@defgroup VPBA_doc 
   ///@class VPBA 
   ///@brief is the part of the hook that mounts to the front/back plate 
   class VPBA : public AgBlock 
   {  public: 
      VPBA() : AgBlock("VPBA","is the part of the hook that mounts to the front/back plate"){ }; 
      ~VPBA(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VPBA,1); 
   }; 
   // ---------------------------------------------------------------------- VPBB -- 
   ///@defgroup VPBB_doc 
   ///@class VPBB 
   ///@brief is the part of the hook that mounts to the base plate 
   class VPBB : public AgBlock 
   {  public: 
      VPBB() : AgBlock("VPBB","is the part of the hook that mounts to the base plate"){ }; 
      ~VPBB(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VPBB,1); 
   }; 
   // ---------------------------------------------------------------------- VPFP -- 
   ///@defgroup VPFP_doc 
   ///@class VPFP 
   ///@brief is a single rectangular piece of the frontpanel 
   class VPFP : public AgBlock 
   {  public: 
      VPFP() : AgBlock("VPFP","is a single rectangular piece of the frontpanel"){ }; 
      ~VPFP(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VPFP,1); 
   }; 
   // ---------------------------------------------------------------------- VPFA -- 
   ///@defgroup VPFA_doc 
   ///@class VPFA 
   ///@brief is the central upper part of the frontplate 
   class VPFA : public AgBlock 
   {  public: 
      VPFA() : AgBlock("VPFA","is the central upper part of the frontplate"){ }; 
      ~VPFA(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VPFA,1); 
   }; 
   // ---------------------------------------------------------------------- VPFB -- 
   ///@defgroup VPFB_doc 
   ///@class VPFB 
   ///@brief is the middle upper part of the frontplate 
   class VPFB : public AgBlock 
   {  public: 
      VPFB() : AgBlock("VPFB","is the middle upper part of the frontplate"){ }; 
      ~VPFB(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VPFB,1); 
   }; 
   // ---------------------------------------------------------------------- VPFC -- 
   ///@defgroup VPFC_doc 
   ///@class VPFC 
   ///@brief is the outer upper part of the frontplate 
   class VPFC : public AgBlock 
   {  public: 
      VPFC() : AgBlock("VPFC","is the outer upper part of the frontplate"){ }; 
      ~VPFC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VPFC,1); 
   }; 
   // ---------------------------------------------------------------------- VPST -- 
   ///@defgroup VPST_doc 
   ///@class VPST 
   ///@brief is the strut volume 
   class VPST : public AgBlock 
   {  public: 
      VPST() : AgBlock("VPST","is the strut volume"){ }; 
      ~VPST(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VPST,1); 
   }; 
   // ---------------------------------------------------------------------- VPSV -- 
   ///@defgroup VPSV_doc 
   ///@class VPSV 
   ///@brief is the actual strut between front and backplates 
   class VPSV : public AgBlock 
   {  public: 
      VPSV() : AgBlock("VPSV","is the actual strut between front and backplates"){ }; 
      ~VPSV(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VPSV,1); 
   }; 
   // ---------------------------------------------------------------------- VPSW -- 
   ///@defgroup VPSW_doc 
   ///@class VPSW 
   ///@brief is a tiny piece of aluminium that belongs to the strut 
   class VPSW : public AgBlock 
   {  public: 
      VPSW() : AgBlock("VPSW","is a tiny piece of aluminium that belongs to the strut"){ }; 
      ~VPSW(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VPSW,1); 
   }; 
   // ---------------------------------------------------------------------- VPSC -- 
   ///@defgroup VPSC_doc 
   ///@class VPSC 
   ///@brief is a clamp that holds the strut 
   class VPSC : public AgBlock 
   {  public: 
      VPSC() : AgBlock("VPSC","is a clamp that holds the strut"){ }; 
      ~VPSC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VPSC,1); 
   }; 
   // ---------------------------------------------------------------------- VPSA -- 
   ///@defgroup VPSA_doc 
   ///@class VPSA 
   ///@brief is part of a strut clamp that holds to the frontplate 
   class VPSA : public AgBlock 
   {  public: 
      VPSA() : AgBlock("VPSA","is part of a strut clamp that holds to the frontplate"){ }; 
      ~VPSA(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VPSA,1); 
   }; 
   // ---------------------------------------------------------------------- VPSB -- 
   ///@defgroup VPSB_doc 
   ///@class VPSB 
   ///@brief is part of a strut clamp that holds to the strut 
   class VPSB : public AgBlock 
   {  public: 
      VPSB() : AgBlock("VPSB","is part of a strut clamp that holds to the strut"){ }; 
      ~VPSB(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VPSB,1); 
   }; 
   // ---------------------------------------------------------------------- VPBX -- 
   ///@defgroup VPBX_doc 
   ///@class VPBX 
   ///@brief is the FEE box 
   class VPBX : public AgBlock 
   {  public: 
      VPBX() : AgBlock("VPBX","is the FEE box"){ }; 
      ~VPBX(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VPBX,1); 
   }; 
   // ---------------------------------------------------------------------- VPBI -- 
   ///@defgroup VPBI_doc 
   ///@class VPBI 
   ///@brief is the empty space inside of the FEE box 
   class VPBI : public AgBlock 
   {  public: 
      VPBI() : AgBlock("VPBI","is the empty space inside of the FEE box"){ }; 
      ~VPBI(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VPBI,1); 
   }; 
   // ---------------------------------------------------------------------- VFEE -- 
   ///@defgroup VFEE_doc 
   ///@class VFEE 
   ///@brief is the FEE inside the box 
   class VFEE : public AgBlock 
   {  public: 
      VFEE() : AgBlock("VFEE","is the FEE inside the box"){ }; 
      ~VFEE(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VFEE,1); 
   }; 
   // ---------------------------------------------------------------------- VLEM -- 
   ///@defgroup VLEM_doc 
   ///@class VLEM 
   ///@brief is a Lemo connector on the FEE boards 
   class VLEM : public AgBlock 
   {  public: 
      VLEM() : AgBlock("VLEM","is a Lemo connector on the FEE boards"){ }; 
      ~VLEM(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VLEM,1); 
   }; 
   // ---------------------------------------------------------------------- VPIP -- 
   ///@defgroup VPIP_doc 
   ///@class VPIP 
   ///@brief is the Long Pipe 
   class VPIP : public AgBlock 
   {  public: 
      VPIP() : AgBlock("VPIP","is the Long Pipe"){ }; 
      ~VPIP(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VPIP,1); 
   }; 
   // ---------------------------------------------------------------------- VRNG -- 
   ///@defgroup VRNG_doc 
   ///@class VRNG 
   ///@brief is a single VPD Ring 
   class VRNG : public AgBlock 
   {  public: 
      VRNG() : AgBlock("VRNG","is a single VPD Ring"){ }; 
      ~VRNG(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VRNG,1); 
   }; 
   // ---------------------------------------------------------------------- VSEC -- 
   ///@defgroup VSEC_doc 
   ///@class VSEC 
   ///@brief is one VPD sector with all stuff inside 
   class VSEC : public AgBlock 
   {  public: 
      VSEC() : AgBlock("VSEC","is one VPD sector with all stuff inside"){ }; 
      ~VSEC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VSEC,1); 
   }; 
   // ---------------------------------------------------------------------- VDET -- 
   ///@defgroup VDET_doc 
   ///@class VDET 
   ///@brief is a single detector (Radiator+converter and PMT+electroncs) 
   class VDET : public AgBlock 
   {  public: 
      VDET() : AgBlock("VDET","is a single detector (Radiator+converter and PMT+electroncs)"){ }; 
      ~VDET(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VDET,1); 
   }; 
   // ---------------------------------------------------------------------- VDTI -- 
   ///@defgroup VDTI_doc 
   ///@class VDTI 
   ///@brief is inner part of the single detector 
   class VDTI : public AgBlock 
   {  public: 
      VDTI() : AgBlock("VDTI","is inner part of the single detector"){ }; 
      ~VDTI(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VDTI,1); 
   }; 
   // ---------------------------------------------------------------------- VCNV -- 
   ///@defgroup VCNV_doc 
   ///@class VCNV 
   ///@brief is converter layer (radiator included) 
   class VCNV : public AgBlock 
   {  public: 
      VCNV() : AgBlock("VCNV","is converter layer (radiator included)"){ }; 
      ~VCNV(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VCNV,1); 
   }; 
   // ---------------------------------------------------------------------- VRAD -- 
   ///@defgroup VRAD_doc 
   ///@class VRAD 
   ///@brief is Cerenkov Radiator layer 
   class VRAD : public AgBlock 
   {  public: 
      VRAD() : AgBlock("VRAD","is Cerenkov Radiator layer"){ }; 
      ~VRAD(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VRAD,1); 
   }; 
   // ---------------------------------------------------------------------- VPMT -- 
   ///@defgroup VPMT_doc 
   ///@class VPMT 
   ///@brief is the PMT inner volume 
   class VPMT : public AgBlock 
   {  public: 
      VPMT() : AgBlock("VPMT","is the PMT inner volume"){ }; 
      ~VPMT(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VPMT,1); 
   }; 
   // ---------------------------------------------------------------------- VXST -- 
   ///@defgroup VXST_doc 
   ///@class VXST 
   ///@brief are PMT output cables (just to look nicer) 
   class VXST : public AgBlock 
   {  public: 
      VXST() : AgBlock("VXST","are PMT output cables (just to look nicer)"){ }; 
      ~VXST(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VXST,1); 
   }; 
   // ---------------------------------------------------------------------- VPCL -- 
   ///@defgroup VPCL_doc 
   ///@class VPCL 
   ///@brief is the boat clamp 
   class VPCL : public AgBlock 
   {  public: 
      VPCL() : AgBlock("VPCL","is the boat clamp"){ }; 
      ~VPCL(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VPCL,1); 
   }; 
   // ---------------------------------------------------------------------- VPCF -- 
   ///@defgroup VPCF_doc 
   ///@class VPCF 
   ///@brief is the front plate of the boat clamp 
   class VPCF : public AgBlock 
   {  public: 
      VPCF() : AgBlock("VPCF","is the front plate of the boat clamp"){ }; 
      ~VPCF(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VPCF,1); 
   }; 
   // ---------------------------------------------------------------------- VPCH -- 
   ///@defgroup VPCH_doc 
   ///@class VPCH 
   ///@brief is the horizontal plate of the boat clamp 
   class VPCH : public AgBlock 
   {  public: 
      VPCH() : AgBlock("VPCH","is the horizontal plate of the boat clamp"){ }; 
      ~VPCH(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VPCH,1); 
   }; 
   // ---------------------------------------------------------------------- VPCV -- 
   ///@defgroup VPCV_doc 
   ///@class VPCV 
   ///@brief is the vertical plate of the boat clamp 
   class VPCV : public AgBlock 
   {  public: 
      VPCV() : AgBlock("VPCV","is the vertical plate of the boat clamp"){ }; 
      ~VPCV(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(VPCV,1); 
   }; 
   // ---------------------------------------------------------------------- IBEM -- 
   ///@defgroup IBEM_doc 
   ///@class IBEM 
   ///@brief is the IBeam structure beneath the Bell reducer cone 
   class IBEM : public AgBlock 
   {  public: 
      IBEM() : AgBlock("IBEM","is the IBeam structure beneath the Bell reducer cone"){ }; 
      ~IBEM(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(IBEM,1); 
   }; 
   // ---------------------------------------------------------------------- IBEH -- 
   ///@defgroup IBEH_doc 
   ///@class IBEH 
   ///@brief is a horizontal IBeam plate 
   class IBEH : public AgBlock 
   {  public: 
      IBEH() : AgBlock("IBEH","is a horizontal IBeam plate"){ }; 
      ~IBEH(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(IBEH,1); 
   }; 
   // ---------------------------------------------------------------------- IBEV -- 
   ///@defgroup IBEV_doc 
   ///@class IBEV 
   ///@brief is a vertical IBeam plate 
   class IBEV : public AgBlock 
   {  public: 
      IBEV() : AgBlock("IBEV","is a vertical IBeam plate"){ }; 
      ~IBEV(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(IBEV,1); 
   }; 
   // ---------------------------------------------------------------------- IBEW -- 
   ///@defgroup IBEW_doc 
   ///@class IBEW 
   ///@brief is the first part of the IBeam plate 
   class IBEW : public AgBlock 
   {  public: 
      IBEW() : AgBlock("IBEW","is the first part of the IBeam plate"){ }; 
      ~IBEW(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(IBEW,1); 
   }; 
   /// \class VpddGeo1 
   /// \brief   is the Pseudo Vertex Position Detector of STAR  
   class VpddGeo1 : public AgModule 
   { 
      public: 
      VpddGeo1(); 
      virtual void ConstructGeometry( const Char_t *dummy="" ); 
      ~VpddGeo1(){ }; 
      ClassDef(VpddGeo1,1); 
      public: 
   }; 
   // endElement in class Module 
}; // namespace VpddGeo1 
#endif // __VpddGeo1__ 

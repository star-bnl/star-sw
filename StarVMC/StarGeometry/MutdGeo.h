#ifndef __MutdGeo__ 
#define __MutdGeo__ 
 
#include "StarVMC/StarAgmlLib/AgModule.h" 
 
namespace MUTDGEO // $NMSPC 
{ 
   class Mtdg_t : public AgStructure 
   { 
      ClassDef(Mtdg_t,1); 
      public: 
      Float_t version; 
      Float_t rpmtin; 
      Float_t rpmtout; 
      Float_t rmrpcin; 
      Float_t rmrpcout; 
      Float_t rmin; 
      Float_t rmax; 
      Float_t dz; 
      Float_t length; 
      Array_t<Float_t> radii; 
      Mtdg_t() : AgStructure("Mtdg_t","User-defined AgML structure") 
      { 
         version=0; 
         rpmtin=0; 
         rpmtout=0; 
         rmrpcin=0; 
         rmrpcout=0; 
         rmin=0; 
         rmax=0; 
         dz=0; 
         length=0; 
         radii = Array_t<Float_t>(2); 
         _index=0; 
      } 
      ~ Mtdg_t(){ /* nada */ }; 
   }; 
   class Mtry_t : public AgStructure 
   { 
      ClassDef(Mtry_t,1); 
      public: 
      Float_t height; 
      Float_t width; 
      Float_t length; 
      Float_t wallthk; 
      Float_t supfullh; 
      Float_t supfullw; 
      Float_t suplen; 
      Float_t supbaset; 
      Float_t supbasew; 
      Float_t suparmt; 
      Float_t cooloutr; 
      Float_t coolinnr; 
      Float_t stript; 
      Float_t footinse; 
      Float_t footthk; 
      Float_t foot1len; 
      Float_t foot2thk; 
      Float_t foot3len; 
      Mtry_t() : AgStructure("Mtry_t","User-defined AgML structure") 
      { 
         height=0; 
         width=0; 
         length=0; 
         wallthk=0; 
         supfullh=0; 
         supfullw=0; 
         suplen=0; 
         supbaset=0; 
         supbasew=0; 
         suparmt=0; 
         cooloutr=0; 
         coolinnr=0; 
         stript=0; 
         footinse=0; 
         footthk=0; 
         foot1len=0; 
         foot2thk=0; 
         foot3len=0; 
         _index=0; 
      } 
      ~ Mtry_t(){ /* nada */ }; 
   }; 
   class Mtbb_t : public AgStructure 
   { 
      ClassDef(Mtbb_t,1); 
      public: 
      Float_t slab1len; 
      Float_t slab2len; 
      Float_t slab1x; 
      Float_t slab2x; 
      Float_t slabthck; 
      Float_t slabwid; 
      Float_t convlen; 
      Float_t convwidm; 
      Float_t convthck; 
      Float_t pmtlen; 
      Float_t pmtmaxr; 
      Float_t pmtminr; 
      Float_t baselen; 
      Float_t basemaxr; 
      Float_t baseminr; 
      Float_t electhck; 
      Float_t wrap; 
      Float_t shim; 
      Mtbb_t() : AgStructure("Mtbb_t","User-defined AgML structure") 
      { 
         slab1len=0; 
         slab2len=0; 
         slab1x=0; 
         slab2x=0; 
         slabthck=0; 
         slabwid=0; 
         convlen=0; 
         convwidm=0; 
         convthck=0; 
         pmtlen=0; 
         pmtmaxr=0; 
         pmtminr=0; 
         baselen=0; 
         basemaxr=0; 
         baseminr=0; 
         electhck=0; 
         wrap=0; 
         shim=0; 
         _index=0; 
      } 
      ~ Mtbb_t(){ /* nada */ }; 
   }; 
   class Moff_t : public AgStructure 
   { 
      ClassDef(Moff_t,1); 
      public: 
      Float_t boxwidth; 
      Float_t slatlen; 
      Float_t slat01z; 
      Float_t slat02z; 
      Float_t slat03z; 
      Float_t slat04z; 
      Float_t slat05z; 
      Float_t slat06z; 
      Float_t slat07z; 
      Float_t slat08z; 
      Float_t slat09z; 
      Float_t slat10z; 
      Float_t slatthck; 
      Float_t slatwid; 
      Float_t slatang; 
      Float_t pmtlen; 
      Float_t pmtmaxr; 
      Float_t pmtminr; 
      Float_t baselen; 
      Float_t basemaxr; 
      Float_t baseminr; 
      Float_t socklen; 
      Float_t cellwid; 
      Float_t cellhgt; 
      Float_t elechgt; 
      Float_t electhck; 
      Float_t elecwid; 
      Float_t eleclen; 
      Float_t elec01z; 
      Float_t elec02z; 
      Float_t elec03z; 
      Float_t elec04z; 
      Float_t elec05z; 
      Float_t elec06z; 
      Float_t elec07z; 
      Float_t elec08z; 
      Float_t elec09z; 
      Float_t elec10z; 
      Float_t railthck; 
      Float_t railwid; 
      Float_t coolinnr; 
      Float_t cooloutr; 
      Moff_t() : AgStructure("Moff_t","User-defined AgML structure") 
      { 
         boxwidth=0; 
         slatlen=0; 
         slat01z=0; 
         slat02z=0; 
         slat03z=0; 
         slat04z=0; 
         slat05z=0; 
         slat06z=0; 
         slat07z=0; 
         slat08z=0; 
         slat09z=0; 
         slat10z=0; 
         slatthck=0; 
         slatwid=0; 
         slatang=0; 
         pmtlen=0; 
         pmtmaxr=0; 
         pmtminr=0; 
         baselen=0; 
         basemaxr=0; 
         baseminr=0; 
         socklen=0; 
         cellwid=0; 
         cellhgt=0; 
         elechgt=0; 
         electhck=0; 
         elecwid=0; 
         eleclen=0; 
         elec01z=0; 
         elec02z=0; 
         elec03z=0; 
         elec04z=0; 
         elec05z=0; 
         elec06z=0; 
         elec07z=0; 
         elec08z=0; 
         elec09z=0; 
         elec10z=0; 
         railthck=0; 
         railwid=0; 
         coolinnr=0; 
         cooloutr=0; 
         _index=0; 
      } 
      ~ Moff_t(){ /* nada */ }; 
   }; 
   // ---------------------------------------------------------------------- MUTD -- 
   ///@defgroup MUTD_doc 
   ///@class MUTD 
   ///@brief is the muon detector mother 
   class MUTD : public AgBlock 
   {  public: 
      MUTD() : AgBlock("MUTD","is the muon detector mother"){ }; 
      ~MUTD(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(MUTD,1); 
   }; 
   // ---------------------------------------------------------------------- MUSC -- 
   ///@defgroup MUSC_doc 
   ///@class MUSC 
   ///@brief is a sector of MUON Trigger Barrel Scintillators 
   class MUSC : public AgBlock 
   {  public: 
      MUSC() : AgBlock("MUSC","is a sector of MUON Trigger Barrel Scintillators"){ }; 
      ~MUSC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(MUSC,1); 
   }; 
   // ---------------------------------------------------------------------- MTRA -- 
   ///@defgroup MTRA_doc 
   ///@class MTRA 
   ///@brief is one full tray plus supporting structure for CTB/TOF 
   class MTRA : public AgBlock 
   {  public: 
      MTRA() : AgBlock("MTRA","is one full tray plus supporting structure for CTB/TOF"){ }; 
      ~MTRA(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(MTRA,1); 
   }; 
   // ---------------------------------------------------------------------- MXTR -- 
   ///@defgroup MXTR_doc 
   ///@class MXTR 
   ///@brief is a Main TRay covering box for CTB or TOF 
   class MXTR : public AgBlock 
   {  public: 
      MXTR() : AgBlock("MXTR","is a Main TRay covering box for CTB or TOF"){ }; 
      ~MXTR(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(MXTR,1); 
   }; 
   // ---------------------------------------------------------------------- MMTC -- 
   ///@defgroup MMTC_doc 
   ///@class MMTC 
   ///@brief is the Main Tray Cavity filled with the details for CTB 
   class MMTC : public AgBlock 
   {  public: 
      MMTC() : AgBlock("MMTC","is the Main Tray Cavity filled with the details for CTB"){ }; 
      ~MMTC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(MMTC,1); 
   }; 
   // ---------------------------------------------------------------------- MXSA -- 
   ///@defgroup MXSA_doc 
   ///@class MXSA 
   ///@brief is the active trigger scintillator SLAB for ctb 
   class MXSA : public AgBlock 
   {  public: 
      MXSA() : AgBlock("MXSA","is the active trigger scintillator SLAB for ctb"){ }; 
      ~MXSA(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(MXSA,1); 
   }; 
   // ---------------------------------------------------------------------- MPMT -- 
   ///@defgroup MPMT_doc 
   ///@class MPMT 
   ///@brief is a Main TRay covering box for PMT 
   class MPMT : public AgBlock 
   {  public: 
      MPMT() : AgBlock("MPMT","is a Main TRay covering box for PMT"){ }; 
      ~MPMT(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(MPMT,1); 
   }; 
   // ---------------------------------------------------------------------- MMRP -- 
   ///@defgroup MMRP_doc 
   ///@class MMRP 
   ///@brief is a Main TRay covering box for MRPC 
   class MMRP : public AgBlock 
   {  public: 
      MMRP() : AgBlock("MMRP","is a Main TRay covering box for MRPC"){ }; 
      ~MMRP(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(MMRP,1); 
   }; 
   /// \class MutdGeo 
   /// \brief  is the geometry of the STAR muon trigger system  
   class MutdGeo : public AgModule 
   { 
      public: 
      MutdGeo(); 
      virtual void ConstructGeometry( const Char_t *dummy="" ); 
      ~MutdGeo(){ }; 
      ClassDef(MutdGeo,1); 
      public: 
   }; 
   // endElement in class Module 
}; // namespace MutdGeo 
#endif // __MutdGeo__ 

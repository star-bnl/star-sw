#ifndef __FtpcGeo1__ 
#define __FtpcGeo1__ 
 
#include "StarVMC/StarAgmlLib/AgModule.h" 
 
namespace FTPCGEO1 // $NMSPC 
{ 
   class Ftpg_t : public AgStructure 
   { 
      ClassDef(Ftpg_t,1); 
      public: 
      Float_t version; 
      Float_t rinnerms; 
      Float_t routerms; 
      Float_t rgasout; 
      Float_t rrom; 
      Float_t relcard; 
      Float_t rcooplm; 
      Float_t rcoople; 
      Float_t zstart; 
      Float_t totlen; 
      Float_t laylen; 
      Float_t hitlay; 
      Float_t drinall1; 
      Float_t drinall2; 
      Float_t drinisol; 
      Float_t dzkapton; 
      Float_t drifr; 
      Float_t dzifr; 
      Float_t dzer; 
      Float_t dzrom; 
      Float_t dzsura; 
      Float_t dzsurb; 
      Float_t dzsmpr; 
      Float_t dzbipr; 
      Float_t msrdz; 
      Float_t serhole; 
      Float_t risring; 
      Float_t isringdz; 
      Float_t sbsrdx; 
      Float_t sbsrdy; 
      Float_t sbsrdz; 
      Float_t gasvoldz; 
      Ftpg_t() : AgStructure("Ftpg_t","User-defined AgML structure") 
      { 
         version=0; 
         rinnerms=0; 
         routerms=0; 
         rgasout=0; 
         rrom=0; 
         relcard=0; 
         rcooplm=0; 
         rcoople=0; 
         zstart=0; 
         totlen=0; 
         laylen=0; 
         hitlay=0; 
         drinall1=0; 
         drinall2=0; 
         drinisol=0; 
         dzkapton=0; 
         drifr=0; 
         dzifr=0; 
         dzer=0; 
         dzrom=0; 
         dzsura=0; 
         dzsurb=0; 
         dzsmpr=0; 
         dzbipr=0; 
         msrdz=0; 
         serhole=0; 
         risring=0; 
         isringdz=0; 
         sbsrdx=0; 
         sbsrdy=0; 
         sbsrdz=0; 
         gasvoldz=0; 
         _index=0; 
      } 
      ~ Ftpg_t(){ /* nada */ }; 
   }; 
   class Ffcc_t : public AgStructure 
   { 
      ClassDef(Ffcc_t,1); 
      public: 
      Float_t version; 
      Float_t stileng; 
      Float_t stidia; 
      Float_t stirpos; 
      Float_t rithick; 
      Float_t ridr; 
      Float_t rigap; 
      Float_t barleng; 
      Float_t barwidt; 
      Float_t barthik; 
      Ffcc_t() : AgStructure("Ffcc_t","User-defined AgML structure") 
      { 
         version=0; 
         stileng=0; 
         stidia=0; 
         stirpos=0; 
         rithick=0; 
         ridr=0; 
         rigap=0; 
         barleng=0; 
         barwidt=0; 
         barthik=0; 
         _index=0; 
      } 
      ~ Ffcc_t(){ /* nada */ }; 
   }; 
   class Frbd_t : public AgStructure 
   { 
      ClassDef(Frbd_t,1); 
      public: 
      Float_t version; 
      Float_t phi1; 
      Float_t phi2; 
      Float_t phi3; 
      Float_t phi4; 
      Float_t phi5; 
      Float_t phi6; 
      Float_t phi7; 
      Float_t phi8; 
      Float_t phi9; 
      Float_t phi10; 
      Float_t phi11; 
      Float_t phi12; 
      Float_t phi13; 
      Float_t xrom; 
      Float_t yrom; 
      Float_t zrom; 
      Float_t rahol; 
      Float_t xehol; 
      Float_t yehol; 
      Float_t xlhol; 
      Float_t ylhol; 
      Float_t boffset; 
      Float_t zoffb; 
      Float_t modleng; 
      Float_t electrdx; 
      Float_t electrdy; 
      Float_t electrdz; 
      Float_t coolpldx; 
      Float_t coolpldy; 
      Float_t coolpldz; 
      Float_t eclpldx; 
      Float_t eclpldy; 
      Float_t eclpldz; 
      Float_t cakehir; 
      Float_t cakehor; 
      Float_t cakehwz; 
      Float_t boxhx; 
      Float_t boxhy; 
      Float_t boxhz; 
      Float_t eboxhx; 
      Float_t eboxhy; 
      Float_t eboxhz; 
      Float_t lboxhx; 
      Float_t lboxhy; 
      Float_t lboxhz; 
      Frbd_t() : AgStructure("Frbd_t","User-defined AgML structure") 
      { 
         version=0; 
         phi1=0; 
         phi2=0; 
         phi3=0; 
         phi4=0; 
         phi5=0; 
         phi6=0; 
         phi7=0; 
         phi8=0; 
         phi9=0; 
         phi10=0; 
         phi11=0; 
         phi12=0; 
         phi13=0; 
         xrom=0; 
         yrom=0; 
         zrom=0; 
         rahol=0; 
         xehol=0; 
         yehol=0; 
         xlhol=0; 
         ylhol=0; 
         boffset=0; 
         zoffb=0; 
         modleng=0; 
         electrdx=0; 
         electrdy=0; 
         electrdz=0; 
         coolpldx=0; 
         coolpldy=0; 
         coolpldz=0; 
         eclpldx=0; 
         eclpldy=0; 
         eclpldz=0; 
         cakehir=0; 
         cakehor=0; 
         cakehwz=0; 
         boxhx=0; 
         boxhy=0; 
         boxhz=0; 
         eboxhx=0; 
         eboxhy=0; 
         eboxhz=0; 
         lboxhx=0; 
         lboxhy=0; 
         lboxhz=0; 
         _index=0; 
      } 
      ~ Frbd_t(){ /* nada */ }; 
   }; 
   class Fssd_t : public AgStructure 
   { 
      ClassDef(Fssd_t,1); 
      public: 
      Float_t version; 
      Float_t eringrmn; 
      Float_t eringrmx; 
      Float_t eringdz; 
      Float_t oeringdz; 
      Float_t erposz; 
      Float_t meringrm; 
      Float_t meringdz; 
      Float_t erpolyrm; 
      Float_t trapr; 
      Float_t polyr; 
      Float_t polydz; 
      Float_t polyir; 
      Float_t polyor; 
      Float_t trapx1; 
      Float_t trapx2; 
      Float_t trapdy; 
      Float_t trapdz; 
      Float_t pgonpdz; 
      Float_t sbsdy; 
      Fssd_t() : AgStructure("Fssd_t","User-defined AgML structure") 
      { 
         version=0; 
         eringrmn=0; 
         eringrmx=0; 
         eringdz=0; 
         oeringdz=0; 
         erposz=0; 
         meringrm=0; 
         meringdz=0; 
         erpolyrm=0; 
         trapr=0; 
         polyr=0; 
         polydz=0; 
         polyir=0; 
         polyor=0; 
         trapx1=0; 
         trapx2=0; 
         trapdy=0; 
         trapdz=0; 
         pgonpdz=0; 
         sbsdy=0; 
         _index=0; 
      } 
      ~ Fssd_t(){ /* nada */ }; 
   }; 
   // ---------------------------------------------------------------------- FTPC -- 
   ///@defgroup FTPC_doc 
   ///@class FTPC 
   ///@brief is the Forward TPC mother (needed for standalong test only) 
   class FTPC : public AgBlock 
   {  public: 
      FTPC() : AgBlock("FTPC","is the Forward TPC mother (needed for standalong test only)"){ }; 
      ~FTPC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FTPC,1); 
   }; 
   // ---------------------------------------------------------------------- FIAL -- 
   ///@defgroup FIAL_doc 
   ///@class FIAL 
   ///@brief is the inner AL-tube of the FTPC 
   class FIAL : public AgBlock 
   {  public: 
      FIAL() : AgBlock("FIAL","is the inner AL-tube of the FTPC"){ }; 
      ~FIAL(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FIAL,1); 
   }; 
   // ---------------------------------------------------------------------- FMPT -- 
   ///@defgroup FMPT_doc 
   ///@class FMPT 
   ///@brief is the insulating plastic tube of the drift-electrode 
   class FMPT : public AgBlock 
   {  public: 
      FMPT() : AgBlock("FMPT","is the insulating plastic tube of the drift-electrode"){ }; 
      ~FMPT(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FMPT,1); 
   }; 
   // ---------------------------------------------------------------------- FOAL -- 
   ///@defgroup FOAL_doc 
   ///@class FOAL 
   ///@brief is the Al drift-electrode 
   class FOAL : public AgBlock 
   {  public: 
      FOAL() : AgBlock("FOAL","is the Al drift-electrode"){ }; 
      ~FOAL(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FOAL,1); 
   }; 
   // ---------------------------------------------------------------------- FGAS -- 
   ///@defgroup FGAS_doc 
   ///@class FGAS 
   ///@brief is the FTPC gas volume 
   class FGAS : public AgBlock 
   {  public: 
      FGAS() : AgBlock("FGAS","is the FTPC gas volume"){ }; 
      ~FGAS(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FGAS,1); 
   }; 
   // ---------------------------------------------------------------------- FSEN -- 
   ///@defgroup FSEN_doc 
   ///@class FSEN 
   ///@brief is the sensitive gas volume 
   class FSEN : public AgBlock 
   {  public: 
      FSEN() : AgBlock("FSEN","is the sensitive gas volume"){ }; 
      ~FSEN(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FSEN,1); 
   }; 
   // ---------------------------------------------------------------------- FSEC -- 
   ///@defgroup FSEC_doc 
   ///@class FSEC 
   ///@brief is a sensitive gas sector 
   class FSEC : public AgBlock 
   {  public: 
      FSEC() : AgBlock("FSEC","is a sensitive gas sector"){ }; 
      ~FSEC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FSEC,1); 
   }; 
   // ---------------------------------------------------------------------- FIFR -- 
   ///@defgroup FIFR_doc 
   ///@class FIFR 
   ///@brief is the Al inner flange ring 
   class FIFR : public AgBlock 
   {  public: 
      FIFR() : AgBlock("FIFR","is the Al inner flange ring"){ }; 
      ~FIFR(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FIFR,1); 
   }; 
   // ---------------------------------------------------------------------- FKWI -- 
   ///@defgroup FKWI_doc 
   ///@class FKWI 
   ///@brief is the double Kapton window 
   class FKWI : public AgBlock 
   {  public: 
      FKWI() : AgBlock("FKWI","is the double Kapton window"){ }; 
      ~FKWI(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FKWI,1); 
   }; 
   // ---------------------------------------------------------------------- FFSL -- 
   ///@defgroup FFSL_doc 
   ///@class FFSL 
   ///@brief is ceramic holder for fieldcage rings 
   class FFSL : public AgBlock 
   {  public: 
      FFSL() : AgBlock("FFSL","is ceramic holder for fieldcage rings"){ }; 
      ~FFSL(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FFSL,1); 
   }; 
   // ---------------------------------------------------------------------- FFCE -- 
   ///@defgroup FFCE_doc 
   ///@class FFCE 
   ///@brief is the Fildcage Enhanced Support Structure 
   class FFCE : public AgBlock 
   {  public: 
      FFCE() : AgBlock("FFCE","is the Fildcage Enhanced Support Structure"){ }; 
      ~FFCE(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FFCE,1); 
   }; 
   // ---------------------------------------------------------------------- FROS -- 
   ///@defgroup FROS_doc 
   ///@class FROS 
   ///@brief is one Ring of Readout Modules in the support Structure 
   class FROS : public AgBlock 
   {  public: 
      FROS() : AgBlock("FROS","is one Ring of Readout Modules in the support Structure"){ }; 
      ~FROS(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FROS,1); 
   }; 
   // ---------------------------------------------------------------------- FROM -- 
   ///@defgroup FROM_doc 
   ///@class FROM 
   ///@brief is one Module of the Readout Chamber 
   class FROM : public AgBlock 
   {  public: 
      FROM() : AgBlock("FROM","is one Module of the Readout Chamber"){ }; 
      ~FROM(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FROM,1); 
   }; 
   // ---------------------------------------------------------------------- FROB -- 
   ///@defgroup FROB_doc 
   ///@class FROB 
   ///@brief are the Box Holes in the Readout Chamber 
   class FROB : public AgBlock 
   {  public: 
      FROB() : AgBlock("FROB","are the Box Holes in the Readout Chamber"){ }; 
      ~FROB(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FROB,1); 
   }; 
   // ---------------------------------------------------------------------- FROE -- 
   ///@defgroup FROE_doc 
   ///@class FROE 
   ///@brief are the End Box Holes in the Readout Chamber 
   class FROE : public AgBlock 
   {  public: 
      FROE() : AgBlock("FROE","are the End Box Holes in the Readout Chamber"){ }; 
      ~FROE(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FROE,1); 
   }; 
   // ---------------------------------------------------------------------- FROL -- 
   ///@defgroup FROL_doc 
   ///@class FROL 
   ///@brief are the Length side Box Holes in the Readout Chamber 
   class FROL : public AgBlock 
   {  public: 
      FROL() : AgBlock("FROL","are the Length side Box Holes in the Readout Chamber"){ }; 
      ~FROL(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FROL,1); 
   }; 
   // ---------------------------------------------------------------------- FROP -- 
   ///@defgroup FROP_doc 
   ///@class FROP 
   ///@brief are the Polygon part of the support bar 
   class FROP : public AgBlock 
   {  public: 
      FROP() : AgBlock("FROP","are the Polygon part of the support bar"){ }; 
      ~FROP(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FROP,1); 
   }; 
   // ---------------------------------------------------------------------- FROT -- 
   ///@defgroup FROT_doc 
   ///@class FROT 
   ///@brief are the Trapezoid part of the support bar 
   class FROT : public AgBlock 
   {  public: 
      FROT() : AgBlock("FROT","are the Trapezoid part of the support bar"){ }; 
      ~FROT(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FROT,1); 
   }; 
   // ---------------------------------------------------------------------- FREL -- 
   ///@defgroup FREL_doc 
   ///@class FREL 
   ///@brief is the Electronics Layer of the Readout Chamber 
   class FREL : public AgBlock 
   {  public: 
      FREL() : AgBlock("FREL","is the Electronics Layer of the Readout Chamber"){ }; 
      ~FREL(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FREL,1); 
   }; 
   // ---------------------------------------------------------------------- FRCC -- 
   ///@defgroup FRCC_doc 
   ///@class FRCC 
   ///@brief is the Copper Cooling Layer of the Readout Chamber (Middle) 
   class FRCC : public AgBlock 
   {  public: 
      FRCC() : AgBlock("FRCC","is the Copper Cooling Layer of the Readout Chamber (Middle)"){ }; 
      ~FRCC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FRCC,1); 
   }; 
   // ---------------------------------------------------------------------- FRCE -- 
   ///@defgroup FRCE_doc 
   ///@class FRCE 
   ///@brief is the Copper Cooling Layer of the Readout Chamber (Ends) 
   class FRCE : public AgBlock 
   {  public: 
      FRCE() : AgBlock("FRCE","is the Copper Cooling Layer of the Readout Chamber (Ends)"){ }; 
      ~FRCE(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FRCE,1); 
   }; 
   // ---------------------------------------------------------------------- FSER -- 
   ///@defgroup FSER_doc 
   ///@class FSER 
   ///@brief is the Support End Ring 
   class FSER : public AgBlock 
   {  public: 
      FSER() : AgBlock("FSER","is the Support End Ring"){ }; 
      ~FSER(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FSER,1); 
   }; 
   // ---------------------------------------------------------------------- FSRA -- 
   ///@defgroup FSRA_doc 
   ///@class FSRA 
   ///@brief is the outer Support End Ring 
   class FSRA : public AgBlock 
   {  public: 
      FSRA() : AgBlock("FSRA","is the outer Support End Ring"){ }; 
      ~FSRA(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FSRA,1); 
   }; 
   // ---------------------------------------------------------------------- FSRB -- 
   ///@defgroup FSRB_doc 
   ///@class FSRB 
   ///@brief is the medium Support End Ring 
   class FSRB : public AgBlock 
   {  public: 
      FSRB() : AgBlock("FSRB","is the medium Support End Ring"){ }; 
      ~FSRB(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FSRB,1); 
   }; 
   // ---------------------------------------------------------------------- FSSM -- 
   ///@defgroup FSSM_doc 
   ///@class FSSM 
   ///@brief is the main Support Stucture Module 
   class FSSM : public AgBlock 
   {  public: 
      FSSM() : AgBlock("FSSM","is the main Support Stucture Module"){ }; 
      ~FSSM(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FSSM,1); 
   }; 
   // ---------------------------------------------------------------------- FSPG -- 
   ///@defgroup FSPG_doc 
   ///@class FSPG 
   ///@brief is the inner Support End Ring and the outer support Rings 
   class FSPG : public AgBlock 
   {  public: 
      FSPG() : AgBlock("FSPG","is the inner Support End Ring and the outer support Rings"){ }; 
      ~FSPG(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FSPG,1); 
   }; 
   // ---------------------------------------------------------------------- FSPI -- 
   ///@defgroup FSPI_doc 
   ///@class FSPI 
   ///@brief is the Hole of the inner Support End Ring 
   class FSPI : public AgBlock 
   {  public: 
      FSPI() : AgBlock("FSPI","is the Hole of the inner Support End Ring"){ }; 
      ~FSPI(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FSPI,1); 
   }; 
   // ---------------------------------------------------------------------- FSRI -- 
   ///@defgroup FSRI_doc 
   ///@class FSRI 
   ///@brief is the inner Support Ring 
   class FSRI : public AgBlock 
   {  public: 
      FSRI() : AgBlock("FSRI","is the inner Support Ring"){ }; 
      ~FSRI(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FSRI,1); 
   }; 
   // ---------------------------------------------------------------------- FSBA -- 
   ///@defgroup FSBA_doc 
   ///@class FSBA 
   ///@brief are the Stabilizer Block for the inner Support Ring 
   class FSBA : public AgBlock 
   {  public: 
      FSBA() : AgBlock("FSBA","are the Stabilizer Block for the inner Support Ring"){ }; 
      ~FSBA(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FSBA,1); 
   }; 
   // ---------------------------------------------------------------------- FPAD -- 
   ///@defgroup FPAD_doc 
   ///@class FPAD 
   ///@brief is the Pad plane of the FTPC 
   class FPAD : public AgBlock 
   {  public: 
      FPAD() : AgBlock("FPAD","is the Pad plane of the FTPC"){ }; 
      ~FPAD(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FPAD,1); 
   }; 
   // ---------------------------------------------------------------------- FFRA -- 
   ///@defgroup FFRA_doc 
   ///@class FFRA 
   ///@brief is outermost FC Ring 
   class FFRA : public AgBlock 
   {  public: 
      FFRA() : AgBlock("FFRA","is outermost FC Ring"){ }; 
      ~FFRA(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FFRA,1); 
   }; 
   /// \class FtpcGeo1 
   /// \brief   is the Forward TPC in STAR with corrected GAS  
   class FtpcGeo1 : public AgModule 
   { 
      public: 
      FtpcGeo1(); 
      virtual void ConstructGeometry( const Char_t *dummy="" ); 
      ~FtpcGeo1(){ }; 
      ClassDef(FtpcGeo1,1); 
      public: 
   }; 
   // endElement in class Module 
}; // namespace FtpcGeo1 
#endif // __FtpcGeo1__ 

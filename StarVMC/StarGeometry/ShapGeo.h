#ifndef __ShapGeo__ 
#define __ShapGeo__ 
 
#include "StarVMC/StarAgmlLib/AgModule.h" 
 
namespace SHAPGEO // $NMSPC 
{ 
   // ---------------------------------------------------------------------- BBOX -- 
   ///@defgroup BBOX_doc 
   ///@class BBOX 
   ///@brief A box / TGeoBBox 
   class BBOX : public AgBlock 
   {  public: 
      BBOX() : AgBlock("BBOX","A box / TGeoBBox"){ }; 
      ~BBOX(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(BBOX,1); 
   }; 
   // ---------------------------------------------------------------------- PARA -- 
   ///@defgroup PARA_doc 
   ///@class PARA 
   ///@brief A parallelpiped TGeoPara 
   class PARA : public AgBlock 
   {  public: 
      PARA() : AgBlock("PARA","A parallelpiped TGeoPara"){ }; 
      ~PARA(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PARA,1); 
   }; 
   // ---------------------------------------------------------------------- TRDA -- 
   ///@defgroup TRDA_doc 
   ///@class TRDA 
   ///@brief A trapezoid TGeoTrd1 
   class TRDA : public AgBlock 
   {  public: 
      TRDA() : AgBlock("TRDA","A trapezoid TGeoTrd1"){ }; 
      ~TRDA(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TRDA,1); 
   }; 
   // ---------------------------------------------------------------------- TRDB -- 
   ///@defgroup TRDB_doc 
   ///@class TRDB 
   ///@brief A trapezoid TGeoTrd2 
   class TRDB : public AgBlock 
   {  public: 
      TRDB() : AgBlock("TRDB","A trapezoid TGeoTrd2"){ }; 
      ~TRDB(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TRDB,1); 
   }; 
   // ---------------------------------------------------------------------- TRAP -- 
   ///@defgroup TRAP_doc 
   ///@class TRAP 
   ///@brief A trapezoid TGeoTrap 
   class TRAP : public AgBlock 
   {  public: 
      TRAP() : AgBlock("TRAP","A trapezoid TGeoTrap"){ }; 
      ~TRAP(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TRAP,1); 
   }; 
   // ---------------------------------------------------------------------- GTRA -- 
   ///@defgroup GTRA_doc 
   ///@class GTRA 
   ///@brief A trapezoid TGeoGtra 
   class GTRA : public AgBlock 
   {  public: 
      GTRA() : AgBlock("GTRA","A trapezoid TGeoGtra"){ }; 
      ~GTRA(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(GTRA,1); 
   }; 
   // ---------------------------------------------------------------------- SPHE -- 
   ///@defgroup SPHE_doc 
   ///@class SPHE 
   ///@brief A sphere 
   class SPHE : public AgBlock 
   {  public: 
      SPHE() : AgBlock("SPHE","A sphere"){ }; 
      ~SPHE(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SPHE,1); 
   }; 
   // ---------------------------------------------------------------------- TUBE -- 
   ///@defgroup TUBE_doc 
   ///@class TUBE 
   ///@brief A box 
   class TUBE : public AgBlock 
   {  public: 
      TUBE() : AgBlock("TUBE","A box"){ }; 
      ~TUBE(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TUBE,1); 
   }; 
   // ---------------------------------------------------------------------- TUBS -- 
   ///@defgroup TUBS_doc 
   ///@class TUBS 
   ///@brief A box 
   class TUBS : public AgBlock 
   {  public: 
      TUBS() : AgBlock("TUBS","A box"){ }; 
      ~TUBS(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TUBS,1); 
   }; 
   // ---------------------------------------------------------------------- CTUB -- 
   ///@defgroup CTUB_doc 
   ///@class CTUB 
   ///@brief A box 
   class CTUB : public AgBlock 
   {  public: 
      CTUB() : AgBlock("CTUB","A box"){ }; 
      ~CTUB(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(CTUB,1); 
   }; 
   // ---------------------------------------------------------------------- ELTU -- 
   ///@defgroup ELTU_doc 
   ///@class ELTU 
   ///@brief An elliptical tube TGeoEltu 
   class ELTU : public AgBlock 
   {  public: 
      ELTU() : AgBlock("ELTU","An elliptical tube TGeoEltu"){ }; 
      ~ELTU(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(ELTU,1); 
   }; 
   // ---------------------------------------------------------------------- CONE -- 
   ///@defgroup CONE_doc 
   ///@class CONE 
   ///@brief A box 
   class CONE : public AgBlock 
   {  public: 
      CONE() : AgBlock("CONE","A box"){ }; 
      ~CONE(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(CONE,1); 
   }; 
   // ---------------------------------------------------------------------- CONS -- 
   ///@defgroup CONS_doc 
   ///@class CONS 
   ///@brief A box 
   class CONS : public AgBlock 
   {  public: 
      CONS() : AgBlock("CONS","A box"){ }; 
      ~CONS(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(CONS,1); 
   }; 
   // ---------------------------------------------------------------------- PCON -- 
   ///@defgroup PCON_doc 
   ///@class PCON 
   ///@brief A polycone 
   class PCON : public AgBlock 
   {  public: 
      PCON() : AgBlock("PCON","A polycone"){ }; 
      ~PCON(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PCON,1); 
   }; 
   // ---------------------------------------------------------------------- PGON -- 
   ///@defgroup PGON_doc 
   ///@class PGON 
   ///@brief A polygon 
   class PGON : public AgBlock 
   {  public: 
      PGON() : AgBlock("PGON","A polygon"){ }; 
      ~PGON(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PGON,1); 
   }; 
   /// \class ShapGeo 
   /// \brief Shapes Geometry : places AgML shapes into the cave 
   class ShapGeo : public AgModule 
   { 
      public: 
      ShapGeo(); 
      virtual void ConstructGeometry(); 
      ~ShapGeo(){ }; 
      ClassDef(ShapGeo,1); 
      public: 
   }; 
   // endElement in class Module 
}; // namespace ShapGeo 
#endif // __ShapGeo__ 

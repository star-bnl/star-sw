#include "ShldGeo.h"  
 // ---------------------------------------------------------------------------------------------------  
 //  
 #include "StarVMC/StarAgmlLib/StarAgmlStacker.h"  
 //  
 #include "StarVMC/StarAgmlLib/AgMaterial.h"  
 #include "StarVMC/StarAgmlLib/AgMedium.h"  
 #include "StarVMC/StarAgmlLib/AgShape.h"  
 #include "StarVMC/StarAgmlLib/AgBlock.h"  
 #include "StarVMC/StarAgmlLib/AgMath.h"  
 #include "StarVMC/StarAgmlLib/AgSTAR.h"  
 //  
 #include "StarVMC/StarAgmlLib/Mortran.h"  
 #include "StarVMC/StarAgmlLib/AgMath.h"  
 #include <iostream>  
 #include <vector>  
 #include <map>  
 const Int_t _printlevel = 0;  
 #define LOG_PRINT if(_printlevel>0) std::cout << GetName() << " -Print- "  
 #define LOG_INFO  if(_printlevel>1) std::cout << GetName() << " -Info-  "  
 #define LOG_DEBUG if(_printlevel>2) std::cout << GetName() << " -Debug- "  
 #define LOG_WARN  if(_printlevel>3) std::cout << GetName() << " -Warn-  "  
 #define printf(fmt,...) LOG_PRINT << Form(fmt,##__VA_ARGS__) << std::endl;  
 #include "StarVMC/Geometry/Helpers.h"  
 //  
 namespace SHLDGEO // $NMSPC  
 {     
       //     
       // ---------------------------------------------------------------------------------------------------     
          //  -----------------------------------------------------     
          /// @defgroup shlg_doc     
          /// \class Shlg_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t version;     
          ///Float_t z;     
          ///Float_t dx;     
          ///Float_t dy;     
          ///Float_t dz;     
          ///Float_t baselevel;     
          ///Float_t basez;     
          ///Float_t basedx;     
          ///Float_t basedy;     
          ///Float_t slabx;     
          ///Float_t slabz;     
          ///Float_t slabdy;     
          ///Float_t slabdz;     
          ///Float_t fidz;     
          ///Float_t fidy;     
          ///Float_t holex;     
          ///Float_t holey;     
          ///Float_t floorthk;     
          ///Float_t floorlen;     
          ///Float_t floorwidth;     
          ///Float_t floorpos;     
          ///Int_t _index;     
          //     
          Shlg_t shlg;     
          //     
          ///@addtogroup ShldGeo_vars     
          ///@{        
                Float_t yslab,shieldhalfheight,yfi;        
                //        
                /// Float_t yslab,shieldhalfheight,yfi        
          ///@}     
       ShldGeo::ShldGeo()     
         : AgModule("ShldGeo"," is the shielding ")     
       {        
       }     
          // ---------------------------------------------------------------------------------------------------     
          void SHLD::Block( AgCreate create )     
          {         
                ///@addtogroup SHLD_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      /// Medium Standard           
                      {  AgMedium med = AgMedium::CopyMedium("Standard");              
                            _medium = med;              
                      }           
                      { AgAttribute attr = AgAttribute("SHLD");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=shlg.dx;              
                            shape.par("dy")=shieldhalfheight;              
                            shape.par("dz")=shlg.dz;              
                            /// Shape Bbox dx=shlg.dx dy=shieldhalfheight dz=shlg.dz               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SHLD;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("SHBS");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create SHBS              
                            Create("SHBS");               
                      }           
                      { AgPlacement place = AgPlacement("SHBS","SHLD");              
                            /// Add daughter volume SHBS to mother SHLD              
                            place.TranslateX(+shlg.dx-shlg.basedx);              
                            /// Translate x = +shlg.dx-shlg.basedx              
                            place.TranslateY(-shieldhalfheight+shlg.basedy);              
                            /// Translate y = -shieldhalfheight+shlg.basedy              
                            place.TranslateZ(0);              
                            /// Translate z = 0              
                            _stacker -> Position( AgBlock::Find("SHBS"), place );              
                      } // end placement of SHBS           
                      { AgPlacement place = AgPlacement("SHBS","SHLD");              
                            /// Add daughter volume SHBS to mother SHLD              
                            place.TranslateX(-shlg.dx+shlg.basedx);              
                            /// Translate x = -shlg.dx+shlg.basedx              
                            place.TranslateY(-shieldhalfheight+shlg.basedy);              
                            /// Translate y = -shieldhalfheight+shlg.basedy              
                            place.TranslateZ(0);              
                            /// Translate z = 0              
                            _stacker -> Position( AgBlock::Find("SHBS"), place );              
                      } // end placement of SHBS           
                      yslab = -shieldhalfheight+2.0*shlg.basedy+shlg.slabdy;           
                      _create = AgCreate("SHLS");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create SHLS              
                            Create("SHLS");               
                      }           
                      { AgPlacement place = AgPlacement("SHLS","SHLD");              
                            /// Add daughter volume SHLS to mother SHLD              
                            place.TranslateX(shlg.slabx);              
                            /// Translate x = shlg.slabx              
                            place.TranslateY(yslab);              
                            /// Translate y = yslab              
                            place.TranslateZ(-shlg.dz+shlg.slabdz);              
                            /// Translate z = -shlg.dz+shlg.slabdz              
                            _stacker -> Position( AgBlock::Find("SHLS"), place );              
                      } // end placement of SHLS           
                      _create = AgCreate("SHBI");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create SHBI              
                            Create("SHBI");               
                      }           
                      { AgPlacement place = AgPlacement("SHBI","SHLD");              
                            /// Add daughter volume SHBI to mother SHLD              
                            place.TranslateX(0.0);              
                            /// Translate x = 0.0              
                            place.TranslateY(yslab);              
                            /// Translate y = yslab              
                            place.TranslateZ(shlg.slabdz);              
                            /// Translate z = shlg.slabdz              
                            _stacker -> Position( AgBlock::Find("SHBI"), place );              
                      } // end placement of SHBI           
                      yfi = -shieldhalfheight+2.0*(shlg.basedy+shlg.slabdy)+shlg.fidy;           
                      _create = AgCreate("SHFI");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create SHFI              
                            Create("SHFI");               
                      }           
                      { AgPlacement place = AgPlacement("SHFI","SHLD");              
                            /// Add daughter volume SHFI to mother SHLD              
                            place.TranslateX(0.0);              
                            /// Translate x = 0.0              
                            place.TranslateY(yfi);              
                            /// Translate y = yfi              
                            place.TranslateZ(-shlg.dz+shlg.fidz);              
                            /// Translate z = -shlg.dz+shlg.fidz              
                            _stacker -> Position( AgBlock::Find("SHFI"), place );              
                      } // end placement of SHFI           
                      END_OF_SHLD:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SHLD     
          // ---------------------------------------------------------------------------------------------------     
          void SFLR::Block( AgCreate create )     
          {         
                ///@addtogroup SFLR_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("SFLR");              
                            attr.par("seen")=1;              
                            attr.par("colo")=3;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Component Si	a=28.08	z=14	w=1           
                      /// Component O2	a=16	z=8	w=2           
                      /// Mixture ShieldConc dens=2.5           
                      {  AgMaterial &mix = AgMaterial::Get("Shieldconc");              
                            mix.Component("Si",28.08,14,1);              
                            mix.Component("O2",16,8,2);              
                            mix.par("dens")=2.5;              
                            mix.lock();              
                            _material = mix;              
                            _material.lock();              
                      }           
                      /// Medium Standard           
                      {  AgMedium med = AgMedium::CopyMedium("Standard");              
                            _medium = med;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=shlg.floorwidth/2.0;              
                            shape.par("dy")=shlg.floorthk/2.0;              
                            shape.par("dz")=shlg.floorlen/2.0;              
                            /// Shape Bbox dx=shlg.floorwidth/2.0 dy=shlg.floorthk/2.0 dz=shlg.floorlen/2.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SFLR;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_SFLR:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SFLR     
          // ---------------------------------------------------------------------------------------------------     
          void SHBS::Block( AgCreate create )     
          {         
                ///@addtogroup SHBS_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("SHBS");              
                            attr.par("seen")=1;              
                            attr.par("colo")=3;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Component Si	a=28.08	z=14	w=1           
                      /// Component O2	a=16	z=8	w=2           
                      /// Mixture BaseConc dens=2.5           
                      {  AgMaterial &mix = AgMaterial::Get("Baseconc");              
                            mix.Component("Si",28.08,14,1);              
                            mix.Component("O2",16,8,2);              
                            mix.par("dens")=2.5;              
                            mix.lock();              
                            _material = mix;              
                            _material.lock();              
                      }           
                      /// Medium Standard           
                      {  AgMedium med = AgMedium::CopyMedium("Standard");              
                            _medium = med;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=shlg.basedx;              
                            shape.par("dy")=shlg.basedy;              
                            shape.par("dz")=shlg.dz;              
                            /// Shape Bbox dx=shlg.basedx dy=shlg.basedy dz=shlg.dz               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SHBS;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_SHBS:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SHBS     
          // ---------------------------------------------------------------------------------------------------     
          void SHLS::Block( AgCreate create )     
          {         
                ///@addtogroup SHLS_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material BaseConc            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Baseconc");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("SHBS");              
                            attr.par("seen")=1;              
                            attr.par("colo")=3;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=shlg.dx;              
                            shape.par("dy")=shlg.slabdy;              
                            shape.par("dz")=shlg.slabdz;              
                            /// Shape Bbox dx=shlg.dx dy=shlg.slabdy dz=shlg.slabdz               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SHLS;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_SHLS:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SHLS     
          // ---------------------------------------------------------------------------------------------------     
          void SHBI::Block( AgCreate create )     
          {         
                ///@addtogroup SHBI_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Iron            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Iron");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("SHBI");              
                            attr.par("seen")=1;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=shlg.dx;              
                            shape.par("dy")=shlg.slabdy;              
                            shape.par("dz")=shlg.dz-shlg.slabdz;              
                            /// Shape Bbox dx=shlg.dx dy=shlg.slabdy dz=shlg.dz-shlg.slabdz               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SHBI;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_SHBI:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SHBI     
          // ---------------------------------------------------------------------------------------------------     
          void SHFI::Block( AgCreate create )     
          {         
                ///@addtogroup SHFI_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Iron            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Iron");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("SHFI");              
                            attr.par("seen")=1;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=shlg.dx;              
                            shape.par("dy")=shlg.fidy;              
                            shape.par("dz")=shlg.fidz;              
                            /// Shape Bbox dx=shlg.dx dy=shlg.fidy dz=shlg.fidz               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SHFI;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("SHOL");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create SHOL              
                            Create("SHOL");               
                      }           
                      { AgPlacement place = AgPlacement("SHOL","SHFI");              
                            /// Add daughter volume SHOL to mother SHFI              
                            place.TranslateY(-shlg.fidy+shlg.holey);              
                            /// Translate y = -shlg.fidy+shlg.holey              
                            _stacker -> Position( AgBlock::Find("SHOL"), place );              
                      } // end placement of SHOL           
                      END_OF_SHFI:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SHFI     
          // ---------------------------------------------------------------------------------------------------     
          void SHOL::Block( AgCreate create )     
          {         
                ///@addtogroup SHOL_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("SHOL");              
                            attr.par("seen")=1;              
                            attr.par("colo")=6;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=shlg.holex;              
                            shape.par("dy")=shlg.holey;              
                            shape.par("dz")=shlg.fidz;              
                            /// Shape Bbox dx=shlg.holex dy=shlg.holey dz=shlg.fidz               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SHOL;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_SHOL:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SHOL     
    // ----------------------------------------------------------------------- geoctr
       void ShldGeo::ConstructGeometry( const Char_t *dummy )     
       {        
             ///@addtogroup ShldGeo_revision        
             ///@{           
                   /// Created:   15-Aug-2005             
             ///@}        
             ///@addtogroup ShldGeo_revision        
             ///@{           
                   /// Author: Maxim Potekhin           
             ///@}        
             AddBlock("SHLD");        
             AddBlock("SHBS");        
             AddBlock("SHLS");        
             AddBlock("SHBI");        
             AddBlock("SHFI");        
             AddBlock("SHOL");        
             AddBlock("SFLR");        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup shlg_doc        
             ///@{           
                   ++shlg._index;           
                   shlg . version = 1; //  geometry version              
                   /// shlg . version = 1; //  geometry version              
                   shlg . z = 1750; //  position of the shielding            
                   /// shlg . z = 1750; //  position of the shielding            
                   shlg . dx = 170; //  half-x dimension            
                   /// shlg . dx = 170; //  half-x dimension            
                   shlg . dy = 150; //  half-y dimension            
                   /// shlg . dy = 150; //  half-y dimension            
                   shlg . dz = 100; //  half-z dimension            
                   /// shlg . dz = 100; //  half-z dimension            
                   shlg . baselevel = -125; //  base position on the floor            
                   /// shlg . baselevel = -125; //  base position on the floor            
                   shlg . basez = 0; //  base position            
                   /// shlg . basez = 0; //  base position            
                   shlg . basedx = 60; //  base half-x dimension            
                   /// shlg . basedx = 60; //  base half-x dimension            
                   shlg . basedy = 30; //  base half-y dimension            
                   /// shlg . basedy = 30; //  base half-y dimension            
                   shlg . slabx = 0; //  slab position            
                   /// shlg . slabx = 0; //  slab position            
                   shlg . slabz = -30; //  slab position            
                   /// shlg . slabz = -30; //  slab position            
                   shlg . slabdy = 30; //  slab half-y dimension            
                   /// shlg . slabdy = 30; //  slab half-y dimension            
                   shlg . slabdz = 30; //  slab half-z dimension            
                   /// shlg . slabdz = 30; //  slab half-z dimension            
                   shlg . fidz = 47; //  forward iron slab half-thickness            
                   /// shlg . fidz = 47; //  forward iron slab half-thickness            
                   shlg . fidy = 55; //  half-height            
                   /// shlg . fidy = 55; //  half-height            
                   shlg . holex = 20; //  beam hole half-size in X            
                   /// shlg . holex = 20; //  beam hole half-size in X            
                   shlg . holey = 10; //  beam hole half-size in Y            
                   /// shlg . holey = 10; //  beam hole half-size in Y            
                   shlg . floorthk = 70; //  Concrete floor thickness            
                   /// shlg . floorthk = 70; //  Concrete floor thickness            
                   shlg . floorlen = 3900; //  Concrete floor length            
                   /// shlg . floorlen = 3900; //  Concrete floor length            
                   shlg . floorwidth = 340; //  Concrete floor width            
                   /// shlg . floorwidth = 340; //  Concrete floor width            
                   shlg . floorpos = 2800; //  Concrete floor z-position            
                   /// shlg . floorpos = 2800; //  Concrete floor z-position            
                   //           
                   shlg.fill();           
             ///@}        
             //        
             /// USE shlg _index=1;        
             shlg.Use();        
             shieldhalfheight=(shlg.basedy+shlg.slabdy+shlg.fidy);        
             _create = AgCreate("SHLD");        
             {           
                   AgShape myshape; // undefined shape           
                   ///Create SHLD           
                   Create("SHLD");            
             }        
             { AgPlacement place = AgPlacement("SHLD","CAVE");           
                   /// Add daughter volume SHLD to mother CAVE           
                   place.TranslateX(0.0);           
                   /// Translate x = 0.0           
                   place.TranslateY(shlg.baselevel+shieldhalfheight);           
                   /// Translate y = shlg.baselevel+shieldhalfheight           
                   place.TranslateZ(+shlg.z);           
                   /// Translate z = +shlg.z           
                   _stacker -> Position( AgBlock::Find("SHLD"), place );           
             } // end placement of SHLD        
             { AgPlacement place = AgPlacement("SHLD","CAVE");           
                   /// Add daughter volume SHLD to mother CAVE           
                   place.TranslateX(0.0);           
                   /// Translate x = 0.0           
                   place.TranslateY(shlg.baselevel+shieldhalfheight);           
                   /// Translate y = shlg.baselevel+shieldhalfheight           
                   place.TranslateZ(-shlg.z);           
                   /// Translate z = -shlg.z           
                   /// G3 Reference: thetax = 90           
                   /// G3 Reference: phix = 0           
                   /// G3 Reference: thetay = 90           
                   /// G3 Reference: phiy = 90           
                   /// G3 Reference: thetaz = 180           
                   /// G3 Reference: phiz = 0           
                   Double_t _thetax=90,_phix=0,_thetay=90,_phiy=90,_thetaz=180,_phiz=0;           
                   place.Reference( _thetax, _phix, _thetay, _phiy, _thetaz, _phiz );           
                   _stacker -> Position( AgBlock::Find("SHLD"), place );           
             } // end placement of SHLD        
             _create = AgCreate("SFLR");        
             {           
                   AgShape myshape; // undefined shape           
                   ///Create SFLR           
                   Create("SFLR");            
             }        
             { AgPlacement place = AgPlacement("SFLR","CAVE");           
                   /// Add daughter volume SFLR to mother CAVE           
                   place.TranslateX(0);           
                   /// Translate x = 0           
                   place.TranslateY(shlg.baselevel-0.5*shlg.floorthk);           
                   /// Translate y = shlg.baselevel-0.5*shlg.floorthk           
                   place.TranslateZ(+shlg.floorpos);           
                   /// Translate z = +shlg.floorpos           
                   _stacker -> Position( AgBlock::Find("SFLR"), place );           
             } // end placement of SFLR        
             { AgPlacement place = AgPlacement("SFLR","CAVE");           
                   /// Add daughter volume SFLR to mother CAVE           
                   place.TranslateX(0);           
                   /// Translate x = 0           
                   place.TranslateY(shlg.baselevel-0.5*shlg.floorthk);           
                   /// Translate y = shlg.baselevel-0.5*shlg.floorthk           
                   place.TranslateZ(-shlg.floorpos);           
                   /// Translate z = -shlg.floorpos           
                   /// G3 Reference: thetax = 90           
                   /// G3 Reference: phix = 0           
                   /// G3 Reference: thetay = 90           
                   /// G3 Reference: phiy = 90           
                   /// G3 Reference: thetaz = 180           
                   /// G3 Reference: phiz = 0           
                   Double_t _thetax=90,_phix=0,_thetay=90,_phiy=90,_thetaz=180,_phiz=0;           
                   place.Reference( _thetax, _phix, _thetay, _phiy, _thetaz, _phiz );           
                   _stacker -> Position( AgBlock::Find("SFLR"), place );           
             } // end placement of SFLR        
       }; // ShldGeo     
 }; // namespace ShldGeo  
 
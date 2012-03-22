#include "ZcalGeo.h"  
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
 namespace ZCALGEO // $NMSPC  
 {     
       //     
       // ---------------------------------------------------------------------------------------------------     
          //  -----------------------------------------------------     
          /// @defgroup calp_doc     
          /// \class Calp_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t version;     
          ///Float_t droutr;     
          ///Float_t drdz;     
          ///Float_t phleng;     
          ///Float_t phinnr;     
          ///Float_t phoutr;     
          ///Float_t pltdz;     
          ///Float_t pltoutr;     
          ///Float_t houtr;     
          ///Float_t pjinnr;     
          ///Float_t pjoutr;     
          ///Float_t pjleng;     
          ///Float_t qcdx;     
          ///Float_t qcdy;     
          ///Float_t qcdz;     
          ///Float_t scdz;     
          ///Float_t sdiv;     
          ///Int_t _index;     
          //     
          Calp_t calp;     
          //     
          ///@addtogroup ZcalGeo_vars     
          ///@{        
                Float_t z1,z2,z3,z4;        
                //        
                /// Float_t z1,z2,z3,z4        
          ///@}     
       ZcalGeo::ZcalGeo()     
         : AgModule("ZcalGeo"," is the geometry of the Zero deg. Quartz Calorimeter ")     
       {        
       }     
          // ---------------------------------------------------------------------------------------------------     
          void ZCAL::Block( AgCreate create )     
          {         
                ///@addtogroup ZCAL_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      /// Medium standard           
                      {  AgMedium med = AgMedium::CopyMedium("Standard");              
                            _medium = med;              
                      }           
                      { AgAttribute attr = AgAttribute("ZCAL");              
                            attr.par("seen")=0;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=0.0;              
                            shape.par("rmax")=calp.droutr;              
                            shape.par("dz")=calp.drdz;              
                            /// Shape Tube rmin=0.0 rmax=calp.droutr dz=calp.drdz               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_ZCAL;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("PIPH");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PIPH              
                            Create("PIPH");               
                      }           
                      { AgPlacement place = AgPlacement("PIPH","ZCAL");              
                            /// Add daughter volume PIPH to mother ZCAL              
                            place.TranslateZ(z1);              
                            /// Translate z = z1              
                            _stacker -> Position( AgBlock::Find("PIPH"), place );              
                      } // end placement of PIPH           
                      _create = AgCreate("PLAT");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PLAT              
                            Create("PLAT");               
                      }           
                      { AgPlacement place = AgPlacement("PLAT","ZCAL");              
                            /// Add daughter volume PLAT to mother ZCAL              
                            place.TranslateZ(z2);              
                            /// Translate z = z2              
                            _stacker -> Position( AgBlock::Find("PLAT"), place );              
                      } // end placement of PLAT           
                      _create = AgCreate("QCAL");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create QCAL              
                            Create("QCAL");               
                      }           
                      { AgPlacement place = AgPlacement("QCAL","ZCAL");              
                            /// Add daughter volume QCAL to mother ZCAL              
                            place.TranslateZ(z4);              
                            /// Translate z = z4              
                            _stacker -> Position( AgBlock::Find("QCAL"), place );              
                      } // end placement of QCAL           
                      _create = AgCreate("PIPJ");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PIPJ              
                            Create("PIPJ");               
                      }           
                      { AgPlacement place = AgPlacement("PIPJ","ZCAL");              
                            /// Add daughter volume PIPJ to mother ZCAL              
                            place.TranslateX(12.82);              
                            /// Translate x = 12.82              
                            place.TranslateZ(z3);              
                            /// Translate z = z3              
                            place.AlphaY(1.074);              
                            /// Rotate: AlphaY = 1.074              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            _stacker -> Position( AgBlock::Find("PIPJ"), place );              
                      } // end placement of PIPJ           
                      { AgPlacement place = AgPlacement("PIPJ","ZCAL");              
                            /// Add daughter volume PIPJ to mother ZCAL              
                            place.TranslateX(-12.82);              
                            /// Translate x = -12.82              
                            place.TranslateZ(z3);              
                            /// Translate z = z3              
                            place.AlphaY(-1.074);              
                            /// Rotate: AlphaY = -1.074              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            _stacker -> Position( AgBlock::Find("PIPJ"), place );              
                      } // end placement of PIPJ           
                      END_OF_ZCAL:           
                      mCurrent = _save;           
                ///@}        
          } // End Block ZCAL     
          // ---------------------------------------------------------------------------------------------------     
          void PIPH::Block( AgCreate create )     
          {         
                ///@addtogroup PIPH_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Iron            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Iron");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("Piph");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=0;              
                            shape.par("rmax")=calp.phoutr;              
                            shape.par("dz")=calp.phleng;              
                            /// Shape Tube rmin=0 rmax=calp.phoutr dz=calp.phleng               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PIPH;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("PVAH");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PVAH              
                            Create("PVAH");               
                      }           
                      { AgPlacement place = AgPlacement("PVAH","PIPH");              
                            /// Add daughter volume PVAH to mother PIPH              
                            _stacker -> Position( AgBlock::Find("PVAH"), place );              
                      } // end placement of PVAH           
                      END_OF_PIPH:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PIPH     
          // ---------------------------------------------------------------------------------------------------     
          void PVAH::Block( AgCreate create )     
          {         
                ///@addtogroup PVAH_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Vacuum            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Vacuum");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmax")=calp.phinnr;              
                            /// Shape Tube rmax=calp.phinnr               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PVAH;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_PVAH:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PVAH     
          // ---------------------------------------------------------------------------------------------------     
          void PLAT::Block( AgCreate create )     
          {         
                ///@addtogroup PLAT_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Iron            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Iron");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("Plat");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=0;              
                            shape.par("rmax")=calp.pltoutr;              
                            shape.par("dz")=calp.pltdz;              
                            /// Shape Tube rmin=0 rmax=calp.pltoutr dz=calp.pltdz               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PLAT;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("PLVA");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PLVA              
                            Create("PLVA");               
                      }           
                      { AgPlacement place = AgPlacement("PLVA","PLAT");              
                            /// Add daughter volume PLVA to mother PLAT              
                            place.TranslateX(11.10);              
                            /// Translate x = 11.10              
                            _stacker -> Position( AgBlock::Find("PLVA"), place );              
                      } // end placement of PLVA           
                      { AgPlacement place = AgPlacement("PLVA","PLAT");              
                            /// Add daughter volume PLVA to mother PLAT              
                            place.TranslateX(-11.10);              
                            /// Translate x = -11.10              
                            _stacker -> Position( AgBlock::Find("PLVA"), place );              
                      } // end placement of PLVA           
                      END_OF_PLAT:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PLAT     
          // ---------------------------------------------------------------------------------------------------     
          void PLVA::Block( AgCreate create )     
          {         
                ///@addtogroup PLVA_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Vacuum            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Vacuum");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmax")=calp.houtr;              
                            /// Shape Tube rmax=calp.houtr               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PLVA;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_PLVA:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PLVA     
          // ---------------------------------------------------------------------------------------------------     
          void PIPJ::Block( AgCreate create )     
          {         
                ///@addtogroup PIPJ_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Iron            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Iron");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("PIPJ");              
                            attr.par("seen")=1;              
                            attr.par("colo")=7;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=0;              
                            shape.par("rmax")=calp.pjoutr;              
                            shape.par("dz")=calp.pjleng;              
                            /// Shape Tube rmin=0 rmax=calp.pjoutr dz=calp.pjleng               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PIPJ;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("PVAJ");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PVAJ              
                            Create("PVAJ");               
                      }           
                      { AgPlacement place = AgPlacement("PVAJ","PIPJ");              
                            /// Add daughter volume PVAJ to mother PIPJ              
                            _stacker -> Position( AgBlock::Find("PVAJ"), place );              
                      } // end placement of PVAJ           
                      END_OF_PIPJ:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PIPJ     
          // ---------------------------------------------------------------------------------------------------     
          void PVAJ::Block( AgCreate create )     
          {         
                ///@addtogroup PVAJ_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Vacuum            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Vacuum");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmax")=calp.pjinnr;              
                            /// Shape Tube rmax=calp.pjinnr               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PVAJ;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_PVAJ:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PVAJ     
          // ---------------------------------------------------------------------------------------------------     
          void QCAL::Block( AgCreate create )     
          {         
                ///@addtogroup QCAL_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Lead            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Lead");              
                            _material = mat;              
                      }           
                      /// Material dirty_lead isvol=0            
                      { AgMaterial &mat = AgMaterial::Get("Dirty_lead");              
                            mat.par("isvol")=0;              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("Qcal");              
                            attr.par("seen")=1;              
                            attr.par("colo")=4;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=calp.qcdx;              
                            shape.par("dy")=calp.qcdy;              
                            shape.par("dz")=calp.qcdz;              
                            /// Shape Bbox dx=calp.qcdx dy=calp.qcdy dz=calp.qcdz               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_QCAL;              
                            _stacker -> Build(this);              
                      }           
                      // _medium.par("CUTGAM") = 0.0005;           
                      // _medium.par("CUTELE") = 0.00015;           
                      _create = AgCreate("QDIV");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create QDIV              
                            Create("QDIV");               
                      }           
                      END_OF_QCAL:           
                      mCurrent = _save;           
                ///@}        
          } // End Block QCAL     
          // ---------------------------------------------------------------------------------------------------     
          void QDIV::Block( AgCreate create )     
          {         
                ///@addtogroup QDIV_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      {  AgShape shape = AgShape("Division");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("ndiv")=calp.sdiv;              
                            shape.par("iaxis")=3;              
                            /// Shape Division ndiv=calp.sdiv iaxis=3               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_QDIV;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("QSCI");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create QSCI              
                            Create("QSCI");               
                      }           
                      { AgPlacement place = AgPlacement("QSCI","QDIV");              
                            /// Add daughter volume QSCI to mother QDIV              
                            place.TranslateZ(-calp.qcdz/(calp.sdiv)+calp.scdz);              
                            /// Translate z = -calp.qcdz/(calp.sdiv)+calp.scdz              
                            _stacker -> Position( AgBlock::Find("QSCI"), place );              
                      } // end placement of QSCI           
                      END_OF_QDIV:           
                      mCurrent = _save;           
                ///@}        
          } // End Block QDIV     
          // ---------------------------------------------------------------------------------------------------     
          void QSCI::Block( AgCreate create )     
          {         
                ///@addtogroup QSCI_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Polystyren            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Polystyren");              
                            _material = mat;              
                      }           
                      /// Material scintillator isvol=1            
                      { AgMaterial &mat = AgMaterial::Get("Scintillator");              
                            mat.par("isvol")=1;              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("QSCI");              
                            attr.par("seen")=1;              
                            attr.par("colo")=3;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dz")=calp.scdz;              
                            /// Shape Bbox dz=calp.scdz               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_QSCI;              
                            _stacker -> Build(this);              
                      }           
                      // _medium.par("CUTGAM") = 0.0005;           
                      // _medium.par("CUTELE") = 0.00015;           
                      END_OF_QSCI:           
                      mCurrent = _save;           
                ///@}        
          } // End Block QSCI     
    // ----------------------------------------------------------------------- geoctr
       void ZcalGeo::ConstructGeometry( const Char_t *dummy )     
       {        
             ///@addtogroup ZcalGeo_revision        
             ///@{           
                   /// Created:  10-Feb-1997            
             ///@}        
             ///@addtogroup ZcalGeo_revision        
             ///@{           
                   /// Author: W.B.Christie           
             ///@}        
             AddBlock("ZCAL");        
             AddBlock("QCAL");        
             AddBlock("QDIV");        
             AddBlock("QSCI");        
             AddBlock("PIPH");        
             AddBlock("PVAH");        
             AddBlock("PLAT");        
             AddBlock("PLVA");        
             AddBlock("PIPJ");        
             AddBlock("PVAJ");        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup calp_doc        
             ///@{           
                   ++calp._index;           
                   calp . version = 1; //  Geometry version number            
                   /// calp . version = 1; //  Geometry version number            
                   calp . droutr = 40.0; //  Outer radius of mother volume tube            
                   /// calp . droutr = 40.0; //  Outer radius of mother volume tube            
                   calp . drdz = 250.0; //  Half length of mother volume            
                   /// calp . drdz = 250.0; //  Half length of mother volume            
                   calp . phleng = 133.35; //  Half Length of large diameter Pipe.            
                   /// calp . phleng = 133.35; //  Half Length of large diameter Pipe.            
                   calp . phinnr = 20.0; //  Inner radius of Pipe H            
                   /// calp . phinnr = 20.0; //  Inner radius of Pipe H            
                   calp . phoutr = 20.96; //  Outer radius of Pipe H            
                   /// calp . phoutr = 20.96; //  Outer radius of Pipe H            
                   calp . pltdz = 0.47; //  Half thickness of plate at end of large dia pipe.            
                   /// calp . pltdz = 0.47; //  Half thickness of plate at end of large dia pipe.            
                   calp . pltoutr = 20.96; //  Radius of plate.            
                   /// calp . pltoutr = 20.96; //  Radius of plate.            
                   calp . houtr = 6.35; //  Radius of holes in the End plate            
                   /// calp . houtr = 6.35; //  Radius of holes in the End plate            
                   calp . pjinnr = 6.07; //  Inner radius of final beam pipe            
                   /// calp . pjinnr = 6.07; //  Inner radius of final beam pipe            
                   calp . pjoutr = 6.35; //  Outer radius of final beam pipe            
                   /// calp . pjoutr = 6.35; //  Outer radius of final beam pipe            
                   calp . pjleng = 91.5; //  Half length of final beam pipe            
                   /// calp . pjleng = 91.5; //  Half length of final beam pipe            
                   calp . qcdx = 5.0; //  Half width of Qcal (cm)            
                   /// calp . qcdx = 5.0; //  Half width of Qcal (cm)            
                   calp . qcdy = 5.0; //  Half width of Qcal (cm)            
                   /// calp . qcdy = 5.0; //  Half width of Qcal (cm)            
                   calp . qcdz = 65.0; //  Half length of Qcal (cm)            
                   /// calp . qcdz = 65.0; //  Half length of Qcal (cm)            
                   calp . scdz = 0.05; //  Half length of Fiber layer (cm)            
                   /// calp . scdz = 0.05; //  Half length of Fiber layer (cm)            
                   calp . sdiv = 260; //  Number of Fiber layers in Qcal            
                   /// calp . sdiv = 260; //  Number of Fiber layers in Qcal            
                   //           
                   calp.fill();           
             ///@}        
             //        
             /// USE calp version= 1 ;        
             calp.Use("version",(Float_t) 1 );        
             z1 = -calp.drdz + calp.phleng  ;// center of large dia. pipe (-116.65);        
             z2 = z1+calp.phleng+calp.pltdz ;// center of lrg pipe end plate (17.17);        
             z3 = z2+calp.pltdz+0.24+calp.pjleng    ;// center of beam pipes;        
             z4 = z2+calp.pltdz+40.0+calp.qcdz ;// center of qcal;        
             _create = AgCreate("ZCAL");        
             {           
                   AgShape myshape; // undefined shape           
                   ///Create ZCAL           
                   Create("ZCAL");            
             }        
             { AgPlacement place = AgPlacement("ZCAL","CAVE");           
                   /// Add daughter volume ZCAL to mother CAVE           
                   place.TranslateZ(1767.66);           
                   /// Translate z = 1767.66           
                   _stacker -> Position( AgBlock::Find("ZCAL"), place );           
             } // end placement of ZCAL        
             { AgPlacement place = AgPlacement("ZCAL","CAVE");           
                   /// Add daughter volume ZCAL to mother CAVE           
                   place.TranslateZ(-1767.66);           
                   /// Translate z = -1767.66           
                   /// G3 Reference: thetax = 90           
                   /// G3 Reference: phix = 0           
                   /// G3 Reference: thetay = 90           
                   /// G3 Reference: phiy = 90           
                   /// G3 Reference: thetaz = 180           
                   /// G3 Reference: phiz = 0           
                   Double_t _thetax=90,_phix=0,_thetay=90,_phiy=90,_thetaz=180,_phiz=0;           
                   place.Reference( _thetax, _phix, _thetay, _phiy, _thetaz, _phiz );           
                   _stacker -> Position( AgBlock::Find("ZCAL"), place );           
             } // end placement of ZCAL        
       }; // ZcalGeo     
 }; // namespace ZcalGeo  
 
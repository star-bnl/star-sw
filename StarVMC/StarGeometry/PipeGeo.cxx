#include "PipeGeo.h"  
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
 namespace PIPEGEO // $NMSPC  
 {     
       //     
       // ---------------------------------------------------------------------------------------------------     
          //  -----------------------------------------------------     
          /// @defgroup pipv_doc     
          /// \class Pipv_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t version;     
          ///Float_t pipeconfig;     
          ///Int_t pipeflag;     
          ///Int_t _index;     
          //     
          Pipv_t pipv;     
          //     
          //  -----------------------------------------------------     
          /// @defgroup pipg_doc     
          /// \class Pipg_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t config;     
          ///Float_t beinnr;     
          ///Float_t beoutr;     
          ///Float_t beleng;     
          ///TString material;     
          ///Float_t s1innr;     
          ///Float_t s1outr;     
          ///Float_t s1leng;     
          ///Float_t s2innr;     
          ///Float_t s2outr;     
          ///Float_t s2leng;     
          ///Float_t s3innr;     
          ///Float_t s3outr;     
          ///Float_t s3leng;     
          ///Float_t s4innr;     
          ///Float_t s4outr;     
          ///Float_t s4leng;     
          ///Float_t flange1t;     
          ///Float_t flange1r;     
          ///Float_t conelen;     
          ///Float_t ribnum;     
          ///Float_t ribspa;     
          ///Float_t ribthk;     
          ///Float_t riboutr;     
          ///Float_t ribcent;     
          ///Float_t wrpinnr;     
          ///Float_t wrpoutr;     
          ///Float_t wrpleng;     
          ///Float_t sldinnr;     
          ///Float_t sldoutr;     
          ///Float_t sldleng;     
          ///Int_t _index;     
          //     
          Pipg_t pipg;     
          //     
          ///@addtogroup PipeGeo_vars     
          ///@{        
                Float_t z1,z2,z3,z4,r1,r2,vacuum=1.e-5;        
                //        
                /// Float_t z1,z2,z3,z4,r1,r2,vacuum=1.e-5        
          ///@}     
          ///@addtogroup PipeGeo_vars     
          ///@{        
                Float_t wrpthk,sldthk;        
                //        
                /// Float_t wrpthk,sldthk        
          ///@}     
       PipeGeo::PipeGeo()     
         : AgModule("PipeGeo"," is the geometry  of the STAR beam pipe. ")     
       {        
       }     
          // ---------------------------------------------------------------------------------------------------     
          void PIPE::Block( AgCreate create )     
          {         
                ///@addtogroup PIPE_doc        
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
                      { AgAttribute attr = AgAttribute("Pipe");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Pcon");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("phi1")=0;              
                            shape.par("dphi")=360;              
                            shape.par("nz")=4;              
                            shape.Z(0)=0;              
                            shape.Z(1)=z1-pipg.flange1t;              
                            shape.Z(2)=z1-pipg.flange1t;              
                            shape.Z(3)=z3;              
                            shape.Rmin(0)=0;              
                            shape.Rmin(1)=0;              
                            shape.Rmin(2)=0;              
                            shape.Rmin(3)=0;              
                            shape.Rmax(0)=r1;              
                            shape.Rmax(1)=r1;              
                            shape.Rmax(2)=r2;              
                            shape.Rmax(3)=r2;              
                            /// Shape Pcon phi1=0 dphi=360 nz=4               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PIPE;              
                            _stacker -> Build(this);              
                      }           
                      if ( pipg.material=="iron" )           
                      {              
                            /// Material Iron               
                            {  AgMaterial mat = AgMaterial::CopyMaterial("Iron");                 
                                  _material = mat;                 
                            }              
                      }           
                      else           
                      {              
                            /// Material Aluminium               
                            {  AgMaterial mat = AgMaterial::CopyMaterial("Aluminium");                 
                                  _material = mat;                 
                            }              
                      }           
                      /// Material pipe dens=(_material.par("dens"))             
                      { AgMaterial &mat = AgMaterial::Get("Pipe");              
                            mat.par("dens")=(_material.par("dens")) ;              
                            _material = mat;              
                      }           
                      _create = AgCreate("PIPC");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PIPC              
                            Create("PIPC");               
                      }           
                      { AgPlacement place = AgPlacement("PIPC","PIPE");              
                            /// Add daughter volume PIPC to mother PIPE              
                            place.TranslateZ(pipg.beleng/2);              
                            /// Translate z = pipg.beleng/2              
                            _stacker -> Position( AgBlock::Find("PIPC"), place );              
                      } // end placement of PIPC           
                      _create = AgCreate("PIPO");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PIPO              
                            Create("PIPO");               
                      }           
                      { AgPlacement place = AgPlacement("PIPO","PIPE");              
                            /// Add daughter volume PIPO to mother PIPE              
                            place.TranslateZ(pipg.beleng+pipg.s1leng);              
                            /// Translate z = pipg.beleng+pipg.s1leng              
                            _stacker -> Position( AgBlock::Find("PIPO"), place );              
                      } // end placement of PIPO           
                      _create = AgCreate("PIPI");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PIPI              
                            Create("PIPI");               
                      }           
                      { AgPlacement place = AgPlacement("PIPI","PIPE");              
                            /// Add daughter volume PIPI to mother PIPE              
                            place.TranslateZ((z1+z2)/2);              
                            /// Translate z = (z1+z2)/2              
                            _stacker -> Position( AgBlock::Find("PIPI"), place );              
                      } // end placement of PIPI           
                      _create = AgCreate("PIPT");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PIPT              
                            Create("PIPT");               
                      }           
                      { AgPlacement place = AgPlacement("PIPT","PIPE");              
                            /// Add daughter volume PIPT to mother PIPE              
                            place.TranslateZ(z2+pipg.flange1t+pipg.s3leng);              
                            /// Translate z = z2+pipg.flange1t+pipg.s3leng              
                            _stacker -> Position( AgBlock::Find("PIPT"), place );              
                      } // end placement of PIPT           
                      _create = AgCreate("PIPB");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PIPB              
                            Create("PIPB");               
                      }           
                      { AgPlacement place = AgPlacement("PIPB","PIPE");              
                            /// Add daughter volume PIPB to mother PIPE              
                            place.TranslateZ(z4);              
                            /// Translate z = z4              
                            _stacker -> Position( AgBlock::Find("PIPB"), place );              
                      } // end placement of PIPB           
                      _create = AgCreate("PFLO");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PFLO              
                            Create("PFLO");               
                      }           
                      { AgPlacement place = AgPlacement("PFLO","PIPE");              
                            /// Add daughter volume PFLO to mother PIPE              
                            place.TranslateZ(z1);              
                            /// Translate z = z1              
                            _stacker -> Position( AgBlock::Find("PFLO"), place );              
                      } // end placement of PFLO           
                      _create = AgCreate("PFLT");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PFLT              
                            Create("PFLT");               
                      }           
                      { AgPlacement place = AgPlacement("PFLT","PIPE");              
                            /// Add daughter volume PFLT to mother PIPE              
                            place.TranslateZ(z2);              
                            /// Translate z = z2              
                            _stacker -> Position( AgBlock::Find("PFLT"), place );              
                      } // end placement of PFLT           
                      _create = AgCreate("PIPS");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PIPS              
                            Create("PIPS");               
                      }           
                      { AgPlacement place = AgPlacement("PIPS","PIPE");              
                            /// Add daughter volume PIPS to mother PIPE              
                            place.TranslateZ(z3-pipg.s4leng);              
                            /// Translate z = z3-pipg.s4leng              
                            _stacker -> Position( AgBlock::Find("PIPS"), place );              
                      } // end placement of PIPS           
                      _create = AgCreate("PRIS");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PRIS              
                            Create("PRIS");               
                      }           
                      { AgPlacement place = AgPlacement("PRIS","PIPE");              
                            /// Add daughter volume PRIS to mother PIPE              
                            place.TranslateZ((z1+z2)/2);              
                            /// Translate z = (z1+z2)/2              
                            _stacker -> Position( AgBlock::Find("PRIS"), place );              
                      } // end placement of PRIS           
                      if ( iand(pipv.pipeflag,1)!=0 )           
                      {              
                            _create = AgCreate("PWRP");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create PWRP                 
                                  Create("PWRP");                  
                            }              
                            { AgPlacement place = AgPlacement("PWRP","PIPE");                 
                                  /// Add daughter volume PWRP to mother PIPE                 
                                  place.TranslateZ(pipg.wrpleng/2);                 
                                  /// Translate z = pipg.wrpleng/2                 
                                  _stacker -> Position( AgBlock::Find("PWRP"), place );                 
                            } // end placement of PWRP              
                      }           
                      if ( iand(pipv.pipeflag,2)!=0 )           
                      {              
                            _create = AgCreate("PSLD");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create PSLD                 
                                  Create("PSLD");                  
                            }              
                            { AgPlacement place = AgPlacement("PSLD","PIPE");                 
                                  /// Add daughter volume PSLD to mother PIPE                 
                                  place.TranslateZ(pipg.sldleng/2);                 
                                  /// Translate z = pipg.sldleng/2                 
                                  _stacker -> Position( AgBlock::Find("PSLD"), place );                 
                            } // end placement of PSLD              
                      }           
                      END_OF_PIPE:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PIPE     
          // ---------------------------------------------------------------------------------------------------     
          void PIPC::Block( AgCreate create )     
          {         
                ///@addtogroup PIPC_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Berillium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Berillium");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("Pipc");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=0;              
                            shape.par("rmax")=pipg.beoutr;              
                            shape.par("dz")=pipg.beleng/2;              
                            /// Shape Tube rmin=0 rmax=pipg.beoutr dz=pipg.beleng/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PIPC;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("PVAC");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PVAC              
                            Create("PVAC");               
                      }           
                      { AgPlacement place = AgPlacement("PVAC","PIPC");              
                            /// Add daughter volume PVAC to mother PIPC              
                            _stacker -> Position( AgBlock::Find("PVAC"), place );              
                      } // end placement of PVAC           
                      END_OF_PIPC:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PIPC     
          // ---------------------------------------------------------------------------------------------------     
          void PVAC::Block( AgCreate create )     
          {         
                ///@addtogroup PVAC_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      /// Material PVacuum dens=(_material.par("dens")) *vacuum absl=(_material.par("absl")) /vacuum radl=(_material.par("radl")) /vacuum            
                      { AgMaterial &mat = AgMaterial::Get("Pvacuum");              
                            mat.par("dens")=(_material.par("dens")) *vacuum;              
                            mat.par("absl")=(_material.par("absl")) /vacuum;              
                            mat.par("radl")=(_material.par("radl")) /vacuum;              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmax")=pipg.beinnr;              
                            /// Shape Tube rmax=pipg.beinnr               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PVAC;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_PVAC:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PVAC     
          // ---------------------------------------------------------------------------------------------------     
          void PIPO::Block( AgCreate create )     
          {         
                ///@addtogroup PIPO_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material pipe            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Pipe");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("Pipo");              
                            attr.par("seen")=1;              
                            attr.par("colo")=3;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=0;              
                            shape.par("rmax")=pipg.s1outr;              
                            shape.par("dz")=pipg.s1leng;              
                            /// Shape Tube rmin=0 rmax=pipg.s1outr dz=pipg.s1leng               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PIPO;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("PVAO");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PVAO              
                            Create("PVAO");               
                      }           
                      { AgPlacement place = AgPlacement("PVAO","PIPO");              
                            /// Add daughter volume PVAO to mother PIPO              
                            _stacker -> Position( AgBlock::Find("PVAO"), place );              
                      } // end placement of PVAO           
                      END_OF_PIPO:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PIPO     
          // ---------------------------------------------------------------------------------------------------     
          void PVAO::Block( AgCreate create )     
          {         
                ///@addtogroup PVAO_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Pvacuum            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Pvacuum");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmax")=pipg.s1innr;              
                            /// Shape Tube rmax=pipg.s1innr               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PVAO;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_PVAO:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PVAO     
          // ---------------------------------------------------------------------------------------------------     
          void PIPI::Block( AgCreate create )     
          {         
                ///@addtogroup PIPI_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material pipe            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Pipe");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("Pipi");              
                            attr.par("seen")=1;              
                            attr.par("colo")=3;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=0;              
                            shape.par("rmax")=pipg.s2outr;              
                            shape.par("dz")=pipg.s2leng;              
                            /// Shape Tube rmin=0 rmax=pipg.s2outr dz=pipg.s2leng               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PIPI;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("PVAI");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PVAI              
                            Create("PVAI");               
                      }           
                      { AgPlacement place = AgPlacement("PVAI","PIPI");              
                            /// Add daughter volume PVAI to mother PIPI              
                            _stacker -> Position( AgBlock::Find("PVAI"), place );              
                      } // end placement of PVAI           
                      END_OF_PIPI:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PIPI     
          // ---------------------------------------------------------------------------------------------------     
          void PVAI::Block( AgCreate create )     
          {         
                ///@addtogroup PVAI_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Pvacuum            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Pvacuum");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmax")=pipg.s2innr;              
                            /// Shape Tube rmax=pipg.s2innr               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PVAI;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_PVAI:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PVAI     
          // ---------------------------------------------------------------------------------------------------     
          void PIPT::Block( AgCreate create )     
          {         
                ///@addtogroup PIPT_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Pipe            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Pipe");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("Pipt");              
                            attr.par("seen")=1;              
                            attr.par("colo")=3;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=0;              
                            shape.par("rmax")=pipg.s3outr;              
                            shape.par("dz")=pipg.s3leng;              
                            /// Shape Tube rmin=0 rmax=pipg.s3outr dz=pipg.s3leng               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PIPT;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("PVAT");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PVAT              
                            Create("PVAT");               
                      }           
                      { AgPlacement place = AgPlacement("PVAT","PIPT");              
                            /// Add daughter volume PVAT to mother PIPT              
                            _stacker -> Position( AgBlock::Find("PVAT"), place );              
                      } // end placement of PVAT           
                      END_OF_PIPT:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PIPT     
          // ---------------------------------------------------------------------------------------------------     
          void PVAT::Block( AgCreate create )     
          {         
                ///@addtogroup PVAT_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Pvacuum            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Pvacuum");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmax")=pipg.s3innr;              
                            /// Shape Tube rmax=pipg.s3innr               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PVAT;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_PVAT:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PVAT     
          // ---------------------------------------------------------------------------------------------------     
          void PIPB::Block( AgCreate create )     
          {         
                ///@addtogroup PIPB_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Iron            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Iron");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("Pipb");              
                            attr.par("seen")=1;              
                            attr.par("colo")=3;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Cone");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dz")=pipg.conelen;              
                            shape.par("rmn1")=0;              
                            shape.par("rmx1")=pipg.s2outr;              
                            shape.par("rmn2")=0;              
                            shape.par("rmx2")=pipg.s4outr;              
                            /// Shape Cone dz=pipg.conelen rmn1=0 rmx1=pipg.s2outr rmn2=0 rmx2=pipg.s4outr               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PIPB;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("PVAB");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PVAB              
                            Create("PVAB");               
                      }           
                      { AgPlacement place = AgPlacement("PVAB","PIPB");              
                            /// Add daughter volume PVAB to mother PIPB              
                            _stacker -> Position( AgBlock::Find("PVAB"), place );              
                      } // end placement of PVAB           
                      END_OF_PIPB:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PIPB     
          // ---------------------------------------------------------------------------------------------------     
          void PVAB::Block( AgCreate create )     
          {         
                ///@addtogroup PVAB_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Pvacuum            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Pvacuum");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Cone");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmx1")=pipg.s3innr;              
                            shape.par("rmx2")=pipg.s4innr;              
                            /// Shape Cone rmx1=pipg.s3innr rmx2=pipg.s4innr               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PVAB;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_PVAB:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PVAB     
          // ---------------------------------------------------------------------------------------------------     
          void PIPS::Block( AgCreate create )     
          {         
                ///@addtogroup PIPS_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Iron            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Iron");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("Pips");              
                            attr.par("seen")=1;              
                            attr.par("colo")=3;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=0;              
                            shape.par("rmax")=pipg.s4outr;              
                            shape.par("dz")=pipg.s4leng;              
                            /// Shape Tube rmin=0 rmax=pipg.s4outr dz=pipg.s4leng               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PIPS;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("PVAS");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PVAS              
                            Create("PVAS");               
                      }           
                      { AgPlacement place = AgPlacement("PVAS","PIPS");              
                            /// Add daughter volume PVAS to mother PIPS              
                            _stacker -> Position( AgBlock::Find("PVAS"), place );              
                      } // end placement of PVAS           
                      END_OF_PIPS:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PIPS     
          // ---------------------------------------------------------------------------------------------------     
          void PVAS::Block( AgCreate create )     
          {         
                ///@addtogroup PVAS_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Pvacuum            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Pvacuum");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmax")=pipg.s4innr;              
                            /// Shape Tube rmax=pipg.s4innr               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PVAS;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_PVAS:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PVAS     
          // ---------------------------------------------------------------------------------------------------     
          void PFLO::Block( AgCreate create )     
          {         
                ///@addtogroup PFLO_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Iron            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Iron");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("Pflo");              
                            attr.par("seen")=1;              
                            attr.par("colo")=4;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=pipg.s2outr;              
                            shape.par("rmax")=pipg.flange1r;              
                            shape.par("dz")=pipg.flange1t;              
                            /// Shape Tube rmin=pipg.s2outr rmax=pipg.flange1r dz=pipg.flange1t               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PFLO;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_PFLO:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PFLO     
          // ---------------------------------------------------------------------------------------------------     
          void PFLT::Block( AgCreate create )     
          {         
                ///@addtogroup PFLT_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Iron            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Iron");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("Pflt");              
                            attr.par("seen")=1;              
                            attr.par("colo")=4;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=pipg.s2outr;              
                            shape.par("rmax")=pipg.flange1r;              
                            shape.par("dz")=pipg.flange1t;              
                            /// Shape Tube rmin=pipg.s2outr rmax=pipg.flange1r dz=pipg.flange1t               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PFLT;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_PFLT:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PFLT     
          // ---------------------------------------------------------------------------------------------------     
          void PRIS::Block( AgCreate create )     
          {         
                ///@addtogroup PRIS_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("PRIB");              
                            attr.par("seen")=0;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=pipg.s2outr;              
                            shape.par("rmax")=pipg.riboutr;              
                            shape.par("dz")=pipg.ribnum*pipg.ribspa/2.;              
                            /// Shape Tube rmin=pipg.s2outr rmax=pipg.riboutr dz=pipg.ribnum*pipg.ribspa/2.               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PRIS;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("PRID");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PRID              
                            Create("PRID");               
                      }           
                      { AgPlacement place = AgPlacement("PRID","PRIS");              
                            /// Add daughter volume PRID to mother PRIS              
                            _stacker -> Position( AgBlock::Find("PRID"), place );              
                      } // end placement of PRID           
                      END_OF_PRIS:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PRIS     
          // ---------------------------------------------------------------------------------------------------     
          void PRID::Block( AgCreate create )     
          {         
                ///@addtogroup PRID_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      {  AgShape shape = AgShape("Division");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("ndiv")=pipg.ribnum;              
                            shape.par("iaxis")=3;              
                            /// Shape Division ndiv=pipg.ribnum iaxis=3               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PRID;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("PRIB");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PRIB              
                            Create("PRIB");               
                      }           
                      { AgPlacement place = AgPlacement("PRIB","PRID");              
                            /// Add daughter volume PRIB to mother PRID              
                            _stacker -> Position( AgBlock::Find("PRIB"), place );              
                      } // end placement of PRIB           
                      END_OF_PRID:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PRID     
          // ---------------------------------------------------------------------------------------------------     
          void PRIB::Block( AgCreate create )     
          {         
                ///@addtogroup PRIB_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Iron            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Iron");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("PRIB");              
                            attr.par("seen")=1;              
                            attr.par("colo")=7;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dz")=pipg.ribthk;              
                            /// Shape Tube dz=pipg.ribthk               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PRIB;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_PRIB:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PRIB     
          // ---------------------------------------------------------------------------------------------------     
          void PWRP::Block( AgCreate create )     
          {         
                ///@addtogroup PWRP_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Component C5	a=12	z=6	w=500000           
                      /// Component H4	a=1	z=1	w=400000           
                      /// Component O2	a=16	z=8	w=200000           
                      /// Component Al	a=27	z=13	w=345327           
                      /// Mixture MLI dens=1.82667           
                      {  AgMaterial &mix = AgMaterial::Get("Mli");              
                            mix.Component("C5",12,6,500000);              
                            mix.Component("H4",1,1,400000);              
                            mix.Component("O2",16,8,200000);              
                            mix.Component("Al",27,13,345327);              
                            mix.par("dens")=1.82667;              
                            mix.lock();              
                            _material = mix;              
                            _material.lock();              
                      }           
                      { AgAttribute attr = AgAttribute("Pwrp");              
                            attr.par("seen")=1;              
                            attr.par("colo")=3;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=pipg.wrpinnr;              
                            shape.par("rmax")=pipg.wrpoutr;              
                            shape.par("dz")=pipg.wrpleng/2;              
                            /// Shape Tube rmin=pipg.wrpinnr rmax=pipg.wrpoutr dz=pipg.wrpleng/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PWRP;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_PWRP:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PWRP     
          // ---------------------------------------------------------------------------------------------------     
          void PSLD::Block( AgCreate create )     
          {         
                ///@addtogroup PSLD_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Component C5	a=12	z=6	w=50000           
                      /// Component H4	a=1	z=1	w=40000           
                      /// Component O2	a=16	z=8	w=20000           
                      /// Component Al	a=27	z=13	w=2302           
                      /// Mixture ALKAP dens=1.432           
                      {  AgMaterial &mix = AgMaterial::Get("Alkap");              
                            mix.Component("C5",12,6,50000);              
                            mix.Component("H4",1,1,40000);              
                            mix.Component("O2",16,8,20000);              
                            mix.Component("Al",27,13,2302);              
                            mix.par("dens")=1.432;              
                            mix.lock();              
                            _material = mix;              
                            _material.lock();              
                      }           
                      { AgAttribute attr = AgAttribute("PSLD");              
                            attr.par("seen")=1;              
                            attr.par("colo")=3;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=pipg.sldinnr;              
                            shape.par("rmax")=pipg.sldoutr;              
                            shape.par("dz")=pipg.sldleng/2;              
                            /// Shape Tube rmin=pipg.sldinnr rmax=pipg.sldoutr dz=pipg.sldleng/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PSLD;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_PSLD:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PSLD     
    // ----------------------------------------------------------------------- geoctr
       void PipeGeo::ConstructGeometry()     
       {        
             ///@addtogroup PipeGeo_revision        
             ///@{           
                   /// Created:   30-03-99            
             ///@}        
             ///@addtogroup PipeGeo_revision        
             ///@{           
                   /// Author: W.B.Christie           
             ///@}        
             AddBlock("PIPE");        
             AddBlock("PIPC");        
             AddBlock("PIPO");        
             AddBlock("PIPS");        
             AddBlock("PIPB");        
             AddBlock("PIPT");        
             AddBlock("PFLO");        
             AddBlock("PFLT");        
             AddBlock("PVAC");        
             AddBlock("PVAO");        
             AddBlock("PVAS");        
             AddBlock("PVAB");        
             AddBlock("PRIS");        
             AddBlock("PRID");        
             AddBlock("PRIB");        
             AddBlock("PIPI");        
             AddBlock("PVAI");        
             AddBlock("PVAT");        
             AddBlock("PWRP");        
             AddBlock("PSLD");        
             wrpthk = (3.+1.5)*2*mil_p;        
             sldthk = (3.+0.1)  *mil_p;        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup pipv_doc        
             ///@{           
                   ++pipv._index;           
                   pipv . version = 1; //  geometry version            
                   /// pipv . version = 1; //  geometry version            
                   pipv . pipeconfig = 2; //  pipe version (2 is the default, unfortunately)            
                   /// pipv . pipeconfig = 2; //  pipe version (2 is the default, unfortunately)            
                   pipv . pipeflag = 0; //  1=PWRP ;//2=PSLD            
                   /// pipv . pipeflag = 0; //  1=PWRP ;//2=PSLD            
                   //           
                   pipv.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup pipg_doc        
             ///@{           
                   ++pipg._index;           
                   pipg . config = 2; //  both material and geo params            
                   /// pipg . config = 2; //  both material and geo params            
                   pipg . beinnr = 3.9; //  Berillium section inner radius            
                   /// pipg . beinnr = 3.9; //  Berillium section inner radius            
                   pipg . beoutr = 4.0; //  Berillium section outer radius            
                   /// pipg . beoutr = 4.0; //  Berillium section outer radius            
                   pipg . beleng = 76.2; //  Berillium section half length            
                   /// pipg . beleng = 76.2; //  Berillium section half length            
                   pipg . material = "alum"; //  pipe main section material             
                   /// pipg . material = "alum"; //  pipe main section material             
                   pipg . s1innr = 3.875; //  first Aluminum section inner radius            
                   /// pipg . s1innr = 3.875; //  first Aluminum section inner radius            
                   pipg . s1outr = 4.0; //  first Aluminum section outer radius            
                   /// pipg . s1outr = 4.0; //  first Aluminum section outer radius            
                   pipg . s1leng = 153.4; //  first Aluminum section half length            
                   /// pipg . s1leng = 153.4; //  first Aluminum section half length            
                   pipg . s2innr = 3.875; //  second Aluminum section inner radius            
                   /// pipg . s2innr = 3.875; //  second Aluminum section inner radius            
                   pipg . s2outr = 4.00; //  second Aluminum section outer radius            
                   /// pipg . s2outr = 4.00; //  second Aluminum section outer radius            
                   pipg . s2leng = 18.0; //  second Aluminum section half length            
                   /// pipg . s2leng = 18.0; //  second Aluminum section half length            
                   pipg . s3innr = 3.875; //  Transition Stub Aluminum section inner radius            
                   /// pipg . s3innr = 3.875; //  Transition Stub Aluminum section inner radius            
                   pipg . s3outr = 4.0; //  Transition Stub Aluminum section outer radius            
                   /// pipg . s3outr = 4.0; //  Transition Stub Aluminum section outer radius            
                   pipg . s3leng = 1.0; //  Transition Stub Aluminum section half length            
                   /// pipg . s3leng = 1.0; //  Transition Stub Aluminum section half length            
                   pipg . s4innr = 6.20; //  Large OD Aluminum section inner radius            
                   /// pipg . s4innr = 6.20; //  Large OD Aluminum section inner radius            
                   pipg . s4outr = 6.35; //  Large OD Aluminum section outer radius            
                   /// pipg . s4outr = 6.35; //  Large OD Aluminum section outer radius            
                   pipg . s4leng = 150.0; //  Large OD Aluminum section half length            
                   /// pipg . s4leng = 150.0; //  Large OD Aluminum section half length            
                   pipg . conelen = 12.5; //  half length of the Bell Reducer Cone            
                   /// pipg . conelen = 12.5; //  half length of the Bell Reducer Cone            
                   pipg . flange1t = 2.0; //  flange SET half thickness            
                   /// pipg . flange1t = 2.0; //  flange SET half thickness            
                   pipg . flange1r = 5.85; //  flange outer radius            
                   /// pipg . flange1r = 5.85; //  flange outer radius            
                   pipg . ribnum = 8; //  number of Ribs            
                   /// pipg . ribnum = 8; //  number of Ribs            
                   pipg . ribspa = 1.75; //  spacing between Ribs            
                   /// pipg . ribspa = 1.75; //  spacing between Ribs            
                   pipg . ribthk = 0.05; //  Rib half thickness            
                   /// pipg . ribthk = 0.05; //  Rib half thickness            
                   pipg . riboutr = 4.8; //  Rib Outer Radius            
                   /// pipg . riboutr = 4.8; //  Rib Outer Radius            
                   pipg . ribcent = 454.5; //  Rib Set center             
                   /// pipg . ribcent = 454.5; //  Rib Set center             
                   pipg . wrpinnr = 4.0; //  inner radius of beampipe multi layer insulation            
                   /// pipg . wrpinnr = 4.0; //  inner radius of beampipe multi layer insulation            
                   pipg . wrpoutr = 4.0+wrpthk; //  outer radius of beampipe multi layer insulation            
                   /// pipg . wrpoutr = 4.0+wrpthk; //  outer radius of beampipe multi layer insulation            
                   pipg . wrpleng = 300; //  length of beampipe multi layer insulation            
                   /// pipg . wrpleng = 300; //  length of beampipe multi layer insulation            
                   pipg . sldinnr = 4.0+wrpthk; //  inner radius of SVT beam pipe shield            
                   /// pipg . sldinnr = 4.0+wrpthk; //  inner radius of SVT beam pipe shield            
                   pipg . sldoutr = 4.0+wrpthk+sldthk; //  outer radius of SVT beam pipe shield            
                   /// pipg . sldoutr = 4.0+wrpthk+sldthk; //  outer radius of SVT beam pipe shield            
                   pipg . sldleng = 56; //  length of SVT beam pipe shield            
                   /// pipg . sldleng = 56; //  length of SVT beam pipe shield            
                   //           
                   pipg.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup pipg_doc        
             ///@{           
                   ++pipg._index;           
                   pipg . config = 1; //  both material and geo params            
                   /// pipg . config = 1; //  both material and geo params            
                   pipg . material = "iron"; //  material is steel            
                   /// pipg . material = "iron"; //  material is steel            
                   //           
                   pipg.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup pipg_doc        
             ///@{           
                   ++pipg._index;           
                   pipg . config = 4; //  both material and geo params            
                   /// pipg . config = 4; //  both material and geo params            
                   pipg . beinnr = 1.374; //  Berillium section inner radius            
                   /// pipg . beinnr = 1.374; //  Berillium section inner radius            
                   pipg . beoutr = 1.450; //  Berillium section outer radius            
                   /// pipg . beoutr = 1.450; //  Berillium section outer radius            
                   pipg . material = "alum"; //  pipe main section material             
                   /// pipg . material = "alum"; //  pipe main section material             
                   //           
                   pipg.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup pipg_doc        
             ///@{           
                   ++pipg._index;           
                   pipg . config = 5; //  both material and geo params            
                   /// pipg . config = 5; //  both material and geo params            
                   pipg . beinnr = 1.374; //  Berillium section inner radius            
                   /// pipg . beinnr = 1.374; //  Berillium section inner radius            
                   pipg . beoutr = 1.424; //  Berillium section outer radius            
                   /// pipg . beoutr = 1.424; //  Berillium section outer radius            
                   //           
                   pipg.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup pipg_doc        
             ///@{           
                   ++pipg._index;           
                   pipg . config = 6; //  both material and geo params            
                   /// pipg . config = 6; //  both material and geo params            
                   pipg . beinnr = 2.0; //  Berillium section inner radius (Jan upgr16)            
                   /// pipg . beinnr = 2.0; //  Berillium section inner radius (Jan upgr16)            
                   pipg . beoutr = 2.076; //  Berillium section outer radius            
                   /// pipg . beoutr = 2.076; //  Berillium section outer radius            
                   //           
                   pipg.fill();           
             ///@}        
             //        
             /// USE pipv _index=1;        
             pipv.Use();        
             /// USE pipg config=pipv.pipeconfig;        
             pipg.Use("config",(Float_t)pipv.pipeconfig);        
             // Print<level=%i> fmt=%s fortran format statements not supported        
             z1 = pipg.beleng + 2*pipg.s1leng + pipg.flange1t;        
             z2 = z1 - 2*pipg.flange1t + 2*pipg.s2leng;        
             z3 = z2 + pipg.flange1t +2*pipg.s3leng +2*pipg.conelen +2*pipg.s4leng;        
             z4 = z2 + pipg.flange1t + 2*pipg.s3leng + pipg.conelen;        
             r1 = pipg.sldoutr;        
             r2 = pipg.s4outr;        
             _create = AgCreate("PIPE");        
             {           
                   AgShape myshape; // undefined shape           
                   ///Create PIPE           
                   Create("PIPE");            
             }        
             if ( pipv.pipeconfig>=4 )        
             {           
                   { AgPlacement place = AgPlacement("PIPE","CAVE");              
                         /// Add daughter volume PIPE to mother CAVE              
                         place.par("only")=AgPlacement::kMany;              
                         /// Overlap: agplacement::kmany              
                         _stacker -> Position( AgBlock::Find("PIPE"), place );              
                   } // end placement of PIPE           
                   { AgPlacement place = AgPlacement("PIPE","CAVE");              
                         /// Add daughter volume PIPE to mother CAVE              
                         place.par("only")=AgPlacement::kMany;              
                         /// Overlap: agplacement::kmany              
                         /// G3 Reference: thetax = 90              
                         /// G3 Reference: phix = 0              
                         /// G3 Reference: thetay = 90              
                         /// G3 Reference: phiy = 90              
                         /// G3 Reference: thetaz = 180              
                         /// G3 Reference: phiz = 0              
                         Double_t _thetax=90,_phix=0,_thetay=90,_phiy=90,_thetaz=180,_phiz=0;              
                         place.Reference( _thetax, _phix, _thetay, _phiy, _thetaz, _phiz );              
                         _stacker -> Position( AgBlock::Find("PIPE"), place );              
                   } // end placement of PIPE           
             }        
             else        
             {           
                   { AgPlacement place = AgPlacement("PIPE","CAVE");              
                         /// Add daughter volume PIPE to mother CAVE              
                         _stacker -> Position( AgBlock::Find("PIPE"), place );              
                   } // end placement of PIPE           
                   { AgPlacement place = AgPlacement("PIPE","CAVE");              
                         /// Add daughter volume PIPE to mother CAVE              
                         /// G3 Reference: thetax = 90              
                         /// G3 Reference: phix = 0              
                         /// G3 Reference: thetay = 90              
                         /// G3 Reference: phiy = 90              
                         /// G3 Reference: thetaz = 180              
                         /// G3 Reference: phiz = 0              
                         Double_t _thetax=90,_phix=0,_thetay=90,_phiy=90,_thetaz=180,_phiz=0;              
                         place.Reference( _thetax, _phix, _thetay, _phiy, _thetaz, _phiz );              
                         _stacker -> Position( AgBlock::Find("PIPE"), place );              
                   } // end placement of PIPE           
             }        
       }; // PipeGeo     
 }; // namespace PipeGeo  
 
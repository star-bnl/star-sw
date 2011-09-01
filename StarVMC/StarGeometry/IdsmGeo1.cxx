#include "IdsmGeo1.h"  
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
 namespace IDSMGEO1 // $NMSPC  
 {     
       //     
       // ---------------------------------------------------------------------------------------------------     
          ///@addtogroup IdsmGeo1_vars     
          ///@{        
                Float_t inr,outr,lengthz,k,sina,cosa,resr,angr,m,pm;        
                //        
                /// Float_t inr,outr,lengthz,k,sina,cosa,resr,angr,m,pm        
          ///@}     
          //  -----------------------------------------------------     
          /// @defgroup idsg_doc     
          /// \class Idsg_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t version;     
          ///Float_t rf;     
          ///Float_t angflat;     
          ///Float_t r1res;     
          ///Float_t r2res;     
          ///Float_t rrres;     
          ///Float_t dangres;     
          ///Float_t dxres;     
          ///Float_t dyres;     
          ///Int_t _index;     
          //     
          Idsg_t idsg;     
          //     
          //  -----------------------------------------------------     
          /// @defgroup idsa_doc     
          /// \class Idsa_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t version;     
          ///Float_t x;     
          ///Float_t y;     
          ///Float_t z;     
          ///Float_t thetax;     
          ///Float_t thetay;     
          ///Float_t thetaz;     
          ///Float_t phix;     
          ///Float_t phiy;     
          ///Float_t phiz;     
          ///Int_t _index;     
          //     
          Idsa_t idsa;     
          //     
       IdsmGeo1::IdsmGeo1()     
         : AgModule("IdsmGeo1"," simplified  beam support cone for 2012 ")     
       {        
       }     
          // ---------------------------------------------------------------------------------------------------     
          void IDSM::Block( AgCreate create )     
          {         
                ///@addtogroup IDSM_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("IDSM");              
                            attr.par("seen")=0;              
                            attr.par("colo")=4;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=inr;              
                            shape.par("rmax")=outr;              
                            shape.par("dz")=lengthz/2.;              
                            /// Shape Tube rmin=inr rmax=outr dz=lengthz/2.               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_IDSM;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("SUCA");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create SUCA              
                            Create("SUCA");               
                      }           
                      { AgPlacement place = AgPlacement("SUCA","IDSM");              
                            /// Add daughter volume SUCA to mother IDSM              
                            place.TranslateZ(0);              
                            /// Translate z = 0              
                            _stacker -> Position( AgBlock::Find("SUCA"), place );              
                      } // end placement of SUCA           
                      /// Loop on k from 0 to 1 step=1           
                      for ( k=0; (1>0)? (k<=1):(k>=1); k+=1 )           
                      {              
                            pm=1.-2*k;              
                            _create = AgCreate("SUCB");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create SUCB                 
                                  Create("SUCB");                  
                            }              
                            { AgPlacement place = AgPlacement("SUCB","IDSM");                 
                                  /// Add daughter volume SUCB to mother IDSM                 
                                  place.TranslateZ(pm*55.35);                 
                                  /// Translate z = pm*55.35                 
                                  _stacker -> Position( AgBlock::Find("SUCB"), place );                 
                            } // end placement of SUCB              
                            if ( k==0 )              
                            {                 
                                  _create = AgCreate("SUCC");                 
                                  {                    
                                        AgShape myshape; // undefined shape                    
                                        ///Create SUCC                    
                                        Create("SUCC");                     
                                  }                 
                                  { AgPlacement place = AgPlacement("SUCC","IDSM");                    
                                        /// Add daughter volume SUCC to mother IDSM                    
                                        place.TranslateZ(59.55);                    
                                        /// Translate z = 59.55                    
                                        _stacker -> Position( AgBlock::Find("SUCC"), place );                    
                                  } // end placement of SUCC                 
                            }              
                            else              
                            {                 
                                  { AgPlacement place = AgPlacement("SUCC","IDSM");                    
                                        /// Add daughter volume SUCC to mother IDSM                    
                                        place.TranslateZ(-59.55);                    
                                        /// Translate z = -59.55                    
                                        place.AlphaX(180.);                    
                                        /// Rotate: AlphaX = 180.                    
                                        /// G3 Reference: thetax = 90                    
                                        /// G3 Reference: phix = 0                    
                                        /// G3 Reference: thetay = 90                    
                                        /// G3 Reference: phiy = 90                    
                                        /// G3 Reference: thetaz = 0                    
                                        /// G3 Reference: phiz = 0                    
                                        _stacker -> Position( AgBlock::Find("SUCC"), place );                    
                                  } // end placement of SUCC                 
                            }              
                            _create = AgCreate("SUCD");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create SUCD                 
                                  Create("SUCD");                  
                            }              
                            { AgPlacement place = AgPlacement("SUCD","IDSM");                 
                                  /// Add daughter volume SUCD to mother IDSM                 
                                  place.TranslateZ(pm*63.41);                 
                                  /// Translate z = pm*63.41                 
                                  _stacker -> Position( AgBlock::Find("SUCD"), place );                 
                            } // end placement of SUCD              
                            _create = AgCreate("SUCE");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create SUCE                 
                                  Create("SUCE");                  
                            }              
                            { AgPlacement place = AgPlacement("SUCE","IDSM");                 
                                  /// Add daughter volume SUCE to mother IDSM                 
                                  place.TranslateZ(pm*144.52);                 
                                  /// Translate z = pm*144.52                 
                                  _stacker -> Position( AgBlock::Find("SUCE"), place );                 
                            } // end placement of SUCE              
                            _create = AgCreate("SUCF");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create SUCF                 
                                  Create("SUCF");                  
                            }              
                            { AgPlacement place = AgPlacement("SUCF","IDSM");                 
                                  /// Add daughter volume SUCF to mother IDSM                 
                                  place.TranslateZ(pm*224.52);                 
                                  /// Translate z = pm*224.52                 
                                  _stacker -> Position( AgBlock::Find("SUCF"), place );                 
                            } // end placement of SUCF              
                            _create = AgCreate("SUCG");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create SUCG                 
                                  Create("SUCG");                  
                            }              
                            { AgPlacement place = AgPlacement("SUCG","IDSM");                 
                                  /// Add daughter volume SUCG to mother IDSM                 
                                  place.TranslateZ(pm*225.4);                 
                                  /// Translate z = pm*225.4                 
                                  _stacker -> Position( AgBlock::Find("SUCG"), place );                 
                            } // end placement of SUCG              
                      }           
                      /// Loop on m from 0 to 1 step=1           
                      for ( m=0; (1>0)? (m<=1):(m>=1); m+=1 )           
                      {              
                            angr = idsg.angflat - idsg.dangres/2.;              
                            if ( m==1 )              
                            {                 
                                  angr = idsg.angflat+idsg.dangres/2.;                 
                            }              
                            _create = AgCreate("TPRT");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create TPRT                 
                                  Create("TPRT");                  
                            }              
                            { AgPlacement place = AgPlacement("TPRT","IDSM");                 
                                  /// Add daughter volume TPRT to mother IDSM                 
                                  place.TranslateX(idsg.rrres*cos(angr/180.*3.1416));                 
                                  /// Translate x = idsg.rrres*cos(angr/180.*3.1416)                 
                                  place.TranslateY(idsg.rrres*sin(angr/180.*3.1416));                 
                                  /// Translate y = idsg.rrres*sin(angr/180.*3.1416)                 
                                  place.TranslateZ(0);                 
                                  /// Translate z = 0                 
                                  _stacker -> Position( AgBlock::Find("TPRT"), place );                 
                            } // end placement of TPRT              
                      }           
                      _create = AgCreate("FGCB");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FGCB              
                            Create("FGCB");               
                      }           
                      { AgPlacement place = AgPlacement("FGCB","IDSM");              
                            /// Add daughter volume FGCB to mother IDSM              
                            place.TranslateZ(lengthz/2.);              
                            /// Translate z = lengthz/2.              
                            place.AlphaZ(16);              
                            /// Rotate: AlphaZ = 16              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            _stacker -> Position( AgBlock::Find("FGCB"), place );              
                      } // end placement of FGCB           
                      _create = AgCreate("FGCB");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FGCB              
                            Create("FGCB");               
                      }           
                      { AgPlacement place = AgPlacement("FGCB","IDSM");              
                            /// Add daughter volume FGCB to mother IDSM              
                            place.TranslateZ(lengthz/2.);              
                            /// Translate z = lengthz/2.              
                            place.AlphaZ(180.+16);              
                            /// Rotate: AlphaZ = 180.+16              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            _stacker -> Position( AgBlock::Find("FGCB"), place );              
                      } // end placement of FGCB           
                      _create = AgCreate("TPRR");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create TPRR              
                            Create("TPRR");               
                      }           
                      { AgPlacement place = AgPlacement("TPRR","IDSM");              
                            /// Add daughter volume TPRR to mother IDSM              
                            place.TranslateX(idsg.rrres*cosa);              
                            /// Translate x = idsg.rrres*cosa              
                            place.TranslateY(idsg.rrres*sina);              
                            /// Translate y = idsg.rrres*sina              
                            place.TranslateZ(0);              
                            /// Translate z = 0              
                            place.AlphaZ(idsg.angflat);              
                            /// Rotate: AlphaZ = idsg.angflat              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            _stacker -> Position( AgBlock::Find("TPRR"), place );              
                      } // end placement of TPRR           
                      END_OF_IDSM:           
                      mCurrent = _save;           
                ///@}        
          } // End Block IDSM     
          // ---------------------------------------------------------------------------------------------------     
          void SUCA::Block( AgCreate create )     
          {         
                ///@addtogroup SUCA_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("SUCA");              
                            attr.par("seen")=1;              
                            attr.par("colo")=6;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material CFRPMix            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Cfrpmix");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=21.5;              
                            shape.par("rmax")=21.6;              
                            shape.par("dz")=112./2.;              
                            /// Shape Tube rmin=21.5 rmax=21.6 dz=112./2.               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SUCA;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_SUCA:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SUCA     
          // ---------------------------------------------------------------------------------------------------     
          void SUCB::Block( AgCreate create )     
          {         
                ///@addtogroup SUCB_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("SUCB");              
                            attr.par("seen")=1;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material SUCMix            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Sucmix");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=21.6;              
                            shape.par("rmax")=22.7;              
                            shape.par("dz")=1.3/2.;              
                            /// Shape Tube rmin=21.6 rmax=22.7 dz=1.3/2.               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SUCB;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_SUCB:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SUCB     
          // ---------------------------------------------------------------------------------------------------     
          void SUCC::Block( AgCreate create )     
          {         
                ///@addtogroup SUCC_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("SUCC");              
                            attr.par("seen")=1;              
                            attr.par("colo")=6;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material CFRPMix            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Cfrpmix");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Cone");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dz")=7.1/2.;              
                            shape.par("rmn1")=21.6;              
                            shape.par("rmx1")=22.75;              
                            shape.par("rmn2")=38.75;              
                            shape.par("rmx2")=39.9;              
                            /// Shape Cone dz=7.1/2. rmn1=21.6 rmx1=22.75 rmn2=38.75 rmx2=39.9               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SUCC;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_SUCC:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SUCC     
          // ---------------------------------------------------------------------------------------------------     
          void SUCD::Block( AgCreate create )     
          {         
                ///@addtogroup SUCD_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("SUCD");              
                            attr.par("seen")=1;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material SUCMix            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Sucmix");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=37.8;              
                            shape.par("rmax")=39.9;              
                            shape.par("dz")=0.62/2.;              
                            /// Shape Tube rmin=37.8 rmax=39.9 dz=0.62/2.               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SUCD;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_SUCD:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SUCD     
          // ---------------------------------------------------------------------------------------------------     
          void SUCE::Block( AgCreate create )     
          {         
                ///@addtogroup SUCE_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("SUCE");              
                            attr.par("seen")=1;              
                            attr.par("colo")=6;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material CFRPMix            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Cfrpmix");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=39.8;              
                            shape.par("rmax")=39.9;              
                            shape.par("dz")=161.6/2.;              
                            /// Shape Tube rmin=39.8 rmax=39.9 dz=161.6/2.               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SUCE;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_SUCE:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SUCE     
          // ---------------------------------------------------------------------------------------------------     
          void SUCF::Block( AgCreate create )     
          {         
                ///@addtogroup SUCF_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("SUCF");              
                            attr.par("seen")=1;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material AlPure            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Alpure");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=40.;              
                            shape.par("rmax")=40.5;              
                            shape.par("dz")=1.6/2.;              
                            /// Shape Tube rmin=40. rmax=40.5 dz=1.6/2.               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SUCF;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_SUCF:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SUCF     
          // ---------------------------------------------------------------------------------------------------     
          void SUCG::Block( AgCreate create )     
          {         
                ///@addtogroup SUCG_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("SUCG");              
                            attr.par("seen")=1;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material AlPure            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Alpure");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=10.3;              
                            shape.par("rmax")=40.5;              
                            shape.par("dz")=0.16/2.;              
                            /// Shape Tube rmin=10.3 rmax=40.5 dz=0.16/2.               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SUCG;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_SUCG:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SUCG     
          // ---------------------------------------------------------------------------------------------------     
          void TPRR::Block( AgCreate create )     
          {         
                ///@addtogroup TPRR_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("TPRR");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material Alumina            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Alumina");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=idsg.dxres/2;              
                            shape.par("dy")=idsg.dyres/2.;              
                            shape.par("dz")=lengthz/2.;              
                            /// Shape Bbox dx=idsg.dxres/2 dy=idsg.dyres/2. dz=lengthz/2.               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_TPRR;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_TPRR:           
                      mCurrent = _save;           
                ///@}        
          } // End Block TPRR     
          // ---------------------------------------------------------------------------------------------------     
          void TPRT::Block( AgCreate create )     
          {         
                ///@addtogroup TPRT_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("TPRT");              
                            attr.par("seen")=1;              
                            attr.par("colo")=3;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material FR4            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Fr4");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=idsg.r1res;              
                            shape.par("rmax")=idsg.r2res;              
                            shape.par("dz")=lengthz/2.;              
                            /// Shape Tube rmin=idsg.r1res rmax=idsg.r2res dz=lengthz/2.               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_TPRT;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_TPRT:           
                      mCurrent = _save;           
                ///@}        
          } // End Block TPRT     
          // ---------------------------------------------------------------------------------------------------     
          void FGCB::Block( AgCreate create )     
          {         
                ///@addtogroup FGCB_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("FGCB");              
                            attr.par("seen")=1;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material badFgtCables            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Badfgtcables");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Tubs");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=idsg.rrres-1.;              
                            shape.par("rmax")=idsg.rrres+0.5;              
                            shape.par("phi1")=-10.;              
                            shape.par("phi2")=10.;              
                            shape.par("dz")=lengthz/4.;              
                            /// Shape Tubs rmin=idsg.rrres-1. rmax=idsg.rrres+0.5 phi1=-10. phi2=10. dz=lengthz/4.               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FGCB;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_FGCB:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FGCB     
    // ----------------------------------------------------------------------- geoctr
       void IdsmGeo1::ConstructGeometry()     
       {        
             ///@addtogroup IdsmGeo1_revision        
             ///@{           
                   /// Created:   8/30/2011            
             ///@}        
             ///@addtogroup IdsmGeo1_revision        
             ///@{           
                   /// Author: Jan Balewski MIT            
             ///@}        
             AddBlock("IDSM");        
             AddBlock("TPRR");        
             AddBlock("TPRT");        
             AddBlock("FGCB");        
             AddBlock("SUCA");        
             AddBlock("SUCB");        
             AddBlock("SUCC");        
             AddBlock("SUCD");        
             AddBlock("SUCE");        
             AddBlock("SUCF");        
             AddBlock("SUCG");        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup idsg_doc        
             ///@{           
                   ++idsg._index;           
                   idsg . version = 1.0; // Versioning of the IDS geometry           
                   /// idsg . version = 1.0; // Versioning of the IDS geometry           
                   idsg . rf = 8.0; //  radii of inner volume boundary           
                   /// idsg . rf = 8.0; //  radii of inner volume boundary           
                   idsg . angflat = 106.; //  angle (deg) for center of flat           
                   /// idsg . angflat = 106.; //  angle (deg) for center of flat           
                   idsg . rrres = 43.; //  radial distance of  for TPC resistor tubes           
                   /// idsg . rrres = 43.; //  radial distance of  for TPC resistor tubes           
                   idsg . r1res = 1.17; //  inner radii for TPC resistor tubes           
                   /// idsg . r1res = 1.17; //  inner radii for TPC resistor tubes           
                   idsg . r2res = 1.27; //  outer radii for TPC resistor tubes           
                   /// idsg . r2res = 1.27; //  outer radii for TPC resistor tubes           
                   idsg . dangres = 11.3; //  opening angle (deg) for TPC resistor tubes           
                   /// idsg . dangres = 11.3; //  opening angle (deg) for TPC resistor tubes           
                   idsg . dxres = 0.13; //  thicknessfor TPC resistor           
                   /// idsg . dxres = 0.13; //  thicknessfor TPC resistor           
                   idsg . dyres = 2.; //  dy for TPC resistor           
                   /// idsg . dyres = 2.; //  dy for TPC resistor           
                   //           
                   idsg.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup idsa_doc        
             ///@{           
                   ++idsa._index;           
                   idsa . version = 2.0; // Default alignment of IDSM at (0,0,0) with no rotation           
                   /// idsa . version = 2.0; // Default alignment of IDSM at (0,0,0) with no rotation           
                   idsa . x = 0.0; // x-alignment           
                   /// idsa . x = 0.0; // x-alignment           
                   idsa . y = 0.0; // y-alignment           
                   /// idsa . y = 0.0; // y-alignment           
                   idsa . z = 0.0; // z-alignment           
                   /// idsa . z = 0.0; // z-alignment           
                   idsa . thetax = 90.0; // align x`-axis 90 degrees in theta wrt cave           
                   /// idsa . thetax = 90.0; // align x`-axis 90 degrees in theta wrt cave           
                   idsa . phix =  0.0; // align x`-axis  0 degrees in phi   wrt cave           
                   /// idsa . phix =  0.0; // align x`-axis  0 degrees in phi   wrt cave           
                   idsa . thetay = 90.0; // align y`-axis 90 degrees in theta wrt cave           
                   /// idsa . thetay = 90.0; // align y`-axis 90 degrees in theta wrt cave           
                   idsa . phiy = 90.0; // align y`-axis  0 degrees in phi   wrt cave           
                   /// idsa . phiy = 90.0; // align y`-axis  0 degrees in phi   wrt cave           
                   idsa . thetaz =  0.0; // align z`-axis  0 degrees in theta wrt cave           
                   /// idsa . thetaz =  0.0; // align z`-axis  0 degrees in theta wrt cave           
                   idsa . phiz =  0.0; // align z`-axis  0 degrees in phi   wrt cave           
                   /// idsa . phiz =  0.0; // align z`-axis  0 degrees in phi   wrt cave           
                   //           
                   idsa.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup idsa_doc        
             ///@{           
                   ++idsa._index;           
                   idsa . version = 2.0; // Default alignment of IDSM at (0,0,0) with no rotation           
                   /// idsa . version = 2.0; // Default alignment of IDSM at (0,0,0) with no rotation           
                   idsa . x = 0.0; // x-alignment           
                   /// idsa . x = 0.0; // x-alignment           
                   idsa . y = 0.0; // y-alignment           
                   /// idsa . y = 0.0; // y-alignment           
                   idsa . z = 0.0; // z-alignment           
                   /// idsa . z = 0.0; // z-alignment           
                   idsa . thetax = 90.0; // align x`-axis 90 degrees in theta wrt cave           
                   /// idsa . thetax = 90.0; // align x`-axis 90 degrees in theta wrt cave           
                   idsa . phix =  0.0; // align x`-axis  0 degrees in phi   wrt cave           
                   /// idsa . phix =  0.0; // align x`-axis  0 degrees in phi   wrt cave           
                   idsa . thetay = 90.0; // align y`-axis 90 degrees in theta wrt cave           
                   /// idsa . thetay = 90.0; // align y`-axis 90 degrees in theta wrt cave           
                   idsa . phiy = 90.0; // align y`-axis  0 degrees in phi   wrt cave           
                   /// idsa . phiy = 90.0; // align y`-axis  0 degrees in phi   wrt cave           
                   idsa . thetaz =  0.0; // align z`-axis  0 degrees in theta wrt cave           
                   /// idsa . thetaz =  0.0; // align z`-axis  0 degrees in theta wrt cave           
                   idsa . phiz =  0.0; // align z`-axis  0 degrees in phi   wrt cave           
                   /// idsa . phiz =  0.0; // align z`-axis  0 degrees in phi   wrt cave           
                   //           
                   idsa.fill();           
             ///@}        
             //        
             /// Component O	a=16	z=8	w=0.062        
             /// Component C	a=12	z=6	w=0.892        
             /// Component H	a=1	z=1	w=0.019        
             /// Component Cl	a=35.5	z=17	w=0.027        
             /// Mixture CFRPMix dens=1.78        
             {  AgMaterial &mix = AgMaterial::Get("Cfrpmix");           
                   mix.Component("O",16,8,0.062);           
                   mix.Component("C",12,6,0.892);           
                   mix.Component("H",1,1,0.019);           
                   mix.Component("Cl",35.5,17,0.027);           
                   mix.par("dens")=1.78;           
                   mix.lock();           
                   _material = mix;           
                   _material.lock();           
             }        
             /// Component AL	a=27	z=13	w=1.        
             /// Mixture AlPure dens=2.80        
             {  AgMaterial &mix = AgMaterial::Get("Alpure");           
                   mix.Component("AL",27,13,1.);           
                   mix.par("dens")=2.80;           
                   mix.lock();           
                   _material = mix;           
                   _material.lock();           
             }        
             /// Component AL	a=27	z=13	w=0.747        
             /// Component Cu	a=63.5	z=29	w=0.012        
             /// Component Fe	a=55.8	z=26	w=0.056        
             /// Component Cr	a=52.0	z=24	w=0.014        
             /// Component Ni	a=58.7	z=28	w=0.008        
             /// Component Zn	a=65.4	z=30	w=0.045        
             /// Component Ti	a=47.9	z=22	w=0.094        
             /// Component V	a=50.9	z=23	w=0.004        
             /// Component Mg	a=24.3	z=12	w=0.020        
             /// Mixture SUCMix dens=3.4        
             {  AgMaterial &mix = AgMaterial::Get("Sucmix");           
                   mix.Component("AL",27,13,0.747);           
                   mix.Component("Cu",63.5,29,0.012);           
                   mix.Component("Fe",55.8,26,0.056);           
                   mix.Component("Cr",52.0,24,0.014);           
                   mix.Component("Ni",58.7,28,0.008);           
                   mix.Component("Zn",65.4,30,0.045);           
                   mix.Component("Ti",47.9,22,0.094);           
                   mix.Component("V",50.9,23,0.004);           
                   mix.Component("Mg",24.3,12,0.020);           
                   mix.par("dens")=3.4;           
                   mix.lock();           
                   _material = mix;           
                   _material.lock();           
             }        
             /// Component AL	a=27	z=13	w=2.        
             /// Component O	a=16	z=8	w=3.        
             /// Mixture Alumina dens=3.90        
             {  AgMaterial &mix = AgMaterial::Get("Alumina");           
                   mix.Component("AL",27,13,2.);           
                   mix.Component("O",16,8,3.);           
                   mix.par("dens")=3.90;           
                   mix.lock();           
                   _material = mix;           
                   _material.lock();           
             }        
             /// Component Si	a=28.08	z=14	w=0.6*1*28./60.        
             /// Component O	a=16	z=8	w=0.6*2*16./60.        
             /// Component C	a=12	z=6	w=0.4*8*12./174.        
             /// Component H	a=1	z=1	w=0.4*14*1./174.        
             /// Component O	a=16	z=8	w=0.4*4*16./174.        
             /// Mixture FR4 dens=1.80        
             {  AgMaterial &mix = AgMaterial::Get("Fr4");           
                   mix.Component("Si",28.08,14,0.6*1*28./60.);           
                   mix.Component("O",16,8,0.6*2*16./60.);           
                   mix.Component("C",12,6,0.4*8*12./174.);           
                   mix.Component("H",1,1,0.4*14*1./174.);           
                   mix.Component("O",16,8,0.4*4*16./174.);           
                   mix.par("dens")=1.80;           
                   mix.lock();           
                   _material = mix;           
                   _material.lock();           
             }        
             /// Component CU	a=63.54	z=29	w=0.586        
             /// Component C	a=12.01	z=6	w=0.259        
             /// Component O	a=15.999	z=8	w=0.138        
             /// Component H	a=1.00794	z=1	w=0.017        
             /// Mixture badFgtCables dens=2.68        
             {  AgMaterial &mix = AgMaterial::Get("Badfgtcables");           
                   mix.Component("CU",63.54,29,0.586);           
                   mix.Component("C",12.01,6,0.259);           
                   mix.Component("O",15.999,8,0.138);           
                   mix.Component("H",1.00794,1,0.017);           
                   mix.par("dens")=2.68;           
                   mix.lock();           
                   _material = mix;           
                   _material.lock();           
             }        
             /// USE idsg version=1.0;        
             idsg.Use("version",(Float_t)1.0);        
             /// USE idsa version=1.0;        
             idsa.Use("version",(Float_t)1.0);        
             inr     = idsg.rf;        
             outr    = idsg.rrres + idsg.r2res;        
             lengthz = 460.;        
             sina = sin( idsg.angflat * degrad );        
             cosa = cos( idsg.angflat * degrad );        
             _create = AgCreate("IDSM");        
             {           
                   AgShape myshape; // undefined shape           
                   ///Create IDSM           
                   Create("IDSM");            
             }        
             { AgPlacement place = AgPlacement("IDSM","CAVE");           
                   /// Add daughter volume IDSM to mother CAVE           
                   place.TranslateX(idsa.x);           
                   place.TranslateX(idsa.y);           
                   place.TranslateX(idsa.z);           
                   /// G3 Reference: thetax = idsa.thetax           
                   /// G3 Reference: phix = idsa.phix           
                   /// G3 Reference: thetay = idsa.thetay           
                   /// G3 Reference: phiy = idsa.phiy           
                   /// G3 Reference: thetaz = idsa.thetaz           
                   /// G3 Reference: phiz = idsa.phiz           
                   Double_t _thetax=idsa.thetax,_phix=idsa.phix,_thetay=idsa.thetay,_phiy=idsa.phiy,_thetaz=idsa.thetaz,_phiz=idsa.phiz;           
                   place.Reference( _thetax, _phix, _thetay, _phiy, _thetaz, _phiz );           
                   _stacker -> Position( AgBlock::Find("IDSM"), place );           
             } // end placement of IDSM        
       }; // IdsmGeo1     
 }; // namespace IdsmGeo1  
 
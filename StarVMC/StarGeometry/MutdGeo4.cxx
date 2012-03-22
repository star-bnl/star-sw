#include "MutdGeo4.h"  
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
 namespace MUTDGEO4 // $NMSPC  
 {     
       //     
       // ---------------------------------------------------------------------------------------------------     
          //  -----------------------------------------------------     
          /// @defgroup mtdg_doc     
          /// \class Mtdg_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t version;     
          ///Int_t config;     
          ///Array_t<Float_t> blconfig;     
          ///Int_t _index;     
          //     
          Mtdg_t mtdg;     
          //     
          //  -----------------------------------------------------     
          /// @defgroup mtdd_doc     
          /// \class Mtdd_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t rmin;     
          ///Float_t rmax;     
          ///Float_t dzmother;     
          ///Float_t backlegr;     
          ///Float_t bemcelectboxdx;     
          ///Float_t bemcelectboxdy;     
          ///Float_t bemcelectboxdz3;     
          ///Float_t bemcelectboxdz5;     
          ///Float_t rgap;     
          ///Float_t mtdmotherdx;     
          ///Float_t mtdmotherdy;     
          ///Float_t mtdmotherdz3;     
          ///Float_t mtdmotherdz5;     
          ///Float_t mtdtrayz1;     
          ///Float_t mtdtrayz2;     
          ///Float_t mtdtraydx;     
          ///Float_t mtdtraydy;     
          ///Float_t mtdtraydz;     
          ///Float_t mtdchannel;     
          ///Float_t mtdbplatedx;     
          ///Float_t mtdtplatedx;     
          ///Float_t mtdigstackdx;     
          ///Float_t mtdigstackdy;     
          ///Float_t mtdigstackdz;     
          ///Float_t mtdigngap;     
          ///Float_t mtdigglassdx;     
          ///Float_t mtdiggasgapdx;     
          ///Float_t mtdogglassdx;     
          ///Float_t mtdogglassdy;     
          ///Float_t mtdogglassdz;     
          ///Float_t mtdpcbdx;     
          ///Float_t mtdpcbdy;     
          ///Float_t mtdpcbdz;     
          ///Float_t mtdnomexdx;     
          ///Float_t mtdnomexdy;     
          ///Float_t mtdnomexdz;     
          ///Int_t _index;     
          //     
          Mtdd_t mtdd;     
          //     
          ///@addtogroup MutdGeo4_vars     
          ///@{        
                Int_t ktrayconfig,iphi,ntrayplaced;        
                //        
                /// Int_t ktrayconfig,iphi,ntrayplaced        
          ///@}     
          ///@addtogroup MutdGeo4_vars     
          ///@{        
                Float_t thisphi,thisx,thisxb;        
                //        
                /// Float_t thisphi,thisx,thisxb        
          ///@}     
          ///@addtogroup MutdGeo4_vars     
          ///@{        
                Float_t innerglassstackhalfthickness;        
                //        
                /// Float_t innerglassstackhalfthickness        
          ///@}     
          ///@addtogroup MutdGeo4_vars     
          ///@{        
                Int_t kngaps,kphi;        
                //        
                /// Int_t kngaps,kphi        
          ///@}     
       MutdGeo4::MutdGeo4()     
         : AgModule("MutdGeo4"," is the geometry of the STAR MTD, WMRPC Version ")     
       {        
       }     
          // ---------------------------------------------------------------------------------------------------     
          void MUTD::Block( AgCreate create )     
          {         
                ///@addtogroup MUTD_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("MUTD");              
                            attr.par("seen")=0;              
                            attr.par("colo")=1;              
                            attr.par("serial")=ktrayconfig;              
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
                            shape.par("rmin")=mtdd.rmin;              
                            shape.par("rmax")=mtdd.rmax;              
                            shape.par("dz")=mtdd.dzmother;              
                            /// Shape Tube rmin=mtdd.rmin rmax=mtdd.rmax dz=mtdd.dzmother               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_MUTD;              
                            _stacker -> Build(this);              
                      }           
                      ntrayplaced     = 0;           
                      if ( ktrayconfig==4 )           
                      {              
                            thisphi = 150;              
                            kphi    =   1;              
                            _create = AgCreate("MTMF");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create MTMF                 
                                  Create("MTMF");                  
                            }              
                            { AgPlacement place = AgPlacement("MTMF","MUTD");                 
                                  /// Add daughter volume MTMF to mother MUTD                 
                                  place.TranslateZ(0);                 
                                  /// Translate z = 0                 
                                  place.AlphaZ(thisphi);                 
                                  /// Rotate: AlphaZ = thisphi                 
                                  /// G3 Reference: thetax = 90                 
                                  /// G3 Reference: phix = 0                 
                                  /// G3 Reference: thetay = 90                 
                                  /// G3 Reference: phiy = 90                 
                                  /// G3 Reference: thetaz = 0                 
                                  /// G3 Reference: phiz = 0                 
                                  _stacker -> Position( AgBlock::Find("MTMF"), place );                 
                            } // end placement of MTMF              
                            ntrayplaced = ntrayplaced + 3;              
                      }           
                      else if ( ktrayconfig==5 )           
                      {              
                            /// Loop on iphi from 1 to 30 step=1              
                            for ( iphi=1; (1>0)? (iphi<=30):(iphi>=30); iphi+=1 )              
                            {                 
                                  kphi = iphi                             ;// this is the serialized parameter...;                 
                                  thisphi = -18.0 + (iphi-1)*12.0;                 
                                  _create = AgCreate("MTMF");                 
                                  {                    
                                        AgShape myshape; // undefined shape                    
                                        ///Create MTMF                    
                                        Create("MTMF");                     
                                  }                 
                                  { AgPlacement place = AgPlacement("MTMF","MUTD");                    
                                        /// Add daughter volume MTMF to mother MUTD                    
                                        place.TranslateZ(0);                    
                                        /// Translate z = 0                    
                                        place.AlphaZ(thisphi);                    
                                        /// Rotate: AlphaZ = thisphi                    
                                        /// G3 Reference: thetax = 90                    
                                        /// G3 Reference: phix = 0                    
                                        /// G3 Reference: thetay = 90                    
                                        /// G3 Reference: phiy = 90                    
                                        /// G3 Reference: thetaz = 0                    
                                        /// G3 Reference: phiz = 0                    
                                        _stacker -> Position( AgBlock::Find("MTMF"), place );                    
                                  } // end placement of MTMF                 
                                  ntrayplaced = ntrayplaced + nint(mtdg.blconfig(iphi));                 
                            }              
                      }           
                      END_OF_MUTD:           
                      mCurrent = _save;           
                ///@}        
          } // End Block MUTD     
          // ---------------------------------------------------------------------------------------------------     
          void MTMF::Block( AgCreate create )     
          {         
                ///@addtogroup MTMF_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("MTMF");              
                            attr.par("seen")=1;              
                            attr.par("colo")=6;              
                            attr.par("serial")=kphi;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tubs");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("phi1")=-6.0;              
                            shape.par("phi2")=6.0;              
                            /// Shape Tubs phi1=-6.0 phi2=6.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_MTMF;              
                            _stacker -> Build(this);              
                      }           
                      if ( nint(mtdg.blconfig(kphi))>0 )           
                      {              
                            if ( nint(mtdg.blconfig(kphi))==5 )              
                            {                 
                                  _create = AgCreate("MTLB");                 
                                  {                    
                                        AgShape myshape; // undefined shape                    
                                        ///Create MTLB                    
                                        Create("MTLB");                     
                                  }                 
                                  { AgPlacement place = AgPlacement("MTLB","MTMF");                    
                                        /// Add daughter volume MTLB to mother MTMF                    
                                        place.TranslateX(mtdd.backlegr+mtdd.bemcelectboxdx);                    
                                        /// Translate x = mtdd.backlegr+mtdd.bemcelectboxdx                    
                                        place.TranslateY(0);                    
                                        /// Translate y = 0                    
                                        place.TranslateZ(0);                    
                                        /// Translate z = 0                    
                                        _stacker -> Position( AgBlock::Find("MTLB"), place );                    
                                  } // end placement of MTLB                 
                            }              
                            else if ( nint(mtdg.blconfig(kphi))==3 )              
                            {                 
                                  _create = AgCreate("MTSB");                 
                                  {                    
                                        AgShape myshape; // undefined shape                    
                                        ///Create MTSB                    
                                        Create("MTSB");                     
                                  }                 
                                  { AgPlacement place = AgPlacement("MTSB","MTMF");                    
                                        /// Add daughter volume MTSB to mother MTMF                    
                                        place.TranslateX(mtdd.backlegr+mtdd.bemcelectboxdx);                    
                                        /// Translate x = mtdd.backlegr+mtdd.bemcelectboxdx                    
                                        place.TranslateY(0);                    
                                        /// Translate y = 0                    
                                        place.TranslateZ(0);                    
                                        /// Translate z = 0                    
                                        _stacker -> Position( AgBlock::Find("MTSB"), place );                    
                                  } // end placement of MTSB                 
                            }              
                      }           
                      thisx      = mtdd.backlegr + 2.*(mtdd.bemcelectboxdx + mtdd.rgap)                          + mtdd.mtdmotherdx;           
                      _create = AgCreate("MMBL");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create MMBL              
                            Create("MMBL");               
                      }           
                      { AgPlacement place = AgPlacement("MMBL","MTMF");              
                            /// Add daughter volume MMBL to mother MTMF              
                            place.TranslateX(thisx);              
                            /// Translate x = thisx              
                            _stacker -> Position( AgBlock::Find("MMBL"), place );              
                      } // end placement of MMBL           
                      END_OF_MTMF:           
                      mCurrent = _save;           
                ///@}        
          } // End Block MTMF     
          // ---------------------------------------------------------------------------------------------------     
          void MMBL::Block( AgCreate create )     
          {         
                ///@addtogroup MMBL_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("MMBL");              
                            attr.par("seen")=1;              
                            attr.par("colo")=3;              
                            attr.par("serial")=kphi;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=mtdd.mtdmotherdx;              
                            shape.par("dy")=mtdd.mtdmotherdy;              
                            shape.par("dz")=mtdd.mtdmotherdz5;              
                            /// Shape Bbox dx=mtdd.mtdmotherdx dy=mtdd.mtdmotherdy dz=mtdd.mtdmotherdz5               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_MMBL;              
                            _stacker -> Build(this);              
                      }           
                      if ( ktrayconfig==4 )           
                      {              
                            _create = AgCreate("MTRF");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create MTRF                 
                                  Create("MTRF");                  
                            }              
                            { AgPlacement place = AgPlacement("MTRF","MMBL");                 
                                  /// Add daughter volume MTRF to mother MMBL                 
                                  place.TranslateX(-mtdd.mtdtraydx-0.1);                 
                                  /// Translate x = -mtdd.mtdtraydx-0.1                 
                                  place.TranslateZ(-mtdd.mtdtrayz1);                 
                                  /// Translate z = -mtdd.mtdtrayz1                 
                                  _stacker -> Position( AgBlock::Find("MTRF"), place );                 
                            } // end placement of MTRF              
                            _create = AgCreate("MTRF");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create MTRF                 
                                  Create("MTRF");                  
                            }              
                            { AgPlacement place = AgPlacement("MTRF","MMBL");                 
                                  /// Add daughter volume MTRF to mother MMBL                 
                                  place.TranslateX(mtdd.mtdtraydx+0.1);                 
                                  /// Translate x = mtdd.mtdtraydx+0.1                 
                                  place.TranslateZ(0.0);                 
                                  /// Translate z = 0.0                 
                                  _stacker -> Position( AgBlock::Find("MTRF"), place );                 
                            } // end placement of MTRF              
                            _create = AgCreate("MTRF");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create MTRF                 
                                  Create("MTRF");                  
                            }              
                            { AgPlacement place = AgPlacement("MTRF","MMBL");                 
                                  /// Add daughter volume MTRF to mother MMBL                 
                                  place.TranslateX(-mtdd.mtdtraydx-0.1);                 
                                  /// Translate x = -mtdd.mtdtraydx-0.1                 
                                  place.TranslateZ(mtdd.mtdtrayz1);                 
                                  /// Translate z = mtdd.mtdtrayz1                 
                                  _stacker -> Position( AgBlock::Find("MTRF"), place );                 
                            } // end placement of MTRF              
                      }           
                      else if ( ktrayconfig==5 )           
                      {              
                            if ( nint(mtdg.blconfig(kphi))==5 )              
                            {                 
                                  _create = AgCreate("MTRF");                 
                                  {                    
                                        AgShape myshape; // undefined shape                    
                                        ///Create MTRF                    
                                        Create("MTRF");                     
                                  }                 
                                  { AgPlacement place = AgPlacement("MTRF","MMBL");                    
                                        /// Add daughter volume MTRF to mother MMBL                    
                                        place.TranslateX(-mtdd.mtdtraydx-0.1);                    
                                        /// Translate x = -mtdd.mtdtraydx-0.1                    
                                        place.TranslateZ(-mtdd.mtdtrayz2);                    
                                        /// Translate z = -mtdd.mtdtrayz2                    
                                        _stacker -> Position( AgBlock::Find("MTRF"), place );                    
                                  } // end placement of MTRF                 
                                  _create = AgCreate("MTRF");                 
                                  {                    
                                        AgShape myshape; // undefined shape                    
                                        ///Create MTRF                    
                                        Create("MTRF");                     
                                  }                 
                                  { AgPlacement place = AgPlacement("MTRF","MMBL");                    
                                        /// Add daughter volume MTRF to mother MMBL                    
                                        place.TranslateX(mtdd.mtdtraydx+0.1);                    
                                        /// Translate x = mtdd.mtdtraydx+0.1                    
                                        place.TranslateZ(-mtdd.mtdtrayz1);                    
                                        /// Translate z = -mtdd.mtdtrayz1                    
                                        _stacker -> Position( AgBlock::Find("MTRF"), place );                    
                                  } // end placement of MTRF                 
                                  _create = AgCreate("MTRF");                 
                                  {                    
                                        AgShape myshape; // undefined shape                    
                                        ///Create MTRF                    
                                        Create("MTRF");                     
                                  }                 
                                  { AgPlacement place = AgPlacement("MTRF","MMBL");                    
                                        /// Add daughter volume MTRF to mother MMBL                    
                                        place.TranslateX(-mtdd.mtdtraydx-0.1);                    
                                        /// Translate x = -mtdd.mtdtraydx-0.1                    
                                        place.TranslateZ(0.0);                    
                                        /// Translate z = 0.0                    
                                        _stacker -> Position( AgBlock::Find("MTRF"), place );                    
                                  } // end placement of MTRF                 
                                  _create = AgCreate("MTRF");                 
                                  {                    
                                        AgShape myshape; // undefined shape                    
                                        ///Create MTRF                    
                                        Create("MTRF");                     
                                  }                 
                                  { AgPlacement place = AgPlacement("MTRF","MMBL");                    
                                        /// Add daughter volume MTRF to mother MMBL                    
                                        place.TranslateX(mtdd.mtdtraydx+0.1);                    
                                        /// Translate x = mtdd.mtdtraydx+0.1                    
                                        place.TranslateZ(mtdd.mtdtrayz1);                    
                                        /// Translate z = mtdd.mtdtrayz1                    
                                        _stacker -> Position( AgBlock::Find("MTRF"), place );                    
                                  } // end placement of MTRF                 
                                  _create = AgCreate("MTRF");                 
                                  {                    
                                        AgShape myshape; // undefined shape                    
                                        ///Create MTRF                    
                                        Create("MTRF");                     
                                  }                 
                                  { AgPlacement place = AgPlacement("MTRF","MMBL");                    
                                        /// Add daughter volume MTRF to mother MMBL                    
                                        place.TranslateX(-mtdd.mtdtraydx-0.1);                    
                                        /// Translate x = -mtdd.mtdtraydx-0.1                    
                                        place.TranslateZ(mtdd.mtdtrayz2);                    
                                        /// Translate z = mtdd.mtdtrayz2                    
                                        _stacker -> Position( AgBlock::Find("MTRF"), place );                    
                                  } // end placement of MTRF                 
                            }              
                            else if ( nint(mtdg.blconfig(kphi))==3 )              
                            {                 
                                  _create = AgCreate("MTRF");                 
                                  {                    
                                        AgShape myshape; // undefined shape                    
                                        ///Create MTRF                    
                                        Create("MTRF");                     
                                  }                 
                                  { AgPlacement place = AgPlacement("MTRF","MMBL");                    
                                        /// Add daughter volume MTRF to mother MMBL                    
                                        place.TranslateX(-mtdd.mtdtraydx-0.1);                    
                                        /// Translate x = -mtdd.mtdtraydx-0.1                    
                                        place.TranslateZ(-mtdd.mtdtrayz1);                    
                                        /// Translate z = -mtdd.mtdtrayz1                    
                                        _stacker -> Position( AgBlock::Find("MTRF"), place );                    
                                  } // end placement of MTRF                 
                                  _create = AgCreate("MTRF");                 
                                  {                    
                                        AgShape myshape; // undefined shape                    
                                        ///Create MTRF                    
                                        Create("MTRF");                     
                                  }                 
                                  { AgPlacement place = AgPlacement("MTRF","MMBL");                    
                                        /// Add daughter volume MTRF to mother MMBL                    
                                        place.TranslateX(mtdd.mtdtraydx+0.1);                    
                                        /// Translate x = mtdd.mtdtraydx+0.1                    
                                        place.TranslateZ(0.0);                    
                                        /// Translate z = 0.0                    
                                        _stacker -> Position( AgBlock::Find("MTRF"), place );                    
                                  } // end placement of MTRF                 
                                  _create = AgCreate("MTRF");                 
                                  {                    
                                        AgShape myshape; // undefined shape                    
                                        ///Create MTRF                    
                                        Create("MTRF");                     
                                  }                 
                                  { AgPlacement place = AgPlacement("MTRF","MMBL");                    
                                        /// Add daughter volume MTRF to mother MMBL                    
                                        place.TranslateX(-mtdd.mtdtraydx-0.1);                    
                                        /// Translate x = -mtdd.mtdtraydx-0.1                    
                                        place.TranslateZ(mtdd.mtdtrayz1);                    
                                        /// Translate z = mtdd.mtdtrayz1                    
                                        _stacker -> Position( AgBlock::Find("MTRF"), place );                    
                                  } // end placement of MTRF                 
                            }              
                      }           
                      END_OF_MMBL:           
                      mCurrent = _save;           
                ///@}        
          } // End Block MMBL     
          // ---------------------------------------------------------------------------------------------------     
          void MTRF::Block( AgCreate create )     
          {         
                ///@addtogroup MTRF_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material RPCgas            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Rpcgas");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("MTRF");              
                            attr.par("seen")=1;              
                            attr.par("colo")=6;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=mtdd.mtdtraydx;              
                            shape.par("dy")=mtdd.mtdtraydy;              
                            shape.par("dz")=mtdd.mtdtraydz;              
                            /// Shape Bbox dx=mtdd.mtdtraydx dy=mtdd.mtdtraydy dz=mtdd.mtdtraydz               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_MTRF;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("MTBP");           
                      { // Paramters passed in via the Create operatir              
                            AgCreate create("MTBP");              
                            create.par("dz")=mtdd.mtdtraydz;              
                            create.par("dx")=mtdd.mtdbplatedx;              
                            create.par("dy")=mtdd.mtdtraydy;              
                            _create = create;              
                      }           
                      {              
                            AgShape myshape; // undefined shape              
                            /// Set shape par: dz = mtdd.mtdtraydz              
                            myshape.par("dz")=mtdd.mtdtraydz;              
                            /// Set shape par: dx = mtdd.mtdbplatedx              
                            myshape.par("dx")=mtdd.mtdbplatedx;              
                            /// Set shape par: dy = mtdd.mtdtraydy              
                            myshape.par("dy")=mtdd.mtdtraydy;              
                            ///Create MTBP              
                            Create("MTBP");               
                      }           
                      { AgPlacement place = AgPlacement("MTBP","MTRF");              
                            /// Add daughter volume MTBP to mother MTRF              
                            place.TranslateX(-mtdd.mtdtraydx+mtdd.mtdbplatedx);              
                            /// Translate x = -mtdd.mtdtraydx+mtdd.mtdbplatedx              
                            _stacker -> Position( AgBlock::Find("MTBP"), place );              
                      } // end placement of MTBP           
                      _create = AgCreate("MTTP");           
                      { // Paramters passed in via the Create operatir              
                            AgCreate create("MTTP");              
                            create.par("dz")=mtdd.mtdtraydz;              
                            create.par("dx")=mtdd.mtdtplatedx;              
                            create.par("dy")=mtdd.mtdtraydy;              
                            _create = create;              
                      }           
                      {              
                            AgShape myshape; // undefined shape              
                            /// Set shape par: dz = mtdd.mtdtraydz              
                            myshape.par("dz")=mtdd.mtdtraydz;              
                            /// Set shape par: dx = mtdd.mtdtplatedx              
                            myshape.par("dx")=mtdd.mtdtplatedx;              
                            /// Set shape par: dy = mtdd.mtdtraydy              
                            myshape.par("dy")=mtdd.mtdtraydy;              
                            ///Create MTTP              
                            Create("MTTP");               
                      }           
                      { AgPlacement place = AgPlacement("MTTP","MTRF");              
                            /// Add daughter volume MTTP to mother MTRF              
                            place.TranslateX(mtdd.mtdtraydx-mtdd.mtdtplatedx);              
                            /// Translate x = mtdd.mtdtraydx-mtdd.mtdtplatedx              
                            _stacker -> Position( AgBlock::Find("MTTP"), place );              
                      } // end placement of MTTP           
                      _create = AgCreate("MLCH");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create MLCH              
                            Create("MLCH");               
                      }           
                      { AgPlacement place = AgPlacement("MLCH","MTRF");              
                            /// Add daughter volume MLCH to mother MTRF              
                            place.TranslateX(-mtdd.mtdtraydx+2.*mtdd.mtdbplatedx+mtdd.mtdchannel);              
                            /// Translate x = -mtdd.mtdtraydx+2.*mtdd.mtdbplatedx+mtdd.mtdchannel              
                            place.TranslateY(mtdd.mtdtraydy-mtdd.mtdchannel);              
                            /// Translate y = mtdd.mtdtraydy-mtdd.mtdchannel              
                            place.TranslateZ(0);              
                            /// Translate z = 0              
                            _stacker -> Position( AgBlock::Find("MLCH"), place );              
                      } // end placement of MLCH           
                      _create = AgCreate("MLCH");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create MLCH              
                            Create("MLCH");               
                      }           
                      { AgPlacement place = AgPlacement("MLCH","MTRF");              
                            /// Add daughter volume MLCH to mother MTRF              
                            place.TranslateX(-mtdd.mtdtraydx+2.*mtdd.mtdbplatedx+mtdd.mtdchannel);              
                            /// Translate x = -mtdd.mtdtraydx+2.*mtdd.mtdbplatedx+mtdd.mtdchannel              
                            place.TranslateY(-mtdd.mtdtraydy+mtdd.mtdchannel);              
                            /// Translate y = -mtdd.mtdtraydy+mtdd.mtdchannel              
                            place.TranslateZ(0);              
                            /// Translate z = 0              
                            place.AlphaZ(180);              
                            /// Rotate: AlphaZ = 180              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            _stacker -> Position( AgBlock::Find("MLCH"), place );              
                      } // end placement of MLCH           
                      _create = AgCreate("MSCH");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create MSCH              
                            Create("MSCH");               
                      }           
                      { AgPlacement place = AgPlacement("MSCH","MTRF");              
                            /// Add daughter volume MSCH to mother MTRF              
                            place.TranslateX(-mtdd.mtdtraydx+2.*mtdd.mtdbplatedx+mtdd.mtdchannel);              
                            /// Translate x = -mtdd.mtdtraydx+2.*mtdd.mtdbplatedx+mtdd.mtdchannel              
                            place.TranslateY(0);              
                            /// Translate y = 0              
                            place.TranslateZ(mtdd.mtdtraydz-mtdd.mtdchannel);              
                            /// Translate z = mtdd.mtdtraydz-mtdd.mtdchannel              
                            _stacker -> Position( AgBlock::Find("MSCH"), place );              
                      } // end placement of MSCH           
                      _create = AgCreate("MSCH");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create MSCH              
                            Create("MSCH");               
                      }           
                      { AgPlacement place = AgPlacement("MSCH","MTRF");              
                            /// Add daughter volume MSCH to mother MTRF              
                            place.TranslateX(-mtdd.mtdtraydx+2.*mtdd.mtdbplatedx+mtdd.mtdchannel);              
                            /// Translate x = -mtdd.mtdtraydx+2.*mtdd.mtdbplatedx+mtdd.mtdchannel              
                            place.TranslateY(0);              
                            /// Translate y = 0              
                            place.TranslateZ(-mtdd.mtdtraydz+mtdd.mtdchannel);              
                            /// Translate z = -mtdd.mtdtraydz+mtdd.mtdchannel              
                            place.AlphaY(180);              
                            /// Rotate: AlphaY = 180              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            _stacker -> Position( AgBlock::Find("MSCH"), place );              
                      } // end placement of MSCH           
                      _create = AgCreate("MOGL");           
                      { // Paramters passed in via the Create operatir              
                            AgCreate create("MOGL");              
                            create.par("dz")=mtdd.mtdogglassdz;              
                            create.par("dx")=mtdd.mtdogglassdx;              
                            create.par("dy")=mtdd.mtdogglassdy;              
                            _create = create;              
                      }           
                      {              
                            AgShape myshape; // undefined shape              
                            /// Set shape par: dz = mtdd.mtdogglassdz              
                            myshape.par("dz")=mtdd.mtdogglassdz;              
                            /// Set shape par: dx = mtdd.mtdogglassdx              
                            myshape.par("dx")=mtdd.mtdogglassdx;              
                            /// Set shape par: dy = mtdd.mtdogglassdy              
                            myshape.par("dy")=mtdd.mtdogglassdy;              
                            ///Create MOGL              
                            Create("MOGL");               
                      }           
                      { AgPlacement place = AgPlacement("MOGL","MTRF");              
                            /// Add daughter volume MOGL to mother MTRF              
                            place.TranslateX(-innerglassstackhalfthickness-mtdd.mtdogglassdx);              
                            /// Translate x = -innerglassstackhalfthickness-mtdd.mtdogglassdx              
                            place.TranslateY(0);              
                            /// Translate y = 0              
                            place.TranslateZ(0);              
                            /// Translate z = 0              
                            _stacker -> Position( AgBlock::Find("MOGL"), place );              
                      } // end placement of MOGL           
                      { AgPlacement place = AgPlacement("MOGL","MTRF");              
                            /// Add daughter volume MOGL to mother MTRF              
                            place.TranslateX(innerglassstackhalfthickness+mtdd.mtdogglassdx);              
                            /// Translate x = innerglassstackhalfthickness+mtdd.mtdogglassdx              
                            place.TranslateY(0);              
                            /// Translate y = 0              
                            place.TranslateZ(0);              
                            /// Translate z = 0              
                            _stacker -> Position( AgBlock::Find("MOGL"), place );              
                      } // end placement of MOGL           
                      thisxb = innerglassstackhalfthickness+2.*mtdd.mtdogglassdx;           
                      _create = AgCreate("MPCB");           
                      { // Paramters passed in via the Create operatir              
                            AgCreate create("MPCB");              
                            create.par("dz")=mtdd.mtdpcbdz;              
                            create.par("dx")=mtdd.mtdpcbdx;              
                            create.par("dy")=mtdd.mtdpcbdy;              
                            _create = create;              
                      }           
                      {              
                            AgShape myshape; // undefined shape              
                            /// Set shape par: dz = mtdd.mtdpcbdz              
                            myshape.par("dz")=mtdd.mtdpcbdz;              
                            /// Set shape par: dx = mtdd.mtdpcbdx              
                            myshape.par("dx")=mtdd.mtdpcbdx;              
                            /// Set shape par: dy = mtdd.mtdpcbdy              
                            myshape.par("dy")=mtdd.mtdpcbdy;              
                            ///Create MPCB              
                            Create("MPCB");               
                      }           
                      { AgPlacement place = AgPlacement("MPCB","MTRF");              
                            /// Add daughter volume MPCB to mother MTRF              
                            place.TranslateX(-thisxb-mtdd.mtdpcbdx);              
                            /// Translate x = -thisxb-mtdd.mtdpcbdx              
                            place.TranslateY(0);              
                            /// Translate y = 0              
                            place.TranslateZ(0);              
                            /// Translate z = 0              
                            _stacker -> Position( AgBlock::Find("MPCB"), place );              
                      } // end placement of MPCB           
                      { AgPlacement place = AgPlacement("MPCB","MTRF");              
                            /// Add daughter volume MPCB to mother MTRF              
                            place.TranslateX(thisxb+mtdd.mtdpcbdx);              
                            /// Translate x = thisxb+mtdd.mtdpcbdx              
                            place.TranslateY(0);              
                            /// Translate y = 0              
                            place.TranslateZ(0);              
                            /// Translate z = 0              
                            _stacker -> Position( AgBlock::Find("MPCB"), place );              
                      } // end placement of MPCB           
                      thisxb = thisxb+2.*mtdd.mtdpcbdx;           
                      _create = AgCreate("MNOM");           
                      { // Paramters passed in via the Create operatir              
                            AgCreate create("MNOM");              
                            create.par("dz")=mtdd.mtdnomexdz;              
                            create.par("dx")=mtdd.mtdnomexdx;              
                            create.par("dy")=mtdd.mtdnomexdy;              
                            _create = create;              
                      }           
                      {              
                            AgShape myshape; // undefined shape              
                            /// Set shape par: dz = mtdd.mtdnomexdz              
                            myshape.par("dz")=mtdd.mtdnomexdz;              
                            /// Set shape par: dx = mtdd.mtdnomexdx              
                            myshape.par("dx")=mtdd.mtdnomexdx;              
                            /// Set shape par: dy = mtdd.mtdnomexdy              
                            myshape.par("dy")=mtdd.mtdnomexdy;              
                            ///Create MNOM              
                            Create("MNOM");               
                      }           
                      { AgPlacement place = AgPlacement("MNOM","MTRF");              
                            /// Add daughter volume MNOM to mother MTRF              
                            place.TranslateX(-thisxb-mtdd.mtdnomexdx);              
                            /// Translate x = -thisxb-mtdd.mtdnomexdx              
                            place.TranslateY(0);              
                            /// Translate y = 0              
                            place.TranslateZ(0);              
                            /// Translate z = 0              
                            _stacker -> Position( AgBlock::Find("MNOM"), place );              
                      } // end placement of MNOM           
                      { AgPlacement place = AgPlacement("MNOM","MTRF");              
                            /// Add daughter volume MNOM to mother MTRF              
                            place.TranslateX(thisxb+mtdd.mtdnomexdx);              
                            /// Translate x = thisxb+mtdd.mtdnomexdx              
                            place.TranslateY(0);              
                            /// Translate y = 0              
                            place.TranslateZ(0);              
                            /// Translate z = 0              
                            _stacker -> Position( AgBlock::Find("MNOM"), place );              
                      } // end placement of MNOM           
                      thisxb = thisxb+2.*mtdd.mtdnomexdx;           
                      _create = AgCreate("MIGF");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create MIGF              
                            Create("MIGF");               
                      }           
                      { AgPlacement place = AgPlacement("MIGF","MTRF");              
                            /// Add daughter volume MIGF to mother MTRF              
                            place.TranslateX(0);              
                            /// Translate x = 0              
                            place.TranslateY(0);              
                            /// Translate y = 0              
                            place.TranslateZ(0);              
                            /// Translate z = 0              
                            _stacker -> Position( AgBlock::Find("MIGF"), place );              
                      } // end placement of MIGF           
                      END_OF_MTRF:           
                      mCurrent = _save;           
                ///@}        
          } // End Block MTRF     
          // ---------------------------------------------------------------------------------------------------     
          void MIGF::Block( AgCreate create )     
          {         
                ///@addtogroup MIGF_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("MIGF");              
                            attr.par("seen")=1;              
                            attr.par("colo")=6;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material Glass            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Glass");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=innerglassstackhalfthickness;              
                            shape.par("dy")=mtdd.mtdigstackdy;              
                            shape.par("dz")=mtdd.mtdigstackdz;              
                            /// Shape Bbox dx=innerglassstackhalfthickness dy=mtdd.mtdigstackdy dz=mtdd.mtdigstackdz               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_MIGF;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("MGAP");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create MGAP              
                            Create("MGAP");               
                      }           
                      if ( kngaps==5 )           
                      {              
                            { AgPlacement place = AgPlacement("MGAP","MIGF");                 
                                  /// Add daughter volume MGAP to mother MIGF                 
                                  place.TranslateX(4.*mtdd.mtdigglassdx+4.*mtdd.mtdiggasgapdx);                 
                                  /// Translate x = 4.*mtdd.mtdigglassdx+4.*mtdd.mtdiggasgapdx                 
                                  _stacker -> Position( AgBlock::Find("MGAP"), place );                 
                            } // end placement of MGAP              
                            { AgPlacement place = AgPlacement("MGAP","MIGF");                 
                                  /// Add daughter volume MGAP to mother MIGF                 
                                  place.TranslateX(2.*mtdd.mtdigglassdx+2.*mtdd.mtdiggasgapdx);                 
                                  /// Translate x = 2.*mtdd.mtdigglassdx+2.*mtdd.mtdiggasgapdx                 
                                  _stacker -> Position( AgBlock::Find("MGAP"), place );                 
                            } // end placement of MGAP              
                            { AgPlacement place = AgPlacement("MGAP","MIGF");                 
                                  /// Add daughter volume MGAP to mother MIGF                 
                                  place.TranslateX(0.0);                 
                                  /// Translate x = 0.0                 
                                  _stacker -> Position( AgBlock::Find("MGAP"), place );                 
                            } // end placement of MGAP              
                            { AgPlacement place = AgPlacement("MGAP","MIGF");                 
                                  /// Add daughter volume MGAP to mother MIGF                 
                                  place.TranslateX(-2.*mtdd.mtdigglassdx-2.*mtdd.mtdiggasgapdx);                 
                                  /// Translate x = -2.*mtdd.mtdigglassdx-2.*mtdd.mtdiggasgapdx                 
                                  _stacker -> Position( AgBlock::Find("MGAP"), place );                 
                            } // end placement of MGAP              
                            { AgPlacement place = AgPlacement("MGAP","MIGF");                 
                                  /// Add daughter volume MGAP to mother MIGF                 
                                  place.TranslateX(-4.*mtdd.mtdigglassdx-4.*mtdd.mtdiggasgapdx);                 
                                  /// Translate x = -4.*mtdd.mtdigglassdx-4.*mtdd.mtdiggasgapdx                 
                                  _stacker -> Position( AgBlock::Find("MGAP"), place );                 
                            } // end placement of MGAP              
                      }           
                      else if ( kngaps==6 )           
                      {              
                            { AgPlacement place = AgPlacement("MGAP","MIGF");                 
                                  /// Add daughter volume MGAP to mother MIGF                 
                                  place.TranslateX(5.*mtdd.mtdigglassdx+5.*mtdd.mtdiggasgapdx);                 
                                  /// Translate x = 5.*mtdd.mtdigglassdx+5.*mtdd.mtdiggasgapdx                 
                                  _stacker -> Position( AgBlock::Find("MGAP"), place );                 
                            } // end placement of MGAP              
                            { AgPlacement place = AgPlacement("MGAP","MIGF");                 
                                  /// Add daughter volume MGAP to mother MIGF                 
                                  place.TranslateX(3.*mtdd.mtdigglassdx+3.*mtdd.mtdiggasgapdx);                 
                                  /// Translate x = 3.*mtdd.mtdigglassdx+3.*mtdd.mtdiggasgapdx                 
                                  _stacker -> Position( AgBlock::Find("MGAP"), place );                 
                            } // end placement of MGAP              
                            { AgPlacement place = AgPlacement("MGAP","MIGF");                 
                                  /// Add daughter volume MGAP to mother MIGF                 
                                  place.TranslateX(1.*mtdd.mtdigglassdx+1.*mtdd.mtdiggasgapdx);                 
                                  /// Translate x = 1.*mtdd.mtdigglassdx+1.*mtdd.mtdiggasgapdx                 
                                  _stacker -> Position( AgBlock::Find("MGAP"), place );                 
                            } // end placement of MGAP              
                            { AgPlacement place = AgPlacement("MGAP","MIGF");                 
                                  /// Add daughter volume MGAP to mother MIGF                 
                                  place.TranslateX(-1.*mtdd.mtdigglassdx-1.*mtdd.mtdiggasgapdx);                 
                                  /// Translate x = -1.*mtdd.mtdigglassdx-1.*mtdd.mtdiggasgapdx                 
                                  _stacker -> Position( AgBlock::Find("MGAP"), place );                 
                            } // end placement of MGAP              
                            { AgPlacement place = AgPlacement("MGAP","MIGF");                 
                                  /// Add daughter volume MGAP to mother MIGF                 
                                  place.TranslateX(-3.*mtdd.mtdigglassdx-3.*mtdd.mtdiggasgapdx);                 
                                  /// Translate x = -3.*mtdd.mtdigglassdx-3.*mtdd.mtdiggasgapdx                 
                                  _stacker -> Position( AgBlock::Find("MGAP"), place );                 
                            } // end placement of MGAP              
                            { AgPlacement place = AgPlacement("MGAP","MIGF");                 
                                  /// Add daughter volume MGAP to mother MIGF                 
                                  place.TranslateX(-5.*mtdd.mtdigglassdx-5.*mtdd.mtdiggasgapdx);                 
                                  /// Translate x = -5.*mtdd.mtdigglassdx-5.*mtdd.mtdiggasgapdx                 
                                  _stacker -> Position( AgBlock::Find("MGAP"), place );                 
                            } // end placement of MGAP              
                      }           
                      END_OF_MIGF:           
                      mCurrent = _save;           
                ///@}        
          } // End Block MIGF     
          // ---------------------------------------------------------------------------------------------------     
          void MGAP::Block( AgCreate create )     
          {         
                ///@addtogroup MGAP_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material RPCgas            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Rpcgas");              
                            _material = mat;              
                      }           
                      /// Medium sensitive           
                      ///  isvol = 1            
                      {  AgMedium &med = AgMedium::Get("Sensitive");              
                               med.Inherit(this);              
                            med.par("isvol")=1 ;              
                            _medium = med;              
                      }           
                      { AgAttribute attr = AgAttribute("MGAP");              
                            attr.par("seen")=1;              
                            attr.par("colo")=7;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=mtdd.mtdiggasgapdx;              
                            /// Shape Bbox dx=mtdd.mtdiggasgapdx               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_MGAP;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_MGAP:           
                      mCurrent = _save;           
                ///@}        
          } // End Block MGAP     
          // ---------------------------------------------------------------------------------------------------     
          void MTLB::Block( AgCreate create )     
          {         
                ///@addtogroup MTLB_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("MTLB");              
                            attr.par("seen")=1;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=mtdd.bemcelectboxdx;              
                            shape.par("dy")=mtdd.bemcelectboxdy;              
                            shape.par("dz")=mtdd.bemcelectboxdz5;              
                            /// Shape Bbox dx=mtdd.bemcelectboxdx dy=mtdd.bemcelectboxdy dz=mtdd.bemcelectboxdz5               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_MTLB;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_MTLB:           
                      mCurrent = _save;           
                ///@}        
          } // End Block MTLB     
          // ---------------------------------------------------------------------------------------------------     
          void MTSB::Block( AgCreate create )     
          {         
                ///@addtogroup MTSB_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("MTSB");              
                            attr.par("seen")=1;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=mtdd.bemcelectboxdx;              
                            shape.par("dy")=mtdd.bemcelectboxdy;              
                            shape.par("dz")=mtdd.bemcelectboxdz3;              
                            /// Shape Bbox dx=mtdd.bemcelectboxdx dy=mtdd.bemcelectboxdy dz=mtdd.bemcelectboxdz3               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_MTSB;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_MTSB:           
                      mCurrent = _save;           
                ///@}        
          } // End Block MTSB     
          // ---------------------------------------------------------------------------------------------------     
          void MTBP::Block( AgCreate create )     
          {         
                ///@addtogroup MTBP_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("MTBP");              
                            attr.par("seen")=1;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material Aluminium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Aluminium");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            /// Shape Bbox               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_MTBP;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_MTBP:           
                      mCurrent = _save;           
                ///@}        
          } // End Block MTBP     
          // ---------------------------------------------------------------------------------------------------     
          void MTTP::Block( AgCreate create )     
          {         
                ///@addtogroup MTTP_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("MTTP");              
                            attr.par("seen")=1;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material Aluminium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Aluminium");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            /// Shape Bbox               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_MTTP;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_MTTP:           
                      mCurrent = _save;           
                ///@}        
          } // End Block MTTP     
          // ---------------------------------------------------------------------------------------------------     
          void MLCH::Block( AgCreate create )     
          {         
                ///@addtogroup MLCH_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("MLCH");              
                            attr.par("seen")=1;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material Aluminium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Aluminium");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=mtdd.mtdchannel;              
                            shape.par("dy")=mtdd.mtdchannel;              
                            shape.par("dz")=mtdd.mtdtraydz-2.*mtdd.mtdchannel;              
                            /// Shape Bbox dx=mtdd.mtdchannel dy=mtdd.mtdchannel dz=mtdd.mtdtraydz-2.*mtdd.mtdchannel               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_MLCH;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("MLAI");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create MLAI              
                            Create("MLAI");               
                      }           
                      { AgPlacement place = AgPlacement("MLAI","MLCH");              
                            /// Add daughter volume MLAI to mother MLCH              
                            place.TranslateX(0);              
                            /// Translate x = 0              
                            place.TranslateY(0.3175/2.);              
                            /// Translate y = 0.3175/2.              
                            place.TranslateZ(0);              
                            /// Translate z = 0              
                            _stacker -> Position( AgBlock::Find("MLAI"), place );              
                      } // end placement of MLAI           
                      END_OF_MLCH:           
                      mCurrent = _save;           
                ///@}        
          } // End Block MLCH     
          // ---------------------------------------------------------------------------------------------------     
          void MLAI::Block( AgCreate create )     
          {         
                ///@addtogroup MLAI_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("MLAI");              
                            attr.par("seen")=1;              
                            attr.par("colo")=7;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=mtdd.mtdchannel-0.3175;              
                            shape.par("dy")=mtdd.mtdchannel-0.3175/2.;              
                            shape.par("dz")=mtdd.mtdtraydz-2.*mtdd.mtdchannel;              
                            /// Shape Bbox dx=mtdd.mtdchannel-0.3175 dy=mtdd.mtdchannel-0.3175/2. dz=mtdd.mtdtraydz-2.*mtdd.mtdchannel               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_MLAI;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_MLAI:           
                      mCurrent = _save;           
                ///@}        
          } // End Block MLAI     
          // ---------------------------------------------------------------------------------------------------     
          void MSCH::Block( AgCreate create )     
          {         
                ///@addtogroup MSCH_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("MSCH");              
                            attr.par("seen")=1;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material Aluminium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Aluminium");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=mtdd.mtdchannel;              
                            shape.par("dy")=mtdd.mtdtraydy;              
                            shape.par("dz")=mtdd.mtdchannel;              
                            /// Shape Bbox dx=mtdd.mtdchannel dy=mtdd.mtdtraydy dz=mtdd.mtdchannel               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_MSCH;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("MSAI");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create MSAI              
                            Create("MSAI");               
                      }           
                      { AgPlacement place = AgPlacement("MSAI","MSCH");              
                            /// Add daughter volume MSAI to mother MSCH              
                            place.TranslateX(0);              
                            /// Translate x = 0              
                            place.TranslateY(0);              
                            /// Translate y = 0              
                            place.TranslateZ(+0.3175/2.);              
                            /// Translate z = +0.3175/2.              
                            _stacker -> Position( AgBlock::Find("MSAI"), place );              
                      } // end placement of MSAI           
                      END_OF_MSCH:           
                      mCurrent = _save;           
                ///@}        
          } // End Block MSCH     
          // ---------------------------------------------------------------------------------------------------     
          void MSAI::Block( AgCreate create )     
          {         
                ///@addtogroup MSAI_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("MSAI");              
                            attr.par("seen")=1;              
                            attr.par("colo")=7;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=mtdd.mtdchannel-0.3175;              
                            shape.par("dy")=mtdd.mtdtraydy;              
                            shape.par("dz")=mtdd.mtdchannel-0.3175/2.;              
                            /// Shape Bbox dx=mtdd.mtdchannel-0.3175 dy=mtdd.mtdtraydy dz=mtdd.mtdchannel-0.3175/2.               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_MSAI;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_MSAI:           
                      mCurrent = _save;           
                ///@}        
          } // End Block MSAI     
          // ---------------------------------------------------------------------------------------------------     
          void MOGL::Block( AgCreate create )     
          {         
                ///@addtogroup MOGL_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Glass            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Glass");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("MOGL");              
                            attr.par("seen")=1;              
                            attr.par("colo")=4;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            /// Shape Bbox               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_MOGL;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_MOGL:           
                      mCurrent = _save;           
                ///@}        
          } // End Block MOGL     
          // ---------------------------------------------------------------------------------------------------     
          void MPCB::Block( AgCreate create )     
          {         
                ///@addtogroup MPCB_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("MPCB");              
                            attr.par("seen")=1;              
                            attr.par("colo")=3;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material G10            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("G10");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            /// Shape Bbox               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_MPCB;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_MPCB:           
                      mCurrent = _save;           
                ///@}        
          } // End Block MPCB     
          // ---------------------------------------------------------------------------------------------------     
          void MNOM::Block( AgCreate create )     
          {         
                ///@addtogroup MNOM_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Nomex            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Nomex");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("MNOM");              
                            attr.par("seen")=1;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            /// Shape Bbox               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_MNOM;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_MNOM:           
                      mCurrent = _save;           
                ///@}        
          } // End Block MNOM     
    // ----------------------------------------------------------------------- geoctr
       void MutdGeo4::ConstructGeometry( const Char_t *dummy )     
       {        
             ///@addtogroup MutdGeo4_revision        
             ///@{           
                   /// Author: W.J. Llope           
             ///@}        
             ///@addtogroup MutdGeo4_revision        
             ///@{           
                   /// Created:    21 July 2010            
             ///@}        
             AddBlock("MUTD");        
             AddBlock("MTMF");        
             AddBlock("MTLB");        
             AddBlock("MTSB");        
             AddBlock("MMBL");        
             AddBlock("MTRF");        
             AddBlock("MTBP");        
             AddBlock("MTTP");        
             AddBlock("MLCH");        
             AddBlock("MSCH");        
             AddBlock("MLAI");        
             AddBlock("MSAI");        
             AddBlock("MIGF");        
             AddBlock("MGAP");        
             AddBlock("MOGL");        
             AddBlock("MPCB");        
             AddBlock("MNOM");        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup mtdg_doc        
             ///@{           
                   ++mtdg._index;           
                   mtdg . version = 1; //  version number            
                   /// mtdg . version = 1; //  version number            
                   mtdg . config = 1; //  =4 single backleg (Run11), =5 full system (Run15)            
                   /// mtdg . config = 1; //  =4 single backleg (Run11), =5 full system (Run15)            
                   mtdg . blconfig.at(0) = 5; //  Ntrays per backleg configuration...            
                   ///mtdg . blconfig.at(0) = 5; //  Ntrays per backleg configuration...            
                   mtdg . blconfig.at(1) = 0; //  Ntrays per backleg configuration...            
                   ///mtdg . blconfig.at(1) = 0; //  Ntrays per backleg configuration...            
                   mtdg . blconfig.at(2) = 5; //  Ntrays per backleg configuration...            
                   ///mtdg . blconfig.at(2) = 5; //  Ntrays per backleg configuration...            
                   mtdg . blconfig.at(3) = 5; //  Ntrays per backleg configuration...            
                   ///mtdg . blconfig.at(3) = 5; //  Ntrays per backleg configuration...            
                   mtdg . blconfig.at(4) = 5; //  Ntrays per backleg configuration...            
                   ///mtdg . blconfig.at(4) = 5; //  Ntrays per backleg configuration...            
                   mtdg . blconfig.at(5) = 5; //  Ntrays per backleg configuration...            
                   ///mtdg . blconfig.at(5) = 5; //  Ntrays per backleg configuration...            
                   mtdg . blconfig.at(6) = 5; //  Ntrays per backleg configuration...            
                   ///mtdg . blconfig.at(6) = 5; //  Ntrays per backleg configuration...            
                   mtdg . blconfig.at(7) = 5; //  Ntrays per backleg configuration...            
                   ///mtdg . blconfig.at(7) = 5; //  Ntrays per backleg configuration...            
                   mtdg . blconfig.at(8) = 5; //  Ntrays per backleg configuration...            
                   ///mtdg . blconfig.at(8) = 5; //  Ntrays per backleg configuration...            
                   mtdg . blconfig.at(9) = 5; //  Ntrays per backleg configuration...            
                   ///mtdg . blconfig.at(9) = 5; //  Ntrays per backleg configuration...            
                   mtdg . blconfig.at(10) = 5; //  Ntrays per backleg configuration...            
                   ///mtdg . blconfig.at(10) = 5; //  Ntrays per backleg configuration...            
                   mtdg . blconfig.at(11) = 5; //  Ntrays per backleg configuration...            
                   ///mtdg . blconfig.at(11) = 5; //  Ntrays per backleg configuration...            
                   mtdg . blconfig.at(12) = 5; //  Ntrays per backleg configuration...            
                   ///mtdg . blconfig.at(12) = 5; //  Ntrays per backleg configuration...            
                   mtdg . blconfig.at(13) = 5; //  Ntrays per backleg configuration...            
                   ///mtdg . blconfig.at(13) = 5; //  Ntrays per backleg configuration...            
                   mtdg . blconfig.at(14) = 5; //  Ntrays per backleg configuration...            
                   ///mtdg . blconfig.at(14) = 5; //  Ntrays per backleg configuration...            
                   mtdg . blconfig.at(15) = 5; //  Ntrays per backleg configuration...            
                   ///mtdg . blconfig.at(15) = 5; //  Ntrays per backleg configuration...            
                   mtdg . blconfig.at(16) = 5; //  Ntrays per backleg configuration...            
                   ///mtdg . blconfig.at(16) = 5; //  Ntrays per backleg configuration...            
                   mtdg . blconfig.at(17) = 0; //  Ntrays per backleg configuration...            
                   ///mtdg . blconfig.at(17) = 0; //  Ntrays per backleg configuration...            
                   mtdg . blconfig.at(18) = 5; //  Ntrays per backleg configuration...            
                   ///mtdg . blconfig.at(18) = 5; //  Ntrays per backleg configuration...            
                   mtdg . blconfig.at(19) = 3; //  Ntrays per backleg configuration...            
                   ///mtdg . blconfig.at(19) = 3; //  Ntrays per backleg configuration...            
                   mtdg . blconfig.at(20) = 3; //  Ntrays per backleg configuration...            
                   ///mtdg . blconfig.at(20) = 3; //  Ntrays per backleg configuration...            
                   mtdg . blconfig.at(21) = 3; //  Ntrays per backleg configuration...            
                   ///mtdg . blconfig.at(21) = 3; //  Ntrays per backleg configuration...            
                   mtdg . blconfig.at(22) = 3; //  Ntrays per backleg configuration...            
                   ///mtdg . blconfig.at(22) = 3; //  Ntrays per backleg configuration...            
                   mtdg . blconfig.at(23) = 3; //  Ntrays per backleg configuration...            
                   ///mtdg . blconfig.at(23) = 3; //  Ntrays per backleg configuration...            
                   mtdg . blconfig.at(24) = 3; //  Ntrays per backleg configuration...            
                   ///mtdg . blconfig.at(24) = 3; //  Ntrays per backleg configuration...            
                   mtdg . blconfig.at(25) = 3; //  Ntrays per backleg configuration...            
                   ///mtdg . blconfig.at(25) = 3; //  Ntrays per backleg configuration...            
                   mtdg . blconfig.at(26) = 3; //  Ntrays per backleg configuration...            
                   ///mtdg . blconfig.at(26) = 3; //  Ntrays per backleg configuration...            
                   mtdg . blconfig.at(27) = 3; //  Ntrays per backleg configuration...            
                   ///mtdg . blconfig.at(27) = 3; //  Ntrays per backleg configuration...            
                   mtdg . blconfig.at(28) = 3; //  Ntrays per backleg configuration...            
                   ///mtdg . blconfig.at(28) = 3; //  Ntrays per backleg configuration...            
                   mtdg . blconfig.at(29) = 3; //  Ntrays per backleg configuration...            
                   ///mtdg . blconfig.at(29) = 3; //  Ntrays per backleg configuration...            
                   //           
                   mtdg.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup mtdd_doc        
             ///@{           
                   ++mtdd._index;           
                   mtdd . rmin = 364.25; //  integration radius lower            
                   /// mtdd . rmin = 364.25; //  integration radius lower            
                   mtdd . rmax = 415.0; //  integration radius upper            
                   /// mtdd . rmax = 415.0; //  integration radius upper            
                   mtdd . dzmother = 300.0; //  integration half length in Z            
                   /// mtdd . dzmother = 300.0; //  integration half length in Z            
                   mtdd . backlegr = 365.0; //  radius of a backleg at phi-center            
                   /// mtdd . backlegr = 365.0; //  radius of a backleg at phi-center            
                   mtdd . bemcelectboxdx = 11.035; //  BEMC electronics box half-length X (height)            
                   /// mtdd . bemcelectboxdx = 11.035; //  BEMC electronics box half-length X (height)            
                   mtdd . bemcelectboxdy = 30.65; //  BEMC electronics box half-length Y (width)            
                   /// mtdd . bemcelectboxdy = 30.65; //  BEMC electronics box half-length Y (width)            
                   mtdd . bemcelectboxdz5 = 290.0; //  BEMC electronics box half-length Z (length long)            
                   /// mtdd . bemcelectboxdz5 = 290.0; //  BEMC electronics box half-length Z (length long)            
                   mtdd . bemcelectboxdz3 = 200.0; //  BEMC electronics box half-length Z (length short)            
                   /// mtdd . bemcelectboxdz3 = 200.0; //  BEMC electronics box half-length Z (length short)            
                   mtdd . rgap = 3.81; //  Distance top of Bemc box to underside of lowest tray            
                   /// mtdd . rgap = 3.81; //  Distance top of Bemc box to underside of lowest tray            
                   mtdd . mtdmotherdx = 5.4; //  Tray Mother half-length X (height)            
                   /// mtdd . mtdmotherdx = 5.4; //  Tray Mother half-length X (height)            
                   mtdd . mtdmotherdy = 35.0; //  Tray Mother half-length Y (width)            
                   /// mtdd . mtdmotherdy = 35.0; //  Tray Mother half-length Y (width)            
                   mtdd . mtdmotherdz5 = 285.0; //  Tray Mother half-length Z (length long)            
                   /// mtdd . mtdmotherdz5 = 285.0; //  Tray Mother half-length Z (length long)            
                   mtdd . mtdmotherdz3 = 195.0; //  Tray Mother half-length Z (length short)            
                   /// mtdd . mtdmotherdz3 = 195.0; //  Tray Mother half-length Z (length short)            
                   mtdd . mtdtrayz1 = 87.0; //  Tray offset in Z (to center), same as strip length...            
                   /// mtdd . mtdtrayz1 = 87.0; //  Tray offset in Z (to center), same as strip length...            
                   mtdd . mtdtrayz2 = 174.0; //  Tray offset in Z (to center), twice the strip length...            
                   /// mtdd . mtdtrayz2 = 174.0; //  Tray offset in Z (to center), twice the strip length...            
                   mtdd . mtdtraydx = 2.49555; //  Tray half-height (thickness=1.75in+0.125in+0.090in)            
                   /// mtdd . mtdtraydx = 2.49555; //  Tray half-height (thickness=1.75in+0.125in+0.090in)            
                   mtdd . mtdtraydy = 33.815; //  Tray half-width (width=26.625in) 33.815            
                   /// mtdd . mtdtraydy = 33.815; //  Tray half-width (width=26.625in) 33.815            
                   mtdd . mtdtraydz = 54.610; //  Tray half-length (width=43.00in) 54.610            
                   /// mtdd . mtdtraydz = 54.610; //  Tray half-length (width=43.00in) 54.610            
                   mtdd . mtdchannel = 2.2225; //  Architectural Channel (dim=1.75in)             
                   /// mtdd . mtdchannel = 2.2225; //  Architectural Channel (dim=1.75in)             
                   mtdd . mtdbplatedx = 0.15875; //  Tray bottom plate half-thickness (thickness=0.125in)            
                   /// mtdd . mtdbplatedx = 0.15875; //  Tray bottom plate half-thickness (thickness=0.125in)            
                   mtdd . mtdtplatedx = 0.1143; //  Tray top plate half-thickness (thickness=0.090in)            
                   /// mtdd . mtdtplatedx = 0.1143; //  Tray top plate half-thickness (thickness=0.090in)            
                   mtdd . mtdigstackdy = 26.1; //  WMRPC inner glass stack volume half-length Y (<-Phi)            
                   /// mtdd . mtdigstackdy = 26.1; //  WMRPC inner glass stack volume half-length Y (<-Phi)            
                   mtdd . mtdigstackdz = 43.5; //  WMRPC inner glass stack volume half-length Z (<-Z)            
                   /// mtdd . mtdigstackdz = 43.5; //  WMRPC inner glass stack volume half-length Z (<-Z)            
                   mtdd . mtdigngap = 5.0; //  WMRPC number of gas gaps...            
                   /// mtdd . mtdigngap = 5.0; //  WMRPC number of gas gaps...            
                   mtdd . mtdigglassdx = 0.035; //  WMRPC inner glass half-thickness (thickness=0.700mm)            
                   /// mtdd . mtdigglassdx = 0.035; //  WMRPC inner glass half-thickness (thickness=0.700mm)            
                   mtdd . mtdiggasgapdx = 0.0125; //  WMRPC gas gap half-thickness (thickness=0.250mm)            
                   /// mtdd . mtdiggasgapdx = 0.0125; //  WMRPC gas gap half-thickness (thickness=0.250mm)            
                   mtdd . mtdogglassdx = 0.055; //  WMRPC outer glass half-thickness (thickness=1.1mm)            
                   /// mtdd . mtdogglassdx = 0.055; //  WMRPC outer glass half-thickness (thickness=1.1mm)            
                   mtdd . mtdogglassdy = 27.1; //  WMRPC outer glass half-thickness            
                   /// mtdd . mtdogglassdy = 27.1; //  WMRPC outer glass half-thickness            
                   mtdd . mtdogglassdz = 44.5; //  WMRPC outer glass half-thickness            
                   /// mtdd . mtdogglassdz = 44.5; //  WMRPC outer glass half-thickness            
                   mtdd . mtdpcbdx = 0.045; //  WMRPC PCB half-thickness (thickness=0.9mm)            
                   /// mtdd . mtdpcbdx = 0.045; //  WMRPC PCB half-thickness (thickness=0.9mm)            
                   mtdd . mtdpcbdy = 29.0; //  WMRPC PCB half-thickness            
                   /// mtdd . mtdpcbdy = 29.0; //  WMRPC PCB half-thickness            
                   mtdd . mtdpcbdz = 45.75; //  WMRPC PCB half-thickness            
                   /// mtdd . mtdpcbdz = 45.75; //  WMRPC PCB half-thickness            
                   mtdd . mtdnomexdx = 0.5; //  WMRPC Nomex half-thickness (thickness=1cm)            
                   /// mtdd . mtdnomexdx = 0.5; //  WMRPC Nomex half-thickness (thickness=1cm)            
                   mtdd . mtdnomexdy = 28.0; //  WMRPC Nomex half-thickness (PCBdy-1cm)            
                   /// mtdd . mtdnomexdy = 28.0; //  WMRPC Nomex half-thickness (PCBdy-1cm)            
                   mtdd . mtdnomexdz = 44.75; //  WMRPC Nomex half-thickness (PCBdz-1cm)            
                   /// mtdd . mtdnomexdz = 44.75; //  WMRPC Nomex half-thickness (PCBdz-1cm)            
                   //           
                   mtdd.fill();           
             ///@}        
             //        
             /// USE mtdg _index=1;        
             mtdg.Use();        
             /// USE mtdd _index=1;        
             mtdd.Use();        
             ktrayconfig = mtdg.config;        
             kngaps = nint(mtdd.mtdigngap);        
             innerglassstackhalfthickness = mtdd.mtdigngap*mtdd.mtdiggasgapdx + (mtdd.mtdigngap-1.)*mtdd.mtdigglassdx;        
             /// Component H	a=1	z=1	w=0.90*2*1./102.+0.+0.05*10*1./58.        
             /// Component C	a=12	z=6	w=0.90*2*12./102.+0.+0.05*4*12./58.        
             /// Component F	a=19	z=9	w=0.90*4*19./102.+0.05*6*19./146.+0.        
             /// Component S	a=32	z=16	w=0.+0.05*1*32./146.+0.        
             /// Mixture RPCgas dens=4.55e-3        
             {  AgMaterial &mix = AgMaterial::Get("Rpcgas");           
                   mix.Component("H",1,1,0.90*2*1./102.+0.+0.05*10*1./58.);           
                   mix.Component("C",12,6,0.90*2*12./102.+0.+0.05*4*12./58.);           
                   mix.Component("F",19,9,0.90*4*19./102.+0.05*6*19./146.+0.);           
                   mix.Component("S",32,16,0.+0.05*1*32./146.+0.);           
                   mix.par("dens")=4.55e-3;           
                   mix.lock();           
                   _material = mix;           
                   _material.lock();           
             }        
             /// Component Si	a=28	z=14	w=1.        
             /// Component O	a=16	z=8	w=2.        
             /// Mixture Glass dens=2.5        
             {  AgMaterial &mix = AgMaterial::Get("Glass");           
                   mix.Component("Si",28,14,1.);           
                   mix.Component("O",16,8,2.);           
                   mix.par("dens")=2.5;           
                   mix.lock();           
                   _material = mix;           
                   _material.lock();           
             }        
             /// Component Si	a=28.08	z=14	w=0.6*1*28./60.        
             /// Component O	a=16	z=8	w=0.6*2*16./60. + 0.4*4*16./174.        
             /// Component C	a=12	z=6	w=0.4*8*12./174.        
             /// Component H	a=1	z=1	w=0.4*14*1./174.        
             /// Mixture G10 dens=1.7        
             {  AgMaterial &mix = AgMaterial::Get("G10");           
                   mix.Component("Si",28.08,14,0.6*1*28./60.);           
                   mix.Component("O",16,8,0.6*2*16./60. + 0.4*4*16./174.);           
                   mix.Component("C",12,6,0.4*8*12./174.);           
                   mix.Component("H",1,1,0.4*14*1./174.);           
                   mix.par("dens")=1.7;           
                   mix.lock();           
                   _material = mix;           
                   _material.lock();           
             }        
             /// Component Al	a=27	z=13	w=0.0105        
             /// Component N	a=14	z=7	w=0.7395        
             /// Component Adhesive	a=9	z=4.5	w=0.2500        
             /// Mixture Nomex dens=0.73        
             {  AgMaterial &mix = AgMaterial::Get("Nomex");           
                   mix.Component("Al",27,13,0.0105);           
                   mix.Component("N",14,7,0.7395);           
                   mix.Component("Adhesive",9,4.5,0.2500);           
                   mix.par("dens")=0.73;           
                   mix.lock();           
                   _material = mix;           
                   _material.lock();           
             }        
             /// Medium Cave_standard        
             {  AgMedium med = AgMedium::CopyMedium("Cave_standard");           
                   _medium = med;           
             }        
             _create = AgCreate("MUTD");        
             {           
                   AgShape myshape; // undefined shape           
                   ///Create MUTD           
                   Create("MUTD");            
             }        
             { AgPlacement place = AgPlacement("MUTD","CAVE");           
                   /// Add daughter volume MUTD to mother CAVE           
                   _stacker -> Position( AgBlock::Find("MUTD"), place );           
             } // end placement of MUTD        
       }; // MutdGeo4     
 }; // namespace MutdGeo4  
 
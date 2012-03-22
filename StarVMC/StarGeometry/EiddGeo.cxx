#include "EiddGeo.h"  
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
 namespace EIDDGEO // $NMSPC  
 {     
       //     
       // ---------------------------------------------------------------------------------------------------     
          //  -----------------------------------------------------     
          /// @defgroup eidv_doc     
          /// \class Eidv_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t version;     
          ///Float_t eidconfig;     
          ///Int_t _index;     
          //     
          Eidv_t eidv;     
          //     
          //  -----------------------------------------------------     
          /// @defgroup eidg_doc     
          /// \class Eidg_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t version;     
          ///Float_t ettz;     
          ///Float_t etrvz;     
          ///Float_t etfvz;     
          ///Float_t eclvz;     
          ///Float_t rmin;     
          ///Float_t rmax;     
          ///Float_t dphi;     
          ///Float_t ettthick;     
          ///Float_t tfvthick;     
          ///Float_t tofthick;     
          ///Float_t tbxthick;     
          ///Float_t tpbthick;     
          ///Float_t tglthick;     
          ///Float_t tgpthick;     
          ///Float_t elvthick;     
          ///Float_t eclthick;     
          ///Float_t eaathick;     
          ///Float_t eabthick;     
          ///Float_t scnthick;     
          ///Float_t trvthick;     
          ///Float_t trdthick;     
          ///Float_t trathick;     
          ///Float_t tabthick;     
          ///Float_t tbdthick;     
          ///Int_t _index;     
          //     
          Eidg_t eidg;     
          //     
          ///@addtogroup EiddGeo_vars     
          ///@{        
                float zpostof,zposbox,zposgas,zpospcb,zposgla,zposgap,zposeaa,zposeab,zposscin,zposecl;        
                //        
                /// float zpostof,zposbox,zposgas,zpospcb,zposgla,zposgap,zposeaa,zposeab,zposscin,zposecl        
          ///@}     
          ///@addtogroup EiddGeo_vars     
          ///@{        
                float zposett,zpostrv,zpostfv,zposelv,zpostrd,zposrad,zposabs,zposbod;        
                //        
                /// float zposett,zpostrv,zpostfv,zposelv,zpostrd,zposrad,zposabs,zposbod        
          ///@}     
          ///@addtogroup EiddGeo_vars     
          ///@{        
                float phi0;        
                //        
                /// float phi0        
          ///@}     
          ///@addtogroup EiddGeo_vars     
          ///@{        
                int i_phi0,i_pcb,i_glass,i_gap,i_trdlyr;        
                //        
                /// int i_phi0,i_pcb,i_glass,i_gap,i_trdlyr        
          ///@}     
       EiddGeo::EiddGeo()     
         : AgModule("EiddGeo"," is the ETTIE (Endcap Trd-Tof for Iding Electron) Detector of STAR ")     
       {        
       }     
          // ---------------------------------------------------------------------------------------------------     
          void ETTV::Block( AgCreate create )     
          {         
                ///@addtogroup ETTV_doc        
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
                      { AgAttribute attr = AgAttribute("ETTV");              
                            attr.par("seen")=1;              
                            attr.par("colo")=6;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=eidg.rmin;              
                            shape.par("rmax")=eidg.rmax;              
                            shape.par("dz")=eidg.ettthick/2;              
                            /// Shape Tube rmin=eidg.rmin rmax=eidg.rmax dz=eidg.ettthick/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_ETTV;              
                            _stacker -> Build(this);              
                      }           
                      zpostrv = eidg.etrvz - eidg.ettz;           
                      _create = AgCreate("ETRV");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create ETRV              
                            Create("ETRV");               
                      }           
                      { AgPlacement place = AgPlacement("ETRV","ETTV");              
                            /// Add daughter volume ETRV to mother ETTV              
                            place.TranslateZ(zpostrv);              
                            /// Translate z = zpostrv              
                            _stacker -> Position( AgBlock::Find("ETRV"), place );              
                      } // end placement of ETRV           
                      zpostfv = eidg.etfvz - eidg.ettz;           
                      _create = AgCreate("ETFV");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create ETFV              
                            Create("ETFV");               
                      }           
                      { AgPlacement place = AgPlacement("ETFV","ETTV");              
                            /// Add daughter volume ETFV to mother ETTV              
                            place.TranslateZ(zpostfv);              
                            /// Translate z = zpostfv              
                            _stacker -> Position( AgBlock::Find("ETFV"), place );              
                      } // end placement of ETFV           
                      zposelv = eidg.eclvz - eidg.ettz;           
                      _create = AgCreate("ECLV");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create ECLV              
                            Create("ECLV");               
                      }           
                      { AgPlacement place = AgPlacement("ECLV","ETTV");              
                            /// Add daughter volume ECLV to mother ETTV              
                            place.TranslateZ(zposelv);              
                            /// Translate z = zposelv              
                            _stacker -> Position( AgBlock::Find("ECLV"), place );              
                      } // end placement of ECLV           
                      END_OF_ETTV:           
                      mCurrent = _save;           
                ///@}        
          } // End Block ETTV     
          // ---------------------------------------------------------------------------------------------------     
          void ETRV::Block( AgCreate create )     
          {         
                ///@addtogroup ETRV_doc        
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
                      { AgAttribute attr = AgAttribute("ETRV");              
                            attr.par("seen")=1;              
                            attr.par("colo")=7;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=eidg.rmin;              
                            shape.par("rmax")=eidg.rmax;              
                            shape.par("dz")=eidg.trvthick/2;              
                            /// Shape Tube rmin=eidg.rmin rmax=eidg.rmax dz=eidg.trvthick/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_ETRV;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("ETRD");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create ETRD              
                            Create("ETRD");               
                      }           
                      /// Loop on i_trdlyr from 0 to 2 step=1           
                      for ( i_trdlyr=0; (1>0)? (i_trdlyr<=2):(i_trdlyr>=2); i_trdlyr+=1 )           
                      {              
                            zpostrd = (i_trdlyr-1)*eidg.trdthick;              
                            /// Loop on i_phi0 from 0 to 11 step=1              
                            for ( i_phi0=0; (1>0)? (i_phi0<=11):(i_phi0>=11); i_phi0+=1 )              
                            {                 
                                  phi0 = i_phi0*30;                 
                                  { AgPlacement place = AgPlacement("ETRD","ETRV");                    
                                        /// Add daughter volume ETRD to mother ETRV                    
                                        place.TranslateZ(zpostrd);                    
                                        /// Translate z = zpostrd                    
                                        /// G3 Reference: thetax = 90                    
                                        /// G3 Reference: phix = phi0                    
                                        /// G3 Reference: thetay = 90                    
                                        /// G3 Reference: phiy = phi0+90                    
                                        /// G3 Reference: thetaz = 0                    
                                        /// G3 Reference: phiz = 0                    
                                        Double_t _thetax=90,_phix=phi0,_thetay=90,_phiy=phi0+90,_thetaz=0,_phiz=0;                    
                                        place.Reference( _thetax, _phix, _thetay, _phiy, _thetaz, _phiz );                    
                                        _stacker -> Position( AgBlock::Find("ETRD"), place );                    
                                  } // end placement of ETRD                 
                            }              
                      }           
                      END_OF_ETRV:           
                      mCurrent = _save;           
                ///@}        
          } // End Block ETRV     
          // ---------------------------------------------------------------------------------------------------     
          void ECLV::Block( AgCreate create )     
          {         
                ///@addtogroup ECLV_doc        
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
                      { AgAttribute attr = AgAttribute("ECLV");              
                            attr.par("seen")=1;              
                            attr.par("colo")=8;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=eidg.rmin;              
                            shape.par("rmax")=eidg.rmax;              
                            shape.par("dz")=eidg.elvthick/2;              
                            /// Shape Tube rmin=eidg.rmin rmax=eidg.rmax dz=eidg.elvthick/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_ECLV;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("TECL");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create TECL              
                            Create("TECL");               
                      }           
                      zposecl = 0;           
                      /// Loop on i_phi0 from 0 to 11 step=1           
                      for ( i_phi0=0; (1>0)? (i_phi0<=11):(i_phi0>=11); i_phi0+=1 )           
                      {              
                            phi0 = i_phi0*30;              
                            { AgPlacement place = AgPlacement("TECL","ECLV");                 
                                  /// Add daughter volume TECL to mother ECLV                 
                                  place.TranslateZ(zposecl);                 
                                  /// Translate z = zposecl                 
                                  /// G3 Reference: thetax = 90                 
                                  /// G3 Reference: phix = phi0                 
                                  /// G3 Reference: thetay = 90                 
                                  /// G3 Reference: phiy = phi0+90                 
                                  /// G3 Reference: thetaz = 0                 
                                  /// G3 Reference: phiz = 0                 
                                  Double_t _thetax=90,_phix=phi0,_thetay=90,_phiy=phi0+90,_thetaz=0,_phiz=0;                 
                                  place.Reference( _thetax, _phix, _thetay, _phiy, _thetaz, _phiz );                 
                                  _stacker -> Position( AgBlock::Find("TECL"), place );                 
                            } // end placement of TECL              
                      }           
                      END_OF_ECLV:           
                      mCurrent = _save;           
                ///@}        
          } // End Block ECLV     
          // ---------------------------------------------------------------------------------------------------     
          void ETFV::Block( AgCreate create )     
          {         
                ///@addtogroup ETFV_doc        
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
                      { AgAttribute attr = AgAttribute("ETFV");              
                            attr.par("seen")=1;              
                            attr.par("colo")=6;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=eidg.rmin;              
                            shape.par("rmax")=eidg.rmax;              
                            shape.par("dz")=eidg.tfvthick/2;              
                            /// Shape Tube rmin=eidg.rmin rmax=eidg.rmax dz=eidg.tfvthick/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_ETFV;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("ETOF");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create ETOF              
                            Create("ETOF");               
                      }           
                      zpostof = 0;           
                      /// Loop on i_phi0 from 0 to 11 step=1           
                      for ( i_phi0=0; (1>0)? (i_phi0<=11):(i_phi0>=11); i_phi0+=1 )           
                      {              
                            phi0 = i_phi0*30;              
                            { AgPlacement place = AgPlacement("ETOF","ETFV");                 
                                  /// Add daughter volume ETOF to mother ETFV                 
                                  place.TranslateZ(zpostof);                 
                                  /// Translate z = zpostof                 
                                  /// G3 Reference: thetax = 90                 
                                  /// G3 Reference: phix = phi0                 
                                  /// G3 Reference: thetay = 90                 
                                  /// G3 Reference: phiy = phi0+90                 
                                  /// G3 Reference: thetaz = 0                 
                                  /// G3 Reference: phiz = 0                 
                                  Double_t _thetax=90,_phix=phi0,_thetay=90,_phiy=phi0+90,_thetaz=0,_phiz=0;                 
                                  place.Reference( _thetax, _phix, _thetay, _phiy, _thetaz, _phiz );                 
                                  _stacker -> Position( AgBlock::Find("ETOF"), place );                 
                            } // end placement of ETOF              
                      }           
                      END_OF_ETFV:           
                      mCurrent = _save;           
                ///@}        
          } // End Block ETFV     
          // ---------------------------------------------------------------------------------------------------     
          void ETOF::Block( AgCreate create )     
          {         
                ///@addtogroup ETOF_doc        
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
                      { AgAttribute attr = AgAttribute("ETOF");              
                            attr.par("seen")=1;              
                            attr.par("colo")=8;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tubs");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=eidg.rmin;              
                            shape.par("rmax")=eidg.rmax;              
                            shape.par("phi1")=-eidg.dphi/2;              
                            shape.par("phi2")=eidg.dphi/2;              
                            shape.par("dz")=eidg.tofthick/2;              
                            /// Shape Tubs rmin=eidg.rmin rmax=eidg.rmax phi1=-eidg.dphi/2 phi2=eidg.dphi/2 dz=eidg.tofthick/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_ETOF;              
                            _stacker -> Build(this);              
                      }           
                      zposbox = 0;           
                      _create = AgCreate("TBOX");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create TBOX              
                            Create("TBOX");               
                      }           
                      { AgPlacement place = AgPlacement("TBOX","ETOF");              
                            /// Add daughter volume TBOX to mother ETOF              
                            place.TranslateZ(zposbox);              
                            /// Translate z = zposbox              
                            _stacker -> Position( AgBlock::Find("TBOX"), place );              
                      } // end placement of TBOX           
                      END_OF_ETOF:           
                      mCurrent = _save;           
                ///@}        
          } // End Block ETOF     
          // ---------------------------------------------------------------------------------------------------     
          void TBOX::Block( AgCreate create )     
          {         
                ///@addtogroup TBOX_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Aluminium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Aluminium");              
                            _material = mat;              
                      }           
                      /// Medium Standard           
                      {  AgMedium med = AgMedium::CopyMedium("Standard");              
                            _medium = med;              
                      }           
                      { AgAttribute attr = AgAttribute("TBOX");              
                            attr.par("seen")=1;              
                            attr.par("colo")=3;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tubs");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=eidg.rmin;              
                            shape.par("rmax")=eidg.rmax;              
                            shape.par("phi1")=-eidg.dphi/2;              
                            shape.par("phi2")=eidg.dphi/2;              
                            shape.par("dz")=eidg.tofthick/2;              
                            /// Shape Tubs rmin=eidg.rmin rmax=eidg.rmax phi1=-eidg.dphi/2 phi2=eidg.dphi/2 dz=eidg.tofthick/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_TBOX;              
                            _stacker -> Build(this);              
                      }           
                      zposgas = 0;           
                      _create = AgCreate("TGAS");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create TGAS              
                            Create("TGAS");               
                      }           
                      { AgPlacement place = AgPlacement("TGAS","TBOX");              
                            /// Add daughter volume TGAS to mother TBOX              
                            place.TranslateZ(zposgas);              
                            /// Translate z = zposgas              
                            _stacker -> Position( AgBlock::Find("TGAS"), place );              
                      } // end placement of TGAS           
                      END_OF_TBOX:           
                      mCurrent = _save;           
                ///@}        
          } // End Block TBOX     
          // ---------------------------------------------------------------------------------------------------     
          void TGAS::Block( AgCreate create )     
          {         
                ///@addtogroup TGAS_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Freon            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Freon");              
                            _material = mat;              
                      }           
                      /// Medium Standard           
                      {  AgMedium med = AgMedium::CopyMedium("Standard");              
                            _medium = med;              
                      }           
                      { AgAttribute attr = AgAttribute("TGAS");              
                            attr.par("seen")=1;              
                            attr.par("colo")=4;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tubs");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=eidg.rmin;              
                            shape.par("rmax")=eidg.rmax;              
                            shape.par("phi1")=-eidg.dphi/2;              
                            shape.par("phi2")=eidg.dphi/2;              
                            shape.par("dz")=eidg.tofthick/2-eidg.tbxthick;              
                            /// Shape Tubs rmin=eidg.rmin rmax=eidg.rmax phi1=-eidg.dphi/2 phi2=eidg.dphi/2 dz=eidg.tofthick/2-eidg.tbxthick               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_TGAS;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("TPCB");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create TPCB              
                            Create("TPCB");               
                      }           
                      /// Loop on i_pcb from 0 to 1 step=1           
                      for ( i_pcb=0; (1>0)? (i_pcb<=1):(i_pcb>=1); i_pcb+=1 )           
                      {              
                            zpospcb = (i_pcb-0.5)*(eidg.tglthick*7+eidg.tgpthick*6+eidg.tpbthick);              
                            { AgPlacement place = AgPlacement("TPCB","TGAS");                 
                                  /// Add daughter volume TPCB to mother TGAS                 
                                  place.TranslateZ(zpospcb);                 
                                  /// Translate z = zpospcb                 
                                  _stacker -> Position( AgBlock::Find("TPCB"), place );                 
                            } // end placement of TPCB              
                      }           
                      _create = AgCreate("TGLA");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create TGLA              
                            Create("TGLA");               
                      }           
                      /// Loop on i_glass from 0 to 6 step=1           
                      for ( i_glass=0; (1>0)? (i_glass<=6):(i_glass>=6); i_glass+=1 )           
                      {              
                            zposgla = (eidg.tglthick+eidg.tgpthick)*(i_glass-3);              
                            { AgPlacement place = AgPlacement("TGLA","TGAS");                 
                                  /// Add daughter volume TGLA to mother TGAS                 
                                  place.TranslateZ(zposgla);                 
                                  /// Translate z = zposgla                 
                                  _stacker -> Position( AgBlock::Find("TGLA"), place );                 
                            } // end placement of TGLA              
                      }           
                      _create = AgCreate("TGAP");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create TGAP              
                            Create("TGAP");               
                      }           
                      /// Loop on i_gap from 0 to 5 step=1           
                      for ( i_gap=0; (1>0)? (i_gap<=5):(i_gap>=5); i_gap+=1 )           
                      {              
                            zposgap = (eidg.tglthick+eidg.tgpthick)*(i_glass-2.5);              
                            { AgPlacement place = AgPlacement("TGAP","TGAS");                 
                                  /// Add daughter volume TGAP to mother TGAS                 
                                  place.TranslateZ(zposgap);                 
                                  /// Translate z = zposgap                 
                                  _stacker -> Position( AgBlock::Find("TGAP"), place );                 
                            } // end placement of TGAP              
                      }           
                      END_OF_TGAS:           
                      mCurrent = _save;           
                ///@}        
          } // End Block TGAS     
          // ---------------------------------------------------------------------------------------------------     
          void TPCB::Block( AgCreate create )     
          {         
                ///@addtogroup TPCB_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material G10            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("G10");              
                            _material = mat;              
                      }           
                      /// Medium Standard           
                      {  AgMedium med = AgMedium::CopyMedium("Standard");              
                            _medium = med;              
                      }           
                      { AgAttribute attr = AgAttribute("TPCB");              
                            attr.par("seen")=1;              
                            attr.par("colo")=5;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tubs");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=eidg.rmin;              
                            shape.par("rmax")=eidg.rmax;              
                            shape.par("phi1")=-eidg.dphi/2;              
                            shape.par("phi2")=eidg.dphi/2;              
                            shape.par("dz")=eidg.tpbthick/2;              
                            /// Shape Tubs rmin=eidg.rmin rmax=eidg.rmax phi1=-eidg.dphi/2 phi2=eidg.dphi/2 dz=eidg.tpbthick/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_TPCB;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_TPCB:           
                      mCurrent = _save;           
                ///@}        
          } // End Block TPCB     
          // ---------------------------------------------------------------------------------------------------     
          void TGLA::Block( AgCreate create )     
          {         
                ///@addtogroup TGLA_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Glass            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Glass");              
                            _material = mat;              
                      }           
                      /// Medium Standard           
                      {  AgMedium med = AgMedium::CopyMedium("Standard");              
                            _medium = med;              
                      }           
                      { AgAttribute attr = AgAttribute("TGLA");              
                            attr.par("seen")=1;              
                            attr.par("colo")=6;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tubs");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=eidg.rmin;              
                            shape.par("rmax")=eidg.rmax;              
                            shape.par("phi1")=-eidg.dphi/2;              
                            shape.par("phi2")=eidg.dphi/2;              
                            shape.par("dz")=eidg.tglthick/2;              
                            /// Shape Tubs rmin=eidg.rmin rmax=eidg.rmax phi1=-eidg.dphi/2 phi2=eidg.dphi/2 dz=eidg.tglthick/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_TGLA;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_TGLA:           
                      mCurrent = _save;           
                ///@}        
          } // End Block TGLA     
          // ---------------------------------------------------------------------------------------------------     
          void TGAP::Block( AgCreate create )     
          {         
                ///@addtogroup TGAP_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Freon            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Freon");              
                            _material = mat;              
                      }           
                      /// Medium Sensitive           
                      ///  isvol = 1           
                      {  AgMedium &med = AgMedium::Get("Sensitive");              
                               med.Inherit(this);              
                            med.par("isvol")=1;              
                            _medium = med;              
                      }           
                      { AgAttribute attr = AgAttribute("TGAP");              
                            attr.par("seen")=1;              
                            attr.par("colo")=8;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tubs");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=eidg.rmin;              
                            shape.par("rmax")=eidg.rmax;              
                            shape.par("phi1")=-eidg.dphi/2;              
                            shape.par("phi2")=eidg.dphi/2;              
                            shape.par("dz")=eidg.tgpthick/2;              
                            /// Shape Tubs rmin=eidg.rmin rmax=eidg.rmax phi1=-eidg.dphi/2 phi2=eidg.dphi/2 dz=eidg.tgpthick/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_TGAP;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_TGAP:           
                      mCurrent = _save;           
                ///@}        
          } // End Block TGAP     
          // ---------------------------------------------------------------------------------------------------     
          void TECL::Block( AgCreate create )     
          {         
                ///@addtogroup TECL_doc        
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
                      { AgAttribute attr = AgAttribute("TECL");              
                            attr.par("seen")=1;              
                            attr.par("colo")=7;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tubs");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=eidg.rmin;              
                            shape.par("rmax")=eidg.rmax;              
                            shape.par("phi1")=-eidg.dphi/2;              
                            shape.par("phi2")=eidg.dphi/2;              
                            shape.par("dz")=eidg.eclthick/2;              
                            /// Shape Tubs rmin=eidg.rmin rmax=eidg.rmax phi1=-eidg.dphi/2 phi2=eidg.dphi/2 dz=eidg.eclthick/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_TECL;              
                            _stacker -> Build(this);              
                      }           
                      zposeaa = -eidg.eclthick/2+eidg.eaathick/2;           
                      _create = AgCreate("TEAA");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create TEAA              
                            Create("TEAA");               
                      }           
                      { AgPlacement place = AgPlacement("TEAA","TECL");              
                            /// Add daughter volume TEAA to mother TECL              
                            place.TranslateZ(zposeaa);              
                            /// Translate z = zposeaa              
                            _stacker -> Position( AgBlock::Find("TEAA"), place );              
                      } // end placement of TEAA           
                      zposeab = -eidg.eclthick/2+eidg.eaathick+eidg.scnthick+eidg.eabthick/2;           
                      _create = AgCreate("TEAB");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create TEAB              
                            Create("TEAB");               
                      }           
                      { AgPlacement place = AgPlacement("TEAB","TECL");              
                            /// Add daughter volume TEAB to mother TECL              
                            place.TranslateZ(zposeab);              
                            /// Translate z = zposeab              
                            _stacker -> Position( AgBlock::Find("TEAB"), place );              
                      } // end placement of TEAB           
                      _create = AgCreate("SCIN");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create SCIN              
                            Create("SCIN");               
                      }           
                      zposscin = -eidg.eclthick/2+eidg.eaathick+eidg.scnthick/2;           
                      { AgPlacement place = AgPlacement("SCIN","TECL");              
                            /// Add daughter volume SCIN to mother TECL              
                            place.TranslateZ(zposscin);              
                            /// Translate z = zposscin              
                            _stacker -> Position( AgBlock::Find("SCIN"), place );              
                      } // end placement of SCIN           
                      zposscin = eidg.eclthick/2-eidg.scnthick/2;           
                      { AgPlacement place = AgPlacement("SCIN","TECL");              
                            /// Add daughter volume SCIN to mother TECL              
                            place.TranslateZ(zposscin);              
                            /// Translate z = zposscin              
                            _stacker -> Position( AgBlock::Find("SCIN"), place );              
                      } // end placement of SCIN           
                      END_OF_TECL:           
                      mCurrent = _save;           
                ///@}        
          } // End Block TECL     
          // ---------------------------------------------------------------------------------------------------     
          void TEAA::Block( AgCreate create )     
          {         
                ///@addtogroup TEAA_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Lead            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Lead");              
                            _material = mat;              
                      }           
                      /// Medium Standard           
                      {  AgMedium med = AgMedium::CopyMedium("Standard");              
                            _medium = med;              
                      }           
                      { AgAttribute attr = AgAttribute("TEAA");              
                            attr.par("seen")=1;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tubs");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=eidg.rmin;              
                            shape.par("rmax")=eidg.rmax;              
                            shape.par("phi1")=-eidg.dphi/2;              
                            shape.par("phi2")=eidg.dphi/2;              
                            shape.par("dz")=eidg.eaathick/2;              
                            /// Shape Tubs rmin=eidg.rmin rmax=eidg.rmax phi1=-eidg.dphi/2 phi2=eidg.dphi/2 dz=eidg.eaathick/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_TEAA;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_TEAA:           
                      mCurrent = _save;           
                ///@}        
          } // End Block TEAA     
          // ---------------------------------------------------------------------------------------------------     
          void TEAB::Block( AgCreate create )     
          {         
                ///@addtogroup TEAB_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Lead            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Lead");              
                            _material = mat;              
                      }           
                      /// Medium Standard           
                      {  AgMedium med = AgMedium::CopyMedium("Standard");              
                            _medium = med;              
                      }           
                      { AgAttribute attr = AgAttribute("TEAB");              
                            attr.par("seen")=1;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tubs");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=eidg.rmin;              
                            shape.par("rmax")=eidg.rmax;              
                            shape.par("phi1")=-eidg.dphi/2;              
                            shape.par("phi2")=eidg.dphi/2;              
                            shape.par("dz")=eidg.eabthick/2;              
                            /// Shape Tubs rmin=eidg.rmin rmax=eidg.rmax phi1=-eidg.dphi/2 phi2=eidg.dphi/2 dz=eidg.eabthick/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_TEAB;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_TEAB:           
                      mCurrent = _save;           
                ///@}        
          } // End Block TEAB     
          // ---------------------------------------------------------------------------------------------------     
          void SCIN::Block( AgCreate create )     
          {         
                ///@addtogroup SCIN_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Polystyrene            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Polystyrene");              
                            _material = mat;              
                      }           
                      /// Medium Sensitive           
                      ///  isvol = 1           
                      {  AgMedium &med = AgMedium::Get("Sensitive");              
                               med.Inherit(this);              
                            med.par("isvol")=1;              
                            _medium = med;              
                      }           
                      { AgAttribute attr = AgAttribute("SCIN");              
                            attr.par("seen")=1;              
                            attr.par("colo")=8;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tubs");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=eidg.rmin;              
                            shape.par("rmax")=eidg.rmax;              
                            shape.par("phi1")=-eidg.dphi/2;              
                            shape.par("phi2")=eidg.dphi/2;              
                            shape.par("dz")=eidg.scnthick/2;              
                            /// Shape Tubs rmin=eidg.rmin rmax=eidg.rmax phi1=-eidg.dphi/2 phi2=eidg.dphi/2 dz=eidg.scnthick/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SCIN;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_SCIN:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SCIN     
          // ---------------------------------------------------------------------------------------------------     
          void ETRD::Block( AgCreate create )     
          {         
                ///@addtogroup ETRD_doc        
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
                      { AgAttribute attr = AgAttribute("ETRD");              
                            attr.par("seen")=1;              
                            attr.par("colo")=4;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tubs");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=eidg.rmin;              
                            shape.par("rmax")=eidg.rmax;              
                            shape.par("phi1")=-eidg.dphi/2;              
                            shape.par("phi2")=eidg.dphi/2;              
                            shape.par("dz")=eidg.trdthick/2;              
                            /// Shape Tubs rmin=eidg.rmin rmax=eidg.rmax phi1=-eidg.dphi/2 phi2=eidg.dphi/2 dz=eidg.trdthick/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_ETRD;              
                            _stacker -> Build(this);              
                      }           
                      zposrad = -eidg.trdthick/2+eidg.trathick/2;           
                      _create = AgCreate("TRAD");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create TRAD              
                            Create("TRAD");               
                      }           
                      { AgPlacement place = AgPlacement("TRAD","ETRD");              
                            /// Add daughter volume TRAD to mother ETRD              
                            place.TranslateZ(zposrad);              
                            /// Translate z = zposrad              
                            _stacker -> Position( AgBlock::Find("TRAD"), place );              
                      } // end placement of TRAD           
                      zposabs = -eidg.trdthick/2+eidg.trathick+eidg.tabthick/2;           
                      _create = AgCreate("TABS");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create TABS              
                            Create("TABS");               
                      }           
                      { AgPlacement place = AgPlacement("TABS","ETRD");              
                            /// Add daughter volume TABS to mother ETRD              
                            place.TranslateZ(zposabs);              
                            /// Translate z = zposabs              
                            _stacker -> Position( AgBlock::Find("TABS"), place );              
                      } // end placement of TABS           
                      zposbod = -eidg.trdthick/2+eidg.trathick+eidg.tabthick+eidg.tbdthick/2;           
                      _create = AgCreate("TBOD");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create TBOD              
                            Create("TBOD");               
                      }           
                      { AgPlacement place = AgPlacement("TBOD","ETRD");              
                            /// Add daughter volume TBOD to mother ETRD              
                            place.TranslateZ(zposbod);              
                            /// Translate z = zposbod              
                            _stacker -> Position( AgBlock::Find("TBOD"), place );              
                      } // end placement of TBOD           
                      END_OF_ETRD:           
                      mCurrent = _save;           
                ///@}        
          } // End Block ETRD     
          // ---------------------------------------------------------------------------------------------------     
          void TRAD::Block( AgCreate create )     
          {         
                ///@addtogroup TRAD_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Radiator            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Radiator");              
                            _material = mat;              
                      }           
                      /// Medium Standard           
                      {  AgMedium med = AgMedium::CopyMedium("Standard");              
                            _medium = med;              
                      }           
                      { AgAttribute attr = AgAttribute("TRAD");              
                            attr.par("seen")=1;              
                            attr.par("colo")=6;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tubs");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=eidg.rmin;              
                            shape.par("rmax")=eidg.rmax;              
                            shape.par("phi1")=-eidg.dphi/2;              
                            shape.par("phi2")=eidg.dphi/2;              
                            shape.par("dz")=eidg.trdthick/2;              
                            /// Shape Tubs rmin=eidg.rmin rmax=eidg.rmax phi1=-eidg.dphi/2 phi2=eidg.dphi/2 dz=eidg.trdthick/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_TRAD;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_TRAD:           
                      mCurrent = _save;           
                ///@}        
          } // End Block TRAD     
          // ---------------------------------------------------------------------------------------------------     
          void TABS::Block( AgCreate create )     
          {         
                ///@addtogroup TABS_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Xe15CO2            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Xe15co2");              
                            _material = mat;              
                      }           
                      /// Medium Sensitive           
                      ///  isvol = 1           
                      {  AgMedium &med = AgMedium::Get("Sensitive");              
                               med.Inherit(this);              
                            med.par("isvol")=1;              
                            _medium = med;              
                      }           
                      { AgAttribute attr = AgAttribute("TABS");              
                            attr.par("seen")=1;              
                            attr.par("colo")=8;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tubs");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=eidg.rmin;              
                            shape.par("rmax")=eidg.rmax;              
                            shape.par("phi1")=-eidg.dphi/2;              
                            shape.par("phi2")=eidg.dphi/2;              
                            shape.par("dz")=eidg.tabthick/2;              
                            /// Shape Tubs rmin=eidg.rmin rmax=eidg.rmax phi1=-eidg.dphi/2 phi2=eidg.dphi/2 dz=eidg.tabthick/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_TABS;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_TABS:           
                      mCurrent = _save;           
                ///@}        
          } // End Block TABS     
          // ---------------------------------------------------------------------------------------------------     
          void TBOD::Block( AgCreate create )     
          {         
                ///@addtogroup TBOD_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material G10            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("G10");              
                            _material = mat;              
                      }           
                      /// Medium Standard           
                      {  AgMedium med = AgMedium::CopyMedium("Standard");              
                            _medium = med;              
                      }           
                      { AgAttribute attr = AgAttribute("TBOD");              
                            attr.par("seen")=1;              
                            attr.par("colo")=7;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tubs");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=eidg.rmin;              
                            shape.par("rmax")=eidg.rmax;              
                            shape.par("phi1")=-eidg.dphi/2;              
                            shape.par("phi2")=eidg.dphi/2;              
                            shape.par("dz")=eidg.tbdthick/2;              
                            /// Shape Tubs rmin=eidg.rmin rmax=eidg.rmax phi1=-eidg.dphi/2 phi2=eidg.dphi/2 dz=eidg.tbdthick/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_TBOD;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_TBOD:           
                      mCurrent = _save;           
                ///@}        
          } // End Block TBOD     
    // ----------------------------------------------------------------------- geoctr
       void EiddGeo::ConstructGeometry( const Char_t *dummy )     
       {        
             ///@addtogroup EiddGeo_revision        
             ///@{           
                   /// Author: Ming Shao           
             ///@}        
             ///@addtogroup EiddGeo_revision        
             ///@{           
                   /// Created:  28 September 2011            
             ///@}        
             AddBlock("ETTV");        
             AddBlock("ETRV");        
             AddBlock("ETFV");        
             AddBlock("ECLV");        
             AddBlock("ETOF");        
             AddBlock("TECL");        
             AddBlock("ETRD");        
             AddBlock("TBOX");        
             AddBlock("TGAS");        
             AddBlock("TPCB");        
             AddBlock("TGLA");        
             AddBlock("TGAP");        
             AddBlock("TEAA");        
             AddBlock("TEAB");        
             AddBlock("SCIN");        
             AddBlock("TRAD");        
             AddBlock("TABS");        
             AddBlock("TBOD");        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup eidv_doc        
             ///@{           
                   ++eidv._index;           
                   eidv . version = 1; //  version            
                   /// eidv . version = 1; //  version            
                   eidv . eidconfig = 1; //  EID configuration            
                   /// eidv . eidconfig = 1; //  EID configuration            
                   //           
                   eidv.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup eidg_doc        
             ///@{           
                   ++eidg._index;           
                   eidg . version = 1; //  geometry version            
                   /// eidg . version = 1; //  geometry version            
                   eidg . ettz = 286.00; //  Z position of ETTIE Detector            
                   /// eidg . ettz = 286.00; //  Z position of ETTIE Detector            
                   eidg . etrvz = 280.00; //  Z position of ETRD            
                   /// eidg . etrvz = 280.00; //  Z position of ETRD            
                   eidg . etfvz = 299.00; //  Z position ETOF            
                   /// eidg . etfvz = 299.00; //  Z position ETOF            
                   eidg . eclvz = 305.00; //  Z position TECL            
                   /// eidg . eclvz = 305.00; //  Z position TECL            
                   eidg . rmin = 46.107; //  mothervolume rmin (TPC inner radius)            
                   /// eidg . rmin = 46.107; //  mothervolume rmin (TPC inner radius)            
                   eidg . rmax = 206.75; //  mothervolume rmin (TPC outer radius)            
                   /// eidg . rmax = 206.75; //  mothervolume rmin (TPC outer radius)            
                   eidg . dphi = 28.0; //  sector phi acceptan (follow TPC sectors)            
                   /// eidg . dphi = 28.0; //  sector phi acceptan (follow TPC sectors)            
                   eidg . ettthick = 48.000; //  ETTIE volume thick            
                   /// eidg . ettthick = 48.000; //  ETTIE volume thick            
                   eidg . tfvthick = 2.0000; //  ETFV volume thick            
                   /// eidg . tfvthick = 2.0000; //  ETFV volume thick            
                   eidg . tofthick = 1.5202; //  ETOF total thick            
                   /// eidg . tofthick = 1.5202; //  ETOF total thick            
                   eidg . tbxthick = 0.300; //  ETOF box thick            
                   /// eidg . tbxthick = 0.300; //  ETOF box thick            
                   eidg . tpbthick = 0.150; //  ETOF PCB thick            
                   /// eidg . tpbthick = 0.150; //  ETOF PCB thick            
                   eidg . tglthick = 0.050; //  ETOF glass thick            
                   /// eidg . tglthick = 0.050; //  ETOF glass thick            
                   eidg . tgpthick = 0.022; //  ETOF gas gap thick            
                   /// eidg . tgpthick = 0.022; //  ETOF gas gap thick            
                   eidg . elvthick = 10.00; //  ECLV volume thick            
                   /// eidg . elvthick = 10.00; //  ECLV volume thick            
                   eidg . eclthick = 8.52; //  TECL total thick            
                   /// eidg . eclthick = 8.52; //  TECL total thick            
                   eidg . eaathick = 1.68; //  Absorber 1 thick            
                   /// eidg . eaathick = 1.68; //  Absorber 1 thick            
                   eidg . eabthick = 0.84; //  Absorber 2 thick            
                   /// eidg . eabthick = 0.84; //  Absorber 2 thick            
                   eidg . scnthick = 3.00; //  Scintillator thick            
                   /// eidg . scnthick = 3.00; //  Scintillator thick            
                   eidg . trvthick = 36.0; //  ETRV volume thick            
                   /// eidg . trvthick = 36.0; //  ETRV volume thick            
                   eidg . trdthick = 12.0; //  ETRD layer thick            
                   /// eidg . trdthick = 12.0; //  ETRD layer thick            
                   eidg . trathick = 5.916; //  TR radiator thick            
                   /// eidg . trathick = 5.916; //  TR radiator thick            
                   eidg . tabthick = 3.83; //  TR absorber thick            
                   /// eidg . tabthick = 3.83; //  TR absorber thick            
                   eidg . tbdthick = 2.00; //  ETRD mother board thick            
                   /// eidg . tbdthick = 2.00; //  ETRD mother board thick            
                   //           
                   eidg.fill();           
             ///@}        
             //        
             /// USE eidv _index=1;        
             eidv.Use();        
             /// USE eidg version=eidv.eidconfig;        
             eidg.Use("version",(Float_t)eidv.eidconfig);        
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
             /// Component H	a=1	z=1	w=0.90*2*1./102.+0.+0.05*10*1./58.        
             /// Component C	a=12	z=6	w=0.90*2*12./102.+0.+0.05*4*12./58.        
             /// Component F	a=19	z=9	w=0.90*4*19./102.+0.05*6*19./146.+0.        
             /// Component S	a=32	z=16	w=0.+0.05*1*32./146.+0.        
             /// Mixture Freon dens=4.55e-3        
             {  AgMaterial &mix = AgMaterial::Get("Freon");           
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
             /// Component N	a=14.01	z=14	w=0.008        
             /// Component O	a=16	z=8	w=0.301        
             /// Component C	a=12	z=6	w=0.653        
             /// Component H	a=1	z=1	w=0.038        
             /// Mixture Radiator dens=0.10416        
             {  AgMaterial &mix = AgMaterial::Get("Radiator");           
                   mix.Component("N",14.01,14,0.008);           
                   mix.Component("O",16,8,0.301);           
                   mix.Component("C",12,6,0.653);           
                   mix.Component("H",1,1,0.038);           
                   mix.par("dens")=0.10416;           
                   mix.lock();           
                   _material = mix;           
                   _material.lock();           
             }        
             /// Component Xe	a=131.29	z=54	w=0.979        
             /// Component O	a=16	z=8	w=0.0147        
             /// Component C	a=12	z=6	w=0.0063        
             /// Mixture Xe15CO2 dens=0.0049        
             {  AgMaterial &mix = AgMaterial::Get("Xe15co2");           
                   mix.Component("Xe",131.29,54,0.979);           
                   mix.Component("O",16,8,0.0147);           
                   mix.Component("C",12,6,0.0063);           
                   mix.par("dens")=0.0049;           
                   mix.lock();           
                   _material = mix;           
                   _material.lock();           
             }        
             /// Component C	a=12	z=6	w=0.923        
             /// Component H	a=1	z=1	w=0.077        
             /// Mixture Polystyrene dens=1.06        
             {  AgMaterial &mix = AgMaterial::Get("Polystyrene");           
                   mix.Component("C",12,6,0.923);           
                   mix.Component("H",1,1,0.077);           
                   mix.par("dens")=1.06;           
                   mix.lock();           
                   _material = mix;           
                   _material.lock();           
             }        
             _create = AgCreate("ETTV");        
             {           
                   AgShape myshape; // undefined shape           
                   ///Create ETTV           
                   Create("ETTV");            
             }        
             zposett = eidg.ettz;        
             { AgPlacement place = AgPlacement("ETTV","CAVE");           
                   /// Add daughter volume ETTV to mother CAVE           
                   place.TranslateZ(-zposett);           
                   /// Translate z = -zposett           
                   /// G3 Reference: thetax = 90           
                   /// G3 Reference: phix = 0           
                   /// G3 Reference: thetay = 90           
                   /// G3 Reference: phiy = 90           
                   /// G3 Reference: thetaz = 180           
                   /// G3 Reference: phiz = 0           
                   Double_t _thetax=90,_phix=0,_thetay=90,_phiy=90,_thetaz=180,_phiz=0;           
                   place.Reference( _thetax, _phix, _thetay, _phiy, _thetaz, _phiz );           
                   _stacker -> Position( AgBlock::Find("ETTV"), place );           
             } // end placement of ETTV        
       }; // EiddGeo     
 }; // namespace EiddGeo  
 
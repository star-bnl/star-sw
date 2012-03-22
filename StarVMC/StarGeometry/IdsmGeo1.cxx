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
                Float_t inr,outr,lengthz,k,sina,cosa,resr,angres,m,pm,j,angrail,rrail,dphihv,angfgtcbl;        
                //        
                /// Float_t inr,outr,lengthz,k,sina,cosa,resr,angres,m,pm,j,angrail,rrail,dphihv,angfgtcbl        
          ///@}     
          //  -----------------------------------------------------     
          /// @defgroup idsc_doc     
          /// \class Idsc_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t version;     
          ///Int_t _index;     
          //     
          Idsc_t idsc;     
          //     
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
          ///Float_t fgtstartz;     
          ///Float_t fgtdiskstepz;     
          ///Int_t fgtndisk;     
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
                            angres = idsg.angflat - idsg.dangres/2.;              
                            if ( m==1 )              
                            {                 
                                  angres = idsg.angflat+idsg.dangres/2.;                 
                            }              
                            _create = AgCreate("TPRT");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create TPRT                 
                                  Create("TPRT");                  
                            }              
                            { AgPlacement place = AgPlacement("TPRT","IDSM");                 
                                  /// Add daughter volume TPRT to mother IDSM                 
                                  place.TranslateX(idsg.rrres*cos(angres/180.*3.1416));                 
                                  /// Translate x = idsg.rrres*cos(angres/180.*3.1416)                 
                                  place.TranslateY(idsg.rrres*sin(angres/180.*3.1416));                 
                                  /// Translate y = idsg.rrres*sin(angres/180.*3.1416)                 
                                  place.TranslateZ(0);                 
                                  /// Translate z = 0                 
                                  _stacker -> Position( AgBlock::Find("TPRT"), place );                 
                            } // end placement of TPRT              
                      }           
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
                      /// Loop on m from 0 to 1 step=1           
                      for ( m=0; (1>0)? (m<=1):(m>=1); m+=1 )           
                      {              
                            angrail=16./180.*3.1416;              
                            if ( m==1 )              
                            {                 
                                  angrail = angrail+3.1416;                 
                            }              
                            _create = AgCreate("FGRL");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create FGRL                 
                                  Create("FGRL");                  
                            }              
                            { AgPlacement place = AgPlacement("FGRL","IDSM");                 
                                  /// Add daughter volume FGRL to mother IDSM                 
                                  place.TranslateX(rrail*cos(angrail));                 
                                  /// Translate x = rrail*cos(angrail)                 
                                  place.TranslateY(rrail*sin(angrail));                 
                                  /// Translate y = rrail*sin(angrail)                 
                                  place.TranslateZ(146.57);                 
                                  /// Translate z = 146.57                 
                                  _stacker -> Position( AgBlock::Find("FGRL"), place );                 
                            } // end placement of FGRL              
                      }           
                      /// Loop on m from 1 to 4 step=1           
                      for ( m=1; (1>0)? (m<=4):(m>=4); m+=1 )           
                      {              
                            angfgtcbl=-90./180.*3.1416;              
                            /// Loop on k from 1 to idsg.fgtndisk step=1              
                            for ( k=1; (1>0)? (k<=idsg.fgtndisk):(k>=idsg.fgtndisk); k+=1 )              
                            {                 
                                  if ( not ((idsg.version==2.0)||(k==1)||(m<=3))) { continue; }                 
                                  pm=1;                 
                                  if ( (k==1)||(k==3)||(k==5) )                 
                                  {                    
                                        pm=-1;                    
                                  }                 
                                  /// Loop on j from k to 16 step=1                 
                                  for ( j=k; (1>0)? (j<=16):(j>=16); j+=1 )                 
                                  {                    
                                        _create = AgCreate("FGHV");                    
                                        {                       
                                              AgShape myshape; // undefined shape                       
                                              ///Create FGHV                       
                                              Create("FGHV");                        
                                        }                    
                                        { AgPlacement place = AgPlacement("FGHV","IDSM");                       
                                              /// Add daughter volume FGHV to mother IDSM                       
                                              place.TranslateX((rrail+0.5*pm)*cos(angfgtcbl+(dphihv*(m+2.2*k-pm*.8))));                       
                                              /// Translate x = (rrail+0.5*pm)*cos(angfgtcbl+(dphihv*(m+2.2*k-pm*.8)))                       
                                              place.TranslateY((rrail+0.5*pm)*sin(angfgtcbl+(dphihv*(m+2.2*k - pm*.8))));                       
                                              /// Translate y = (rrail+0.5*pm)*sin(angfgtcbl+(dphihv*(m+2.2*k - pm*.8)))                       
                                              place.TranslateZ(idsg.fgtstartz+idsg.fgtdiskstepz*(j-0.5));                       
                                              /// Translate z = idsg.fgtstartz+idsg.fgtdiskstepz*(j-0.5)                       
                                              _stacker -> Position( AgBlock::Find("FGHV"), place );                       
                                        } // end placement of FGHV                    
                                  }                 
                            }              
                      }           
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
                      /// Material SUCBMix            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Sucbmix");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=21.6;              
                            shape.par("rmax")=22.4;              
                            shape.par("dz")=1.3/2.;              
                            /// Shape Tube rmin=21.6 rmax=22.4 dz=1.3/2.               
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
                      /// Material SUCDMix            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Sucdmix");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=38.6;              
                            shape.par("rmax")=39.9;              
                            shape.par("dz")=0.62/2.;              
                            /// Shape Tube rmin=38.6 rmax=39.9 dz=0.62/2.               
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
          void FGRL::Block( AgCreate create )     
          {         
                ///@addtogroup FGRL_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("FGRL");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material RailMix            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Railmix");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=0.9;              
                            shape.par("rmax")=1.0;              
                            shape.par("dz")=165.6/2.;              
                            /// Shape Tube rmin=0.9 rmax=1.0 dz=165.6/2.               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FGRL;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_FGRL:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FGRL     
          // ---------------------------------------------------------------------------------------------------     
          void FGHV::Block( AgCreate create )     
          {         
                ///@addtogroup FGHV_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("FGHV");              
                            attr.par("seen")=1;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material CableMix            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Cablemix");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=0.;              
                            shape.par("rmax")=0.43;              
                            shape.par("dz")=idsg.fgtdiskstepz/2.;              
                            /// Shape Tube rmin=0. rmax=0.43 dz=idsg.fgtdiskstepz/2.               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FGHV;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_FGHV:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FGHV     
    // ----------------------------------------------------------------------- geoctr
       void IdsmGeo1::ConstructGeometry( const Char_t *dummy )     
       {        
             ///@addtogroup IdsmGeo1_revision        
             ///@{           
                   /// Created:   10/04/2011            
             ///@}        
             ///@addtogroup IdsmGeo1_revision        
             ///@{           
                   /// Author: Jan Balewski MIT, Willie Leight MIT (material mixes)            
             ///@}        
             AddBlock("IDSM");        
             AddBlock("TPRR");        
             AddBlock("TPRT");        
             AddBlock("SUCA");        
             AddBlock("SUCB");        
             AddBlock("SUCC");        
             AddBlock("SUCD");        
             AddBlock("SUCE");        
             AddBlock("SUCF");        
             AddBlock("SUCG");        
             AddBlock("FGRL");        
             AddBlock("FGHV");        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup idsc_doc        
             ///@{           
                   ++idsc._index;           
                   idsc . version = 1.0; // Versioning of the IDSM geometry           
                   /// idsc . version = 1.0; // Versioning of the IDSM geometry           
                   //           
                   idsc.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup idsg_doc        
             ///@{           
                   ++idsg._index;           
                   idsg . version = 1.0; // 2012 Versionin of the IDS geometry            
                   /// idsg . version = 1.0; // 2012 Versionin of the IDS geometry            
                   idsg . rf = 2.25; //  radii of inner volume boundary           
                   /// idsg . rf = 2.25; //  radii of inner volume boundary           
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
                   idsg . fgtstartz = 70.; //  position of sensitive volume of the 1st disk           
                   /// idsg . fgtstartz = 70.; //  position of sensitive volume of the 1st disk           
                   idsg . fgtdiskstepz = 10.; //  disk separation along Z           
                   /// idsg . fgtdiskstepz = 10.; //  disk separation along Z           
                   idsg . fgtndisk = 6; // number of disks           
                   /// idsg . fgtndisk = 6; // number of disks           
                   //           
                   idsg.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup idsg_doc        
             ///@{           
                   ++idsg._index;           
                   idsg . version = 2.0; // 2013 versionin of the IDS geometry            
                   /// idsg . version = 2.0; // 2013 versionin of the IDS geometry            
                   idsg . rf = 2.25; //  radii of inner volume boundary           
                   /// idsg . rf = 2.25; //  radii of inner volume boundary           
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
                   idsg . fgtstartz = 70.; //  position of sensitive volume of the 1st disk           
                   /// idsg . fgtstartz = 70.; //  position of sensitive volume of the 1st disk           
                   idsg . fgtdiskstepz = 10.; //  disk separation along Z           
                   /// idsg . fgtdiskstepz = 10.; //  disk separation along Z           
                   idsg . fgtndisk = 6; // number of disks           
                   /// idsg . fgtndisk = 6; // number of disks           
                   //           
                   idsg.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup idsa_doc        
             ///@{           
                   ++idsa._index;           
                   idsa . version = 1.0; // Default alignment of IDSM at (0,0,0) with no rotation           
                   /// idsa . version = 1.0; // Default alignment of IDSM at (0,0,0) with no rotation           
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
             /// Component O	a=16	z=8	w=0.043        
             /// Component C	a=12	z=6	w=0.635        
             /// Component H	a=1	z=1	w=0.014        
             /// Component Cl	a=35.5	z=17	w=0.019        
             /// Component AL	a=27	z=13	w=0.017        
             /// Component Ti	a=47.9	z=22	w=0.260        
             /// Component V	a=50.9	z=23	w=0.012        
             /// Mixture SUCBMix dens=2.46        
             {  AgMaterial &mix = AgMaterial::Get("Sucbmix");           
                   mix.Component("O",16,8,0.043);           
                   mix.Component("C",12,6,0.635);           
                   mix.Component("H",1,1,0.014);           
                   mix.Component("Cl",35.5,17,0.019);           
                   mix.Component("AL",27,13,0.017);           
                   mix.Component("Ti",47.9,22,0.260);           
                   mix.Component("V",50.9,23,0.012);           
                   mix.par("dens")=2.46;           
                   mix.lock();           
                   _material = mix;           
                   _material.lock();           
             }        
             /// Component O	a=16	z=8	w=0.048        
             /// Component C	a=12	z=6	w=0.694        
             /// Component H	a=1	z=1	w=0.015        
             /// Component Cl	a=35.5	z=17	w=0.021        
             /// Component AL	a=27	z=13	w=0.013        
             /// Component Ti	a=47.9	z=22	w=0.200        
             /// Component V	a=50.9	z=23	w=0.009        
             /// Mixture SUCDMix dens=2.37        
             {  AgMaterial &mix = AgMaterial::Get("Sucdmix");           
                   mix.Component("O",16,8,0.048);           
                   mix.Component("C",12,6,0.694);           
                   mix.Component("H",1,1,0.015);           
                   mix.Component("Cl",35.5,17,0.021);           
                   mix.Component("AL",27,13,0.013);           
                   mix.Component("Ti",47.9,22,0.200);           
                   mix.Component("V",50.9,23,0.009);           
                   mix.par("dens")=2.37;           
                   mix.lock();           
                   _material = mix;           
                   _material.lock();           
             }        
             /// Component O	a=16	z=8	w=0.032        
             /// Component C	a=12	z=6	w=0.471        
             /// Component H	a=1	z=1	w=0.011        
             /// Component Cl	a=35.5	z=17	w=0.014        
             /// Component AL	a=27	z=13	w=0.472        
             /// Mixture RailMix dens=3.384        
             {  AgMaterial &mix = AgMaterial::Get("Railmix");           
                   mix.Component("O",16,8,0.032);           
                   mix.Component("C",12,6,0.471);           
                   mix.Component("H",1,1,0.011);           
                   mix.Component("Cl",35.5,17,0.014);           
                   mix.Component("AL",27,13,0.472);           
                   mix.par("dens")=3.384;           
                   mix.lock();           
                   _material = mix;           
                   _material.lock();           
             }        
             /// Component Si	a=28.08	z=14	w=0.131        
             /// Component O	a=16	z=8	w=0.117        
             /// Component C	a=12	z=6	w=0.193        
             /// Component H	a=1	z=1	w=0.024        
             /// Component AL	a=27	z=13	w=0.143        
             /// Component Cu	a=63.5	z=29	w=0.106        
             /// Component F	a=19.	z=9	w=0.254        
             /// Component Na	a=23.	z=11	w=0.015        
             /// Component Ca	a=40.1	z=20	w=0.017        
             /// Mixture CableMix dens=1.755        
             {  AgMaterial &mix = AgMaterial::Get("Cablemix");           
                   mix.Component("Si",28.08,14,0.131);           
                   mix.Component("O",16,8,0.117);           
                   mix.Component("C",12,6,0.193);           
                   mix.Component("H",1,1,0.024);           
                   mix.Component("AL",27,13,0.143);           
                   mix.Component("Cu",63.5,29,0.106);           
                   mix.Component("F",19.,9,0.254);           
                   mix.Component("Na",23.,11,0.015);           
                   mix.Component("Ca",40.1,20,0.017);           
                   mix.par("dens")=1.755;           
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
             /// Component Si	a=28.08	z=14	w=0.281        
             /// Component O	a=16	z=8	w=0.467        
             /// Component C	a=12	z=6	w=0.220        
             /// Component H	a=1	z=1	w=0.032        
             /// Mixture FR4 dens=1.80        
             {  AgMaterial &mix = AgMaterial::Get("Fr4");           
                   mix.Component("Si",28.08,14,0.281);           
                   mix.Component("O",16,8,0.467);           
                   mix.Component("C",12,6,0.220);           
                   mix.Component("H",1,1,0.032);           
                   mix.par("dens")=1.80;           
                   mix.lock();           
                   _material = mix;           
                   _material.lock();           
             }        
             /// USE idsc _index=1;        
             idsc.Use();        
             /// USE idsg version=idsc.version;        
             idsg.Use("version",(Float_t)idsc.version);        
             /// USE idsa version=1.0;        
             idsa.Use("version",(Float_t)1.0);        
             std::cout << Form("Inner Detector Support Module with IDSC.version = %5.2f ",idsc.version) << std::endl;        
             inr     = idsg.rf;        
             outr    = idsg.rrres + idsg.r2res;        
             lengthz = 470.;        
             sina = sin( idsg.angflat * degrad );        
             cosa = cos( idsg.angflat * degrad );        
             rrail=41.5;        
             dphihv=0.03;        
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
 
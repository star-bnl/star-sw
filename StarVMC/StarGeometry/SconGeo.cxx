#include "SconGeo.h"  
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
 namespace SCONGEO // $NMSPC  
 {     
       //     
       // ---------------------------------------------------------------------------------------------------     
          //  -----------------------------------------------------     
          /// @defgroup svtg_doc     
          /// \class Svtg_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t version;     
          ///Float_t nlayer;     
          ///Float_t rsizemin;     
          ///Float_t rsizemax;     
          ///Float_t zsizemax;     
          ///Float_t angoff;     
          ///Float_t supportver;     
          ///Float_t conever;     
          ///Float_t ifmany;     
          ///Float_t nmin;     
          ///Int_t _index;     
          //     
          Svtg_t svtg;     
          //     
          //  -----------------------------------------------------     
          /// @defgroup ssup_doc     
          /// \class Ssup_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t version;     
          ///Float_t cabthk;     
          ///Float_t hosrmn;     
          ///Float_t hosrmx;     
          ///Float_t nhoses;     
          ///Float_t wrpmythk;     
          ///Float_t wrpalthk;     
          ///Float_t grphthk;     
          ///Float_t cone1zmn;     
          ///Float_t rodlen;     
          ///Float_t roddist;     
          ///Float_t rodid;     
          ///Float_t rodod;     
          ///Float_t rodidx;     
          ///Float_t rododx;     
          ///Float_t carbonshell;     
          ///Float_t carbondens;     
          ///Float_t nomexdens;     
          ///Float_t con1idmn;     
          ///Float_t con3idmn;     
          ///Float_t con4idmn;     
          ///Float_t con4idmx;     
          ///Float_t cone3zmx;     
          ///Float_t cone4zmx;     
          ///Float_t brathk;     
          ///Float_t erjthk;     
          ///Float_t erjwid;     
          ///Float_t erjlen;     
          ///Float_t erjzdis;     
          ///Float_t erj1x;     
          ///Float_t erj2x;     
          ///Float_t erj2y;     
          ///Float_t erjrad;     
          ///Float_t erjdia;     
          ///Int_t _index;     
          //     
          Ssup_t ssup;     
          //     
          //  -----------------------------------------------------     
          /// @defgroup ssub_doc     
          /// \class Ssub_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t version;     
          ///Float_t kmountid;     
          ///Float_t kmountod;     
          ///Float_t kmntthk;     
          ///Float_t kmcutod;     
          ///Float_t kmcutid;     
          ///Float_t kmcutoa;     
          ///Float_t kmcutoff;     
          ///Float_t sringid;     
          ///Float_t sringod;     
          ///Float_t sringthk;     
          ///Float_t srcutphi;     
          ///Float_t srcutwid;     
          ///Float_t srcutout;     
          ///Float_t srcutin;     
          ///Float_t srollid;     
          ///Float_t srollod;     
          ///Float_t srolllen;     
          ///Float_t swirelen;     
          ///Float_t mblkhgh;     
          ///Float_t mblkowid;     
          ///Float_t mblkolen;     
          ///Float_t mblkiwid;     
          ///Float_t mblkilen;     
          ///Float_t mblkorad;     
          ///Float_t mblkirad;     
          ///Float_t mroddia;     
          ///Int_t _index;     
          //     
          Ssub_t ssub;     
          //     
          ///@addtogroup SconGeo_vars     
          ///@{        
                Int_t ilayer,s,side,ilad,iwaf,i,j;        
                //        
                /// Int_t ilayer,s,side,ilad,iwaf,i,j        
          ///@}     
          ///@addtogroup SconGeo_vars     
          ///@{        
                Float_t ladthk,cone_thk1,cone_thk2,roffset,rsizemax,deg,rad,c0;        
                //        
                /// Float_t ladthk,cone_thk1,cone_thk2,roffset,rsizemax,deg,rad,c0        
          ///@}     
          ///@addtogroup SconGeo_vars     
          ///@{        
                Float_t cone_len,cone_sin,cone_cos,rmin,rmax,zmin,zmax,angle;        
                //        
                /// Float_t cone_len,cone_sin,cone_cos,rmin,rmax,zmin,zmax,angle        
          ///@}     
          ///@addtogroup SconGeo_vars     
          ///@{        
                Float_t xpos,ypos,zpos,clearance,rin,rou,elethk,tablen,radmax;        
                //        
                /// Float_t xpos,ypos,zpos,clearance,rin,rou,elethk,tablen,radmax        
          ///@}     
          ///@addtogroup SconGeo_vars     
          ///@{        
                Float_t endrng_z,brack_z,screw_z,ir_rmin,ang,wafpcklen,dthk,radtilt;        
                //        
                /// Float_t endrng_z,brack_z,screw_z,ir_rmin,ang,wafpcklen,dthk,radtilt        
          ///@}     
          ///@addtogroup SconGeo_vars     
          ///@{        
                Float_t xbuf,phi,xbuf1,xbuf2;        
                //        
                /// Float_t xbuf,phi,xbuf1,xbuf2        
          ///@}     
          ///@addtogroup SconGeo_vars     
          ///@{        
                Float_t ypcb,a,cuthk,sq,tube_angle;        
                //        
                /// Float_t ypcb,a,cuthk,sq,tube_angle        
          ///@}     
          ///@addtogroup SconGeo_vars     
          ///@{        
                Float_t rad_offset;        
                //        
                /// Float_t rad_offset        
                Array_t<Float_t> radii(6);        
                /// radii(6) : array of Float_t        
                Array_t<Float_t> rad_cones_in(5);        
                /// rad_cones_in(5) : array of Float_t        
                Array_t<Float_t> rad_cones_out(5);        
                /// rad_cones_out(5) : array of Float_t        
                Array_t<Float_t> shield_phi(4);        
                /// shield_phi(4) : array of Float_t        
          ///@}     
          ///@addtogroup SconGeo_vars     
          ///@{        
                Float_t trapy,ssidx,ssiry;        
                //        
                /// Float_t trapy,ssidx,ssiry        
          ///@}     
          ///@addtogroup SconGeo_vars     
          ///@{        
                Int_t i_phi;        
                //        
                /// Int_t i_phi        
          ///@}     
       SconGeo::SconGeo()     
         : AgModule("SconGeo"," is Support structures from SVTT moved into CAVE: ")     
       {        
       }     
          // ---------------------------------------------------------------------------------------------------     
          void SCON::Block( AgCreate create )     
          {         
                ///@addtogroup SCON_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      cone_thk1=ssup.grphthk;           
                      cone_thk2=cone_thk1+ssup.cabthk+2*ssup.hosrmx;           
                      cone_thk2=cone_thk2+ssup.wrpmythk+ssup.wrpalthk;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("SCON");              
                            attr.par("seen")=0;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Pcon");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("phi1")=0;              
                            shape.par("dphi")=360;              
                            shape.par("nz")=7;              
                            shape.Z(0)=ssup.cone1zmn;              
                            shape.Z(1)=ssup.rodlen/2;              
                            shape.Z(2)=ssup.rodlen/2;              
                            shape.Z(3)=ssup.rodlen/2+ssup.grphthk;              
                            shape.Z(4)=ssup.rodlen/2+ssup.grphthk;              
                            shape.Z(5)=ssup.cone3zmx;              
                            shape.Z(6)=ssup.cone4zmx;              
                            shape.Rmin(0)=ssup.con1idmn;              
                            shape.Rmin(1)=ssup.con1idmn;              
                            shape.Rmin(2)=ssup.con1idmn;              
                            shape.Rmin(3)=ssup.con1idmn;              
                            shape.Rmin(4)=ssup.con3idmn;              
                            shape.Rmin(5)=ssup.con4idmn;              
                            shape.Rmin(6)=ssup.con4idmx;              
                            shape.Rmax(0)=ssup.con1idmn+cone_thk1;              
                            shape.Rmax(1)=ssup.con1idmn+cone_thk1;              
                            shape.Rmax(2)=ssup.con3idmn+cone_thk1;              
                            shape.Rmax(3)=ssup.con3idmn+cone_thk1;              
                            shape.Rmax(4)=ssup.con3idmn+cone_thk2;              
                            shape.Rmax(5)=ssup.con4idmn+cone_thk2;              
                            shape.Rmax(6)=ssup.con4idmx+cone_thk2;              
                            /// Shape Pcon phi1=0 dphi=360 nz=7               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SCON;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("SGRA");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create SGRA              
                            Create("SGRA");               
                      }           
                      { AgPlacement place = AgPlacement("SGRA","SCON");              
                            /// Add daughter volume SGRA to mother SCON              
                            _stacker -> Position( AgBlock::Find("SGRA"), place );              
                      } // end placement of SGRA           
                      _create = AgCreate("SCMY");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create SCMY              
                            Create("SCMY");               
                      }           
                      { AgPlacement place = AgPlacement("SCMY","SCON");              
                            /// Add daughter volume SCMY to mother SCON              
                            _stacker -> Position( AgBlock::Find("SCMY"), place );              
                      } // end placement of SCMY           
                      END_OF_SCON:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SCON     
          // ---------------------------------------------------------------------------------------------------     
          void SGRA::Block( AgCreate create )     
          {         
                ///@addtogroup SGRA_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Carbon            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Carbon");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("SGRA");              
                            attr.par("seen")=1;              
                            attr.par("colo")=6;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Pcon");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("phi1")=0;              
                            shape.par("dphi")=360;              
                            shape.par("nz")=5;              
                            shape.Z(0)=ssup.rodlen/2;              
                            shape.Z(1)=ssup.rodlen/2+ssup.grphthk;              
                            shape.Z(2)=ssup.rodlen/2+ssup.grphthk;              
                            shape.Z(3)=ssup.cone3zmx;              
                            shape.Z(4)=ssup.cone4zmx;              
                            shape.Rmin(0)=ssup.con1idmn;              
                            shape.Rmin(1)=ssup.con1idmn;              
                            shape.Rmin(2)=ssup.con3idmn;              
                            shape.Rmin(3)=ssup.con4idmn;              
                            shape.Rmin(4)=ssup.con4idmx;              
                            shape.Rmax(0)=ssup.con3idmn+ssup.grphthk;              
                            shape.Rmax(1)=ssup.con3idmn+ssup.grphthk;              
                            shape.Rmax(2)=ssup.con3idmn+ssup.grphthk;              
                            shape.Rmax(3)=ssup.con4idmn+ssup.grphthk;              
                            shape.Rmax(4)=ssup.con4idmx+ssup.grphthk;              
                            /// Shape Pcon phi1=0 dphi=360 nz=5               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SGRA;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_SGRA:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SGRA     
          // ---------------------------------------------------------------------------------------------------     
          void SBSP::Block( AgCreate create )     
          {         
                ///@addtogroup SBSP_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("SBSP");              
                            attr.par("seen")=0;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=svtg.rsizemin;              
                            shape.par("rmax")=ssub.kmountod/2;              
                            shape.par("dz")=ssub.kmntthk/2+ssub.mblkhgh;              
                            /// Shape Tube rmin=svtg.rsizemin rmax=ssub.kmountod/2 dz=ssub.kmntthk/2+ssub.mblkhgh               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SBSP;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("SAKM");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create SAKM              
                            Create("SAKM");               
                      }           
                      { AgPlacement place = AgPlacement("SAKM","SBSP");              
                            /// Add daughter volume SAKM to mother SBSP              
                            _stacker -> Position( AgBlock::Find("SAKM"), place );              
                      } // end placement of SAKM           
                      _create = AgCreate("SBMM");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create SBMM              
                            Create("SBMM");               
                      }           
                      _create = AgCreate("SBRL");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create SBRL              
                            Create("SBRL");               
                      }           
                      _create = AgCreate("SBRX");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create SBRX              
                            Create("SBRX");               
                      }           
                      /// Loop on i from -1 to 1 step=2           
                      for ( i=-1; (2>0)? (i<=1):(i>=1); i+=2 )           
                      {              
                            /// Loop on j from 0 to 1 step=1              
                            for ( j=0; (1>0)? (j<=1):(j>=1); j+=1 )              
                            {                 
                                  phi=i*ssub.srcutphi+180*j;                 
                                  xbuf1=(ssub.kmountod/4.0+(svtg.rsizemin+ssub.srollod)/2.0);                 
                                  { AgPlacement place = AgPlacement("SBMM","SBSP");                    
                                        /// Add daughter volume SBMM to mother SBSP                    
                                        place.TranslateX(xbuf1*sin(degrad*phi));                    
                                        /// Translate x = xbuf1*sin(degrad*phi)                    
                                        place.TranslateY(-xbuf1*cos(degrad*phi));                    
                                        /// Translate y = -xbuf1*cos(degrad*phi)                    
                                        place.TranslateZ(-ssub.kmntthk/2-ssub.mblkhgh/2);                    
                                        /// Translate z = -ssub.kmntthk/2-ssub.mblkhgh/2                    
                                        place.AlphaZ(phi);                    
                                        /// Rotate: AlphaZ = phi                    
                                        /// G3 Reference: thetax = 90                    
                                        /// G3 Reference: phix = 0                    
                                        /// G3 Reference: thetay = 90                    
                                        /// G3 Reference: phiy = 90                    
                                        /// G3 Reference: thetaz = 0                    
                                        /// G3 Reference: phiz = 0                    
                                        _stacker -> Position( AgBlock::Find("SBMM"), place );                    
                                  } // end placement of SBMM                 
                                  xbuf2=svtg.rsizemin+ssub.srollod/2;                 
                                  { AgPlacement place = AgPlacement("SBRL","SBSP");                    
                                        /// Add daughter volume SBRL to mother SBSP                    
                                        place.TranslateX(xbuf2*sin(degrad*phi));                    
                                        /// Translate x = xbuf2*sin(degrad*phi)                    
                                        place.TranslateY(-xbuf2*cos(degrad*phi));                    
                                        /// Translate y = -xbuf2*cos(degrad*phi)                    
                                        place.TranslateZ(ssub.sringthk/2+ssub.srollid/2);                    
                                        /// Translate z = ssub.sringthk/2+ssub.srollid/2                    
                                        place.AlphaZ(phi-90);                    
                                        /// Rotate: AlphaZ = phi-90                    
                                        /// G3 Reference: thetax = 90                    
                                        /// G3 Reference: phix = 0                    
                                        /// G3 Reference: thetay = 90                    
                                        /// G3 Reference: phiy = 90                    
                                        /// G3 Reference: thetaz = 0                    
                                        /// G3 Reference: phiz = 0                    
                                        /// G3 Reference: thetax = 90                    
                                        /// G3 Reference: phix = 0                    
                                        /// G3 Reference: thetay = 90                    
                                        /// G3 Reference: phiy = 90                    
                                        /// G3 Reference: thetaz = 0                    
                                        /// G3 Reference: phiz = 0                    
                                        place.Ortho( "ZXY" ); // ORT=ZXY                    
                                        /// Axis substitution: XYZ --> ZXY                    
                                        _stacker -> Position( AgBlock::Find("SBRL"), place );                    
                                  } // end placement of SBRL                 
                                  { AgPlacement place = AgPlacement("SBRX","SBSP");                    
                                        /// Add daughter volume SBRX to mother SBSP                    
                                        place.TranslateX(xbuf2*sin(degrad*phi));                    
                                        /// Translate x = xbuf2*sin(degrad*phi)                    
                                        place.TranslateY(-xbuf2*cos(degrad*phi));                    
                                        /// Translate y = -xbuf2*cos(degrad*phi)                    
                                        place.TranslateZ(ssub.sringthk/2+ssub.srollid/2);                    
                                        /// Translate z = ssub.sringthk/2+ssub.srollid/2                    
                                        place.AlphaZ(phi-90);                    
                                        /// Rotate: AlphaZ = phi-90                    
                                        /// G3 Reference: thetax = 90                    
                                        /// G3 Reference: phix = 0                    
                                        /// G3 Reference: thetay = 90                    
                                        /// G3 Reference: phiy = 90                    
                                        /// G3 Reference: thetaz = 0                    
                                        /// G3 Reference: phiz = 0                    
                                        /// G3 Reference: thetax = 90                    
                                        /// G3 Reference: phix = 0                    
                                        /// G3 Reference: thetay = 90                    
                                        /// G3 Reference: phiy = 90                    
                                        /// G3 Reference: thetaz = 0                    
                                        /// G3 Reference: phiz = 0                    
                                        place.Ortho( "ZXY" ); // ORT=ZXY                    
                                        /// Axis substitution: XYZ --> ZXY                    
                                        _stacker -> Position( AgBlock::Find("SBRX"), place );                    
                                  } // end placement of SBRX                 
                            }              
                      }           
                      _create = AgCreate("SBSR");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create SBSR              
                            Create("SBSR");               
                      }           
                      { AgPlacement place = AgPlacement("SBSR","SBSP");              
                            /// Add daughter volume SBSR to mother SBSP              
                            _stacker -> Position( AgBlock::Find("SBSR"), place );              
                      } // end placement of SBSR           
                      END_OF_SBSP:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SBSP     
          // ---------------------------------------------------------------------------------------------------     
          void SBMM::Block( AgCreate create )     
          {         
                ///@addtogroup SBMM_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("SBMM");              
                            attr.par("seen")=0;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=ssub.mblkiwid/2;              
                            shape.par("dy")=(ssub.kmountod/2-svtg.rsizemin-ssub.srollod)/2;              
                            shape.par("dz")=ssub.mblkhgh/2;              
                            /// Shape Bbox dx=ssub.mblkiwid/2 dy=(ssub.kmountod/2-svtg.rsizemin-ssub.srollod)/2 dz=ssub.mblkhgh/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SBMM;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("SMRD");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create SMRD              
                            Create("SMRD");               
                      }           
                      xbuf=-(ssub.mblkorad+ssub.mblkirad)/2+svtg.rsizemin+ssub.srollod;           
                      { AgPlacement place = AgPlacement("SMRD","SBMM");              
                            /// Add daughter volume SMRD to mother SBMM              
                            place.TranslateY(xbuf+(ssub.kmountod/2-svtg.rsizemin-ssub.srollod)/2);              
                            /// Translate y = xbuf+(ssub.kmountod/2-svtg.rsizemin-ssub.srollod)/2              
                            place.AlphaX(90);              
                            /// Rotate: AlphaX = 90              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            _stacker -> Position( AgBlock::Find("SMRD"), place );              
                      } // end placement of SMRD           
                      _create = AgCreate("SBMO");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create SBMO              
                            Create("SBMO");               
                      }           
                      xbuf=-ssub.mblkorad+svtg.rsizemin+ssub.srollod;           
                      { AgPlacement place = AgPlacement("SBMO","SBMM");              
                            /// Add daughter volume SBMO to mother SBMM              
                            place.TranslateY(xbuf+(ssub.kmountod/2-svtg.rsizemin-ssub.srollod)/2);              
                            /// Translate y = xbuf+(ssub.kmountod/2-svtg.rsizemin-ssub.srollod)/2              
                            _stacker -> Position( AgBlock::Find("SBMO"), place );              
                      } // end placement of SBMO           
                      _create = AgCreate("SBMI");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create SBMI              
                            Create("SBMI");               
                      }           
                      xbuf=-ssub.mblkirad+svtg.rsizemin+ssub.srollod;           
                      { AgPlacement place = AgPlacement("SBMI","SBMM");              
                            /// Add daughter volume SBMI to mother SBMM              
                            place.TranslateY(xbuf+(ssub.kmountod/2-svtg.rsizemin-ssub.srollod)/2);              
                            /// Translate y = xbuf+(ssub.kmountod/2-svtg.rsizemin-ssub.srollod)/2              
                            _stacker -> Position( AgBlock::Find("SBMI"), place );              
                      } // end placement of SBMI           
                      END_OF_SBMM:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SBMM     
          // ---------------------------------------------------------------------------------------------------     
          void SMRD::Block( AgCreate create )     
          {         
                ///@addtogroup SMRD_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Aluminium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Aluminium");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("SMRD");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=0.0;              
                            shape.par("rmax")=ssub.mroddia/2;              
                            shape.par("dz")=(ssub.mblkorad-ssub.mblkirad+ssub.mblkolen)/2;              
                            /// Shape Tube rmin=0.0 rmax=ssub.mroddia/2 dz=(ssub.mblkorad-ssub.mblkirad+ssub.mblkolen)/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SMRD;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_SMRD:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SMRD     
          // ---------------------------------------------------------------------------------------------------     
          void SBMO::Block( AgCreate create )     
          {         
                ///@addtogroup SBMO_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material G10            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("G10");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("SBMO");              
                            attr.par("seen")=1;              
                            attr.par("colo")=3;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=ssub.mblkowid/2;              
                            shape.par("dy")=ssub.mblkolen/2;              
                            shape.par("dz")=ssub.mblkhgh/2;              
                            /// Shape Bbox dx=ssub.mblkowid/2 dy=ssub.mblkolen/2 dz=ssub.mblkhgh/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SBMO;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_SBMO:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SBMO     
          // ---------------------------------------------------------------------------------------------------     
          void SBMI::Block( AgCreate create )     
          {         
                ///@addtogroup SBMI_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material G10            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("G10");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("SBMI");              
                            attr.par("seen")=1;              
                            attr.par("colo")=3;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=ssub.mblkiwid/2;              
                            shape.par("dy")=ssub.mblkilen/2;              
                            shape.par("dz")=ssub.mblkhgh/2;              
                            /// Shape Bbox dx=ssub.mblkiwid/2 dy=ssub.mblkilen/2 dz=ssub.mblkhgh/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SBMI;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_SBMI:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SBMI     
          // ---------------------------------------------------------------------------------------------------     
          void SAKM::Block( AgCreate create )     
          {         
                ///@addtogroup SAKM_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Aluminium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Aluminium");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("SAKM");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=ssub.kmountid/2;              
                            shape.par("rmax")=ssub.kmountod/2;              
                            shape.par("dz")=ssub.kmntthk/2;              
                            /// Shape Tube rmin=ssub.kmountid/2 rmax=ssub.kmountod/2 dz=ssub.kmntthk/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SAKM;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("SCKM");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create SCKM              
                            Create("SCKM");               
                      }           
                      { AgPlacement place = AgPlacement("SCKM","SAKM");              
                            /// Add daughter volume SCKM to mother SAKM              
                            place.TranslateY(ssub.kmcutoff);              
                            /// Translate y = ssub.kmcutoff              
                            place.par("only")=AgPlacement::kMany;              
                            /// Overlap: agplacement::kmany              
                            _stacker -> Position( AgBlock::Find("SCKM"), place );              
                      } // end placement of SCKM           
                      { AgPlacement place = AgPlacement("SCKM","SAKM");              
                            /// Add daughter volume SCKM to mother SAKM              
                            place.TranslateY(-ssub.kmcutoff);              
                            /// Translate y = -ssub.kmcutoff              
                            place.par("only")=AgPlacement::kMany;              
                            /// Overlap: agplacement::kmany              
                            place.AlphaZ(180);              
                            /// Rotate: AlphaZ = 180              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            _stacker -> Position( AgBlock::Find("SCKM"), place );              
                      } // end placement of SCKM           
                      END_OF_SAKM:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SAKM     
          // ---------------------------------------------------------------------------------------------------     
          void SCKM::Block( AgCreate create )     
          {         
                ///@addtogroup SCKM_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("SCKM");              
                            attr.par("seen")=1;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tubs");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=ssub.kmcutid/2;              
                            shape.par("rmax")=ssub.kmcutod/2;              
                            shape.par("phi1")=270-ssub.kmcutoa;              
                            shape.par("phi2")=270+ssub.kmcutoa;              
                            shape.par("dz")=ssub.kmntthk/2;              
                            /// Shape Tubs rmin=ssub.kmcutid/2 rmax=ssub.kmcutod/2 phi1=270-ssub.kmcutoa phi2=270+ssub.kmcutoa dz=ssub.kmntthk/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SCKM;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_SCKM:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SCKM     
          // ---------------------------------------------------------------------------------------------------     
          void SBRL::Block( AgCreate create )     
          {         
                ///@addtogroup SBRL_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material PYREX a=20.719 z=10.307 dens=2.23 absl=50.7 radl=12.6            
                      { AgMaterial &mat = AgMaterial::Get("Pyrex");              
                            mat.par("a")=20.719;              
                            mat.par("z")=10.307;              
                            mat.par("dens")=2.23;              
                            mat.par("absl")=50.7;              
                            mat.par("radl")=12.6;              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("SBRL");              
                            attr.par("seen")=1;              
                            attr.par("colo")=6;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=ssub.srollid/2;              
                            shape.par("rmax")=ssub.srollod/2;              
                            /// Shape Tube rmin=ssub.srollid/2 rmax=ssub.srollod/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SBRL;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_SBRL:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SBRL     
          // ---------------------------------------------------------------------------------------------------     
          void SBRX::Block( AgCreate create )     
          {         
                ///@addtogroup SBRX_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Iron            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Iron");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("SBRX");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=0.0;              
                            shape.par("rmax")=ssub.srollid/2;              
                            shape.par("dz")=ssub.swirelen/2;              
                            /// Shape Tube rmin=0.0 rmax=ssub.srollid/2 dz=ssub.swirelen/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SBRX;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_SBRX:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SBRX     
          // ---------------------------------------------------------------------------------------------------     
          void SBSR::Block( AgCreate create )     
          {         
                ///@addtogroup SBSR_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material G10            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("G10");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("SBSR");              
                            attr.par("seen")=1;              
                            attr.par("colo")=3;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=ssub.sringid/2;              
                            shape.par("rmax")=ssub.sringod/2;              
                            shape.par("dz")=ssub.sringthk/2;              
                            /// Shape Tube rmin=ssub.sringid/2 rmax=ssub.sringod/2 dz=ssub.sringthk/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SBSR;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("SBCR");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create SBCR              
                            Create("SBCR");               
                      }           
                      xbuf=ssub.srcutin+(ssub.srcutout-ssub.srcutin)/2;           
                      /// Loop on i from -1 to 1 step=2           
                      for ( i=-1; (2>0)? (i<=1):(i>=1); i+=2 )           
                      {              
                            /// Loop on j from 0 to 1 step=1              
                            for ( j=0; (1>0)? (j<=1):(j>=1); j+=1 )              
                            {                 
                                  phi=i*ssub.srcutphi+180*j;                 
                                  { AgPlacement place = AgPlacement("SBCR","SBSR");                    
                                        /// Add daughter volume SBCR to mother SBSR                    
                                        place.TranslateX(xbuf*sin(degrad*phi));                    
                                        /// Translate x = xbuf*sin(degrad*phi)                    
                                        place.TranslateY(-xbuf*cos(degrad*phi));                    
                                        /// Translate y = -xbuf*cos(degrad*phi)                    
                                        place.par("only")=AgPlacement::kMany;                    
                                        /// Overlap: agplacement::kmany                    
                                        place.AlphaZ(phi);                    
                                        /// Rotate: AlphaZ = phi                    
                                        /// G3 Reference: thetax = 90                    
                                        /// G3 Reference: phix = 0                    
                                        /// G3 Reference: thetay = 90                    
                                        /// G3 Reference: phiy = 90                    
                                        /// G3 Reference: thetaz = 0                    
                                        /// G3 Reference: phiz = 0                    
                                        _stacker -> Position( AgBlock::Find("SBCR"), place );                    
                                  } // end placement of SBCR                 
                            }              
                      }           
                      END_OF_SBSR:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SBSR     
          // ---------------------------------------------------------------------------------------------------     
          void SBCR::Block( AgCreate create )     
          {         
                ///@addtogroup SBCR_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("SBCR");              
                            attr.par("seen")=1;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=ssub.srcutwid/2;              
                            shape.par("dy")=(ssub.srcutout-ssub.srcutin)/2;              
                            shape.par("dz")=ssub.sringthk/2;              
                            /// Shape Bbox dx=ssub.srcutwid/2 dy=(ssub.srcutout-ssub.srcutin)/2 dz=ssub.sringthk/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SBCR;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_SBCR:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SBCR     
          // ---------------------------------------------------------------------------------------------------     
          void SCMY::Block( AgCreate create )     
          {         
                ///@addtogroup SCMY_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      roffset=ssup.grphthk+ssup.cabthk+2.0*ssup.hosrmx;           
                      /// Component C5	a=12	z=6	w=5           
                      /// Component H4	a=1	z=1	w=4           
                      /// Component O2	a=16	z=8	w=2           
                      /// Mixture Mylar dens=1.39           
                      {  AgMaterial &mix = AgMaterial::Get("Mylar");              
                            mix.Component("C5",12,6,5);              
                            mix.Component("H4",1,1,4);              
                            mix.Component("O2",16,8,2);              
                            mix.par("dens")=1.39;              
                            mix.lock();              
                            _material = mix;              
                            _material.lock();              
                      }           
                      { AgAttribute attr = AgAttribute("SCMY");              
                            attr.par("seen")=1;              
                            attr.par("colo")=3;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Pcon");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("phi1")=0;              
                            shape.par("dphi")=360;              
                            shape.par("nz")=3;              
                            shape.Z(0)=ssup.rodlen/2+ssup.grphthk;              
                            shape.Z(1)=ssup.cone3zmx;              
                            shape.Z(2)=ssup.cone4zmx;              
                            shape.Rmin(0)=ssup.con3idmn+roffset;              
                            shape.Rmin(1)=ssup.con4idmn+roffset;              
                            shape.Rmin(2)=ssup.con4idmx+roffset;              
                            shape.Rmax(0)=ssup.con3idmn+roffset+ssup.wrpmythk;              
                            shape.Rmax(1)=ssup.con4idmn+roffset+ssup.wrpmythk;              
                            shape.Rmax(2)=ssup.con4idmx+roffset+ssup.wrpmythk;              
                            /// Shape Pcon phi1=0 dphi=360 nz=3               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SCMY;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_SCMY:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SCMY     
          // ---------------------------------------------------------------------------------------------------     
          void SROD::Block( AgCreate create )     
          {         
                ///@addtogroup SROD_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Carbon            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Carbon");              
                            _material = mat;              
                      }           
                      if ( ssup.version>=4.00 )           
                      {              
                            /// Material CarbonFiber dens=ssup_carbondens               
                            { AgMaterial &mat = AgMaterial::Get("Carbonfiber");                 
                                  mat.par("dens")=ssup.carbondens;                 
                                  _material = mat;                 
                            }              
                      }           
                      { AgAttribute attr = AgAttribute("SROD");              
                            attr.par("seen")=1;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Eltu");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("p1")=ssup.rododx/2;              
                            shape.par("p2")=ssup.rodod/2;              
                            shape.par("dz")=ssup.rodlen/2;              
                            /// Shape Eltu p1=ssup.rododx/2 p2=ssup.rodod/2 dz=ssup.rodlen/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SROD;              
                            _stacker -> Build(this);              
                      }           
                      if ( ssup.version>=4.00 )           
                      {              
                            _create = AgCreate("SRON");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create SRON                 
                                  Create("SRON");                  
                            }              
                            { AgPlacement place = AgPlacement("SRON","SROD");                 
                                  /// Add daughter volume SRON to mother SROD                 
                                  _stacker -> Position( AgBlock::Find("SRON"), place );                 
                            } // end placement of SRON              
                      }           
                      else           
                      {              
                            _create = AgCreate("SROH");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create SROH                 
                                  Create("SROH");                  
                            }              
                            { AgPlacement place = AgPlacement("SROH","SROD");                 
                                  /// Add daughter volume SROH to mother SROD                 
                                  _stacker -> Position( AgBlock::Find("SROH"), place );                 
                            } // end placement of SROH              
                      }           
                      END_OF_SROD:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SROD     
          // ---------------------------------------------------------------------------------------------------     
          void SRON::Block( AgCreate create )     
          {         
                ///@addtogroup SRON_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Component C	a=12	z=6	w=5           
                      /// Component H	a=1	z=1	w=8           
                      /// Component O	a=16	z=8	w=2           
                      /// Mixture Nomex dens=ssup_nomexdens           
                      {  AgMaterial &mix = AgMaterial::Get("Nomex");              
                            mix.Component("C",12,6,5);              
                            mix.Component("H",1,1,8);              
                            mix.Component("O",16,8,2);              
                            mix.par("dens")=ssup.nomexdens;              
                            mix.lock();              
                            _material = mix;              
                            _material.lock();              
                      }           
                      { AgAttribute attr = AgAttribute("SRON");              
                            attr.par("seen")=1;              
                            attr.par("colo")=5;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Eltu");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("p1")=ssup.rododx/2-ssup.carbonshell;              
                            shape.par("p2")=ssup.rodod/2-ssup.carbonshell;              
                            /// Shape Eltu p1=ssup.rododx/2-ssup.carbonshell p2=ssup.rodod/2-ssup.carbonshell               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SRON;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("SROI");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create SROI              
                            Create("SROI");               
                      }           
                      { AgPlacement place = AgPlacement("SROI","SRON");              
                            /// Add daughter volume SROI to mother SRON              
                            _stacker -> Position( AgBlock::Find("SROI"), place );              
                      } // end placement of SROI           
                      END_OF_SRON:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SRON     
          // ---------------------------------------------------------------------------------------------------     
          void SROI::Block( AgCreate create )     
          {         
                ///@addtogroup SROI_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material CarbonFiber            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Carbonfiber");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("SROI");              
                            attr.par("seen")=1;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Eltu");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("p1")=ssup.rodidx/2+ssup.carbonshell;              
                            shape.par("p2")=ssup.rodid/2+ssup.carbonshell;              
                            /// Shape Eltu p1=ssup.rodidx/2+ssup.carbonshell p2=ssup.rodid/2+ssup.carbonshell               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SROI;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("SROH");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create SROH              
                            Create("SROH");               
                      }           
                      { AgPlacement place = AgPlacement("SROH","SROI");              
                            /// Add daughter volume SROH to mother SROI              
                            _stacker -> Position( AgBlock::Find("SROH"), place );              
                      } // end placement of SROH           
                      END_OF_SROI:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SROI     
          // ---------------------------------------------------------------------------------------------------     
          void SROH::Block( AgCreate create )     
          {         
                ///@addtogroup SROH_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("SROH");              
                            attr.par("seen")=1;              
                            attr.par("colo")=3;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Eltu");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("p1")=ssup.rodidx/2;              
                            shape.par("p2")=ssup.rodid/2;              
                            shape.par("dz")=ssup.rodlen/2;              
                            /// Shape Eltu p1=ssup.rodidx/2 p2=ssup.rodid/2 dz=ssup.rodlen/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SROH;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_SROH:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SROH     
    // ----------------------------------------------------------------------- geoctr
       void SconGeo::ConstructGeometry( const Char_t *dummy )     
       {        
             ///@addtogroup SconGeo_revision        
             ///@{           
                   /// Author: Victor Perev           
             ///@}        
             ///@addtogroup SconGeo_revision        
             ///@{           
                   /// Created:  31 Oct 2007            
             ///@}        
             AddBlock("SCON");        
             AddBlock("SCMY");        
             AddBlock("SGRA");        
             AddBlock("SBSP");        
             AddBlock("SAKM");        
             AddBlock("SCKM");        
             AddBlock("SBMM");        
             AddBlock("SBMI");        
             AddBlock("SBMO");        
             AddBlock("SMRD");        
             AddBlock("SBRL");        
             AddBlock("SBRX");        
             AddBlock("SBSR");        
             AddBlock("SBCR");        
             AddBlock("SROD");        
             AddBlock("SROH");        
             AddBlock("SRON");        
             AddBlock("SROI");        
             shield_phi(1)=9.0;        
             shield_phi(2)=27.0;        
             shield_phi(3)=45.0;        
             shield_phi(4)=63.0;        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup svtg_doc        
             ///@{           
                   ++svtg._index;           
                   svtg . version = 2; //  geometry version            
                   /// svtg . version = 2; //  geometry version            
                   svtg . nlayer = 7; //  number of svt layers (was 7)            
                   /// svtg . nlayer = 7; //  number of svt layers (was 7)            
                   svtg . rsizemin = 4.100; //  STV innermost radius            
                   /// svtg . rsizemin = 4.100; //  STV innermost radius            
                   svtg . rsizemax = 46.107; //  STV outermost radius            
                   /// svtg . rsizemax = 46.107; //  STV outermost radius            
                   svtg . zsizemax = 270; //  SVT+FTPC length            
                   /// svtg . zsizemax = 270; //  SVT+FTPC length            
                   svtg . angoff = 0; //  angular offset x1 for slayer 2 x2 for slayer 3            
                   /// svtg . angoff = 0; //  angular offset x1 for slayer 2 x2 for slayer 3            
                   svtg . supportver = 1; //  versioning of the shield            
                   /// svtg . supportver = 1; //  versioning of the shield            
                   svtg . conever = 1; //  versioning of the support cone            
                   /// svtg . conever = 1; //  versioning of the support cone            
                   svtg . ifmany = 0; //  whether we use the geant MANY option            
                   /// svtg . ifmany = 0; //  whether we use the geant MANY option            
                   svtg . nmin = 1; //  the index of the innermost layer            
                   /// svtg . nmin = 1; //  the index of the innermost layer            
                   //           
                   svtg.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup ssup_doc        
             ///@{           
                   ++ssup._index;           
                   ssup . version = 1; //  geometry version            
                   /// ssup . version = 1; //  geometry version            
                   ssup . cabthk = 0.05; //  thickness of layer of cables on support cone            
                   /// ssup . cabthk = 0.05; //  thickness of layer of cables on support cone            
                   ssup . hosrmn = 0.75; //  inner radius of water hoses on support cone            
                   /// ssup . hosrmn = 0.75; //  inner radius of water hoses on support cone            
                   ssup . hosrmx = 0.95; //  outer radius of water hoses on support cone            
                   /// ssup . hosrmx = 0.95; //  outer radius of water hoses on support cone            
                   ssup . nhoses = 10; //  number of water hoses            
                   /// ssup . nhoses = 10; //  number of water hoses            
                   ssup . wrpmythk = 0.10; //  thickness of mylar wrap around cone (guess)            
                   /// ssup . wrpmythk = 0.10; //  thickness of mylar wrap around cone (guess)            
                   ssup . wrpalthk = 0.01; //  thickness of Al on mylar wrap (guess)            
                   /// ssup . wrpalthk = 0.01; //  thickness of Al on mylar wrap (guess)            
                   ssup . grphthk = 0.16; //  support cone thickness            
                   /// ssup . grphthk = 0.16; //  support cone thickness            
                   ssup . cone1zmn = 52.23; //  Cone z min (parts 1,2,3,4 in increasing z)            
                   /// ssup . cone1zmn = 52.23; //  Cone z min (parts 1,2,3,4 in increasing z)            
                   ssup . rodlen = 110.8; //  Length of support rods            
                   /// ssup . rodlen = 110.8; //  Length of support rods            
                   ssup . roddist = 17.5; //  Distance of support rod od from beam axis            
                   /// ssup . roddist = 17.5; //  Distance of support rod od from beam axis            
                   ssup . rodid = 2.5; //  ID of Carbon support rods (approx)            
                   /// ssup . rodid = 2.5; //  ID of Carbon support rods (approx)            
                   ssup . rodod = 3.05; //  OD of Carbon support rods (approx)            
                   /// ssup . rodod = 3.05; //  OD of Carbon support rods (approx)            
                   ssup . con1idmn = 15.67; //  Minimum id of cone 1            
                   /// ssup . con1idmn = 15.67; //  Minimum id of cone 1            
                   ssup . con3idmn = 21.67; //  Minimum id of cone 3 (TBD)            
                   /// ssup . con3idmn = 21.67; //  Minimum id of cone 3 (TBD)            
                   ssup . con4idmn = 37.4; //  Minimum id of cone 4 (TBD)            
                   /// ssup . con4idmn = 37.4; //  Minimum id of cone 4 (TBD)            
                   ssup . con4idmx = 37.4; //  Maximum id of cone 4 (TBD)            
                   /// ssup . con4idmx = 37.4; //  Maximum id of cone 4 (TBD)            
                   ssup . cone3zmx = 150.0; //  Maximum z of cone 3 (TBD)            
                   /// ssup . cone3zmx = 150.0; //  Maximum z of cone 3 (TBD)            
                   ssup . cone4zmx = 229.36; //  Maximum z of cone 4 (TBD)            
                   /// ssup . cone4zmx = 229.36; //  Maximum z of cone 4 (TBD)            
                   ssup . brathk = .2; //  thickness of Al brackets            
                   /// ssup . brathk = .2; //  thickness of Al brackets            
                   ssup . erjthk = .1; //  (z) thickness of end ring joining brackets            
                   /// ssup . erjthk = .1; //  (z) thickness of end ring joining brackets            
                   ssup . erjwid = 2.07; //  (azimuthal) width of end ring joining brackets            
                   /// ssup . erjwid = 2.07; //  (azimuthal) width of end ring joining brackets            
                   ssup . erjlen = 5.19; //  (radial) length of end ring joining brackets            
                   /// ssup . erjlen = 5.19; //  (radial) length of end ring joining brackets            
                   ssup . erjzdis = 2.0; //  dist from ladder ends to ERJ (guess)            
                   /// ssup . erjzdis = 2.0; //  dist from ladder ends to ERJ (guess)            
                   ssup . erj1x = 0.31; //  ERJ screw 1 x position (radial)            
                   /// ssup . erj1x = 0.31; //  ERJ screw 1 x position (radial)            
                   ssup . erj2x = 1.15; //  ERJ screw 2 x position            
                   /// ssup . erj2x = 1.15; //  ERJ screw 2 x position            
                   ssup . erj2y = 0.72; //  ERJ screw 2 y position            
                   /// ssup . erj2y = 0.72; //  ERJ screw 2 y position            
                   ssup . erjrad = 10.80; //  distance of ERJ center from beam axis            
                   /// ssup . erjrad = 10.80; //  distance of ERJ center from beam axis            
                   ssup . erjdia = 0.17; //  ERJ screw diameter            
                   /// ssup . erjdia = 0.17; //  ERJ screw diameter            
                   //           
                   ssup.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup ssup_doc        
             ///@{           
                   ++ssup._index;           
                   ssup . version = 2; //  geometry version            
                   /// ssup . version = 2; //  geometry version            
                   ssup . cabthk = 0.21; //  thickness of layer of cables on support cone            
                   /// ssup . cabthk = 0.21; //  thickness of layer of cables on support cone            
                   ssup . hosrmn = 0.75; //  inner radius of water hoses on support cone            
                   /// ssup . hosrmn = 0.75; //  inner radius of water hoses on support cone            
                   ssup . hosrmx = 0.95; //  outer radius of water hoses on support cone            
                   /// ssup . hosrmx = 0.95; //  outer radius of water hoses on support cone            
                   ssup . nhoses = 10; //  number of water hoses            
                   /// ssup . nhoses = 10; //  number of water hoses            
                   ssup . wrpmythk = 0.10; //  thickness of mylar wrap around cone (guess)            
                   /// ssup . wrpmythk = 0.10; //  thickness of mylar wrap around cone (guess)            
                   ssup . wrpalthk = 0.01; //  thickness of Al on mylar wrap (guess)            
                   /// ssup . wrpalthk = 0.01; //  thickness of Al on mylar wrap (guess)            
                   ssup . grphthk = 0.16; //  support cone thickness            
                   /// ssup . grphthk = 0.16; //  support cone thickness            
                   ssup . cone1zmn = 52.23; //  Cone z min (parts 1,2,3,4 in increasing z)            
                   /// ssup . cone1zmn = 52.23; //  Cone z min (parts 1,2,3,4 in increasing z)            
                   ssup . rodlen = 110.8; //  Length of support rods            
                   /// ssup . rodlen = 110.8; //  Length of support rods            
                   ssup . roddist = 17.5; //  Distance of support rod od from beam axis            
                   /// ssup . roddist = 17.5; //  Distance of support rod od from beam axis            
                   ssup . rodid = 2.5; //  ID of Carbon support rods (approx)            
                   /// ssup . rodid = 2.5; //  ID of Carbon support rods (approx)            
                   ssup . rodod = 3.05; //  OD of Carbon support rods (approx)            
                   /// ssup . rodod = 3.05; //  OD of Carbon support rods (approx)            
                   ssup . con1idmn = 15.67; //  Minimum id of cone 1            
                   /// ssup . con1idmn = 15.67; //  Minimum id of cone 1            
                   ssup . con3idmn = 21.67; //  Minimum id of cone 3 (TBD)            
                   /// ssup . con3idmn = 21.67; //  Minimum id of cone 3 (TBD)            
                   ssup . con4idmn = 37.4; //  Minimum id of cone 4 (TBD)            
                   /// ssup . con4idmn = 37.4; //  Minimum id of cone 4 (TBD)            
                   ssup . con4idmx = 37.4; //  Maximum id of cone 4 (TBD)            
                   /// ssup . con4idmx = 37.4; //  Maximum id of cone 4 (TBD)            
                   ssup . cone3zmx = 150.0; //  Maximum z of cone 3 (TBD)            
                   /// ssup . cone3zmx = 150.0; //  Maximum z of cone 3 (TBD)            
                   ssup . cone4zmx = 229.36; //  Maximum z of cone 4 (TBD)            
                   /// ssup . cone4zmx = 229.36; //  Maximum z of cone 4 (TBD)            
                   ssup . brathk = .2; //  thickness of Al brackets            
                   /// ssup . brathk = .2; //  thickness of Al brackets            
                   ssup . erjthk = .1; //  (z) thickness of end ring joining brackets            
                   /// ssup . erjthk = .1; //  (z) thickness of end ring joining brackets            
                   ssup . erjwid = 2.07; //  (azimuthal) width of end ring joining brackets            
                   /// ssup . erjwid = 2.07; //  (azimuthal) width of end ring joining brackets            
                   ssup . erjlen = 5.19; //  (radial) length of end ring joining brackets            
                   /// ssup . erjlen = 5.19; //  (radial) length of end ring joining brackets            
                   ssup . erjzdis = 2.0; //  dist from ladder ends to ERJ (guess)            
                   /// ssup . erjzdis = 2.0; //  dist from ladder ends to ERJ (guess)            
                   ssup . erj1x = 0.31; //  ERJ screw 1 x position (radial)            
                   /// ssup . erj1x = 0.31; //  ERJ screw 1 x position (radial)            
                   ssup . erj2x = 1.15; //  ERJ screw 2 x position            
                   /// ssup . erj2x = 1.15; //  ERJ screw 2 x position            
                   ssup . erj2y = 0.72; //  ERJ screw 2 y position            
                   /// ssup . erj2y = 0.72; //  ERJ screw 2 y position            
                   ssup . erjrad = 10.80; //  distance of ERJ center from beam axis            
                   /// ssup . erjrad = 10.80; //  distance of ERJ center from beam axis            
                   ssup . erjdia = 0.17; //  ERJ screw diameter            
                   /// ssup . erjdia = 0.17; //  ERJ screw diameter            
                   //           
                   ssup.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup ssup_doc        
             ///@{           
                   ++ssup._index;           
                   ssup . version = 3; //  geometry version            
                   /// ssup . version = 3; //  geometry version            
                   ssup . rodlen = 110.8; //  Length of support rods            
                   /// ssup . rodlen = 110.8; //  Length of support rods            
                   ssup . roddist = 17.5; //  Distance of support rod od from beam axis            
                   /// ssup . roddist = 17.5; //  Distance of support rod od from beam axis            
                   ssup . rodid = 3.64; //  ID of Carbon support rods             
                   /// ssup . rodid = 3.64; //  ID of Carbon support rods             
                   ssup . rodod = 4.50; //  OD of Carbon support rods             
                   /// ssup . rodod = 4.50; //  OD of Carbon support rods             
                   ssup . rodidx = 8.72; //  ID of Carbon support rods             
                   /// ssup . rodidx = 8.72; //  ID of Carbon support rods             
                   ssup . rododx = 9.58; //  OD of Carbon support rods             
                   /// ssup . rododx = 9.58; //  OD of Carbon support rods             
                   //           
                   ssup.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup ssup_doc        
             ///@{           
                   ++ssup._index;           
                   ssup . version = 4; //  geometry version            
                   /// ssup . version = 4; //  geometry version            
                   ssup . carbonshell = 0.04; //  0.4mm carbon fiber shell            
                   /// ssup . carbonshell = 0.04; //  0.4mm carbon fiber shell            
                   ssup . carbondens = 1.78; //  1.78 g/cm^3 is a typical carbon composite density            
                   /// ssup . carbondens = 1.78; //  1.78 g/cm^3 is a typical carbon composite density            
                   ssup . nomexdens = 0.048; //  Ballpark figure            
                   /// ssup . nomexdens = 0.048; //  Ballpark figure            
                   //           
                   ssup.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup ssub_doc        
             ///@{           
                   ++ssub._index;           
                   ssub . version = 1; //  geometry version            
                   /// ssub . version = 1; //  geometry version            
                   ssub . kmountid = 31.34; //  id of beampipe support kinematic mount            
                   /// ssub . kmountid = 31.34; //  id of beampipe support kinematic mount            
                   ssub . kmountod = 38.96; //  od of beampipe support kinematic mount            
                   /// ssub . kmountod = 38.96; //  od of beampipe support kinematic mount            
                   ssub . kmntthk = 0.64; //  thickness of support kinematic mount            
                   /// ssub . kmntthk = 0.64; //  thickness of support kinematic mount            
                   ssub . kmcutod = 18.31; //  od of cutout in kinematic mount            
                   /// ssub . kmcutod = 18.31; //  od of cutout in kinematic mount            
                   ssub . kmcutid = 14; //  id of cutout in kinematic mount            
                   /// ssub . kmcutid = 14; //  id of cutout in kinematic mount            
                   ssub . kmcutoa = 38; //  opening angle of cutout            
                   /// ssub . kmcutoa = 38; //  opening angle of cutout            
                   ssub . kmcutoff = 26.58; //  offset of cutout center from axis            
                   /// ssub . kmcutoff = 26.58; //  offset of cutout center from axis            
                   ssub . sringid = 8.47; //  id of beampipe support ring            
                   /// ssub . sringid = 8.47; //  id of beampipe support ring            
                   ssub . sringod = 12.78; //  od of beampipe support ring            
                   /// ssub . sringod = 12.78; //  od of beampipe support ring            
                   ssub . sringthk = 0.64; //  thichkness of beampipe support ring            
                   /// ssub . sringthk = 0.64; //  thichkness of beampipe support ring            
                   ssub . srcutphi = 38; //  support ring cutout angle to z-axis            
                   /// ssub . srcutphi = 38; //  support ring cutout angle to z-axis            
                   ssub . srcutwid = 3.63; //  support ring cutout width            
                   /// ssub . srcutwid = 3.63; //  support ring cutout width            
                   ssub . srcutout = 5.08; //  support ring cutout depth            
                   /// ssub . srcutout = 5.08; //  support ring cutout depth            
                   ssub . srcutin = 3.5; //  support ring cutout start            
                   /// ssub . srcutin = 3.5; //  support ring cutout start            
                   ssub . srollid = 0.2; //  support roller Id            
                   /// ssub . srollid = 0.2; //  support roller Id            
                   ssub . srollod = 0.62; //  support roller Od            
                   /// ssub . srollod = 0.62; //  support roller Od            
                   ssub . srolllen = 2.54; //  support roller length            
                   /// ssub . srolllen = 2.54; //  support roller length            
                   ssub . swirelen = 5.08; //  support roller axis length            
                   /// ssub . swirelen = 5.08; //  support roller axis length            
                   ssub . mblkhgh = 0.97; //  mounting block height            
                   /// ssub . mblkhgh = 0.97; //  mounting block height            
                   ssub . mblkowid = 2.54; //  outer mounting block width            
                   /// ssub . mblkowid = 2.54; //  outer mounting block width            
                   ssub . mblkolen = 1.27; //  outer mounting block length            
                   /// ssub . mblkolen = 1.27; //  outer mounting block length            
                   ssub . mblkiwid = 3.175; //  inner mounting block width            
                   /// ssub . mblkiwid = 3.175; //  inner mounting block width            
                   ssub . mblkilen = 1.27; //  inner mounting block length            
                   /// ssub . mblkilen = 1.27; //  inner mounting block length            
                   ssub . mblkorad = 17.4; //  outer mounting block at radius            
                   /// ssub . mblkorad = 17.4; //  outer mounting block at radius            
                   ssub . mblkirad = 5.42; //  inner mounting block at radius            
                   /// ssub . mblkirad = 5.42; //  inner mounting block at radius            
                   ssub . mroddia = 0.32; //  mounting support rod diameter            
                   /// ssub . mroddia = 0.32; //  mounting support rod diameter            
                   //           
                   ssub.fill();           
             ///@}        
             //        
             /// Loop on i from 1 to 4 step=1        
             for ( i=1; (1>0)? (i<=4):(i>=4); i+=1 )        
             {           
                   rad_cones_in(i) = 8.5+2.60*(i-1);           
                   rad_cones_out(i)=15.0+0.85*(i-1);           
             }        
             /// USE svtg _index=1;        
             svtg.Use();        
             /// USE ssup version=svtg.conever ;        
             ssup.Use("version",(Float_t)svtg.conever );        
             /// USE ssub _index=1;        
             ssub.Use();        
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
             _create = AgCreate("SCON");        
             {           
                   AgShape myshape; // undefined shape           
                   ///Create SCON           
                   Create("SCON");            
             }        
             { AgPlacement place = AgPlacement("SCON","CAVE");           
                   /// Add daughter volume SCON to mother CAVE           
                   _stacker -> Position( AgBlock::Find("SCON"), place );           
             } // end placement of SCON        
             _create = AgCreate("SCON");        
             {           
                   AgShape myshape; // undefined shape           
                   ///Create SCON           
                   Create("SCON");            
             }        
             { AgPlacement place = AgPlacement("SCON","CAVE");           
                   /// Add daughter volume SCON to mother CAVE           
                   /// G3 Reference: thetax = 90           
                   /// G3 Reference: phix = 0           
                   /// G3 Reference: thetay = 90           
                   /// G3 Reference: phiy = 90           
                   /// G3 Reference: thetaz = 180           
                   /// G3 Reference: phiz = 0           
                   Double_t _thetax=90,_phix=0,_thetay=90,_phiy=90,_thetaz=180,_phiz=0;           
                   place.Reference( _thetax, _phix, _thetay, _phiy, _thetaz, _phiz );           
                   _stacker -> Position( AgBlock::Find("SCON"), place );           
             } // end placement of SCON        
             _create = AgCreate("SROD");        
             {           
                   AgShape myshape; // undefined shape           
                   ///Create SROD           
                   Create("SROD");            
             }        
             { AgPlacement place = AgPlacement("SROD","CAVE");           
                   /// Add daughter volume SROD to mother CAVE           
                   place.TranslateY(ssup.roddist+ssup.rodod/2);           
                   /// Translate y = ssup.roddist+ssup.rodod/2           
                   _stacker -> Position( AgBlock::Find("SROD"), place );           
             } // end placement of SROD        
             { AgPlacement place = AgPlacement("SROD","CAVE");           
                   /// Add daughter volume SROD to mother CAVE           
                   place.TranslateY(-ssup.roddist-ssup.rodod/2);           
                   /// Translate y = -ssup.roddist-ssup.rodod/2           
                   _stacker -> Position( AgBlock::Find("SROD"), place );           
             } // end placement of SROD        
             _create = AgCreate("SBSP");        
             {           
                   AgShape myshape; // undefined shape           
                   ///Create SBSP           
                   Create("SBSP");            
             }        
             { AgPlacement place = AgPlacement("SBSP","CAVE");           
                   /// Add daughter volume SBSP to mother CAVE           
                   place.TranslateZ((ssup.rodlen/2-ssub.kmntthk/2));           
                   /// Translate z = (ssup.rodlen/2-ssub.kmntthk/2)           
                   _stacker -> Position( AgBlock::Find("SBSP"), place );           
             } // end placement of SBSP        
             _create = AgCreate("SBSP");        
             {           
                   AgShape myshape; // undefined shape           
                   ///Create SBSP           
                   Create("SBSP");            
             }        
             { AgPlacement place = AgPlacement("SBSP","CAVE");           
                   /// Add daughter volume SBSP to mother CAVE           
                   place.TranslateZ(-(ssup.rodlen/2-ssub.kmntthk/2));           
                   /// Translate z = -(ssup.rodlen/2-ssub.kmntthk/2)           
                   /// G3 Reference: thetax = 90           
                   /// G3 Reference: phix = 0           
                   /// G3 Reference: thetay = 90           
                   /// G3 Reference: phiy = 90           
                   /// G3 Reference: thetaz = 180           
                   /// G3 Reference: phiz = 0           
                   Double_t _thetax=90,_phix=0,_thetay=90,_phiy=90,_thetaz=180,_phiz=0;           
                   place.Reference( _thetax, _phix, _thetay, _phiy, _thetaz, _phiz );           
                   _stacker -> Position( AgBlock::Find("SBSP"), place );           
             } // end placement of SBSP        
       }; // SconGeo     
 }; // namespace SconGeo  
 
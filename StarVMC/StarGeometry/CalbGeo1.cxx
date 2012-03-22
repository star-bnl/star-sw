#include "CalbGeo1.h"  
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
 namespace CALBGEO1 // $NMSPC  
 {     
       //     
       // ---------------------------------------------------------------------------------------------------     
          //  -----------------------------------------------------     
          /// @defgroup calg_doc     
          /// \class Calg_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t version;     
          ///Float_t rmin;     
          ///Float_t etacut;     
          ///Float_t crackwd;     
          ///Float_t frontthk;     
          ///Float_t compthk;     
          ///Float_t airthk;     
          ///Float_t backthk;     
          ///Float_t spacethk;     
          ///Array_t<Float_t> scintthk;     
          ///Float_t absorthk;     
          ///Float_t abpapthk;     
          ///Float_t g10sbthk;     
          ///Float_t smalfwdh;     
          ///Float_t smalfthk;     
          ///Float_t smgasthk;     
          ///Float_t smgaswdh;     
          ///Float_t smgasrad;     
          ///Float_t smaffwdh;     
          ///Float_t smafbwdh;     
          ///Float_t smetawdh;     
          ///Float_t seta1wdh;     
          ///Float_t netfirst;     
          ///Float_t seta2wdh;     
          ///Float_t netsecon;     
          ///Float_t set12wdh;     
          ///Float_t sphiwdh;     
          ///Float_t sphidwdh;     
          ///Float_t nphistr;     
          ///Float_t nsmdalw;     
          ///Float_t nsuper;     
          ///Float_t nsmd;     
          ///Array_t<Float_t> nsublay;     
          ///Array_t<Float_t> nmodule;     
          ///Array_t<Float_t> shift;     
          ///Float_t maxmodule;     
          ///Float_t netat;     
          ///Float_t nsub;     
          ///Float_t netasmdp;     
          ///Array_t<Float_t> modmap;     
          ///Int_t _index;     
          //     
          Calg_t calg;     
          //     
          //  -----------------------------------------------------     
          /// @defgroup calr_doc     
          /// \class Calr_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t rmin;     
          ///Float_t rprs;     
          ///Float_t rsmd1;     
          ///Float_t rsmd2;     
          ///Float_t rmax;     
          ///Int_t _index;     
          //     
          Calr_t calr;     
          //     
          ///@addtogroup CalbGeo1_vars     
          ///@{        
                Float_t rkb2sc=0.013,rkb3sc=9.6e-6;        
                //        
                /// Float_t rkb2sc=0.013,rkb3sc=9.6e-6        
          ///@}     
          ///@addtogroup CalbGeo1_vars     
          ///@{        
                Float_t current_depth,current,tan_theta,smd_width,smd_width1,smd_width2,smd_width3,cut_length,cut_radius,future_depth,c_dep,c_lead_dep,eta_lenght,current_csda,h_eta1,h_eta2,h_phi1,h_phi2,sh_eta1,sh_eta2,sh_phi1,sh_phi2,rmax,hleng,deta,angular_offset,dphitot,dphimod,dphit,r1,r2,r3,r4;        
                //        
                /// Float_t current_depth,current,tan_theta,smd_width,smd_width1,smd_width2,smd_width3,cut_length,cut_radius,future_depth,c_dep,c_lead_dep,eta_lenght,current_csda,h_eta1,h_eta2,h_phi1,h_phi2,sh_eta1,sh_eta2,sh_phi1,sh_phi2,rmax,hleng,deta,angular_offset,dphitot,dphimod,dphit,r1,r2,r3,r4        
                Array_t<Float_t> layer_width(2);        
                /// layer_width(2) : array of Float_t        
                Array_t<Float_t> rr(2);        
                /// rr(2) : array of Float_t        
          ///@}     
          ///@addtogroup CalbGeo1_vars     
          ///@{        
                Int_t layer,super,sub,i,j,ii,nn,imod;        
                //        
                /// Int_t layer,super,sub,i,j,ii,nn,imod        
          ///@}     
       CalbGeo1::CalbGeo1()     
         : AgModule("CalbGeo1"," is the geometry of the Barrel EM Calorimeter ")     
       {        
       }     
          // ---------------------------------------------------------------------------------------------------     
          void CALB::Block( AgCreate create )     
          {         
                ///@addtogroup CALB_doc        
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
                      { AgAttribute attr = AgAttribute("CALB");              
                            attr.par("seen")=0;              
                            attr.par("colo")=7;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Pcon");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("phi1")=0;              
                            shape.par("dphi")=360;              
                            shape.par("nz")=4;              
                            shape.Z(0)=-hleng;              
                            shape.Z(1)=-cut_length;              
                            shape.Z(2)=cut_length;              
                            shape.Z(3)=hleng;              
                            shape.Rmin(0)=cut_radius;              
                            shape.Rmin(1)=calg.rmin;              
                            shape.Rmin(2)=calg.rmin;              
                            shape.Rmin(3)=cut_radius;              
                            shape.Rmax(0)=rmax;              
                            shape.Rmax(1)=rmax;              
                            shape.Rmax(2)=rmax;              
                            shape.Rmax(3)=rmax;              
                            /// Shape Pcon phi1=0 dphi=360 nz=4               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_CALB;              
                            _stacker -> Build(this);              
                      }           
                      if ( calg.nmodule(1)>0 )           
                      {              
                            ii=1;;              
                            _create = AgCreate("CHLV");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create CHLV                 
                                  Create("CHLV");                  
                            }              
                            { AgPlacement place = AgPlacement("CHLV","CALB");                 
                                  /// Add daughter volume CHLV to mother CALB                 
                                  _stacker -> Position( AgBlock::Find("CHLV"), place );                 
                            } // end placement of CHLV              
                      }           
                      if ( calg.nmodule(2)>0 )           
                      {              
                            ii=2;;              
                            _create = AgCreate("CHLV");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create CHLV                 
                                  Create("CHLV");                  
                            }              
                            { AgPlacement place = AgPlacement("CHLV","CALB");                 
                                  /// Add daughter volume CHLV to mother CALB                 
                                  /// G3 Reference: thetax = 90                 
                                  /// G3 Reference: phix = 0                 
                                  /// G3 Reference: thetay = 90                 
                                  /// G3 Reference: phiy = 90                 
                                  /// G3 Reference: thetaz = 180                 
                                  /// G3 Reference: phiz = 0                 
                                  Double_t _thetax=90,_phix=0,_thetay=90,_phiy=90,_thetaz=180,_phiz=0;                 
                                  place.Reference( _thetax, _phix, _thetay, _phiy, _thetaz, _phiz );                 
                                  _stacker -> Position( AgBlock::Find("CHLV"), place );                 
                            } // end placement of CHLV              
                      }           
                      END_OF_CALB:           
                      mCurrent = _save;           
                ///@}        
          } // End Block CALB     
          // ---------------------------------------------------------------------------------------------------     
          void CHLV::Block( AgCreate create )     
          {         
                ///@addtogroup CHLV_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      {  AgShape shape = AgShape("Pcon");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("phi1")=calg.shift(ii);              
                            shape.par("dphi")=dphimod*calg.nmodule(ii);              
                            shape.par("nz")=3;              
                            shape.Z(0)=0;              
                            shape.Z(1)=cut_length;              
                            shape.Z(2)=hleng;              
                            shape.Rmin(0)=calg.rmin;              
                            shape.Rmin(1)=calg.rmin;              
                            shape.Rmin(2)=cut_radius;              
                            shape.Rmax(0)=rmax;              
                            shape.Rmax(1)=rmax;              
                            shape.Rmax(2)=rmax;              
                            /// Shape Pcon phi1=calg.shift(ii) dphi=dphimod*calg.nmodule(ii) nz=3               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_CHLV;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("CPHI");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create CPHI              
                            Create("CPHI");               
                      }           
                      /// Loop on imod from 1 to calg.nmodule(ii) step=1           
                      for ( imod=1; (1>0)? (imod<=calg.nmodule(ii)):(imod>=calg.nmodule(ii)); imod+=1 )           
                      {              
                            angular_offset=calg.shift(ii)+3.0+6.0*(imod-1);              
                            if ( ii==1 )              
                            {                 
                                  { AgPlacement place = AgPlacement("CPHI","CHLV");                    
                                        /// Add daughter volume CPHI to mother CHLV                    
                                        place.par("ncopy")=imod;                    
                                        /// Ncopy: imod                    
                                        place.AlphaZ(angular_offset);                    
                                        /// Rotate: AlphaZ = angular_offset                    
                                        /// G3 Reference: thetax = 90                    
                                        /// G3 Reference: phix = 0                    
                                        /// G3 Reference: thetay = 90                    
                                        /// G3 Reference: phiy = 90                    
                                        /// G3 Reference: thetaz = 0                    
                                        /// G3 Reference: phiz = 0                    
                                        _stacker -> Position( AgBlock::Find("CPHI"), place );                    
                                  } // end placement of CPHI                 
                            }              
                            if ( ii==2&&calg.modmap(imod)>0 )              
                            {                 
                                  { AgPlacement place = AgPlacement("CPHI","CHLV");                    
                                        /// Add daughter volume CPHI to mother CHLV                    
                                        place.par("ncopy")=imod;                    
                                        /// Ncopy: imod                    
                                        place.AlphaZ(angular_offset);                    
                                        /// Rotate: AlphaZ = angular_offset                    
                                        /// G3 Reference: thetax = 90                    
                                        /// G3 Reference: phix = 0                    
                                        /// G3 Reference: thetay = 90                    
                                        /// G3 Reference: phiy = 90                    
                                        /// G3 Reference: thetaz = 0                    
                                        /// G3 Reference: phiz = 0                    
                                        _stacker -> Position( AgBlock::Find("CPHI"), place );                    
                                  } // end placement of CPHI                 
                            }              
                      }           
                      END_OF_CHLV:           
                      mCurrent = _save;           
                ///@}        
          } // End Block CHLV     
          // ---------------------------------------------------------------------------------------------------     
          void CPHI::Block( AgCreate create )     
          {         
                ///@addtogroup CPHI_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("CPHI");              
                            attr.par("seen")=1;              
                            attr.par("colo")=5;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Pcon");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("phi1")=-3.0;              
                            shape.par("dphi")=6.0;              
                            shape.par("nz")=3;              
                            shape.Z(0)=0;              
                            shape.Z(1)=cut_length;              
                            shape.Z(2)=hleng;              
                            shape.Rmin(0)=calg.rmin;              
                            shape.Rmin(1)=calg.rmin;              
                            shape.Rmin(2)=cut_radius;              
                            shape.Rmax(0)=rmax;              
                            shape.Rmax(1)=rmax;              
                            shape.Rmax(2)=rmax;              
                            /// Shape Pcon phi1=-3.0 dphi=6.0 nz=3               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_CPHI;              
                            _stacker -> Build(this);              
                      }           
                      current_depth = calg.rmin;           
                      c_dep=current_depth;           
                      _create = AgCreate("CBTW");           
                      { // Paramters passed in via the Create operatir              
                            AgCreate create("CBTW");              
                            create.par("dx")=calg.frontthk;              
                            _create = create;              
                      }           
                      {              
                            AgShape myshape; // undefined shape              
                            /// Set shape par: dx = calg.frontthk              
                            myshape.par("dx")=calg.frontthk;              
                            ///Create CBTW              
                            Create("CBTW");               
                      }           
                      { AgPlacement place = AgPlacement("CBTW","CPHI");              
                            /// Add daughter volume CBTW to mother CPHI              
                            place.TranslateX(calg.rmin+calg.frontthk);              
                            /// Translate x = calg.rmin+calg.frontthk              
                            place.TranslateZ(current_depth/tan_theta/2);              
                            /// Translate z = current_depth/tan_theta/2              
                            _stacker -> Position( AgBlock::Find("CBTW"), place );              
                      } // end placement of CBTW           
                      current_depth = current_depth + 2*calg.frontthk;           
                      layer = 0;           
                      /// Loop on super from 1 to nint(calg.nsuper) step=1           
                      for ( super=1; (1>0)? (super<=nint(calg.nsuper)):(super>=nint(calg.nsuper)); super+=1 )           
                      {              
                            _create = AgCreate("CSUP");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create CSUP                 
                                  Create("CSUP");                  
                            }              
                            { AgPlacement place = AgPlacement("CSUP","CPHI");                 
                                  /// Add daughter volume CSUP to mother CPHI                 
                                  _stacker -> Position( AgBlock::Find("CSUP"), place );                 
                            } // end placement of CSUP              
                      }           
                      _create = AgCreate("CBTW");           
                      { // Paramters passed in via the Create operatir              
                            AgCreate create("CBTW");              
                            create.par("dx")=calg.compthk;              
                            _create = create;              
                      }           
                      {              
                            AgShape myshape; // undefined shape              
                            /// Set shape par: dx = calg.compthk              
                            myshape.par("dx")=calg.compthk;              
                            ///Create CBTW              
                            Create("CBTW");               
                      }           
                      { AgPlacement place = AgPlacement("CBTW","CPHI");              
                            /// Add daughter volume CBTW to mother CPHI              
                            place.TranslateX(current_depth+calg.compthk);              
                            /// Translate x = current_depth+calg.compthk              
                            place.TranslateZ(current_depth/tan_theta/2);              
                            /// Translate z = current_depth/tan_theta/2              
                            _stacker -> Position( AgBlock::Find("CBTW"), place );              
                      } // end placement of CBTW           
                      c_dep=2.*calg.compthk+2.*calg.airthk;           
                      _create = AgCreate("CBTW");           
                      { // Paramters passed in via the Create operatir              
                            AgCreate create("CBTW");              
                            create.par("dx")=calg.backthk;              
                            _create = create;              
                      }           
                      {              
                            AgShape myshape; // undefined shape              
                            /// Set shape par: dx = calg.backthk              
                            myshape.par("dx")=calg.backthk;              
                            ///Create CBTW              
                            Create("CBTW");               
                      }           
                      { AgPlacement place = AgPlacement("CBTW","CPHI");              
                            /// Add daughter volume CBTW to mother CPHI              
                            place.TranslateX(current_depth+c_dep+calg.backthk);              
                            /// Translate x = current_depth+c_dep+calg.backthk              
                            place.TranslateZ(current_depth/tan_theta/2);              
                            /// Translate z = current_depth/tan_theta/2              
                            _stacker -> Position( AgBlock::Find("CBTW"), place );              
                      } // end placement of CBTW           
                      c_dep=c_dep+2.*calg.backthk;           
                      _create = AgCreate("CBTW");           
                      { // Paramters passed in via the Create operatir              
                            AgCreate create("CBTW");              
                            create.par("dx")=calg.spacethk;              
                            _create = create;              
                      }           
                      {              
                            AgShape myshape; // undefined shape              
                            /// Set shape par: dx = calg.spacethk              
                            myshape.par("dx")=calg.spacethk;              
                            ///Create CBTW              
                            Create("CBTW");               
                      }           
                      { AgPlacement place = AgPlacement("CBTW","CPHI");              
                            /// Add daughter volume CBTW to mother CPHI              
                            place.TranslateX(current_depth+c_dep+calg.spacethk);              
                            /// Translate x = current_depth+c_dep+calg.spacethk              
                            place.TranslateZ(current_depth/tan_theta/2);              
                            /// Translate z = current_depth/tan_theta/2              
                            _stacker -> Position( AgBlock::Find("CBTW"), place );              
                      } // end placement of CBTW           
                      c_dep=c_dep+2.*calg.spacethk;           
                      current_depth=current_depth+c_dep;           
                      END_OF_CPHI:           
                      mCurrent = _save;           
                ///@}        
          } // End Block CPHI     
          // ---------------------------------------------------------------------------------------------------     
          void CSUP::Block( AgCreate create )     
          {         
                ///@addtogroup CSUP_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      future_depth=current_depth+                 (calg.nsublay(super)-super+1)*layer_width(super)*2+                 (smd_width+calg.scintthk(super)+2.*calg.abpapthk)*2*(super-1);           
                      /// Component C	a=12.01	z=6.	w=6./21.           
                      /// Component H	a=1.	z=1.	w=10./21.           
                      /// Component O	a=16.	z=8.	w=5./21.           
                      /// Mixture Cellulose isvol=1 dens=0.35           
                      {  AgMaterial &mix = AgMaterial::Get("Cellulose");              
                            mix.Component("C",12.01,6.,6./21.);              
                            mix.Component("H",1.,1.,10./21.);              
                            mix.Component("O",16.,8.,5./21.);              
                            mix.par("isvol")=1;              
                            mix.par("dens")=0.35;              
                            mix.lock();              
                            _material = mix;              
                            _material.lock();              
                      }           
                      { AgAttribute attr = AgAttribute("CSUP");              
                            attr.par("seen")=0;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Pcon");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("phi1")=-3.0;              
                            shape.par("dphi")=dphimod;              
                            shape.par("nz")=3;              
                            shape.Z(0)=0;              
                            shape.Z(1)=current_depth/tan_theta;              
                            shape.Z(2)=future_depth/tan_theta;              
                            shape.Rmin(0)=current_depth;              
                            shape.Rmin(1)=current_depth;              
                            shape.Rmin(2)=future_depth;              
                            shape.Rmax(0)=future_depth;              
                            shape.Rmax(1)=future_depth;              
                            shape.Rmax(2)=future_depth;              
                            /// Shape Pcon phi1=-3.0 dphi=dphimod nz=3               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_CSUP;              
                            _stacker -> Build(this);              
                      }           
                      CALBPAR( ag_imed,"absorber" );// CALL CALBPAR           
                      /// Loop on sub from 1 to nint(calg.nsublay(super)) step=1           
                      for ( sub=1; (1>0)? (sub<=nint(calg.nsublay(super))):(sub>=nint(calg.nsublay(super))); sub+=1 )           
                      {              
                            layer = layer + 1;              
                            if ( layer<nint(calg.nsublay(1)+calg.nsublay(2)) )              
                            {                 
                                  _create = AgCreate("CSCI");                 
                                  {                    
                                        AgShape myshape; // undefined shape                    
                                        ///Create CSCI                    
                                        Create("CSCI");                     
                                  }                 
                                  { AgPlacement place = AgPlacement("CSCI","CSUP");                    
                                        /// Add daughter volume CSCI to mother CSUP                    
                                        place.TranslateX(current_depth+calg.scintthk(super)+2.*calg.abpapthk);                    
                                        /// Translate x = current_depth+calg.scintthk(super)+2.*calg.abpapthk                    
                                        place.TranslateZ(current_depth/tan_theta/2);                    
                                        /// Translate z = current_depth/tan_theta/2                    
                                        _stacker -> Position( AgBlock::Find("CSCI"), place );                    
                                  } // end placement of CSCI                 
                                  _create = AgCreate("CPBP");                 
                                  {                    
                                        AgShape myshape; // undefined shape                    
                                        ///Create CPBP                    
                                        Create("CPBP");                     
                                  }                 
                                  c_lead_dep=2.*calg.scintthk(super)+4.*calg.abpapthk;                 
                                  { AgPlacement place = AgPlacement("CPBP","CSUP");                    
                                        /// Add daughter volume CPBP to mother CSUP                    
                                        place.TranslateX(current_depth+c_lead_dep+calg.absorthk);                    
                                        /// Translate x = current_depth+c_lead_dep+calg.absorthk                    
                                        place.TranslateZ(current_depth/tan_theta/2);                    
                                        /// Translate z = current_depth/tan_theta/2                    
                                        _stacker -> Position( AgBlock::Find("CPBP"), place );                    
                                  } // end placement of CPBP                 
                                  current_depth = current_depth + 2*layer_width(super);                 
                            }              
                            else              
                            {                 
                                  _create = AgCreate("CSCI");                 
                                  {                    
                                        AgShape myshape; // undefined shape                    
                                        ///Create CSCI                    
                                        Create("CSCI");                     
                                  }                 
                                  { AgPlacement place = AgPlacement("CSCI","CSUP");                    
                                        /// Add daughter volume CSCI to mother CSUP                    
                                        place.TranslateX(current_depth+calg.scintthk(2)+2.*calg.abpapthk);                    
                                        /// Translate x = current_depth+calg.scintthk(2)+2.*calg.abpapthk                    
                                        place.TranslateZ(current_depth/tan_theta/2);                    
                                        /// Translate z = current_depth/tan_theta/2                    
                                        _stacker -> Position( AgBlock::Find("CSCI"), place );                    
                                  } // end placement of CSCI                 
                                  current_depth = current_depth+c_lead_dep;                 
                            }              
                            if ( not (  layer==nint(calg.nsmd) )) { continue; }              
                            _create = AgCreate("CSMD");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create CSMD                 
                                  Create("CSMD");                  
                            }              
                            { AgPlacement place = AgPlacement("CSMD","CSUP");                 
                                  /// Add daughter volume CSMD to mother CSUP                 
                                  place.TranslateX(current_depth+smd_width);                 
                                  /// Translate x = current_depth+smd_width                 
                                  place.TranslateZ(current_depth/tan_theta/2);                 
                                  /// Translate z = current_depth/tan_theta/2                 
                                  _stacker -> Position( AgBlock::Find("CSMD"), place );                 
                            } // end placement of CSMD              
                            current_depth = current_depth + 2*smd_width;              
                      }           
                      END_OF_CSUP:           
                      mCurrent = _save;           
                ///@}        
          } // End Block CSUP     
          // ---------------------------------------------------------------------------------------------------     
          void CPBP::Block( AgCreate create )     
          {         
                ///@addtogroup CPBP_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Lead            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Lead");              
                            _material = mat;              
                      }           
                      /// Material Lead_CPBP isvol=0            
                      { AgMaterial &mat = AgMaterial::Get("Lead_cpbp");              
                            mat.par("isvol")=0;              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("CPBP");              
                            attr.par("seen")=1;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=calg.absorthk;              
                            shape.par("dy")=current_depth*tan(twopi/360*dphit)-calg.crackwd;              
                            shape.par("dz")=current_depth/tan_theta/2;              
                            /// Shape Bbox dx=calg.absorthk dy=current_depth*tan(twopi/360*dphit)-calg.crackwd dz=current_depth/tan_theta/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_CPBP;              
                            _stacker -> Build(this);              
                      }           
                      CALBPAR( ag_imed,"absorber" );// CALL CALBPAR           
                      END_OF_CPBP:           
                      mCurrent = _save;           
                ///@}        
          } // End Block CPBP     
          // ---------------------------------------------------------------------------------------------------     
          void CSCI::Block( AgCreate create )     
          {         
                ///@addtogroup CSCI_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material polystyren            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Polystyren");              
                            _material = mat;              
                      }           
                      /// Material Cpolystyren isvol=1            
                      { AgMaterial &mat = AgMaterial::Get("Cpolystyren");              
                            mat.par("isvol")=1;              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("CSCI");              
                            attr.par("seen")=1;              
                            attr.par("colo")=4;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=calg.scintthk(super);              
                            shape.par("dy")=current_depth*tan(twopi/360*dphit)-calg.crackwd;              
                            shape.par("dz")=current_depth/tan_theta/2;              
                            /// Shape Bbox dx=calg.scintthk(super) dy=current_depth*tan(twopi/360*dphit)-calg.crackwd dz=current_depth/tan_theta/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_CSCI;              
                            _stacker -> Build(this);              
                      }           
                      CALBPAR( ag_imed,"absorber" );// CALL CALBPAR           
                      // _medium.par("BIRK1") = 1.;           
                      // _medium.par("BIRK2") = RKB2sc;           
                      // _medium.par("BIRK3") = RKB3sc;           
                      END_OF_CSCI:           
                      mCurrent = _save;           
                ///@}        
          } // End Block CSCI     
          // ---------------------------------------------------------------------------------------------------     
          void CBTW::Block( AgCreate create )     
          {         
                ///@addtogroup CBTW_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Aluminium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Aluminium");              
                            _material = mat;              
                      }           
                      /// Material Alu_CBTW isvol=1            
                      { AgMaterial &mat = AgMaterial::Get("Alu_cbtw");              
                            mat.par("isvol")=1;              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("CBTW");              
                            attr.par("seen")=1;              
                            attr.par("colo")=6;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dy")=current_depth*tan(twopi/360*dphit)-calg.crackwd;              
                            shape.par("dz")=current_depth/tan_theta/2;              
                            /// Shape Bbox dy=current_depth*tan(twopi/360*dphit)-calg.crackwd dz=current_depth/tan_theta/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_CBTW;              
                            _stacker -> Build(this);              
                      }           
                      CALBPAR( ag_imed,"absorber" );// CALL CALBPAR           
                      END_OF_CBTW:           
                      mCurrent = _save;           
                ///@}        
          } // End Block CBTW     
          // ---------------------------------------------------------------------------------------------------     
          void CSMD::Block( AgCreate create )     
          {         
                ///@addtogroup CSMD_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      current = -smd_width;           
                      /// Component C	a=12.01	z=6.	w=6./21.           
                      /// Component H	a=1.	z=1.	w=10./21.           
                      /// Component O	a=16.	z=8.	w=5./21.           
                      /// Mixture Cellulose isvol=1 dens=0.35           
                      {  AgMaterial &mix = AgMaterial::Get("Cellulose");              
                            mix.Component("C",12.01,6.,6./21.);              
                            mix.Component("H",1.,1.,10./21.);              
                            mix.Component("O",16.,8.,5./21.);              
                            mix.par("isvol")=1;              
                            mix.par("dens")=0.35;              
                            mix.lock();              
                            _material = mix;              
                            _material.lock();              
                      }           
                      { AgAttribute attr = AgAttribute("CSMD");              
                            attr.par("seen")=1;              
                            attr.par("colo")=6;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=smd_width;              
                            shape.par("dy")=current_depth*tan(twopi/120.)-calg.crackwd;              
                            shape.par("dz")=current_depth/tan_theta/2;              
                            /// Shape Bbox dx=smd_width dy=current_depth*tan(twopi/120.)-calg.crackwd dz=current_depth/tan_theta/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_CSMD;              
                            _stacker -> Build(this);              
                      }           
                      CALBPAR( ag_imed,"sensitive" );// CALL CALBPAR           
                      _create = AgCreate("CSMG");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create CSMG              
                            Create("CSMG");               
                      }           
                      { AgPlacement place = AgPlacement("CSMG","CSMD");              
                            /// Add daughter volume CSMG to mother CSMD              
                            place.TranslateX(-smd_width+calg.g10sbthk+2.*calg.abpapthk);              
                            /// Translate x = -smd_width+calg.g10sbthk+2.*calg.abpapthk              
                            _stacker -> Position( AgBlock::Find("CSMG"), place );              
                      } // end placement of CSMG           
                      { AgPlacement place = AgPlacement("CSMG","CSMD");              
                            /// Add daughter volume CSMG to mother CSMD              
                            place.TranslateX(smd_width-calg.g10sbthk-2.*calg.abpapthk);              
                            /// Translate x = smd_width-calg.g10sbthk-2.*calg.abpapthk              
                            _stacker -> Position( AgBlock::Find("CSMG"), place );              
                      } // end placement of CSMG           
                      current = current+2.*calg.g10sbthk+2.*calg.abpapthk;           
                      /// Loop on j from 1 to 4 step=1           
                      for ( j=1; (1>0)? (j<=4):(j>=4); j+=1 )           
                      {              
                            current_csda=-current_depth/tan_theta/2;              
                            if ( j==1 )              
                            {                 
                                  eta_lenght=calg.netfirst*(calg.seta1wdh+calg.set12wdh);                 
                                  _create = AgCreate("CSDA");                 
                                  {                    
                                        AgShape myshape; // undefined shape                    
                                        ///Create CSDA                    
                                        Create("CSDA");                     
                                  }                 
                                  { AgPlacement place = AgPlacement("CSDA","CSMD");                    
                                        /// Add daughter volume CSDA to mother CSMD                    
                                        place.TranslateX(current+calg.smalfthk);                    
                                        /// Translate x = current+calg.smalfthk                    
                                        place.TranslateZ(current_csda+2.*calg.smetawdh+eta_lenght);                    
                                        /// Translate z = current_csda+2.*calg.smetawdh+eta_lenght                    
                                        _stacker -> Position( AgBlock::Find("CSDA"), place );                    
                                  } // end placement of CSDA                 
                            }              
                            else if ( j==2 )              
                            {                 
                                  current_csda=current_csda+2.*(calg.smetawdh+eta_lenght);                 
                                  eta_lenght=calg.netfirst*(calg.seta2wdh+calg.set12wdh);                 
                                  _create = AgCreate("CSDA");                 
                                  {                    
                                        AgShape myshape; // undefined shape                    
                                        ///Create CSDA                    
                                        Create("CSDA");                     
                                  }                 
                                  { AgPlacement place = AgPlacement("CSDA","CSMD");                    
                                        /// Add daughter volume CSDA to mother CSMD                    
                                        place.TranslateX(current+calg.smalfthk);                    
                                        /// Translate x = current+calg.smalfthk                    
                                        place.TranslateZ(current_csda+eta_lenght);                    
                                        /// Translate z = current_csda+eta_lenght                    
                                        _stacker -> Position( AgBlock::Find("CSDA"), place );                    
                                  } // end placement of CSDA                 
                            }              
                            else if ( j==3 )              
                            {                 
                                  eta_lenght=calg.netfirst*(calg.seta1wdh+calg.set12wdh);                 
                                  _create = AgCreate("CSDA");                 
                                  {                    
                                        AgShape myshape; // undefined shape                    
                                        ///Create CSDA                    
                                        Create("CSDA");                     
                                  }                 
                                  { AgPlacement place = AgPlacement("CSDA","CSMD");                    
                                        /// Add daughter volume CSDA to mother CSMD                    
                                        place.TranslateX(current+3.*calg.smalfthk);                    
                                        /// Translate x = current+3.*calg.smalfthk                    
                                        place.TranslateZ(current_csda+2.*calg.smetawdh+eta_lenght);                    
                                        /// Translate z = current_csda+2.*calg.smetawdh+eta_lenght                    
                                        /// G3 Reference: thetax = 90                    
                                        /// G3 Reference: phix = 180                    
                                        /// G3 Reference: thetay = 90                    
                                        /// G3 Reference: phiy = 90                    
                                        /// G3 Reference: thetaz = 0                    
                                        /// G3 Reference: phiz = 0                    
                                        Double_t _thetax=90,_phix=180,_thetay=90,_phiy=90,_thetaz=0,_phiz=0;                    
                                        place.Reference( _thetax, _phix, _thetay, _phiy, _thetaz, _phiz );                    
                                        _stacker -> Position( AgBlock::Find("CSDA"), place );                    
                                  } // end placement of CSDA                 
                            }              
                            else if ( j==4 )              
                            {                 
                                  current_csda=current_csda+2.*(calg.smetawdh+eta_lenght);                 
                                  eta_lenght=calg.netfirst*(calg.seta2wdh+calg.set12wdh);                 
                                  _create = AgCreate("CSDA");                 
                                  {                    
                                        AgShape myshape; // undefined shape                    
                                        ///Create CSDA                    
                                        Create("CSDA");                     
                                  }                 
                                  { AgPlacement place = AgPlacement("CSDA","CSMD");                    
                                        /// Add daughter volume CSDA to mother CSMD                    
                                        place.TranslateX(current+3.*calg.smalfthk);                    
                                        /// Translate x = current+3.*calg.smalfthk                    
                                        place.TranslateZ(current_csda+eta_lenght);                    
                                        /// Translate z = current_csda+eta_lenght                    
                                        /// G3 Reference: thetax = 90                    
                                        /// G3 Reference: phix = 180                    
                                        /// G3 Reference: thetay = 90                    
                                        /// G3 Reference: phiy = 90                    
                                        /// G3 Reference: thetaz = 0                    
                                        /// G3 Reference: phiz = 0                    
                                        Double_t _thetax=90,_phix=180,_thetay=90,_phiy=90,_thetaz=0,_phiz=0;                    
                                        place.Reference( _thetax, _phix, _thetay, _phiy, _thetaz, _phiz );                    
                                        _stacker -> Position( AgBlock::Find("CSDA"), place );                    
                                  } // end placement of CSDA                 
                            }              
                      }           
                      _create = AgCreate("CSMC");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create CSMC              
                            Create("CSMC");               
                      }           
                      { AgPlacement place = AgPlacement("CSMC","CSMD");              
                            /// Add daughter volume CSMC to mother CSMD              
                            place.TranslateX(current+calg.smalfthk);              
                            /// Translate x = current+calg.smalfthk              
                            place.TranslateY(calg.smalfwdh+calg.smaffwdh);              
                            /// Translate y = calg.smalfwdh+calg.smaffwdh              
                            _stacker -> Position( AgBlock::Find("CSMC"), place );              
                      } // end placement of CSMC           
                      { AgPlacement place = AgPlacement("CSMC","CSMD");              
                            /// Add daughter volume CSMC to mother CSMD              
                            place.TranslateX(current+calg.smalfthk);              
                            /// Translate x = current+calg.smalfthk              
                            place.TranslateY(-calg.smalfwdh-calg.smaffwdh);              
                            /// Translate y = -calg.smalfwdh-calg.smaffwdh              
                            _stacker -> Position( AgBlock::Find("CSMC"), place );              
                      } // end placement of CSMC           
                      _create = AgCreate("CSMB");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create CSMB              
                            Create("CSMB");               
                      }           
                      { AgPlacement place = AgPlacement("CSMB","CSMD");              
                            /// Add daughter volume CSMB to mother CSMD              
                            place.TranslateX(current+3.*calg.smalfthk);              
                            /// Translate x = current+3.*calg.smalfthk              
                            place.TranslateY(calg.smalfwdh+calg.smafbwdh);              
                            /// Translate y = calg.smalfwdh+calg.smafbwdh              
                            _stacker -> Position( AgBlock::Find("CSMB"), place );              
                      } // end placement of CSMB           
                      { AgPlacement place = AgPlacement("CSMB","CSMD");              
                            /// Add daughter volume CSMB to mother CSMD              
                            place.TranslateX(current+3.*calg.smalfthk);              
                            /// Translate x = current+3.*calg.smalfthk              
                            place.TranslateY(-calg.smalfwdh-calg.smafbwdh);              
                            /// Translate y = -calg.smalfwdh-calg.smafbwdh              
                            _stacker -> Position( AgBlock::Find("CSMB"), place );              
                      } // end placement of CSMB           
                      END_OF_CSMD:           
                      mCurrent = _save;           
                ///@}        
          } // End Block CSMD     
          // ---------------------------------------------------------------------------------------------------     
          void CSMG::Block( AgCreate create )     
          {         
                ///@addtogroup CSMG_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Component Si	a=28.08	z=14	w=0.6*1*28./60.           
                      /// Component O	a=16	z=8	w=0.6*2*16./60.           
                      /// Component C	a=12	z=6	w=0.4*8*12./174.           
                      /// Component H	a=1	z=1	w=0.4*14*1./174.           
                      /// Component O	a=16	z=8	w=0.4*4*16./174.           
                      /// Mixture g10 isvol=0 dens=1.7           
                      {  AgMaterial &mix = AgMaterial::Get("G10");              
                            mix.Component("Si",28.08,14,0.6*1*28./60.);              
                            mix.Component("O",16,8,0.6*2*16./60.);              
                            mix.Component("C",12,6,0.4*8*12./174.);              
                            mix.Component("H",1,1,0.4*14*1./174.);              
                            mix.Component("O",16,8,0.4*4*16./174.);              
                            mix.par("isvol")=0;              
                            mix.par("dens")=1.7;              
                            mix.lock();              
                            _material = mix;              
                            _material.lock();              
                      }           
                      { AgAttribute attr = AgAttribute("CSMG");              
                            attr.par("seen")=1;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=calg.g10sbthk;              
                            /// Shape Bbox dx=calg.g10sbthk               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_CSMG;              
                            _stacker -> Build(this);              
                      }           
                      CALBPAR( ag_imed,"sensitive" );// CALL CALBPAR           
                      END_OF_CSMG:           
                      mCurrent = _save;           
                ///@}        
          } // End Block CSMG     
          // ---------------------------------------------------------------------------------------------------     
          void CSDA::Block( AgCreate create )     
          {         
                ///@addtogroup CSDA_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Aluminium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Aluminium");              
                            _material = mat;              
                      }           
                      /// Material Alu_CSDA isvol=0            
                      { AgMaterial &mat = AgMaterial::Get("Alu_csda");              
                            mat.par("isvol")=0;              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("CSDA");              
                            attr.par("seen")=1;              
                            attr.par("colo")=6;              
                            attr.par("serial")=j;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=calg.smalfthk;              
                            shape.par("dy")=calg.smalfwdh;              
                            shape.par("dz")=eta_lenght;              
                            /// Shape Bbox dx=calg.smalfthk dy=calg.smalfwdh dz=eta_lenght               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_CSDA;              
                            _stacker -> Build(this);              
                      }           
                      CALBPAR( ag_imed,"sensitive" );// CALL CALBPAR           
                      _create = AgCreate("CSME");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create CSME              
                            Create("CSME");               
                      }           
                      END_OF_CSDA:           
                      mCurrent = _save;           
                ///@}        
          } // End Block CSDA     
          // ---------------------------------------------------------------------------------------------------     
          void CSMC::Block( AgCreate create )     
          {         
                ///@addtogroup CSMC_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Aluminium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Aluminium");              
                            _material = mat;              
                      }           
                      /// Material Alu_CSMC isvol=0            
                      { AgMaterial &mat = AgMaterial::Get("Alu_csmc");              
                            mat.par("isvol")=0;              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("CSMC");              
                            attr.par("seen")=1;              
                            attr.par("colo")=6;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=calg.smalfthk;              
                            shape.par("dy")=calg.smaffwdh;              
                            /// Shape Bbox dx=calg.smalfthk dy=calg.smaffwdh               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_CSMC;              
                            _stacker -> Build(this);              
                      }           
                      CALBPAR( ag_imed,"sensitive" );// CALL CALBPAR           
                      END_OF_CSMC:           
                      mCurrent = _save;           
                ///@}        
          } // End Block CSMC     
          // ---------------------------------------------------------------------------------------------------     
          void CSMB::Block( AgCreate create )     
          {         
                ///@addtogroup CSMB_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Aluminium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Aluminium");              
                            _material = mat;              
                      }           
                      /// Material Alu_CSMB isvol=0            
                      { AgMaterial &mat = AgMaterial::Get("Alu_csmb");              
                            mat.par("isvol")=0;              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("CSMB");              
                            attr.par("seen")=1;              
                            attr.par("colo")=6;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=calg.smalfthk;              
                            shape.par("dy")=calg.smafbwdh;              
                            /// Shape Bbox dx=calg.smalfthk dy=calg.smafbwdh               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_CSMB;              
                            _stacker -> Build(this);              
                      }           
                      CALBPAR( ag_imed,"sensitive" );// CALL CALBPAR           
                      END_OF_CSMB:           
                      mCurrent = _save;           
                ///@}        
          } // End Block CSMB     
          // ---------------------------------------------------------------------------------------------------     
          void CSME::Block( AgCreate create )     
          {         
                ///@addtogroup CSME_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Aluminium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Aluminium");              
                            _material = mat;              
                      }           
                      /// Material Alu_CSME isvol=0            
                      { AgMaterial &mat = AgMaterial::Get("Alu_csme");              
                            mat.par("isvol")=0;              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("CSME");              
                            attr.par("seen")=1;              
                            attr.par("colo")=6;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Division");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("ndiv")=nint(calg.nsmdalw);              
                            shape.par("iaxis")=2;              
                            /// Shape Division ndiv=nint(calg.nsmdalw) iaxis=2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_CSME;              
                            _stacker -> Build(this);              
                      }           
                      CALBPAR( ag_imed,"sensitive" );// CALL CALBPAR           
                      /// Loop on i from 1 to 2 step=1           
                      for ( i=1; (1>0)? (i<=2):(i>=2); i+=1 )           
                      {              
                            _create = AgCreate("CSHI");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create CSHI                 
                                  Create("CSHI");                  
                            }              
                            if ( i==1 )              
                            {                 
                                  { AgPlacement place = AgPlacement("CSHI","CSME");                    
                                        /// Add daughter volume CSHI to mother CSME                    
                                        place.TranslateX(-calg.smalfthk+calg.smgasthk);                    
                                        /// Translate x = -calg.smalfthk+calg.smgasthk                    
                                        _stacker -> Position( AgBlock::Find("CSHI"), place );                    
                                  } // end placement of CSHI                 
                            }              
                            else              
                            {                 
                                  { AgPlacement place = AgPlacement("CSHI","CSME");                    
                                        /// Add daughter volume CSHI to mother CSME                    
                                        place.TranslateX(2.*calg.smgasthk-calg.smalfthk);                    
                                        /// Translate x = 2.*calg.smgasthk-calg.smalfthk                    
                                        _stacker -> Position( AgBlock::Find("CSHI"), place );                    
                                  } // end placement of CSHI                 
                            }              
                      }           
                      END_OF_CSME:           
                      mCurrent = _save;           
                ///@}        
          } // End Block CSME     
          // ---------------------------------------------------------------------------------------------------     
          void CSHI::Block( AgCreate create )     
          {         
                ///@addtogroup CSHI_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Component Ar	a=39.95	z=18.	w=0.9           
                      /// Component C	a=12.01	z=6.	w=0.1*1*12.01/44.01           
                      /// Component O	a=16.	z=8.	w=0.1*2*16./44.01           
                      /// Mixture sens_gas isvol=1 dens=0.0018015           
                      {  AgMaterial &mix = AgMaterial::Get("Sens_gas");              
                            mix.Component("Ar",39.95,18.,0.9);              
                            mix.Component("C",12.01,6.,0.1*1*12.01/44.01);              
                            mix.Component("O",16.,8.,0.1*2*16./44.01);              
                            mix.par("isvol")=1;              
                            mix.par("dens")=0.0018015;              
                            mix.lock();              
                            _material = mix;              
                            _material.lock();              
                      }           
                      { AgAttribute attr = AgAttribute("CSHI");              
                            attr.par("seen")=1;              
                            attr.par("colo")=4;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      if ( i==1 )           
                      {              
                            {  AgShape shape = AgShape("Bbox");                 
                                  shape     .Inherit( AgBlock::previous() );                 
                                  create     .SetParameters(shape);                 
                                  shape.par("dx")=calg.smgasthk;                 
                                  shape.par("dy")=calg.smgaswdh;                 
                                  /// Shape Bbox dx=calg.smgasthk dy=calg.smgaswdh                  
                                  _same_shape &= _stacker->SearchVolume( shape, _attribute );                 
                                  _shape = shape;                 
                                  if (_same_shape) goto END_OF_CSHI;                 
                                  _stacker -> Build(this);                 
                            }              
                      }           
                      else           
                      {              
                            {  AgShape shape = AgShape("Tubs");                 
                                  shape     .Inherit( AgBlock::previous() );                 
                                  create     .SetParameters(shape);                 
                                  shape.par("rmin")=0;                 
                                  shape.par("rmax")=calg.smgasrad;                 
                                  shape.par("phi1")=270;                 
                                  shape.par("phi2")=450;                 
                                  /// Shape Tubs rmin=0 rmax=calg.smgasrad phi1=270 phi2=450                  
                                  _same_shape &= _stacker->SearchVolume( shape, _attribute );                 
                                  _shape = shape;                 
                                  if (_same_shape) goto END_OF_CSHI;                 
                                  _stacker -> Build(this);                 
                            }              
                      }           
                      CALBPAR( ag_imed,"sensitive" );// CALL CALBPAR           
                      // _medium.par("STRA") = 1.0;           
                      h_eta1=2.*(calg.seta1wdh+calg.set12wdh);           
                      sh_eta1=calg.netfirst*h_eta1;           
                      h_eta2=2.*(calg.seta2wdh+calg.set12wdh);           
                      sh_eta2=calg.netsecon*h_eta2;           
                      h_phi1=2.*(calg.sphiwdh+calg.sphidwdh);           
                      h_phi2=h_phi1;           
                      sh_phi1=calg.nphistr*h_phi1;           
                      sh_phi2=sh_phi1;           
                      END_OF_CSHI:           
                      mCurrent = _save;           
                ///@}        
          } // End Block CSHI     
    // ----------------------------------------------------------------------- geoctr
       void CalbGeo1::ConstructGeometry( const Char_t *dummy )     
       {        
             ///@addtogroup CalbGeo1_revision        
             ///@{           
                   /// Author: Maxim Potekhin BNL           
             ///@}        
             ///@addtogroup CalbGeo1_revision        
             ///@{           
                   /// Created:    January 20, 2004            
             ///@}        
             AddBlock("CALB");        
             AddBlock("CHLV");        
             AddBlock("CPHI");        
             AddBlock("CSUP");        
             AddBlock("CPBP");        
             AddBlock("CSCI");        
             AddBlock("CSMD");        
             AddBlock("CSMG");        
             AddBlock("CSDA");        
             AddBlock("CSMC");        
             AddBlock("CSMB");        
             AddBlock("CSME");        
             AddBlock("CSHI");        
             AddBlock("CBTW");        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup calg_doc        
             ///@{           
                   ++calg._index;           
                   calg . version = 3.0; //  geometry version            
                   /// calg . version = 3.0; //  geometry version            
                   calg . rmin = 223.5; //  inner radius             
                   /// calg . rmin = 223.5; //  inner radius             
                   calg . etacut = 1.0; //  calorimeter rapidity cut            
                   /// calg . etacut = 1.0; //  calorimeter rapidity cut            
                   calg . crackwd = 0.655; //  half width of the crack between modules            
                   /// calg . crackwd = 0.655; //  half width of the crack between modules            
                   calg . frontthk = 0.9525; //  front plate half thickness             
                   /// calg . frontthk = 0.9525; //  front plate half thickness             
                   calg . compthk = 0.9525; //  back plate half thickness            
                   /// calg . compthk = 0.9525; //  back plate half thickness            
                   calg . airthk = 0.158; //  Air gap half thicness            
                   /// calg . airthk = 0.158; //  Air gap half thicness            
                   calg . backthk = 1.5875; //  Module back plate half thicknes            
                   /// calg . backthk = 1.5875; //  Module back plate half thicknes            
                   calg . spacethk = 0.9525; //  Spacer back plate half thicknes            
                   /// calg . spacethk = 0.9525; //  Spacer back plate half thicknes            
                   calg . scintthk.at(0) = 0.3; //  active scintillator plate half thickness            
                   ///calg . scintthk.at(0) = 0.3; //  active scintillator plate half thickness            
                   calg . scintthk.at(1) = 0.25; //  active scintillator plate half thickness            
                   ///calg . scintthk.at(1) = 0.25; //  active scintillator plate half thickness            
                   calg . absorthk = 0.250; //  absorber plate thickness halfpThickness            
                   /// calg . absorthk = 0.250; //  absorber plate thickness halfpThickness            
                   calg . abpapthk = 0.005; //  absorber paper plate thickness half thickness            
                   /// calg . abpapthk = 0.005; //  absorber paper plate thickness half thickness            
                   calg . g10sbthk = 0.115; //  G10 SMD front and back plate half thickness            
                   /// calg . g10sbthk = 0.115; //  G10 SMD front and back plate half thickness            
                   calg . smalfwdh = 11.2014; //  SMD Al front back plate with Ar half width            
                   /// calg . smalfwdh = 11.2014; //  SMD Al front back plate with Ar half width            
                   calg . smalfthk = 0.3893; //  SMD Al front back plate with Ar half thickness            
                   /// calg . smalfthk = 0.3893; //  SMD Al front back plate with Ar half thickness            
                   calg . smgasthk = 0.1359; //  SMD gas BOX volume half thickness            
                   /// calg . smgasthk = 0.1359; //  SMD gas BOX volume half thickness            
                   calg . smgaswdh = 0.2946; //  SMD gas BOX volume half width            
                   /// calg . smgaswdh = 0.2946; //  SMD gas BOX volume half width            
                   calg . smgasrad = 0.2946; //  SMD gas volume TUBS radius            
                   /// calg . smgasrad = 0.2946; //  SMD gas volume TUBS radius            
                   calg . smaffwdh = 0.113; //  SMD Al front first (last) rib half width             
                   /// calg . smaffwdh = 0.113; //  SMD Al front first (last) rib half width             
                   calg . smafbwdh = 0.1664; //  SMD Al back first (last) rib half width            
                   /// calg . smafbwdh = 0.1664; //  SMD Al back first (last) rib half width            
                   calg . smetawdh = 0.9806; //  Eta=0 strip notch half width            
                   /// calg . smetawdh = 0.9806; //  Eta=0 strip notch half width            
                   calg . seta1wdh = 0.7277; //  strip#1-75 half witdh            
                   /// calg . seta1wdh = 0.7277; //  strip#1-75 half witdh            
                   calg . seta2wdh = 0.9398; //  strip#76-150 half witdh            
                   /// calg . seta2wdh = 0.9398; //  strip#76-150 half witdh            
                   calg . set12wdh = 0.04064; //  half distance between strips in eta            
                   /// calg . set12wdh = 0.04064; //  half distance between strips in eta            
                   calg . sphiwdh = 0.6680; //  strip#(1-NPhistr) in phi direction half witdh            
                   /// calg . sphiwdh = 0.6680; //  strip#(1-NPhistr) in phi direction half witdh            
                   calg . sphidwdh = 0.07874; //  half distance between strips in phi            
                   /// calg . sphidwdh = 0.07874; //  half distance between strips in phi            
                   calg . nsmdalw = 30; //  Number SMD gaseus interval in tile            
                   /// calg . nsmdalw = 30; //  Number SMD gaseus interval in tile            
                   calg . nsuper = 2; //  number of readout superlayer            
                   /// calg . nsuper = 2; //  number of readout superlayer            
                   calg . nsmd = 5; //  SMD positioned after sandvich type layers EMC            
                   /// calg . nsmd = 5; //  SMD positioned after sandvich type layers EMC            
                   calg . nsublay.at(0) = 2; //  number of layers in a superlayer            
                   ///calg . nsublay.at(0) = 2; //  number of layers in a superlayer            
                   calg . nsublay.at(1) = 19; //  number of layers in a superlayer            
                   ///calg . nsublay.at(1) = 19; //  number of layers in a superlayer            
                   calg . maxmodule = 60; //  max number of moudle            
                   /// calg . maxmodule = 60; //  max number of moudle            
                   calg . netat = 20; //  Number of eta division for tower/preshower            
                   /// calg . netat = 20; //  Number of eta division for tower/preshower            
                   calg . nsub = 2; //  Number of sub div. in phi for tower/preshower            
                   /// calg . nsub = 2; //  Number of sub div. in phi for tower/preshower            
                   calg . netasmdp = 10; //  Number of eta division in for SMD phi plane            
                   /// calg . netasmdp = 10; //  Number of eta division in for SMD phi plane            
                   calg . nphistr = 15; //  Number of the strip in phi direction            
                   /// calg . nphistr = 15; //  Number of the strip in phi direction            
                   calg . netfirst = 75.; //  Number of strip in first part eta=0-0.5            
                   /// calg . netfirst = 75.; //  Number of strip in first part eta=0-0.5            
                   calg . netsecon = 75.; //  Number of strip in second part eta=0.5-1.0            
                   /// calg . netsecon = 75.; //  Number of strip in second part eta=0.5-1.0            
                   calg . nmodule.at(0) = 60; //  number of modules            
                   ///calg . nmodule.at(0) = 60; //  number of modules            
                   calg . nmodule.at(1) = 60; //  number of modules            
                   ///calg . nmodule.at(1) = 60; //  number of modules            
                   calg . shift.at(0) = 75; //  starting azimuth of the first module               
                   ///calg . shift.at(0) = 75; //  starting azimuth of the first module               
                   calg . shift.at(1) = 105; //  starting azimuth of the first module               
                   ///calg . shift.at(1) = 105; //  starting azimuth of the first module               
                   calg . modmap.at(0) = 1; //  Populated modules map            
                   ///calg . modmap.at(0) = 1; //  Populated modules map            
                   calg . modmap.at(1) = 1; //  Populated modules map            
                   ///calg . modmap.at(1) = 1; //  Populated modules map            
                   calg . modmap.at(2) = 1; //  Populated modules map            
                   ///calg . modmap.at(2) = 1; //  Populated modules map            
                   calg . modmap.at(3) = 1; //  Populated modules map            
                   ///calg . modmap.at(3) = 1; //  Populated modules map            
                   calg . modmap.at(4) = 1; //  Populated modules map            
                   ///calg . modmap.at(4) = 1; //  Populated modules map            
                   calg . modmap.at(5) = 1; //  Populated modules map            
                   ///calg . modmap.at(5) = 1; //  Populated modules map            
                   calg . modmap.at(6) = 1; //  Populated modules map            
                   ///calg . modmap.at(6) = 1; //  Populated modules map            
                   calg . modmap.at(7) = 1; //  Populated modules map            
                   ///calg . modmap.at(7) = 1; //  Populated modules map            
                   calg . modmap.at(8) = 1; //  Populated modules map            
                   ///calg . modmap.at(8) = 1; //  Populated modules map            
                   calg . modmap.at(9) = 1; //  Populated modules map            
                   ///calg . modmap.at(9) = 1; //  Populated modules map            
                   calg . modmap.at(10) = 1; //  Populated modules map            
                   ///calg . modmap.at(10) = 1; //  Populated modules map            
                   calg . modmap.at(11) = 1; //  Populated modules map            
                   ///calg . modmap.at(11) = 1; //  Populated modules map            
                   calg . modmap.at(12) = 1; //  Populated modules map            
                   ///calg . modmap.at(12) = 1; //  Populated modules map            
                   calg . modmap.at(13) = 1; //  Populated modules map            
                   ///calg . modmap.at(13) = 1; //  Populated modules map            
                   calg . modmap.at(14) = 1; //  Populated modules map            
                   ///calg . modmap.at(14) = 1; //  Populated modules map            
                   calg . modmap.at(15) = 0; //  Populated modules map            
                   ///calg . modmap.at(15) = 0; //  Populated modules map            
                   calg . modmap.at(16) = 0; //  Populated modules map            
                   ///calg . modmap.at(16) = 0; //  Populated modules map            
                   calg . modmap.at(17) = 0; //  Populated modules map            
                   ///calg . modmap.at(17) = 0; //  Populated modules map            
                   calg . modmap.at(18) = 0; //  Populated modules map            
                   ///calg . modmap.at(18) = 0; //  Populated modules map            
                   calg . modmap.at(19) = 0; //  Populated modules map            
                   ///calg . modmap.at(19) = 0; //  Populated modules map            
                   calg . modmap.at(20) = 0; //  Populated modules map            
                   ///calg . modmap.at(20) = 0; //  Populated modules map            
                   calg . modmap.at(21) = 0; //  Populated modules map            
                   ///calg . modmap.at(21) = 0; //  Populated modules map            
                   calg . modmap.at(22) = 0; //  Populated modules map            
                   ///calg . modmap.at(22) = 0; //  Populated modules map            
                   calg . modmap.at(23) = 0; //  Populated modules map            
                   ///calg . modmap.at(23) = 0; //  Populated modules map            
                   calg . modmap.at(24) = 0; //  Populated modules map            
                   ///calg . modmap.at(24) = 0; //  Populated modules map            
                   calg . modmap.at(25) = 0; //  Populated modules map            
                   ///calg . modmap.at(25) = 0; //  Populated modules map            
                   calg . modmap.at(26) = 0; //  Populated modules map            
                   ///calg . modmap.at(26) = 0; //  Populated modules map            
                   calg . modmap.at(27) = 0; //  Populated modules map            
                   ///calg . modmap.at(27) = 0; //  Populated modules map            
                   calg . modmap.at(28) = 0; //  Populated modules map            
                   ///calg . modmap.at(28) = 0; //  Populated modules map            
                   calg . modmap.at(29) = 0; //  Populated modules map            
                   ///calg . modmap.at(29) = 0; //  Populated modules map            
                   calg . modmap.at(30) = 0; //  Populated modules map            
                   ///calg . modmap.at(30) = 0; //  Populated modules map            
                   calg . modmap.at(31) = 0; //  Populated modules map            
                   ///calg . modmap.at(31) = 0; //  Populated modules map            
                   calg . modmap.at(32) = 0; //  Populated modules map            
                   ///calg . modmap.at(32) = 0; //  Populated modules map            
                   calg . modmap.at(33) = 0; //  Populated modules map            
                   ///calg . modmap.at(33) = 0; //  Populated modules map            
                   calg . modmap.at(34) = 0; //  Populated modules map            
                   ///calg . modmap.at(34) = 0; //  Populated modules map            
                   calg . modmap.at(35) = 0; //  Populated modules map            
                   ///calg . modmap.at(35) = 0; //  Populated modules map            
                   calg . modmap.at(36) = 0; //  Populated modules map            
                   ///calg . modmap.at(36) = 0; //  Populated modules map            
                   calg . modmap.at(37) = 0; //  Populated modules map            
                   ///calg . modmap.at(37) = 0; //  Populated modules map            
                   calg . modmap.at(38) = 0; //  Populated modules map            
                   ///calg . modmap.at(38) = 0; //  Populated modules map            
                   calg . modmap.at(39) = 0; //  Populated modules map            
                   ///calg . modmap.at(39) = 0; //  Populated modules map            
                   calg . modmap.at(40) = 0; //  Populated modules map            
                   ///calg . modmap.at(40) = 0; //  Populated modules map            
                   calg . modmap.at(41) = 0; //  Populated modules map            
                   ///calg . modmap.at(41) = 0; //  Populated modules map            
                   calg . modmap.at(42) = 0; //  Populated modules map            
                   ///calg . modmap.at(42) = 0; //  Populated modules map            
                   calg . modmap.at(43) = 0; //  Populated modules map            
                   ///calg . modmap.at(43) = 0; //  Populated modules map            
                   calg . modmap.at(44) = 0; //  Populated modules map            
                   ///calg . modmap.at(44) = 0; //  Populated modules map            
                   calg . modmap.at(45) = 1; //  Populated modules map            
                   ///calg . modmap.at(45) = 1; //  Populated modules map            
                   calg . modmap.at(46) = 1; //  Populated modules map            
                   ///calg . modmap.at(46) = 1; //  Populated modules map            
                   calg . modmap.at(47) = 1; //  Populated modules map            
                   ///calg . modmap.at(47) = 1; //  Populated modules map            
                   calg . modmap.at(48) = 1; //  Populated modules map            
                   ///calg . modmap.at(48) = 1; //  Populated modules map            
                   calg . modmap.at(49) = 1; //  Populated modules map            
                   ///calg . modmap.at(49) = 1; //  Populated modules map            
                   calg . modmap.at(50) = 1; //  Populated modules map            
                   ///calg . modmap.at(50) = 1; //  Populated modules map            
                   calg . modmap.at(51) = 1; //  Populated modules map            
                   ///calg . modmap.at(51) = 1; //  Populated modules map            
                   calg . modmap.at(52) = 1; //  Populated modules map            
                   ///calg . modmap.at(52) = 1; //  Populated modules map            
                   calg . modmap.at(53) = 1; //  Populated modules map            
                   ///calg . modmap.at(53) = 1; //  Populated modules map            
                   calg . modmap.at(54) = 1; //  Populated modules map            
                   ///calg . modmap.at(54) = 1; //  Populated modules map            
                   calg . modmap.at(55) = 1; //  Populated modules map            
                   ///calg . modmap.at(55) = 1; //  Populated modules map            
                   calg . modmap.at(56) = 1; //  Populated modules map            
                   ///calg . modmap.at(56) = 1; //  Populated modules map            
                   calg . modmap.at(57) = 1; //  Populated modules map            
                   ///calg . modmap.at(57) = 1; //  Populated modules map            
                   calg . modmap.at(58) = 1; //  Populated modules map            
                   ///calg . modmap.at(58) = 1; //  Populated modules map            
                   calg . modmap.at(59) = 1; //  Populated modules map            
                   ///calg . modmap.at(59) = 1; //  Populated modules map            
                   //           
                   calg.fill();           
             ///@}        
             //        
             /// USE calg _index=1;        
             calg.Use();        
             smd_width=2.*calg.g10sbthk+2.*calg.smalfthk+2.*calg.abpapthk;        
             smd_width1=2.*calg.g10sbthk+2.*calg.abpapthk;        
             smd_width2=smd_width1+calg.smgasthk+calg.smgasrad;        
             smd_width3=2.*smd_width-smd_width1-calg.smgasthk-calg.smgasrad;        
             r1=calg.rmin+2.*calg.frontthk;        
             r2=0.0;        
             /// Loop on i from 1 to nint(calg.nsuper) step=1        
             for ( i=1; (1>0)? (i<=nint(calg.nsuper)):(i>=nint(calg.nsuper)); i+=1 )        
             {           
                   layer_width(i) = calg.scintthk(i) + calg.absorthk+2.*calg.abpapthk;           
                   r2 += (calg.nsublay(i)-i+1)*layer_width(i)*2.0;           
                   rr(i)=r2;           
             }        
             r3=(calg.nsuper*layer_width(1)+(calg.nsmd-calg.nsuper)*layer_width(2))*2.;        
             r4=(smd_width+calg.scintthk(2)+2.*calg.abpapthk)*2.0;        
             cut_radius=r1+r2+r4;        
             rmax=cut_radius+2.*(calg.backthk+calg.spacethk+calg.compthk+calg.airthk);        
             tan_theta  = tan(2*atan(exp(-calg.etacut)));        
             cut_length = calg.rmin/tan_theta;        
             hleng   = cut_radius/tan_theta;        
             nn      = max(calg.nmodule(1),calg.nmodule(2));        
             deta    = 1.0/calg.netat;        
             dphimod = 360/calg.maxmodule;        
             dphit   = dphimod/calg.nsub;        
             dphitot = dphimod*nn;        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup calr_doc        
             ///@{           
                   ++calr._index;           
                   calr . rmin = r1; //  inner raduis of sensitive area            
                   /// calr . rmin = r1; //  inner raduis of sensitive area            
                   calr . rprs = r1+rr(1)/2.0; //  mean raduis of PRS            
                   /// calr . rprs = r1+rr(1)/2.0; //  mean raduis of PRS            
                   calr . rsmd1 = r1+r3+smd_width2; //  mean raduis of SMD            
                   /// calr . rsmd1 = r1+r3+smd_width2; //  mean raduis of SMD            
                   calr . rsmd2 = r1+r3+smd_width3; //  mean raduis of SMD            
                   /// calr . rsmd2 = r1+r3+smd_width3; //  mean raduis of SMD            
                   calr . rmax = cut_radius; //  outer raduis of sensitive area            
                   /// calr . rmax = cut_radius; //  outer raduis of sensitive area            
                   //           
                   calr.fill();           
             ///@}        
             //        
             /// USE calr _index=1;        
             calr.Use();        
             _create = AgCreate("CALB");        
             {           
                   AgShape myshape; // undefined shape           
                   ///Create CALB           
                   Create("CALB");            
             }        
             { AgPlacement place = AgPlacement("CALB","CAVE");           
                   /// Add daughter volume CALB to mother CAVE           
                   _stacker -> Position( AgBlock::Find("CALB"), place );           
             } // end placement of CALB        
             // Print<level=%i> fmt=%s fortran format statements not supported        
       }; // CalbGeo1     
 }; // namespace CalbGeo1  
 
#include "SvttGeo5.h"  
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
 namespace SVTTGEO5 // $NMSPC  
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
          ///Int_t nlayer;     
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
          /// @defgroup swca_doc     
          /// \class Swca_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t version;     
          ///Float_t length;     
          ///Float_t waferwid;     
          ///Float_t waferlen;     
          ///Float_t waferthk;     
          ///Float_t rohathk;     
          ///Float_t wafcarwd;     
          ///Float_t wafcarth;     
          ///Float_t wafergap;     
          ///Float_t drift;     
          ///Float_t strutlen;     
          ///Int_t _index;     
          //     
          Swca_t swca;     
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
          //  -----------------------------------------------------     
          /// @defgroup swam_doc     
          /// \class Swam_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t version;     
          ///Float_t zmin;     
          ///Float_t len;     
          ///Float_t rmin;     
          ///Float_t rmax;     
          ///Float_t tbrdthk;     
          ///Float_t wallthk;     
          ///Int_t _index;     
          //     
          Swam_t swam;     
          //     
          //  -----------------------------------------------------     
          /// @defgroup serg_doc     
          /// \class Serg_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t version;     
          ///Float_t irngtrmx;     
          ///Float_t irngprmn;     
          ///Float_t orngrmin;     
          ///Float_t orngrmax;     
          ///Float_t endrngth;     
          ///Float_t endrngzm;     
          ///Int_t _index;     
          //     
          Serg_t serg;     
          //     
          //  -----------------------------------------------------     
          /// @defgroup selc_doc     
          /// \class Selc_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t version;     
          ///Float_t bethk;     
          ///Float_t watthk;     
          ///Float_t beothk;     
          ///Float_t dyethk;     
          ///Float_t dyewid;     
          ///Float_t dyespc;     
          ///Float_t elcawid;     
          ///Float_t agpdthk;     
          ///Float_t glassthk;     
          ///Float_t cabthk;     
          ///Float_t cabwid;     
          ///Int_t _index;     
          //     
          Selc_t selc;     
          //     
          //  -----------------------------------------------------     
          /// @defgroup svtl_doc     
          /// \class Svtl_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Int_t layer;     
          ///Float_t nladder;     
          ///Float_t nwafer;     
          ///Float_t radius;     
          ///Float_t bareedge;     
          ///Float_t pcblen;     
          ///Float_t pcbwidth;     
          ///Float_t pcbthk;     
          ///Float_t pcbgap;     
          ///Int_t _index;     
          //     
          Svtl_t svtl;     
          //     
          //  -----------------------------------------------------     
          /// @defgroup ssld_doc     
          /// \class Ssld_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t version;     
          ///Float_t sinrinn;     
          ///Float_t sinrout;     
          ///Float_t sinlen;     
          ///Float_t sseprinn;     
          ///Float_t sseprout;     
          ///Float_t sseplen;     
          ///Float_t soutrinn;     
          ///Float_t soutrout;     
          ///Float_t soutlen;     
          ///Float_t almeshid;     
          ///Float_t almeshod;     
          ///Float_t almshthk;     
          ///Float_t almshpos;     
          ///Int_t _index;     
          //     
          Ssld_t ssld;     
          //     
          //  -----------------------------------------------------     
          /// @defgroup scbp_doc     
          /// \class Scbp_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Int_t layer;     
          ///Float_t len;     
          ///Float_t rmin1;     
          ///Float_t rmax1;     
          ///Float_t rmin2;     
          ///Float_t rmax2;     
          ///Float_t vol;     
          ///Int_t _index;     
          //     
          Scbp_t scbp;     
          //     
          //  -----------------------------------------------------     
          /// @defgroup sfep_doc     
          /// \class Sfep_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Int_t layer;     
          ///Float_t len;     
          ///Float_t rmin1;     
          ///Float_t rmax1;     
          ///Float_t rmin2;     
          ///Float_t rmax2;     
          ///Float_t vol;     
          ///Float_t volplast;     
          ///Int_t _index;     
          //     
          Sfep_t sfep;     
          //     
          //  -----------------------------------------------------     
          /// @defgroup swcx_doc     
          /// \class Swcx_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Int_t layer;     
          ///Float_t length;     
          ///Float_t dr;     
          ///Float_t offset;     
          ///Float_t rad;     
          ///Float_t wall;     
          ///Float_t roffset;     
          ///Int_t _index;     
          //     
          Swcx_t swcx;     
          //     
          //  -----------------------------------------------------     
          /// @defgroup soup_doc     
          /// \class Soup_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t version;     
          ///Float_t length;     
          ///Float_t rout;     
          ///Float_t dr;     
          ///Float_t phi1;     
          ///Float_t phi2;     
          ///Float_t diamout;     
          ///Float_t diamin;     
          ///Int_t _index;     
          //     
          Soup_t soup;     
          //     
          ///@addtogroup SvttGeo5_vars     
          ///@{        
                Int_t ilayer,s,side,ilad,iwaf,i,j;        
                //        
                /// Int_t ilayer,s,side,ilad,iwaf,i,j        
          ///@}     
          ///@addtogroup SvttGeo5_vars     
          ///@{        
                Float_t ladthk,cone_thk1,cone_thk2,roffset,rsizemax,deg,rad,c0;        
                //        
                /// Float_t ladthk,cone_thk1,cone_thk2,roffset,rsizemax,deg,rad,c0        
          ///@}     
          ///@addtogroup SvttGeo5_vars     
          ///@{        
                Float_t cone_len,cone_sin,cone_cos,rmin,rmax,zmin,zmax,angle;        
                //        
                /// Float_t cone_len,cone_sin,cone_cos,rmin,rmax,zmin,zmax,angle        
          ///@}     
          ///@addtogroup SvttGeo5_vars     
          ///@{        
                Float_t xpos,ypos,zpos,clearance,rin,rou,elethk,tablen,radmax;        
                //        
                /// Float_t xpos,ypos,zpos,clearance,rin,rou,elethk,tablen,radmax        
          ///@}     
          ///@addtogroup SvttGeo5_vars     
          ///@{        
                Float_t endrng_z,brack_z,screw_z,ir_rmin,ang,wafpcklen,dthk,radtilt;        
                //        
                /// Float_t endrng_z,brack_z,screw_z,ir_rmin,ang,wafpcklen,dthk,radtilt        
          ///@}     
          ///@addtogroup SvttGeo5_vars     
          ///@{        
                Float_t xbuf,phi,xbuf1,xbuf2;        
                //        
                /// Float_t xbuf,phi,xbuf1,xbuf2        
          ///@}     
          ///@addtogroup SvttGeo5_vars     
          ///@{        
                Float_t ypcb,a,cuthk,sq,tube_angle;        
                //        
                /// Float_t ypcb,a,cuthk,sq,tube_angle        
          ///@}     
          ///@addtogroup SvttGeo5_vars     
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
          ///@addtogroup SvttGeo5_vars     
          ///@{        
                Int_t i_phi;        
                //        
                /// Int_t i_phi        
          ///@}     
       SvttGeo5::SvttGeo5()     
         : AgModule("SvttGeo5","  is the SVT geometry for STAR: without the central part ")     
       {        
       }     
          // ---------------------------------------------------------------------------------------------------     
          void SVTT::Block( AgCreate create )     
          {         
                ///@addtogroup SVTT_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      rsizemax=ssup.con4idmx;           
                      rsizemax=rsizemax+ssup.grphthk+ssup.cabthk+2.0*ssup.hosrmx;           
                      rsizemax=rsizemax+ssup.wrpmythk+ssup.wrpalthk;           
                      clearance=svtg.rsizemax-rsizemax;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("SVTT");              
                            attr.par("seen")=0;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=svtg.rsizemin;              
                            shape.par("rmax")=svtg.rsizemax;              
                            shape.par("dz")=svtg.zsizemax;              
                            /// Shape Tube rmin=svtg.rsizemin rmax=svtg.rsizemax dz=svtg.zsizemax               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SVTT;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("SBRG");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create SBRG              
                            Create("SBRG");               
                      }           
                      { AgPlacement place = AgPlacement("SBRG","SVTT");              
                            /// Add daughter volume SBRG to mother SVTT              
                            place.TranslateZ(swca.length/2+ssup.erjzdis+ssup.erjthk/2);              
                            /// Translate z = swca.length/2+ssup.erjzdis+ssup.erjthk/2              
                            _stacker -> Position( AgBlock::Find("SBRG"), place );              
                      } // end placement of SBRG           
                      { AgPlacement place = AgPlacement("SBRG","SVTT");              
                            /// Add daughter volume SBRG to mother SVTT              
                            place.TranslateZ(-swca.length/2-ssup.erjzdis-ssup.erjthk/2);              
                            /// Translate z = -swca.length/2-ssup.erjzdis-ssup.erjthk/2              
                            _stacker -> Position( AgBlock::Find("SBRG"), place );              
                      } // end placement of SBRG           
                      endrng_z=serg.endrngzm+serg.endrngth;           
                      brack_z=swca.length/2+ssup.erjzdis;           
                      screw_z=endrng_z+0.5*(brack_z-endrng_z);           
                      _create = AgCreate("SOES");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create SOES              
                            Create("SOES");               
                      }           
                      { AgPlacement place = AgPlacement("SOES","SVTT");              
                            /// Add daughter volume SOES to mother SVTT              
                            place.TranslateZ(screw_z);              
                            /// Translate z = screw_z              
                            place.par("only")=AgPlacement::kMany;              
                            /// Overlap: agplacement::kmany              
                            _stacker -> Position( AgBlock::Find("SOES"), place );              
                      } // end placement of SOES           
                      { AgPlacement place = AgPlacement("SOES","SVTT");              
                            /// Add daughter volume SOES to mother SVTT              
                            place.TranslateZ(-screw_z);              
                            /// Translate z = -screw_z              
                            place.par("only")=AgPlacement::kMany;              
                            /// Overlap: agplacement::kmany              
                            _stacker -> Position( AgBlock::Find("SOES"), place );              
                      } // end placement of SOES           
                      _create = AgCreate("SIES");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create SIES              
                            Create("SIES");               
                      }           
                      { AgPlacement place = AgPlacement("SIES","SVTT");              
                            /// Add daughter volume SIES to mother SVTT              
                            place.TranslateZ(screw_z);              
                            /// Translate z = screw_z              
                            place.par("only")=AgPlacement::kMany;              
                            /// Overlap: agplacement::kmany              
                            _stacker -> Position( AgBlock::Find("SIES"), place );              
                      } // end placement of SIES           
                      { AgPlacement place = AgPlacement("SIES","SVTT");              
                            /// Add daughter volume SIES to mother SVTT              
                            place.TranslateZ(-screw_z);              
                            /// Translate z = -screw_z              
                            place.par("only")=AgPlacement::kMany;              
                            /// Overlap: agplacement::kmany              
                            _stacker -> Position( AgBlock::Find("SIES"), place );              
                      } // end placement of SIES           
                      _create = AgCreate("SCON");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create SCON              
                            Create("SCON");               
                      }           
                      { AgPlacement place = AgPlacement("SCON","SVTT");              
                            /// Add daughter volume SCON to mother SVTT              
                            _stacker -> Position( AgBlock::Find("SCON"), place );              
                      } // end placement of SCON           
                      { AgPlacement place = AgPlacement("SCON","SVTT");              
                            /// Add daughter volume SCON to mother SVTT              
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
                      _create = AgCreate("SBSP");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create SBSP              
                            Create("SBSP");               
                      }           
                      { AgPlacement place = AgPlacement("SBSP","SVTT");              
                            /// Add daughter volume SBSP to mother SVTT              
                            place.TranslateZ((ssup.rodlen/2-ssub.kmntthk/2));              
                            /// Translate z = (ssup.rodlen/2-ssub.kmntthk/2)              
                            _stacker -> Position( AgBlock::Find("SBSP"), place );              
                      } // end placement of SBSP           
                      { AgPlacement place = AgPlacement("SBSP","SVTT");              
                            /// Add daughter volume SBSP to mother SVTT              
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
                      END_OF_SVTT:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SVTT     
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
                      _create = AgCreate("STAP");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create STAP              
                            Create("STAP");               
                      }           
                      { AgPlacement place = AgPlacement("STAP","SCON");              
                            /// Add daughter volume STAP to mother SCON              
                            _stacker -> Position( AgBlock::Find("STAP"), place );              
                      } // end placement of STAP           
                      _create = AgCreate("STAC");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create STAC              
                            Create("STAC");               
                      }           
                      { AgPlacement place = AgPlacement("STAC","SCON");              
                            /// Add daughter volume STAC to mother SCON              
                            _stacker -> Position( AgBlock::Find("STAC"), place );              
                      } // end placement of STAC           
                      _create = AgCreate("SHLA");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create SHLA              
                            Create("SHLA");               
                      }           
                      { AgPlacement place = AgPlacement("SHLA","SCON");              
                            /// Add daughter volume SHLA to mother SCON              
                            place.TranslateZ(ssup.rodlen/2+ssup.grphthk+0.5*(ssup.cone3zmx-ssup.rodlen/2-ssup.grphthk));              
                            /// Translate z = ssup.rodlen/2+ssup.grphthk+0.5*(ssup.cone3zmx-ssup.rodlen/2-ssup.grphthk)              
                            _stacker -> Position( AgBlock::Find("SHLA"), place );              
                      } // end placement of SHLA           
                      _create = AgCreate("SHLB");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create SHLB              
                            Create("SHLB");               
                      }           
                      { AgPlacement place = AgPlacement("SHLB","SCON");              
                            /// Add daughter volume SHLB to mother SCON              
                            place.TranslateZ(ssup.cone3zmx+0.5*(ssup.cone4zmx-ssup.cone3zmx));              
                            /// Translate z = ssup.cone3zmx+0.5*(ssup.cone4zmx-ssup.cone3zmx)              
                            _stacker -> Position( AgBlock::Find("SHLB"), place );              
                      } // end placement of SHLB           
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
          void STAP::Block( AgCreate create )     
          {         
                ///@addtogroup STAP_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      roffset=ssup.grphthk;           
                      /// Component C	a=12	z=6	w=1           
                      /// Component H2	a=1	z=1	w=2           
                      /// Mixture CH2 dens=0.935           
                      {  AgMaterial &mix = AgMaterial::Get("Ch2");              
                            mix.Component("C",12,6,1);              
                            mix.Component("H2",1,1,2);              
                            mix.par("dens")=0.935;              
                            mix.lock();              
                            _material = mix;              
                            _material.lock();              
                      }           
                      { AgAttribute attr = AgAttribute("STAP");              
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
                            shape.Rmax(0)=ssup.con3idmn+roffset+ssup.cabthk/2;              
                            shape.Rmax(1)=ssup.con4idmn+roffset+ssup.cabthk/2;              
                            shape.Rmax(2)=ssup.con4idmx+roffset+ssup.cabthk/2;              
                            /// Shape Pcon phi1=0 dphi=360 nz=3               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_STAP;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_STAP:           
                      mCurrent = _save;           
                ///@}        
          } // End Block STAP     
          // ---------------------------------------------------------------------------------------------------     
          void STAC::Block( AgCreate create )     
          {         
                ///@addtogroup STAC_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      roffset=ssup.grphthk+ssup.cabthk/2;           
                      /// Material Copper            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Copper");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("STAC");              
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
                            shape.par("nz")=3;              
                            shape.Z(0)=ssup.rodlen/2+ssup.grphthk;              
                            shape.Z(1)=ssup.cone3zmx;              
                            shape.Z(2)=ssup.cone4zmx;              
                            shape.Rmin(0)=ssup.con3idmn+roffset;              
                            shape.Rmin(1)=ssup.con4idmn+roffset;              
                            shape.Rmin(2)=ssup.con4idmx+roffset;              
                            shape.Rmax(0)=ssup.con3idmn+roffset+ssup.cabthk/2;              
                            shape.Rmax(1)=ssup.con4idmn+roffset+ssup.cabthk/2;              
                            shape.Rmax(2)=ssup.con4idmx+roffset+ssup.cabthk/2;              
                            /// Shape Pcon phi1=0 dphi=360 nz=3               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_STAC;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_STAC:           
                      mCurrent = _save;           
                ///@}        
          } // End Block STAC     
          // ---------------------------------------------------------------------------------------------------     
          void SHLA::Block( AgCreate create )     
          {         
                ///@addtogroup SHLA_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      roffset=ssup.grphthk+ssup.cabthk;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("SHLA");              
                            attr.par("seen")=0;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Cone");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dz")=.5*(ssup.cone3zmx-ssup.rodlen/2-ssup.grphthk);              
                            shape.par("rmn1")=ssup.con3idmn+roffset;              
                            shape.par("rmx1")=ssup.con3idmn+roffset+2.0*ssup.hosrmx;              
                            shape.par("rmn2")=ssup.con4idmn+roffset;              
                            shape.par("rmx2")=ssup.con4idmn+roffset+2.0*ssup.hosrmx;              
                            /// Shape Cone dz=.5*(ssup.cone3zmx-ssup.rodlen/2-ssup.grphthk) rmn1=ssup.con3idmn+roffset rmx1=ssup.con3idmn+roffset+2.0*ssup.hosrmx rmn2=ssup.con4idmn+roffset rmx2=ssup.con4idmn+roffset+2.0*ssup.hosrmx               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SHLA;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_SHLA:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SHLA     
          // ---------------------------------------------------------------------------------------------------     
          void SHLB::Block( AgCreate create )     
          {         
                ///@addtogroup SHLB_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      roffset=ssup.grphthk+ssup.cabthk;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("SHLB");              
                            attr.par("seen")=0;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Cone");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dz")=.5*(ssup.cone4zmx-ssup.cone3zmx);              
                            shape.par("rmn1")=ssup.con4idmn+roffset;              
                            shape.par("rmx1")=ssup.con4idmn+roffset+2.0*ssup.hosrmx;              
                            shape.par("rmn2")=ssup.con4idmx+roffset;              
                            shape.par("rmx2")=ssup.con4idmx+roffset+2.0*ssup.hosrmx;              
                            /// Shape Cone dz=.5*(ssup.cone4zmx-ssup.cone3zmx) rmn1=ssup.con4idmn+roffset rmx1=ssup.con4idmn+roffset+2.0*ssup.hosrmx rmn2=ssup.con4idmx+roffset rmx2=ssup.con4idmx+roffset+2.0*ssup.hosrmx               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SHLB;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_SHLB:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SHLB     
          // ---------------------------------------------------------------------------------------------------     
          void SBRG::Block( AgCreate create )     
          {         
                ///@addtogroup SBRG_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      rin=serg.irngprmn;           
                      rou=serg.orngrmax;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("SBRG");              
                            attr.par("seen")=0;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=rin;              
                            shape.par("rmax")=rou;              
                            shape.par("dz")=ssup.erjthk/2;              
                            /// Shape Tube rmin=rin rmax=rou dz=ssup.erjthk/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SBRG;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_SBRG:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SBRG     
          // ---------------------------------------------------------------------------------------------------     
          void SOES::Block( AgCreate create )     
          {         
                ///@addtogroup SOES_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("SOES");              
                            attr.par("seen")=0;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=serg.orngrmin;              
                            shape.par("rmax")=serg.orngrmax;              
                            shape.par("dz")=0.5*(brack_z-endrng_z);              
                            /// Shape Tube rmin=serg.orngrmin rmax=serg.orngrmax dz=0.5*(brack_z-endrng_z)               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SOES;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("SOSM");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create SOSM              
                            Create("SOSM");               
                      }           
                      END_OF_SOES:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SOES     
          // ---------------------------------------------------------------------------------------------------     
          void SIES::Block( AgCreate create )     
          {         
                ///@addtogroup SIES_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("SIES");              
                            attr.par("seen")=0;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=ir_rmin;              
                            shape.par("rmax")=serg.irngtrmx;              
                            shape.par("dz")=0.5*(brack_z-endrng_z);              
                            /// Shape Tube rmin=ir_rmin rmax=serg.irngtrmx dz=0.5*(brack_z-endrng_z)               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SIES;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("SISM");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create SISM              
                            Create("SISM");               
                      }           
                      END_OF_SIES:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SIES     
          // ---------------------------------------------------------------------------------------------------     
          void SISM::Block( AgCreate create )     
          {         
                ///@addtogroup SISM_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("SISM");              
                            attr.par("seen")=0;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Division");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("ndiv")=4;              
                            shape.par("iaxis")=2;              
                            shape.par("c0")=45;              
                            /// Shape Division ndiv=4 iaxis=2 c0=45               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SISM;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("SCRW");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create SCRW              
                            Create("SCRW");               
                      }           
                      { AgPlacement place = AgPlacement("SCRW","SISM");              
                            /// Add daughter volume SCRW to mother SISM              
                            place.TranslateX(ssup.erjrad-ssup.erjlen/2+ssup.erj1x);              
                            /// Translate x = ssup.erjrad-ssup.erjlen/2+ssup.erj1x              
                            _stacker -> Position( AgBlock::Find("SCRW"), place );              
                      } // end placement of SCRW           
                      END_OF_SISM:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SISM     
          // ---------------------------------------------------------------------------------------------------     
          void SOSM::Block( AgCreate create )     
          {         
                ///@addtogroup SOSM_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("SOSM");              
                            attr.par("seen")=0;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Division");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("ndiv")=4;              
                            shape.par("iaxis")=2;              
                            shape.par("c0")=45;              
                            /// Shape Division ndiv=4 iaxis=2 c0=45               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SOSM;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("SCRW");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create SCRW              
                            Create("SCRW");               
                      }           
                      { AgPlacement place = AgPlacement("SCRW","SOSM");              
                            /// Add daughter volume SCRW to mother SOSM              
                            place.TranslateX(ssup.erjrad+ssup.erjlen/2-ssup.erj1x);              
                            /// Translate x = ssup.erjrad+ssup.erjlen/2-ssup.erj1x              
                            _stacker -> Position( AgBlock::Find("SCRW"), place );              
                      } // end placement of SCRW           
                      { AgPlacement place = AgPlacement("SCRW","SOSM");              
                            /// Add daughter volume SCRW to mother SOSM              
                            place.TranslateX(ssup.erjrad+ssup.erjlen/2-ssup.erj2x);              
                            /// Translate x = ssup.erjrad+ssup.erjlen/2-ssup.erj2x              
                            _stacker -> Position( AgBlock::Find("SCRW"), place );              
                      } // end placement of SCRW           
                      { AgPlacement place = AgPlacement("SCRW","SOSM");              
                            /// Add daughter volume SCRW to mother SOSM              
                            place.TranslateX(ssup.erjrad+ssup.erjlen/2-ssup.erj2x);              
                            /// Translate x = ssup.erjrad+ssup.erjlen/2-ssup.erj2x              
                            place.TranslateY(ssup.erj2y);              
                            /// Translate y = ssup.erj2y              
                            _stacker -> Position( AgBlock::Find("SCRW"), place );              
                      } // end placement of SCRW           
                      { AgPlacement place = AgPlacement("SCRW","SOSM");              
                            /// Add daughter volume SCRW to mother SOSM              
                            place.TranslateX(ssup.erjrad+ssup.erjlen/2-ssup.erj2x);              
                            /// Translate x = ssup.erjrad+ssup.erjlen/2-ssup.erj2x              
                            place.TranslateY(-ssup.erj2y);              
                            /// Translate y = -ssup.erj2y              
                            _stacker -> Position( AgBlock::Find("SCRW"), place );              
                      } // end placement of SCRW           
                      END_OF_SOSM:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SOSM     
          // ---------------------------------------------------------------------------------------------------     
          void SCRW::Block( AgCreate create )     
          {         
                ///@addtogroup SCRW_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Berillium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Berillium");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("SCRW");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=0;              
                            shape.par("rmax")=ssup.erjdia/2;              
                            shape.par("dz")=0.5*(brack_z-endrng_z);              
                            /// Shape Tube rmin=0 rmax=ssup.erjdia/2 dz=0.5*(brack_z-endrng_z)               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SCRW;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_SCRW:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SCRW     
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
                      { AgAttribute attr = AgAttribute("SBMO");              
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
    // ----------------------------------------------------------------------- geoctr
       void SvttGeo5::ConstructGeometry( const Char_t *dummy )     
       {        
             ///@addtogroup SvttGeo5_revision        
             ///@{           
                   /// Author: Maxim Potekhin           
             ///@}        
             ///@addtogroup SvttGeo5_revision        
             ///@{           
                   /// Created:  04 Oct 2005            
             ///@}        
             AddBlock("SVTT");        
             AddBlock("SCON");        
             AddBlock("SGRA");        
             AddBlock("STAP");        
             AddBlock("STAC");        
             AddBlock("SHLA");        
             AddBlock("SHLB");        
             AddBlock("SBRG");        
             AddBlock("SOES");        
             AddBlock("SOSM");        
             AddBlock("SCRW");        
             AddBlock("SIES");        
             AddBlock("SISM");        
             AddBlock("SBSP");        
             AddBlock("SAKM");        
             AddBlock("SBMM");        
             AddBlock("SMRD");        
             AddBlock("SBMO");        
             AddBlock("SBMI");        
             AddBlock("SCKM");        
             AddBlock("SBRL");        
             AddBlock("SBRX");        
             AddBlock("SBSR");        
             AddBlock("SBCR");        
             AddBlock("SCMY");        
             radii(1)= 6.37;        
             radii(2)= 7.38;        
             radii(3)=10.38;        
             radii(4)=11.27;        
             radii(5)=14.19;        
             radii(6)=15.13;        
             rad_offset=0.015;        
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
             ///@addtogroup swca_doc        
             ///@{           
                   ++swca._index;           
                   swca . version = 1; //  geometry version            
                   /// swca . version = 1; //  geometry version            
                   swca . length = 56.04; //  ladder length            
                   /// swca . length = 56.04; //  ladder length            
                   swca . waferwid = 6.305; //  wafer width            
                   /// swca . waferwid = 6.305; //  wafer width            
                   swca . waferlen = 6.305; //  wafer length            
                   /// swca . waferlen = 6.305; //  wafer length            
                   swca . waferthk = 0.0300; //  wafer thickness            
                   /// swca . waferthk = 0.0300; //  wafer thickness            
                   swca . rohathk = 0.0381; //  Roha cell plus glue thickness             
                   /// swca . rohathk = 0.0381; //  Roha cell plus glue thickness             
                   swca . wafcarwd = 1.5; //  wafer carrier rails width            
                   /// swca . wafcarwd = 1.5; //  wafer carrier rails width            
                   swca . wafcarth = 0.0300; //  wafer carrier thickness             
                   /// swca . wafcarth = 0.0300; //  wafer carrier thickness             
                   swca . wafergap = 0.05; //  width of the inter-wafer gap (was 0 in prev versions)            
                   /// swca . wafergap = 0.05; //  width of the inter-wafer gap (was 0 in prev versions)            
                   swca . drift = 1; //  drift direction            
                   /// swca . drift = 1; //  drift direction            
                   swca . strutlen = 1.0; //  len (z) of strut between waf car. rails (approx)            
                   /// swca . strutlen = 1.0; //  len (z) of strut between waf car. rails (approx)            
                   //           
                   swca.fill();           
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
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup serg_doc        
             ///@{           
                   ++serg._index;           
                   serg . version = 1; //  geometry version            
                   /// serg . version = 1; //  geometry version            
                   serg . irngtrmx = 9.703; //  Inner end ring tube maximum radius             
                   /// serg . irngtrmx = 9.703; //  Inner end ring tube maximum radius             
                   serg . irngprmn = 7.671; //  Inner end ring polygon minimum radius            
                   /// serg . irngprmn = 7.671; //  Inner end ring polygon minimum radius            
                   serg . orngrmin = 11.900; //  Outer end ring minimum radius            
                   /// serg . orngrmin = 11.900; //  Outer end ring minimum radius            
                   serg . orngrmax = 13.805; //  Outer end ring maximum radius            
                   /// serg . orngrmax = 13.805; //  Outer end ring maximum radius            
                   serg . endrngth = 0.2; //  End ring thickness            
                   /// serg . endrngth = 0.2; //  End ring thickness            
                   serg . endrngzm = 23.01; //  minimum z for end rings            
                   /// serg . endrngzm = 23.01; //  minimum z for end rings            
                   //           
                   serg.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup swam_doc        
             ///@{           
                   ++swam._index;           
                   swam . version = 1; //  geometry version            
                   /// swam . version = 1; //  geometry version            
                   swam . zmin = 33.9; //  minimum z for positioning water manifold            
                   /// swam . zmin = 33.9; //  minimum z for positioning water manifold            
                   swam . len = 16.0; //  water manifold full length             
                   /// swam . len = 16.0; //  water manifold full length             
                   swam . rmin = 15.24; //  water manifold rmin (not including trans. brds)            
                   /// swam . rmin = 15.24; //  water manifold rmin (not including trans. brds)            
                   swam . rmax = 16.83; //  water manifold rmax (not including trans. brds)            
                   /// swam . rmax = 16.83; //  water manifold rmax (not including trans. brds)            
                   swam . tbrdthk = 0.1; //  transition board thickness            
                   /// swam . tbrdthk = 0.1; //  transition board thickness            
                   swam . wallthk = 0.1; //  water manifold wall thickness            
                   /// swam . wallthk = 0.1; //  water manifold wall thickness            
                   //           
                   swam.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup selc_doc        
             ///@{           
                   ++selc._index;           
                   selc . version = 1; //  geometry version            
                   /// selc . version = 1; //  geometry version            
                   selc . bethk = 0.0500; //  thickness of Berillium water channel walls            
                   /// selc . bethk = 0.0500; //  thickness of Berillium water channel walls            
                   selc . watthk = 0.0750; //  thickness of water channel            
                   /// selc . watthk = 0.0750; //  thickness of water channel            
                   selc . beothk = 0.0500; //  thickness of Berrillia (BeO) substra            
                   /// selc . beothk = 0.0500; //  thickness of Berrillia (BeO) substra            
                   selc . dyethk = 0.0340; //  to give .11% of a radiation length of Si             
                   /// selc . dyethk = 0.0340; //  to give .11% of a radiation length of Si             
                   selc . dyewid = 0.3; //  width of ic chips (2 covers 0.3 of area)            
                   /// selc . dyewid = 0.3; //  width of ic chips (2 covers 0.3 of area)            
                   selc . dyespc = 0.1; //  space ic chips            
                   /// selc . dyespc = 0.1; //  space ic chips            
                   selc . elcawid = 2.0; //  electronics carrier width            
                   /// selc . elcawid = 2.0; //  electronics carrier width            
                   selc . agpdthk = 0.0030; //  thickness of Ag-Pd conductor            
                   /// selc . agpdthk = 0.0030; //  thickness of Ag-Pd conductor            
                   selc . glassthk = 0.0150; //  thickness of glass insulator            
                   /// selc . glassthk = 0.0150; //  thickness of glass insulator            
                   selc . cabthk = 0.0033; //  Cu for cables of .23% rad len (weighted average)            
                   /// selc . cabthk = 0.0033; //  Cu for cables of .23% rad len (weighted average)            
                   selc . cabwid = 0.6; //  cable width             
                   /// selc . cabwid = 0.6; //  cable width             
                   //           
                   selc.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup svtl_doc        
             ///@{           
                   ++svtl._index;           
                   svtl . layer = 1; //  layer number            
                   /// svtl . layer = 1; //  layer number            
                   svtl . radius = radii(1)+rad_offset; //  layer radius (center of wafer position)            
                   /// svtl . radius = radii(1)+rad_offset; //  layer radius (center of wafer position)            
                   svtl . nladder = 4; //  number of ladder            
                   /// svtl . nladder = 4; //  number of ladder            
                   svtl . nwafer = 4; //  number of wafers            
                   /// svtl . nwafer = 4; //  number of wafers            
                   svtl . bareedge = 1.0; //  the strip of bare Be which makes the PCB shorter            
                   /// svtl . bareedge = 1.0; //  the strip of bare Be which makes the PCB shorter            
                   svtl . pcblen = 14.9; //  PCB Length            
                   /// svtl . pcblen = 14.9; //  PCB Length            
                   svtl . pcbwidth = 6.3; //  PCB Width            
                   /// svtl . pcbwidth = 6.3; //  PCB Width            
                   svtl . pcbthk = 0.1; //  PCB Thickness -- should be 0.09, but we add extra for copper            
                   /// svtl . pcbthk = 0.1; //  PCB Thickness -- should be 0.09, but we add extra for copper            
                   svtl . pcbgap = 0.2; //  Gap between the PCB and the Si Wafer            
                   /// svtl . pcbgap = 0.2; //  Gap between the PCB and the Si Wafer            
                   //           
                   svtl.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup svtl_doc        
             ///@{           
                   ++svtl._index;           
                   svtl . layer = 2; //  layer number            
                   /// svtl . layer = 2; //  layer number            
                   svtl . radius = radii(2)+rad_offset; //  layer radius            
                   /// svtl . radius = radii(2)+rad_offset; //  layer radius            
                   svtl . pcblen = 14.9; //  PCB Length            
                   /// svtl . pcblen = 14.9; //  PCB Length            
                   //           
                   svtl.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup svtl_doc        
             ///@{           
                   ++svtl._index;           
                   svtl . layer = 3; //  layer number            
                   /// svtl . layer = 3; //  layer number            
                   svtl . radius = radii(3)+rad_offset; //  layer radius            
                   /// svtl . radius = radii(3)+rad_offset; //  layer radius            
                   svtl . nladder = 6; //  number of ladder            
                   /// svtl . nladder = 6; //  number of ladder            
                   svtl . nwafer = 6; //  number of wafers            
                   /// svtl . nwafer = 6; //  number of wafers            
                   svtl . pcblen = 7.5; //  PCB Length            
                   /// svtl . pcblen = 7.5; //  PCB Length            
                   //           
                   svtl.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup svtl_doc        
             ///@{           
                   ++svtl._index;           
                   svtl . layer = 4; //  layer number            
                   /// svtl . layer = 4; //  layer number            
                   svtl . radius = radii(4)+rad_offset; //  layer radius            
                   /// svtl . radius = radii(4)+rad_offset; //  layer radius            
                   svtl . pcblen = 7.5; //  PCB Length            
                   /// svtl . pcblen = 7.5; //  PCB Length            
                   //           
                   svtl.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup svtl_doc        
             ///@{           
                   ++svtl._index;           
                   svtl . layer = 5; //  layer number            
                   /// svtl . layer = 5; //  layer number            
                   svtl . radius = radii(5)+rad_offset; //  layer radius            
                   /// svtl . radius = radii(5)+rad_offset; //  layer radius            
                   svtl . nladder = 8; //  number of ladder            
                   /// svtl . nladder = 8; //  number of ladder            
                   svtl . nwafer = 7; //  number of wafers            
                   /// svtl . nwafer = 7; //  number of wafers            
                   svtl . pcblen = 4.4; //  PCB Length            
                   /// svtl . pcblen = 4.4; //  PCB Length            
                   //           
                   svtl.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup svtl_doc        
             ///@{           
                   ++svtl._index;           
                   svtl . layer = 6; //  layer number            
                   /// svtl . layer = 6; //  layer number            
                   svtl . radius = radii(6)+rad_offset; //  layer radius            
                   /// svtl . radius = radii(6)+rad_offset; //  layer radius            
                   svtl . pcblen = 4.4; //  PCB Length            
                   /// svtl . pcblen = 4.4; //  PCB Length            
                   //           
                   svtl.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup ssld_doc        
             ///@{           
                   ++ssld._index;           
                   ssld . version = 1; //  geometry version            
                   /// ssld . version = 1; //  geometry version            
                   ssld . sinrinn = 5; //  inner shield cylinder, inner radius            
                   /// ssld . sinrinn = 5; //  inner shield cylinder, inner radius            
                   ssld . sinrout = 5.008; //  inner shield cylinder, outer radius            
                   /// ssld . sinrout = 5.008; //  inner shield cylinder, outer radius            
                   ssld . sinlen = 53.5; //  inner shield cylinder, half length            
                   /// ssld . sinlen = 53.5; //  inner shield cylinder, half length            
                   ssld . sseprinn = 22; //  separation shield cylinder, inner radius            
                   /// ssld . sseprinn = 22; //  separation shield cylinder, inner radius            
                   ssld . sseprout = 22.018; //  separation shield cylinder, outer radius            
                   /// ssld . sseprout = 22.018; //  separation shield cylinder, outer radius            
                   ssld . sseplen = 55.4; //  separation shield cylinder, half length            
                   /// ssld . sseplen = 55.4; //  separation shield cylinder, half length            
                   ssld . soutrinn = 29.5; //  outer shield cylinder, inner radius            
                   /// ssld . soutrinn = 29.5; //  outer shield cylinder, inner radius            
                   ssld . soutrout = 29.52; //  outer shield cylinder, outer radius            
                   /// ssld . soutrout = 29.52; //  outer shield cylinder, outer radius            
                   ssld . soutlen = 65.4; //  outer shield cylinder, half length            
                   /// ssld . soutlen = 65.4; //  outer shield cylinder, half length            
                   ssld . almeshid = 9.7; //  Aluminum shield mesh inner diameter            
                   /// ssld . almeshid = 9.7; //  Aluminum shield mesh inner diameter            
                   ssld . almeshod = 44; //  Aluminum shield mesh outer diameter            
                   /// ssld . almeshod = 44; //  Aluminum shield mesh outer diameter            
                   ssld . almshthk = 0.03; //  Aluminum shield mesh effective thickness            
                   /// ssld . almshthk = 0.03; //  Aluminum shield mesh effective thickness            
                   ssld . almshpos = 53.5; //  Aluminum shield mesh z position            
                   /// ssld . almshpos = 53.5; //  Aluminum shield mesh z position            
                   //           
                   ssld.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup ssld_doc        
             ///@{           
                   ++ssld._index;           
                   ssld . version = 2; //  geometry version            
                   /// ssld . version = 2; //  geometry version            
                   ssld . sinrinn = 5.9; //  inner shield cylinder, inner radius            
                   /// ssld . sinrinn = 5.9; //  inner shield cylinder, inner radius            
                   ssld . sinrout = 5.908; //  inner shield cylinder, outer radius            
                   /// ssld . sinrout = 5.908; //  inner shield cylinder, outer radius            
                   //           
                   ssld.fill();           
             ///@}        
             //        
             /// Loop on i from 1 to 4 step=1        
             for ( i=1; (1>0)? (i<=4):(i>=4); i+=1 )        
             {           
                   rad_cones_in(i) = 8.5+2.60*(i-1);           
                   rad_cones_out(i)=15.0+0.85*(i-1);           
             }        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup scbp_doc        
             ///@{           
                   ++scbp._index;           
                   scbp . layer = 1; //  Layer            
                   /// scbp . layer = 1; //  Layer            
                   scbp . len = 1.85; //  Length            
                   /// scbp . len = 1.85; //  Length            
                   scbp . rmin1 = rad_cones_in(1); //  Min radius closer to wafers            
                   /// scbp . rmin1 = rad_cones_in(1); //  Min radius closer to wafers            
                   scbp . rmin2 = rad_cones_out(1); //  Min radius further from wafers            
                   /// scbp . rmin2 = rad_cones_out(1); //  Min radius further from wafers            
                   scbp . vol = 7.24+3.21; //  Volume of copper, LV+HV cables            
                   /// scbp . vol = 7.24+3.21; //  Volume of copper, LV+HV cables            
                   //           
                   scbp.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup scbp_doc        
             ///@{           
                   ++scbp._index;           
                   scbp . layer = 2; //  Layer            
                   /// scbp . layer = 2; //  Layer            
                   scbp . rmin1 = rad_cones_in(2); //  Min radius closer to wafers            
                   /// scbp . rmin1 = rad_cones_in(2); //  Min radius closer to wafers            
                   scbp . rmin2 = rad_cones_out(2); //  Min radius further from wafers            
                   /// scbp . rmin2 = rad_cones_out(2); //  Min radius further from wafers            
                   scbp . vol = 15.54+5.7; //  Volume of copper, LV+HV cables            
                   /// scbp . vol = 15.54+5.7; //  Volume of copper, LV+HV cables            
                   //           
                   scbp.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup scbp_doc        
             ///@{           
                   ++scbp._index;           
                   scbp . layer = 3; //  Layer            
                   /// scbp . layer = 3; //  Layer            
                   scbp . rmin1 = rad_cones_in(3); //  Min radius closer to wafers            
                   /// scbp . rmin1 = rad_cones_in(3); //  Min radius closer to wafers            
                   scbp . rmin2 = rad_cones_out(3); //  Min radius further from wafers            
                   /// scbp . rmin2 = rad_cones_out(3); //  Min radius further from wafers            
                   scbp . vol = 4.05+2.02+3.67+1.69; //  Volume of copper, LV+HV cables -- 3+4 layers coalesce            
                   /// scbp . vol = 4.05+2.02+3.67+1.69; //  Volume of copper, LV+HV cables -- 3+4 layers coalesce            
                   //           
                   scbp.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup scbp_doc        
             ///@{           
                   ++scbp._index;           
                   scbp . layer = 4; //  Layer (former 5th)            
                   /// scbp . layer = 4; //  Layer (former 5th)            
                   scbp . rmin1 = rad_cones_in(4); //  Min radius closer to wafers            
                   /// scbp . rmin1 = rad_cones_in(4); //  Min radius closer to wafers            
                   scbp . rmin2 = rad_cones_out(4); //  Min radius further from wafers            
                   /// scbp . rmin2 = rad_cones_out(4); //  Min radius further from wafers            
                   scbp . vol = 6.95+2.43; //  Volume of copper, LV+HV cables            
                   /// scbp . vol = 6.95+2.43; //  Volume of copper, LV+HV cables            
                   //           
                   scbp.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup sfep_doc        
             ///@{           
                   ++sfep._index;           
                   sfep . layer = 1; //  Layer            
                   /// sfep . layer = 1; //  Layer            
                   sfep . len = 1.85; //  Length            
                   /// sfep . len = 1.85; //  Length            
                   sfep . rmin1 = 0.5*(rad_cones_in(1) +rad_cones_in(2)); //  Min radius closer to wafers            
                   /// sfep . rmin1 = 0.5*(rad_cones_in(1) +rad_cones_in(2)); //  Min radius closer to wafers            
                   sfep . rmin2 = 0.5*(rad_cones_out(1)+rad_cones_out(2)); //  Min radius further from wafers            
                   /// sfep . rmin2 = 0.5*(rad_cones_out(1)+rad_cones_out(2)); //  Min radius further from wafers            
                   sfep . vol = 16.0; //  Volume of water            
                   /// sfep . vol = 16.0; //  Volume of water            
                   sfep . volplast = 38.4; //  Volume of plastic            
                   /// sfep . volplast = 38.4; //  Volume of plastic            
                   //           
                   sfep.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup sfep_doc        
             ///@{           
                   ++sfep._index;           
                   sfep . layer = 2; //  Layer            
                   /// sfep . layer = 2; //  Layer            
                   sfep . rmin1 = 0.5*(rad_cones_in(2) +rad_cones_in(3)); //  Min radius closer to wafers            
                   /// sfep . rmin1 = 0.5*(rad_cones_in(2) +rad_cones_in(3)); //  Min radius closer to wafers            
                   sfep . rmin2 = 0.5*(rad_cones_out(2)+rad_cones_out(3)); //  Min radius further from wafers            
                   /// sfep . rmin2 = 0.5*(rad_cones_out(2)+rad_cones_out(3)); //  Min radius further from wafers            
                   sfep . vol = 24.0; //  Volume of water            
                   /// sfep . vol = 24.0; //  Volume of water            
                   sfep . volplast = 57.6; //  Volume of plastic            
                   /// sfep . volplast = 57.6; //  Volume of plastic            
                   //           
                   sfep.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup sfep_doc        
             ///@{           
                   ++sfep._index;           
                   sfep . layer = 3; //  Layer            
                   /// sfep . layer = 3; //  Layer            
                   sfep . rmin1 = 0.5*(rad_cones_in(3) +rad_cones_in(4)); //  Min radius closer to wafers            
                   /// sfep . rmin1 = 0.5*(rad_cones_in(3) +rad_cones_in(4)); //  Min radius closer to wafers            
                   sfep . rmin2 = 0.5*(rad_cones_out(3)+rad_cones_out(4)); //  Min radius further from wafers            
                   /// sfep . rmin2 = 0.5*(rad_cones_out(3)+rad_cones_out(4)); //  Min radius further from wafers            
                   sfep . vol = 32; //  Volume of water            
                   /// sfep . vol = 32; //  Volume of water            
                   sfep . volplast = 76.8; //  Volume of plastic            
                   /// sfep . volplast = 76.8; //  Volume of plastic            
                   //           
                   sfep.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup swcx_doc        
             ///@{           
                   ++swcx._index;           
                   swcx . layer = 1; //  version            
                   /// swcx . layer = 1; //  version            
                   swcx . length = 2.8; //  of the ring in the Z direction            
                   /// swcx . length = 2.8; //  of the ring in the Z direction            
                   swcx . dr = 0.72; //  thickness of the mother layer            
                   /// swcx . dr = 0.72; //  thickness of the mother layer            
                   swcx . rad = 0.2; //  inner plastic tube radius            
                   /// swcx . rad = 0.2; //  inner plastic tube radius            
                   swcx . offset = -2.0; //  from the edge of the ladder support, inward            
                   /// swcx . offset = -2.0; //  from the edge of the ladder support, inward            
                   swcx . roffset = 1.0; //  Radial offset            
                   /// swcx . roffset = 1.0; //  Radial offset            
                   swcx . wall = 0.16; //  thickness of the plastic pipe wall            
                   /// swcx . wall = 0.16; //  thickness of the plastic pipe wall            
                   //           
                   swcx.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup soup_doc        
             ///@{           
                   ++soup._index;           
                   soup . version = 1; //  Version            
                   /// soup . version = 1; //  Version            
                   soup . length = 82.5; //  Length            
                   /// soup . length = 82.5; //  Length            
                   soup . rout = 19.41; //  Outer radius of the shield            
                   /// soup . rout = 19.41; //  Outer radius of the shield            
                   soup . dr = 0.711; //  Diameter of the tubes constituting the cage (also dR of the mother)            
                   /// soup . dr = 0.711; //  Diameter of the tubes constituting the cage (also dR of the mother)            
                   soup . phi1 = 0.0; //  Starting angle of the mother            
                   /// soup . phi1 = 0.0; //  Starting angle of the mother            
                   soup . phi2 = 70.0; //  Finishing angle of the mother            
                   /// soup . phi2 = 70.0; //  Finishing angle of the mother            
                   soup . diamout = 0.711; //  Outer diam of the carbon tube            
                   /// soup . diamout = 0.711; //  Outer diam of the carbon tube            
                   soup . diamin = 0.620; //  Inner diam of the carbon tube            
                   /// soup . diamin = 0.620; //  Inner diam of the carbon tube            
                   //           
                   soup.fill();           
             ///@}        
             //        
             /// USE svtg _index=1;        
             svtg.Use();        
             /// USE swca _index=1;        
             swca.Use();        
             /// USE selc _index=1;        
             selc.Use();        
             /// USE ssup version=svtg.conever ;        
             ssup.Use("version",(Float_t)svtg.conever );        
             /// USE ssub _index=1;        
             ssub.Use();        
             /// USE serg _index=1;        
             serg.Use();        
             /// USE swam _index=1;        
             swam.Use();        
             /// USE selc _index=1;        
             selc.Use();        
             /// USE ssld version=svtg.supportver ;        
             ssld.Use("version",(Float_t)svtg.supportver );        
             /// USE scbp _index=1;        
             scbp.Use();        
             /// USE swcx _index=1;        
             swcx.Use();        
             /// USE soup _index=1;        
             soup.Use();        
             if ( svtg.supportver==2 )        
             {           
                   svtg.rsizemin = 6.0;           
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
             /// Component C5	a=12	z=6	w=10000*5        
             /// Component H4	a=1	z=1	w=10000*4        
             /// Component O2	a=16	z=8	w=10000*2        
             /// Component Al	a=27	z=13	w=10000*0.2302        
             /// Mixture ALKAP dens=1.432        
             {  AgMaterial &mix = AgMaterial::Get("Alkap");           
                   mix.Component("C5",12,6,10000*5);           
                   mix.Component("H4",1,1,10000*4);           
                   mix.Component("O2",16,8,10000*2);           
                   mix.Component("Al",27,13,10000*0.2302);           
                   mix.par("dens")=1.432;           
                   mix.lock();           
                   _material = mix;           
                   _material.lock();           
             }        
             if ( swam.len>0 )        
             {           
                   /// Component H2	a=1	z=1	w=2           
                   /// Component O	a=16	z=8	w=1           
                   /// Mixture Water dens=1.0           
                   {  AgMaterial &mix = AgMaterial::Get("Water");              
                         mix.Component("H2",1,1,2);              
                         mix.Component("O",16,8,1);              
                         mix.par("dens")=1.0;              
                         mix.lock();              
                         _material = mix;              
                         _material.lock();              
                   }           
             }        
             else        
             {           
                   /// Mixture Water dens=0.0009           
                   {  AgMaterial &mix = AgMaterial::Get("Water");              
                         mix.par("dens")=0.0009;              
                         mix.lock();              
                         _material = mix;              
                         _material.lock();              
                   }           
             }        
             if ( svtg.ifmany>0 )        
             {           
                   _create = AgCreate("SVTT");           
                   {              
                         AgShape myshape; // undefined shape              
                         ///Create SVTT              
                         Create("SVTT");               
                   }           
                   { AgPlacement place = AgPlacement("SVTT","CAVE");              
                         /// Add daughter volume SVTT to mother CAVE              
                         place.par("only")=AgPlacement::kMany;              
                         /// Overlap: agplacement::kmany              
                         _stacker -> Position( AgBlock::Find("SVTT"), place );              
                   } // end placement of SVTT           
             }        
             else        
             {           
                   _create = AgCreate("SVTT");           
                   {              
                         AgShape myshape; // undefined shape              
                         ///Create SVTT              
                         Create("SVTT");               
                   }           
                   { AgPlacement place = AgPlacement("SVTT","CAVE");              
                         /// Add daughter volume SVTT to mother CAVE              
                         _stacker -> Position( AgBlock::Find("SVTT"), place );              
                   } // end placement of SVTT           
             }        
       }; // SvttGeo5     
 }; // namespace SvttGeo5  
 
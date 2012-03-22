#include "PhmdGeo.h"  
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
 namespace PHMDGEO // $NMSPC  
 {     
       //     
       // ---------------------------------------------------------------------------------------------------     
          //  -----------------------------------------------------     
          /// @defgroup pmvr_doc     
          /// \class Pmvr_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t version;     
          ///Int_t config;     
          ///Int_t _index;     
          //     
          Pmvr_t pmvr;     
          //     
          //  -----------------------------------------------------     
          /// @defgroup pmdg_doc     
          /// \class Pmdg_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t version;     
          ///Float_t m_max;     
          ///Float_t m_min;     
          ///Array_t<Float_t> zdist;     
          ///Float_t dpmdx;     
          ///Float_t dpmdy;     
          ///Float_t dpmdz;     
          ///Float_t pargcz;     
          ///Float_t parscz;     
          ///Float_t parfez;     
          ///Float_t parpbz;     
          ///Float_t cell_radius;     
          ///Float_t cell_depth;     
          ///Float_t cell_wall;     
          ///Float_t boundary;     
          ///Float_t th_base;     
          ///Float_t th_air;     
          ///Float_t th_pcb;     
          ///Float_t th_lead;     
          ///Float_t th_steel;     
          ///Array_t<Int_t> nx;     
          ///Array_t<Int_t> ny;     
          ///Array_t<Int_t> mx;     
          ///Array_t<Int_t> my;     
          ///Array_t<Float_t> hexd2;     
          ///Array_t<Float_t> hexd1;     
          ///Array_t<Float_t> dpara;     
          ///Int_t _index;     
          //     
          Pmdg_t pmdg;     
          //     
          ///@addtogroup PhmdGeo_vars     
          ///@{        
                Int_t j,itype,ncellx,ncelly,n,mcellx,ipms;        
                //        
                /// Int_t j,itype,ncellx,ncelly,n,mcellx,ipms        
          ///@}     
          ///@addtogroup PhmdGeo_vars     
          ///@{        
                Float_t xb,yb,zb,xlen,xlen0,ylen,ylen0,phi,phideg,xpos,ypos,xsize,ysize;        
                //        
                /// Float_t xb,yb,zb,xlen,xlen0,ylen,ylen0,phi,phideg,xpos,ypos,xsize,ysize        
          ///@}     
          ///@addtogroup PhmdGeo_vars     
          ///@{        
                Float_t sm_thick,zz,root32,root34,xlen1,xlen2,xsize1,xlen3,ylen3;        
                //        
                /// Float_t sm_thick,zz,root32,root34,xlen1,xlen2,xsize1,xlen3,ylen3        
          ///@}     
          ///@addtogroup PhmdGeo_vars     
          ///@{        
                Float_t phideg1,phideg2,phideg3,phi1,phi2,phi3,xpos1,ypos1,ylen1,ylen2;        
                //        
                /// Float_t phideg1,phideg2,phideg3,phi1,phi2,phi3,xpos1,ypos1,ylen1,ylen2        
          ///@}     
          ///@addtogroup PhmdGeo_vars     
          ///@{        
                Float_t zlen,zlen0,zlen1,sm_thick_a,xx,zlen2;        
                //        
                /// Float_t zlen,zlen0,zlen1,sm_thick_a,xx,zlen2        
          ///@}     
       PhmdGeo::PhmdGeo()     
         : AgModule("PhmdGeo","  is the geometry of photon multiplicity detector ")     
       {        
       }     
          Float_t sizen(Int_t n) { return  ((n + 1./3.)*pmdg.cell_radius)*2 + pmdg.boundary*2.*2./sqrt(3.); }     
          // ---------------------------------------------------------------------------------------------------     
          void PHMD::Block( AgCreate create )     
          {         
                ///@addtogroup PHMD_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      /// Medium STandard           
                      {  AgMedium med = AgMedium::CopyMedium("Standard");              
                            _medium = med;              
                      }           
                      { AgAttribute attr = AgAttribute("PHMD");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=pmdg.m_min;              
                            shape.par("rmax")=pmdg.m_max;              
                            shape.par("dz")=sm_thick/2.;              
                            /// Shape Tube rmin=pmdg.m_min rmax=pmdg.m_max dz=sm_thick/2.               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PHMD;              
                            _stacker -> Build(this);              
                      }           
                      phideg1=90.;           
                      phideg2=210.;           
                      phideg3=330.;           
                      phi1=phideg1*degrad;           
                      phi2=phideg2*degrad;           
                      phi3=phideg3*degrad;           
                      ipms = 1;           
                      _create = AgCreate("PHMS");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PHMS              
                            Create("PHMS");               
                      }           
                      { AgPlacement place = AgPlacement("PHMS","PHMD");              
                            /// Add daughter volume PHMS to mother PHMD              
                            place.TranslateX(xlen3*cos(phi1)-ylen3*sin(phi1)+5*pmdg.th_air);              
                            /// Translate x = xlen3*cos(phi1)-ylen3*sin(phi1)+5*pmdg.th_air              
                            place.TranslateY(xlen3*sin(phi1)+ylen3*cos(phi1));              
                            /// Translate y = xlen3*sin(phi1)+ylen3*cos(phi1)              
                            place.TranslateZ(0);              
                            /// Translate z = 0              
                            place.par("ncopy")=1;              
                            /// Ncopy: 1              
                            place.AlphaZ(phideg1);              
                            /// Rotate: AlphaZ = phideg1              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            _stacker -> Position( AgBlock::Find("PHMS"), place );              
                      } // end placement of PHMS           
                      ipms = 2;           
                      _create = AgCreate("PHMS");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PHMS              
                            Create("PHMS");               
                      }           
                      { AgPlacement place = AgPlacement("PHMS","PHMD");              
                            /// Add daughter volume PHMS to mother PHMD              
                            place.TranslateX(xlen3*cos(phi2)-ylen3*sin(phi2)-5*pmdg.th_air);              
                            /// Translate x = xlen3*cos(phi2)-ylen3*sin(phi2)-5*pmdg.th_air              
                            place.TranslateY(xlen3*sin(phi2)+ylen3*cos(phi2));              
                            /// Translate y = xlen3*sin(phi2)+ylen3*cos(phi2)              
                            place.TranslateZ(0);              
                            /// Translate z = 0              
                            place.par("ncopy")=2;              
                            /// Ncopy: 2              
                            place.AlphaZ(phideg2);              
                            /// Rotate: AlphaZ = phideg2              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            _stacker -> Position( AgBlock::Find("PHMS"), place );              
                      } // end placement of PHMS           
                      ipms = 3;           
                      _create = AgCreate("PHMS");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PHMS              
                            Create("PHMS");               
                      }           
                      { AgPlacement place = AgPlacement("PHMS","PHMD");              
                            /// Add daughter volume PHMS to mother PHMD              
                            place.TranslateX(zlen2*cos(phi3)-zlen1*sin(phi3));              
                            /// Translate x = zlen2*cos(phi3)-zlen1*sin(phi3)              
                            place.TranslateY(zlen2*sin(phi3)+zlen1*cos(phi3));              
                            /// Translate y = zlen2*sin(phi3)+zlen1*cos(phi3)              
                            place.TranslateZ(0);              
                            /// Translate z = 0              
                            place.par("ncopy")=3;              
                            /// Ncopy: 3              
                            place.AlphaZ(phideg3);              
                            /// Rotate: AlphaZ = phideg3              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            _stacker -> Position( AgBlock::Find("PHMS"), place );              
                      } // end placement of PHMS           
                      END_OF_PHMD:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PHMD     
          // ---------------------------------------------------------------------------------------------------     
          void PHMS::Block( AgCreate create )     
          {         
                ///@addtogroup PHMS_doc        
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
                      { AgAttribute attr = AgAttribute("PHMS");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      if ( ipms!=3 )           
                      {              
                            {  AgShape shape = AgShape("Para");                 
                                  shape     .Inherit( AgBlock::previous() );                 
                                  create     .SetParameters(shape);                 
                                  shape.par("dx")=xlen3*2.;                 
                                  shape.par("dy")=-ylen3;                 
                                  shape.par("dz")=sm_thick/2.;                 
                                  shape.par("alph")=30.;                 
                                  shape.par("thet")=0;                 
                                  shape.par("phi")=0;                 
                                  /// Shape Para dx=xlen3*2. dy=-ylen3 dz=sm_thick/2. alph=30. thet=0 phi=0                  
                                  _same_shape &= _stacker->SearchVolume( shape, _attribute );                 
                                  _shape = shape;                 
                                  if (_same_shape) goto END_OF_PHMS;                 
                                  _stacker -> Build(this);                 
                            }              
                            phideg =0;              
                            ypos   = (sizen(72)+sizen(48))*root34;              
                            xlen2 = -(sizen(72)+sizen(48))/4.;              
                            xsize1 = (sizen(72)+sizen(48))/2.;              
                            /// Loop on itype from 1 to 5 step=1              
                            for ( itype=1; (1>0)? (itype<=5):(itype>=5); itype+=1 )              
                            {                 
                                  xsize = sizen(pmdg.nx(itype));                 
                                  ysize = sizen(pmdg.ny(itype))-pmdg.boundary/root32;                 
                                  if ( (itype>2) )                 
                                  {                    
                                        ysize= sizen(pmdg.ny(itype));                    
                                  }                 
                                  xlen0  = xsize/2;                 
                                  ylen0  = (ysize)*root34;                 
                                  ylen   = (ysize-pmdg.boundary/root32)*root34;                 
                                  xpos = xlen2 -xsize1 +xsize/2.+ sizen(pmdg.ny(itype))/4.;                 
                                  if ( (itype>2) )                 
                                  {                    
                                        ylen = (ysize-pmdg.boundary/root34)*root34;                    
                                  }                 
                                  ylen1=0.;                 
                                  ylen2=0.;                 
                                  xlen1=1.;                 
                                  if ( (itype==2) )                 
                                  {                    
                                        xlen1=-1.;                    
                                  }                 
                                  if ( (itype>2) )                 
                                  {                    
                                        xlen1=0.;                    
                                  }                 
                                  zlen=1.;                 
                                  if ( (itype==2) )                 
                                  {                    
                                        zlen=-1.;                    
                                  }                 
                                  if ( (itype>2) )                 
                                  {                    
                                        zlen=0.;                    
                                  }                 
                                  zb = 0;                 
                                  ncellx = pmdg.nx(itype);                 
                                  ncelly = pmdg.ny(itype);                 
                                  if ( (itype==1) )                 
                                  {                    
                                        xpos=xpos + xsize + sizen(pmdg.ny(itype))- 11.*pmdg.boundary/(4.*root32)-pmdg.cell_radius*2.;                    
                                  }                 
                                  if ( (itype==2) )                 
                                  {                    
                                        xpos=xpos + sizen(pmdg.ny(itype))/2.-pmdg.boundary/(4.*root32);                    
                                  }                 
                                  if ( (itype==3) )                 
                                  {                    
                                        xpos1=xpos1 + sizen(pmdg.mx(itype-1))/2 - pmdg.boundary-2.25*pmdg.th_air;                    
                                  }                 
                                  if ( (itype==4) )                 
                                  {                    
                                        xpos=xpos + 2.*xsize1-xsize/2.;                    
                                  }                 
                                  if ( (itype==5) )                 
                                  {                    
                                        xpos1 = xpos1+2.*xsize1-xsize+ sizen(pmdg.mx(itype-1))/2.-ysize +3*pmdg.boundary*root32;                    
                                  }                 
                                  ypos = ypos-ylen0;                 
                                  _create = AgCreate("PHSR");                 
                                  {                    
                                        AgShape myshape; // undefined shape                    
                                        ///Create PHSR                    
                                        Create("PHSR");                     
                                  }                 
                                  { AgPlacement place = AgPlacement("PHSR","PHMS");                    
                                        /// Add daughter volume PHSR to mother PHMS                    
                                        place.TranslateX(xpos);                    
                                        /// Translate x = xpos                    
                                        place.TranslateY(ypos);                    
                                        /// Translate y = ypos                    
                                        place.TranslateZ(0.);                    
                                        /// Translate z = 0.                    
                                        place.AlphaZ(phideg);                    
                                        /// Rotate: AlphaZ = phideg                    
                                        /// G3 Reference: thetax = 90                    
                                        /// G3 Reference: phix = 0                    
                                        /// G3 Reference: thetay = 90                    
                                        /// G3 Reference: phiy = 90                    
                                        /// G3 Reference: thetaz = 0                    
                                        /// G3 Reference: phiz = 0                    
                                        _stacker -> Position( AgBlock::Find("PHSR"), place );                    
                                  } // end placement of PHSR                 
                                  ypos = ypos-ylen0;                 
                                  if ( (itype==3) )                 
                                  {                    
                                        ypos=(sizen(72)+sizen(48))*root34;                    
                                  }                 
                            }              
                      }           
                      else           
                      {              
                            {  AgShape shape = AgShape("Para");                 
                                  shape     .Inherit( AgBlock::previous() );                 
                                  create     .SetParameters(shape);                 
                                  shape.par("dx")=zlen2*2.;                 
                                  shape.par("dy")=-zlen1;                 
                                  shape.par("dz")=sm_thick/2.;                 
                                  shape.par("alph")=30.;                 
                                  shape.par("thet")=0;                 
                                  shape.par("phi")=0;                 
                                  /// Shape Para dx=zlen2*2. dy=-zlen1 dz=sm_thick/2. alph=30. thet=0 phi=0                  
                                  _same_shape &= _stacker->SearchVolume( shape, _attribute );                 
                                  _shape = shape;                 
                                  if (_same_shape) goto END_OF_PHMS;                 
                                  _stacker -> Build(this);                 
                            }              
                            phideg =0;              
                            ypos1   = (sizen(72)+sizen(48)+2.*pmdg.boundary/root32+.375)*root34;              
                            xlen2 = -(sizen(72)+sizen(48)+3.75*pmdg.th_air)/4.;              
                            xsize1 = (sizen(72)+sizen(48)+4.5*pmdg.th_air)/2.;              
                            /// Loop on itype from 1 to 7 step=1              
                            for ( itype=1; (1>0)? (itype<=7):(itype>=7); itype+=1 )              
                            {                 
                                  xsize = sizen(pmdg.mx(itype));                 
                                  ysize = sizen(pmdg.my(itype));                 
                                  xlen0 = xsize/2;                 
                                  ylen0 = (ysize)*root34;                 
                                  zlen0 = (ysize)*root34;                 
                                  if ( (itype==2) )                 
                                  {                    
                                        zlen0=(ysize-pmdg.boundary/root32)*root34+3.75*pmdg.th_air;                    
                                  }                 
                                  if ( (itype==3) )                 
                                  {                    
                                        zlen0=(ysize-pmdg.boundary/root32)*root34-3.75*pmdg.th_air;                    
                                  }                 
                                  if ( (itype==4) )                 
                                  {                    
                                        zlen0=(ysize)*root34+3.75*pmdg.th_air;                    
                                  }                 
                                  if ( (itype==5) )                 
                                  {                    
                                        zlen0=(ysize+pmdg.boundary/root32)*root34;                    
                                  }                 
                                  xpos1 = xlen2 -xsize1 + xsize/2.+ ysize/4.;                 
                                  ylen = (ysize-2*pmdg.boundary)*root34;                 
                                  ylen1 = 0.;                 
                                  if ( (itype==1) )                 
                                  {                    
                                        ylen1=1.;                    
                                  }                 
                                  if ( (itype==2) )                 
                                  {                    
                                        ylen1=-1.;                    
                                  }                 
                                  if ( (itype==3) )                 
                                  {                    
                                        ylen1=-1.;                    
                                  }                 
                                  if ( (itype==5) )                 
                                  {                    
                                        ylen1=2.;                    
                                  }                 
                                  if ( (itype==6) )                 
                                  {                    
                                        ylen1=1.;                    
                                  }                 
                                  ylen2 = 0.;                 
                                  if ( (itype==2) )                 
                                  {                    
                                        ylen2=1.;                    
                                  }                 
                                  if ( (itype==3) )                 
                                  {                    
                                        ylen2=1.;                    
                                  }                 
                                  if ( (itype==5) )                 
                                  {                    
                                        ylen2=-1.;                    
                                  }                 
                                  xlen1=0.;                 
                                  if ( (itype==1) )                 
                                  {                    
                                        xlen1=-2.;                    
                                  }                 
                                  if ( (itype==2) )                 
                                  {                    
                                        xlen1=3.;                    
                                  }                 
                                  if ( (itype==3) )                 
                                  {                    
                                        xlen1=1.;                    
                                  }                 
                                  if ( (itype==5) )                 
                                  {                    
                                        xlen1=-1.;                    
                                  }                 
                                  if ( (itype==6) )                 
                                  {                    
                                        xlen1=2.;                    
                                  }                 
                                  zlen=0.;                 
                                  if ( (itype==2) )                 
                                  {                    
                                        zlen=0.75;                    
                                  }                 
                                  if ( (itype==3) )                 
                                  {                    
                                        zlen=-0.75;                    
                                  }                 
                                  if ( (itype==5) )                 
                                  {                    
                                        zlen=-1.;                    
                                  }                 
                                  zb = 0;                 
                                  ncellx = pmdg.mx(itype);                 
                                  ncelly = pmdg.my(itype);                 
                                  if ( (itype==1) )                 
                                  {                    
                                        xpos1=xpos1 + sizen(pmdg.mx(itype+1))/2.+2.*ysize-2* pmdg.boundary/root32-pmdg.boundary/3.;                    
                                  }                 
                                  if ( (itype==2) )                 
                                  {                    
                                        xpos1=xpos1 + xsize/2.+ ysize/2.-pmdg.boundary*root32-pmdg.boundary-2.25*pmdg.th_air;                    
                                  }                 
                                  if ( (itype==3) )                 
                                  {                    
                                        xpos1=xpos1 + sizen(pmdg.mx(itype-1))/2 - pmdg.boundary-2.25*pmdg.th_air;                    
                                  }                 
                                  if ( (itype==4) )                 
                                  {                    
                                        xpos1 = xpos1 -pmdg.boundary*root32;                    
                                  }                 
                                  if ( (itype==5) )                 
                                  {                    
                                        xpos1 = xpos1+2.*xsize1-xsize+ sizen(pmdg.mx(itype-1))/2.-ysize +3*pmdg.boundary*root32+2.2;                    
                                  }                 
                                  if ( (itype==6) )                 
                                  {                    
                                        xpos1=xpos1 + 2.*xsize1-xsize/2.+pmdg.boundary/root32+2.25*pmdg.th_air;                    
                                  }                 
                                  if ( (itype==7) )                 
                                  {                    
                                        xpos1=xpos1 + 2.*xsize1-xsize-pmdg.boundary/(2.*root32)+pmdg.boundary;                    
                                  }                 
                                  ypos1 = ypos1-zlen0;                 
                                  _create = AgCreate("PHSR");                 
                                  {                    
                                        AgShape myshape; // undefined shape                    
                                        ///Create PHSR                    
                                        Create("PHSR");                     
                                  }                 
                                  { AgPlacement place = AgPlacement("PHSR","PHMS");                    
                                        /// Add daughter volume PHSR to mother PHMS                    
                                        place.TranslateX(xpos1);                    
                                        /// Translate x = xpos1                    
                                        place.TranslateY(ypos1);                    
                                        /// Translate y = ypos1                    
                                        place.TranslateZ(0.);                    
                                        /// Translate z = 0.                    
                                        place.AlphaZ(phideg);                    
                                        /// Rotate: AlphaZ = phideg                    
                                        /// G3 Reference: thetax = 90                    
                                        /// G3 Reference: phix = 0                    
                                        /// G3 Reference: thetay = 90                    
                                        /// G3 Reference: phiy = 90                    
                                        /// G3 Reference: thetaz = 0                    
                                        /// G3 Reference: phiz = 0                    
                                        _stacker -> Position( AgBlock::Find("PHSR"), place );                    
                                  } // end placement of PHSR                 
                                  ypos1 = ypos1-zlen0;                 
                                  if ( (itype==4) )                 
                                  {                    
                                        ypos1=(sizen(72)+sizen(48)+2.*pmdg.boundary/root32+.375)*root34;                    
                                  }                 
                                  if ( (itype==5) )                 
                                  {                    
                                        ypos1=(sizen(72)+sizen(48)+2.*pmdg.boundary/root32+.375)*root34;                    
                                  }                 
                                  if ( (itype==6) )                 
                                  {                    
                                        ypos1=ypos1-4.*pmdg.th_air;                    
                                  }                 
                            }              
                      }           
                      END_OF_PHMS:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PHMS     
          // ---------------------------------------------------------------------------------------------------     
          void PHSR::Block( AgCreate create )     
          {         
                ///@addtogroup PHSR_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("PHSR");              
                            attr.par("seen")=1;              
                            attr.par("colo")=6;              
                            attr.par("serial")=0;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Para");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=xlen0-ylen1*pmdg.boundary/(2.*root32);              
                            shape.par("dy")=ylen0-ylen2*pmdg.boundary*root34/root32;              
                            shape.par("dz")=sm_thick/2;              
                            shape.par("alph")=30;              
                            shape.par("thet")=0;              
                            shape.par("phi")=0;              
                            /// Shape Para dx=xlen0-ylen1*pmdg.boundary/(2.*root32) dy=ylen0-ylen2*pmdg.boundary*root34/root32 dz=sm_thick/2 alph=30 thet=0 phi=0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PHSR;              
                            _stacker -> Build(this);              
                      }           
                      xx = -sm_thick/2.+ sm_thick_a/2.;           
                      _create = AgCreate("PMDA");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PMDA              
                            Create("PMDA");               
                      }           
                      { AgPlacement place = AgPlacement("PMDA","PHSR");              
                            /// Add daughter volume PMDA to mother PHSR              
                            place.TranslateZ(xx);              
                            /// Translate z = xx              
                            _stacker -> Position( AgBlock::Find("PMDA"), place );              
                      } // end placement of PMDA           
                      xx = -sm_thick/2. + sm_thick_a + pmdg.th_lead/2.;           
                      _create = AgCreate("PPBA");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PPBA              
                            Create("PPBA");               
                      }           
                      { AgPlacement place = AgPlacement("PPBA","PHSR");              
                            /// Add daughter volume PPBA to mother PHSR              
                            place.TranslateZ(xx);              
                            /// Translate z = xx              
                            _stacker -> Position( AgBlock::Find("PPBA"), place );              
                      } // end placement of PPBA           
                      xx = xx + pmdg.th_lead/2. + pmdg.th_steel/2.;           
                      _create = AgCreate("PFEA");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PFEA              
                            Create("PFEA");               
                      }           
                      { AgPlacement place = AgPlacement("PFEA","PHSR");              
                            /// Add daughter volume PFEA to mother PHSR              
                            place.TranslateZ(xx);              
                            /// Translate z = xx              
                            _stacker -> Position( AgBlock::Find("PFEA"), place );              
                      } // end placement of PFEA           
                      xx = xx + pmdg.th_steel/2. + sm_thick_a/2.;           
                      _create = AgCreate("PMDA");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PMDA              
                            Create("PMDA");               
                      }           
                      { AgPlacement place = AgPlacement("PMDA","PHSR");              
                            /// Add daughter volume PMDA to mother PHSR              
                            place.TranslateZ(xx);              
                            /// Translate z = xx              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 180              
                            /// G3 Reference: phiz = 0              
                            Double_t _thetax=90,_phix=0,_thetay=90,_phiy=90,_thetaz=180,_phiz=0;              
                            place.Reference( _thetax, _phix, _thetay, _phiy, _thetaz, _phiz );              
                            _stacker -> Position( AgBlock::Find("PMDA"), place );              
                      } // end placement of PMDA           
                      END_OF_PHSR:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PHSR     
          // ---------------------------------------------------------------------------------------------------     
          void PMDA::Block( AgCreate create )     
          {         
                ///@addtogroup PMDA_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Aluminium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Aluminium");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("PMDA");              
                            attr.par("seen")=1;              
                            attr.par("colo")=6;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Para");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=xlen0-ylen1*pmdg.boundary/(2.*root32);              
                            shape.par("dy")=ylen0-ylen2*pmdg.boundary*root34/root32;              
                            shape.par("dz")=sm_thick_a/2.;              
                            shape.par("alph")=30;              
                            shape.par("thet")=0;              
                            shape.par("phi")=0;              
                            /// Shape Para dx=xlen0-ylen1*pmdg.boundary/(2.*root32) dy=ylen0-ylen2*pmdg.boundary*root34/root32 dz=sm_thick_a/2. alph=30 thet=0 phi=0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PMDA;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("AIRA");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create AIRA              
                            Create("AIRA");               
                      }           
                      { AgPlacement place = AgPlacement("AIRA","PMDA");              
                            /// Add daughter volume AIRA to mother PMDA              
                            place.TranslateX(-pmdg.boundary/(4.*root32)*xlen1);              
                            /// Translate x = -pmdg.boundary/(4.*root32)*xlen1              
                            place.TranslateY(-pmdg.boundary/2.*zlen);              
                            /// Translate y = -pmdg.boundary/2.*zlen              
                            _stacker -> Position( AgBlock::Find("AIRA"), place );              
                      } // end placement of AIRA           
                      END_OF_PMDA:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PMDA     
          // ---------------------------------------------------------------------------------------------------     
          void AIRA::Block( AgCreate create )     
          {         
                ///@addtogroup AIRA_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("AIRA");              
                            attr.par("seen")=1;              
                            attr.par("colo")=4;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Para");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=xlen0-pmdg.boundary/root32;              
                            shape.par("dy")=ylen;              
                            shape.par("dz")=sm_thick_a/2.;              
                            /// Shape Para dx=xlen0-pmdg.boundary/root32 dy=ylen dz=sm_thick_a/2.               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_AIRA;              
                            _stacker -> Build(this);              
                      }           
                      zz = -sm_thick_a/2. + pmdg.th_pcb/2.;           
                      _create = AgCreate("PCBA");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PCBA              
                            Create("PCBA");               
                      }           
                      { AgPlacement place = AgPlacement("PCBA","AIRA");              
                            /// Add daughter volume PCBA to mother AIRA              
                            place.TranslateZ(zz);              
                            /// Translate z = zz              
                            _stacker -> Position( AgBlock::Find("PCBA"), place );              
                      } // end placement of PCBA           
                      zz = zz + pmdg.th_pcb/2 +3.* pmdg.th_air + pmdg.th_pcb/2.;           
                      _create = AgCreate("PCBA");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PCBA              
                            Create("PCBA");               
                      }           
                      { AgPlacement place = AgPlacement("PCBA","AIRA");              
                            /// Add daughter volume PCBA to mother AIRA              
                            place.TranslateZ(zz);              
                            /// Translate z = zz              
                            _stacker -> Position( AgBlock::Find("PCBA"), place );              
                      } // end placement of PCBA           
                      zz = zz + pmdg.th_pcb/2. + pmdg.cell_depth/2.;           
                      _create = AgCreate("PHCA");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PHCA              
                            Create("PHCA");               
                      }           
                      { AgPlacement place = AgPlacement("PHCA","AIRA");              
                            /// Add daughter volume PHCA to mother AIRA              
                            place.TranslateZ(zz);              
                            /// Translate z = zz              
                            _stacker -> Position( AgBlock::Find("PHCA"), place );              
                      } // end placement of PHCA           
                      zz = zz + pmdg.cell_depth/2. + pmdg.th_pcb/2.;           
                      _create = AgCreate("PCBA");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PCBA              
                            Create("PCBA");               
                      }           
                      { AgPlacement place = AgPlacement("PCBA","AIRA");              
                            /// Add daughter volume PCBA to mother AIRA              
                            place.TranslateZ(zz);              
                            /// Translate z = zz              
                            _stacker -> Position( AgBlock::Find("PCBA"), place );              
                      } // end placement of PCBA           
                      zz = zz + pmdg.th_pcb/2. + pmdg.th_air + pmdg.th_base/2.;           
                      _create = AgCreate("BASA");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create BASA              
                            Create("BASA");               
                      }           
                      { AgPlacement place = AgPlacement("BASA","AIRA");              
                            /// Add daughter volume BASA to mother AIRA              
                            place.TranslateZ(zz);              
                            /// Translate z = zz              
                            _stacker -> Position( AgBlock::Find("BASA"), place );              
                      } // end placement of BASA           
                      END_OF_AIRA:           
                      mCurrent = _save;           
                ///@}        
          } // End Block AIRA     
          // ---------------------------------------------------------------------------------------------------     
          void PHCA::Block( AgCreate create )     
          {         
                ///@addtogroup PHCA_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("PHCA");              
                            attr.par("seen")=1;              
                            attr.par("colo")=4;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Para");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=xlen0-pmdg.boundary/root32;              
                            shape.par("dy")=ylen;              
                            shape.par("dz")=pmdg.cell_depth/2.;              
                            /// Shape Para dx=xlen0-pmdg.boundary/root32 dy=ylen dz=pmdg.cell_depth/2.               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PHCA;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("ASTR");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create ASTR              
                            Create("ASTR");               
                      }           
                      /// Loop on j from 1 to ncelly step=1           
                      for ( j=1; (1>0)? (j<=ncelly):(j>=ncelly); j+=1 )           
                      {              
                            xb=-ylen*(1/(2*root32))+pmdg.hexd1(7)*2./3.  +(j-1)*pmdg.hexd1(7);              
                            yb=-ylen+(2./sqrt(3.))*pmdg.hexd1(7)+(j-1)*pmdg.hexd1(7)*sqrt(3.);              
                            { AgPlacement place = AgPlacement("ASTR","PHCA");                 
                                  /// Add daughter volume ASTR to mother PHCA                 
                                  place.TranslateX(xb);                 
                                  /// Translate x = xb                 
                                  place.TranslateY(yb);                 
                                  /// Translate y = yb                 
                                  place.TranslateZ(zb);                 
                                  /// Translate z = zb                 
                                  place.par("only")=AgPlacement::kMany;                 
                                  /// Overlap: agplacement::kmany                 
                                  _stacker -> Position( AgBlock::Find("ASTR"), place );                 
                            } // end placement of ASTR              
                      }           
                      END_OF_PHCA:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PHCA     
          // ---------------------------------------------------------------------------------------------------     
          void ASTR::Block( AgCreate create )     
          {         
                ///@addtogroup ASTR_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("ASTR");              
                            attr.par("seen")=0;              
                            attr.par("colo")=6;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      xlen=ncellx*pmdg.cell_radius;           
                      {  AgShape shape = AgShape("Para");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=xlen;              
                            shape.par("dy")=pmdg.cell_radius/root32;              
                            shape.par("dz")=pmdg.cell_depth/2;              
                            shape.par("alph")=0;              
                            /// Shape Para dx=xlen dy=pmdg.cell_radius/root32 dz=pmdg.cell_depth/2 alph=0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_ASTR;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("PSTR");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PSTR              
                            Create("PSTR");               
                      }           
                      END_OF_ASTR:           
                      mCurrent = _save;           
                ///@}        
          } // End Block ASTR     
          // ---------------------------------------------------------------------------------------------------     
          void PSTR::Block( AgCreate create )     
          {         
                ///@addtogroup PSTR_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      {  AgShape shape = AgShape("Division");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("ndiv")=ncellx;              
                            shape.par("iaxis")=1;              
                            /// Shape Division ndiv=ncellx iaxis=1               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PSTR;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("PDCU");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PDCU              
                            Create("PDCU");               
                      }           
                      { AgPlacement place = AgPlacement("PDCU","PSTR");              
                            /// Add daughter volume PDCU to mother PSTR              
                            place.AlphaZ(90);              
                            /// Rotate: AlphaZ = 90              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            _stacker -> Position( AgBlock::Find("PDCU"), place );              
                      } // end placement of PDCU           
                      END_OF_PSTR:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PSTR     
          // ---------------------------------------------------------------------------------------------------     
          void PPBA::Block( AgCreate create )     
          {         
                ///@addtogroup PPBA_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Lead            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Lead");              
                            _material = mat;              
                      }           
                      /// Material Lead_PPBA isvol=0            
                      { AgMaterial &mat = AgMaterial::Get("Lead_ppba");              
                            mat.par("isvol")=0;              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("PPBA");              
                            attr.par("seen")=1;              
                            attr.par("colo")=7;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Para");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=xlen0-ylen1*pmdg.boundary/(2.*root32);              
                            shape.par("dy")=ylen0-ylen2*pmdg.boundary*root34/root32;              
                            shape.par("dz")=pmdg.th_lead/2.;              
                            /// Shape Para dx=xlen0-ylen1*pmdg.boundary/(2.*root32) dy=ylen0-ylen2*pmdg.boundary*root34/root32 dz=pmdg.th_lead/2.               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PPBA;              
                            _stacker -> Build(this);              
                      }           
                      if ( pmvr.config==1 )           
                      {              
                            // _medium.par("CUTGAM") = .0001;              
                            // _medium.par("CUTELE") = .0001;              
                      }           
                      END_OF_PPBA:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PPBA     
          // ---------------------------------------------------------------------------------------------------     
          void PFEA::Block( AgCreate create )     
          {         
                ///@addtogroup PFEA_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Iron            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Iron");              
                            _material = mat;              
                      }           
                      /// Material Iron_PFEA isvol=0            
                      { AgMaterial &mat = AgMaterial::Get("Iron_pfea");              
                            mat.par("isvol")=0;              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("PFEA");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Para");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=xlen0-ylen1*pmdg.boundary/(2.*root32);              
                            shape.par("dy")=ylen0-ylen2*pmdg.boundary*root34/root32;              
                            shape.par("dz")=pmdg.th_steel/2.;              
                            /// Shape Para dx=xlen0-ylen1*pmdg.boundary/(2.*root32) dy=ylen0-ylen2*pmdg.boundary*root34/root32 dz=pmdg.th_steel/2.               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PFEA;              
                            _stacker -> Build(this);              
                      }           
                      if ( pmvr.config==1 )           
                      {              
                            // _medium.par("CUTGAM") = .0001;              
                            // _medium.par("CUTELE") = .0001;              
                      }           
                      END_OF_PFEA:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PFEA     
          // ---------------------------------------------------------------------------------------------------     
          void BASA::Block( AgCreate create )     
          {         
                ///@addtogroup BASA_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
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
                      { AgAttribute attr = AgAttribute("BASA");              
                            attr.par("seen")=1;              
                            attr.par("colo")=6;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Para");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=xlen0-pmdg.boundary/root32;              
                            shape.par("dy")=ylen;              
                            shape.par("dz")=pmdg.th_base/2.;              
                            /// Shape Para dx=xlen0-pmdg.boundary/root32 dy=ylen dz=pmdg.th_base/2.               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_BASA;              
                            _stacker -> Build(this);              
                      }           
                      if ( pmvr.config==1 )           
                      {              
                            // _medium.par("CUTGAM") = .0001;              
                            // _medium.par("CUTELE") = .0001;              
                      }           
                      END_OF_BASA:           
                      mCurrent = _save;           
                ///@}        
          } // End Block BASA     
          // ---------------------------------------------------------------------------------------------------     
          void PCBA::Block( AgCreate create )     
          {         
                ///@addtogroup PCBA_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
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
                      { AgAttribute attr = AgAttribute("PCBA");              
                            attr.par("seen")=1;              
                            attr.par("colo")=4;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Para");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=xlen0-pmdg.boundary/root32;              
                            shape.par("dy")=ylen;              
                            shape.par("dz")=pmdg.th_pcb/2.;              
                            /// Shape Para dx=xlen0-pmdg.boundary/root32 dy=ylen dz=pmdg.th_pcb/2.               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PCBA;              
                            _stacker -> Build(this);              
                      }           
                      if ( pmvr.config==1 )           
                      {              
                            // _medium.par("CUTGAM") = .0001;              
                            // _medium.par("CUTELE") = .0001;              
                      }           
                      END_OF_PCBA:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PCBA     
          // ---------------------------------------------------------------------------------------------------     
          void PDCU::Block( AgCreate create )     
          {         
                ///@addtogroup PDCU_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Copper            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Copper");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("PDCU");              
                            attr.par("seen")=0;              
                            attr.par("colo")=3;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Pgon");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("phi1")=pmdg.hexd1(1);              
                            shape.par("dphi")=pmdg.hexd1(2);              
                            shape.par("npdiv")=pmdg.hexd1(3);              
                            shape.par("nz")=pmdg.hexd1(4);              
                            shape.Z(0)=pmdg.hexd1(5);              
                            shape.Z(1)=pmdg.hexd1(8);              
                            shape.Rmin(0)=pmdg.hexd1(6);              
                            shape.Rmin(1)=pmdg.hexd1(9);              
                            shape.Rmax(0)=pmdg.hexd1(7);              
                            shape.Rmax(1)=pmdg.hexd1(10);              
                            /// Shape Pgon phi1=pmdg.hexd1(1) dphi=pmdg.hexd1(2) npdiv=pmdg.hexd1(3) nz=pmdg.hexd1(4)               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PDCU;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("PDGS");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PDGS              
                            Create("PDGS");               
                      }           
                      { AgPlacement place = AgPlacement("PDGS","PDCU");              
                            /// Add daughter volume PDGS to mother PDCU              
                            place.TranslateX(0.0);              
                            /// Translate x = 0.0              
                            place.TranslateY(0.0);              
                            /// Translate y = 0.0              
                            place.TranslateZ(0.0);              
                            /// Translate z = 0.0              
                            _stacker -> Position( AgBlock::Find("PDGS"), place );              
                      } // end placement of PDGS           
                      END_OF_PDCU:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PDCU     
          // ---------------------------------------------------------------------------------------------------     
          void PDGS::Block( AgCreate create )     
          {         
                ///@addtogroup PDGS_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Medium sensitive           
                      ///  isvol = 1            
                      {  AgMedium &med = AgMedium::Get("Sensitive");              
                               med.Inherit(this);              
                            med.par("isvol")=1 ;              
                            _medium = med;              
                      }           
                      /// Component Ar	a=40	z=18	w=.7           
                      /// Component C	a=12	z=6	w=.3*12/44.           
                      /// Component O	a=16	z=8	w=.3*32/44.           
                      /// Mixture Ar_CO2 dens=0.0018405           
                      {  AgMaterial &mix = AgMaterial::Get("Ar_co2");              
                            mix.Component("Ar",40,18,.7);              
                            mix.Component("C",12,6,.3*12/44.);              
                            mix.Component("O",16,8,.3*32/44.);              
                            mix.par("dens")=0.0018405;              
                            mix.lock();              
                            _material = mix;              
                            _material.lock();              
                      }           
                      { AgAttribute attr = AgAttribute("PDGS");              
                            attr.par("seen")=0;              
                            attr.par("colo")=3;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Pgon");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("phi1")=pmdg.hexd2(1);              
                            shape.par("dphi")=pmdg.hexd2(2);              
                            shape.par("npdiv")=pmdg.hexd2(3);              
                            shape.par("nz")=pmdg.hexd2(4);              
                            shape.Z(0)=pmdg.hexd2(5);              
                            shape.Z(1)=pmdg.hexd2(8);              
                            shape.Rmin(0)=pmdg.hexd2(6);              
                            shape.Rmin(1)=pmdg.hexd2(9);              
                            shape.Rmax(0)=pmdg.hexd2(7);              
                            shape.Rmax(1)=pmdg.hexd2(10);              
                            /// Shape Pgon phi1=pmdg.hexd2(1) dphi=pmdg.hexd2(2) npdiv=pmdg.hexd2(3) nz=pmdg.hexd2(4)               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PDGS;              
                            _stacker -> Build(this);              
                      }           
                      if ( pmvr.config==1 )           
                      {              
                            // _medium.par("CUTGAM") = .0001;              
                            // _medium.par("CUTELE") = .00001;              
                      }           
                      END_OF_PDGS:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PDGS     
    // ----------------------------------------------------------------------- geoctr
       void PhmdGeo::ConstructGeometry( const Char_t *dummy )     
       {        
             ///@addtogroup PhmdGeo_revision        
             ///@{           
                   /// Author: Subhasis, Viyogi, Bedanga,Tapan and Dipak           
             ///@}        
             ///@addtogroup PhmdGeo_revision        
             ///@{           
                   /// Created:    03-july-2001            
             ///@}        
             AddBlock("PHMD");        
             AddBlock("PHMS");        
             AddBlock("PHSR");        
             AddBlock("PMDA");        
             AddBlock("AIRA");        
             AddBlock("PHCA");        
             AddBlock("PPBA");        
             AddBlock("PFEA");        
             AddBlock("PCBA");        
             AddBlock("BASA");        
             AddBlock("ASTR");        
             AddBlock("PSTR");        
             AddBlock("PDCU");        
             AddBlock("PDGS");        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup pmvr_doc        
             ///@{           
                   ++pmvr._index;           
                   pmvr . version = 1; //  geometry version            
                   /// pmvr . version = 1; //  geometry version            
                   pmvr . config = 1; //  general configuration of the detector            
                   /// pmvr . config = 1; //  general configuration of the detector            
                   //           
                   pmvr.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup pmdg_doc        
             ///@{           
                   ++pmdg._index;           
                   pmdg . version = 1; //  geometry version            
                   /// pmdg . version = 1; //  geometry version            
                   pmdg . m_max = 135.0; //  Mother volume max radius            
                   /// pmdg . m_max = 135.0; //  Mother volume max radius            
                   pmdg . m_min = 22.0; //  Mother volume min radius            
                   /// pmdg . m_min = 22.0; //  Mother volume min radius            
                   pmdg . zdist.at(0) = -539.; //  PMD placed at 5.39 metre from the interaction point            
                   ///pmdg . zdist.at(0) = -539.; //  PMD placed at 5.39 metre from the interaction point            
                   pmdg . zdist.at(1) = -550.; //  PMD placed at 5.39 metre from the interaction point            
                   ///pmdg . zdist.at(1) = -550.; //  PMD placed at 5.39 metre from the interaction point            
                   pmdg . dpmdx = 270; //   (X-halfwidth of the PMD box,was 190 earlier)            
                   /// pmdg . dpmdx = 270; //   (X-halfwidth of the PMD box,was 190 earlier)            
                   pmdg . dpmdy = 270; //  Y-halfwidth of the  PMD box.            
                   /// pmdg . dpmdy = 270; //  Y-halfwidth of the  PMD box.            
                   pmdg . dpmdz = 10.; //  total z half-width of the box.            
                   /// pmdg . dpmdz = 10.; //  total z half-width of the box.            
                   pmdg . pargcz = 0.40; //  half thickness of gas (CPV sensitive)            
                   /// pmdg . pargcz = 0.40; //  half thickness of gas (CPV sensitive)            
                   pmdg . parscz = 0.40; //  (8 mm thick gas)            
                   /// pmdg . parscz = 0.40; //  (8 mm thick gas)            
                   pmdg . parfez = 0.25; //  (iron frame)            
                   /// pmdg . parfez = 0.25; //  (iron frame)            
                   pmdg . parpbz = 0.75; //  (3 X0 of lead converter)            
                   /// pmdg . parpbz = 0.75; //  (3 X0 of lead converter)            
                   pmdg . nx.at(0) = 48; // x-dimensions of modules            
                   ///pmdg . nx.at(0) = 48; // x-dimensions of modules            
                   pmdg . nx.at(1) = 72; // x-dimensions of modules            
                   ///pmdg . nx.at(1) = 72; // x-dimensions of modules            
                   pmdg . nx.at(2) = 72; // x-dimensions of modules            
                   ///pmdg . nx.at(2) = 72; // x-dimensions of modules            
                   pmdg . nx.at(3) = 48; // x-dimensions of modules            
                   ///pmdg . nx.at(3) = 48; // x-dimensions of modules            
                   pmdg . nx.at(4) = 48 ; // x-dimensions of modules            
                   ///pmdg . nx.at(4) = 48 ; // x-dimensions of modules            
                   pmdg . ny.at(0) = 24; // y-dimensions of modules            
                   ///pmdg . ny.at(0) = 24; // y-dimensions of modules            
                   pmdg . ny.at(1) = 48; // y-dimensions of modules            
                   ///pmdg . ny.at(1) = 48; // y-dimensions of modules            
                   pmdg . ny.at(2) = 48; // y-dimensions of modules            
                   ///pmdg . ny.at(2) = 48; // y-dimensions of modules            
                   pmdg . ny.at(3) = 72; // y-dimensions of modules            
                   ///pmdg . ny.at(3) = 72; // y-dimensions of modules            
                   pmdg . ny.at(4) = 48 ; // y-dimensions of modules            
                   ///pmdg . ny.at(4) = 48 ; // y-dimensions of modules            
                   pmdg . mx.at(0) = 24; // x-dimensions of modules            
                   ///pmdg . mx.at(0) = 24; // x-dimensions of modules            
                   pmdg . mx.at(1) = 48; // x-dimensions of modules            
                   ///pmdg . mx.at(1) = 48; // x-dimensions of modules            
                   pmdg . mx.at(2) = 72; // x-dimensions of modules            
                   ///pmdg . mx.at(2) = 72; // x-dimensions of modules            
                   pmdg . mx.at(3) = 72; // x-dimensions of modules            
                   ///pmdg . mx.at(3) = 72; // x-dimensions of modules            
                   pmdg . mx.at(4) = 24; // x-dimensions of modules            
                   ///pmdg . mx.at(4) = 24; // x-dimensions of modules            
                   pmdg . mx.at(5) = 48; // x-dimensions of modules            
                   ///pmdg . mx.at(5) = 48; // x-dimensions of modules            
                   pmdg . mx.at(6) = 48 ; // x-dimensions of modules            
                   ///pmdg . mx.at(6) = 48 ; // x-dimensions of modules            
                   pmdg . my.at(0) = 24; // y-dimensions of modules            
                   ///pmdg . my.at(0) = 24; // y-dimensions of modules            
                   pmdg . my.at(1) = 24; // y-dimensions of modules            
                   ///pmdg . my.at(1) = 24; // y-dimensions of modules            
                   pmdg . my.at(2) = 24; // y-dimensions of modules            
                   ///pmdg . my.at(2) = 24; // y-dimensions of modules            
                   pmdg . my.at(3) = 48; // y-dimensions of modules            
                   ///pmdg . my.at(3) = 48; // y-dimensions of modules            
                   pmdg . my.at(4) = 48; // y-dimensions of modules            
                   ///pmdg . my.at(4) = 48; // y-dimensions of modules            
                   pmdg . my.at(5) = 72; // y-dimensions of modules            
                   ///pmdg . my.at(5) = 72; // y-dimensions of modules            
                   pmdg . my.at(6) = 48 ; // y-dimensions of modules            
                   ///pmdg . my.at(6) = 48 ; // y-dimensions of modules            
                   pmdg . hexd2.at(0) = 0.; // inner hex            
                   ///pmdg . hexd2.at(0) = 0.; // inner hex            
                   pmdg . hexd2.at(1) = 360.; // inner hex            
                   ///pmdg . hexd2.at(1) = 360.; // inner hex            
                   pmdg . hexd2.at(2) = 6; // inner hex            
                   ///pmdg . hexd2.at(2) = 6; // inner hex            
                   pmdg . hexd2.at(3) = 2; // inner hex            
                   ///pmdg . hexd2.at(3) = 2; // inner hex            
                   pmdg . hexd2.at(4) = -0.4; // inner hex            
                   ///pmdg . hexd2.at(4) = -0.4; // inner hex            
                   pmdg . hexd2.at(5) = 0.; // inner hex            
                   ///pmdg . hexd2.at(5) = 0.; // inner hex            
                   pmdg . hexd2.at(6) = 0.51; // inner hex            
                   ///pmdg . hexd2.at(6) = 0.51; // inner hex            
                   pmdg . hexd2.at(7) = 0.4; // inner hex            
                   ///pmdg . hexd2.at(7) = 0.4; // inner hex            
                   pmdg . hexd2.at(8) = 0.; // inner hex            
                   ///pmdg . hexd2.at(8) = 0.; // inner hex            
                   pmdg . hexd2.at(9) = 0.51; // inner hex            
                   ///pmdg . hexd2.at(9) = 0.51; // inner hex            
                   pmdg . hexd1.at(0) = 0.; // outer hex            
                   ///pmdg . hexd1.at(0) = 0.; // outer hex            
                   pmdg . hexd1.at(1) = 360.; // outer hex            
                   ///pmdg . hexd1.at(1) = 360.; // outer hex            
                   pmdg . hexd1.at(2) = 6; // outer hex            
                   ///pmdg . hexd1.at(2) = 6; // outer hex            
                   pmdg . hexd1.at(3) = 2; // outer hex            
                   ///pmdg . hexd1.at(3) = 2; // outer hex            
                   pmdg . hexd1.at(4) = -0.4; // outer hex            
                   ///pmdg . hexd1.at(4) = -0.4; // outer hex            
                   pmdg . hexd1.at(5) = 0.; // outer hex            
                   ///pmdg . hexd1.at(5) = 0.; // outer hex            
                   pmdg . hexd1.at(6) = 0.53; // outer hex            
                   ///pmdg . hexd1.at(6) = 0.53; // outer hex            
                   pmdg . hexd1.at(7) = 0.4; // outer hex            
                   ///pmdg . hexd1.at(7) = 0.4; // outer hex            
                   pmdg . hexd1.at(8) = 0.; // outer hex            
                   ///pmdg . hexd1.at(8) = 0.; // outer hex            
                   pmdg . hexd1.at(9) = 0.53; // outer hex            
                   ///pmdg . hexd1.at(9) = 0.53; // outer hex            
                   pmdg . dpara.at(0) = 38.29; // supermodule            
                   ///pmdg . dpara.at(0) = 38.29; // supermodule            
                   pmdg . dpara.at(1) = 33.16; // supermodule            
                   ///pmdg . dpara.at(1) = 33.16; // supermodule            
                   pmdg . dpara.at(2) = 0.4; // supermodule            
                   ///pmdg . dpara.at(2) = 0.4; // supermodule            
                   pmdg . dpara.at(3) = 30.; // supermodule            
                   ///pmdg . dpara.at(3) = 30.; // supermodule            
                   pmdg . dpara.at(4) = 0.; // supermodule            
                   ///pmdg . dpara.at(4) = 0.; // supermodule            
                   pmdg . dpara.at(5) = 0.; // supermodule            
                   ///pmdg . dpara.at(5) = 0.; // supermodule            
                   pmdg . cell_radius = 0.5282; //  Radius of a cell            
                   /// pmdg . cell_radius = 0.5282; //  Radius of a cell            
                   pmdg . cell_depth = 0.8; //  Gas depth            
                   /// pmdg . cell_depth = 0.8; //  Gas depth            
                   pmdg . cell_wall = 0.2; //  Cell wall thickness            
                   /// pmdg . cell_wall = 0.2; //  Cell wall thickness            
                   pmdg . boundary = 0.8; //  Boundary            
                   /// pmdg . boundary = 0.8; //  Boundary            
                   pmdg . th_base = 0.3; //  Thickness of the base plate            
                   /// pmdg . th_base = 0.3; //  Thickness of the base plate            
                   pmdg . th_air = 0.1; //  Air gap            
                   /// pmdg . th_air = 0.1; //  Air gap            
                   pmdg . th_pcb = 0.16; //  Thickness of the PCB            
                   /// pmdg . th_pcb = 0.16; //  Thickness of the PCB            
                   pmdg . th_lead = 1.5; //  Thickness of the Lead            
                   /// pmdg . th_lead = 1.5; //  Thickness of the Lead            
                   pmdg . th_steel = 0.5; //  Thickness of the steel support            
                   /// pmdg . th_steel = 0.5; //  Thickness of the steel support            
                   //           
                   pmdg.fill();           
             ///@}        
             //        
             /// USE pmvr _index=1;        
             pmvr.Use();        
             /// USE pmdg _index=1;        
             pmdg.Use();        
             root32=sqrt(3.0)/2.0;        
             root34=root32/2.0;        
             xlen3=(sizen(72)+sizen(48))/4.0;        
             ylen3=-(sizen(72)+sizen(48))*root34;        
             zlen2=(sizen(72)+sizen(48)+4.5*pmdg.th_air)/4.;        
             zlen1=-(sizen(72)+sizen(48)+(2.0*pmdg.boundary+3.75*pmdg.th_air)/root32)*root34;        
             sm_thick_a = (pmdg.th_base + 4.0*pmdg.th_air + 3.0*pmdg.th_pcb  + pmdg.cell_depth);        
             sm_thick   = 2.0*sm_thick_a  + pmdg.th_lead + pmdg.th_steel;        
             _create = AgCreate("PHMD");        
             {           
                   AgShape myshape; // undefined shape           
                   ///Create PHMD           
                   Create("PHMD");            
             }        
             { AgPlacement place = AgPlacement("PHMD","CAVE");           
                   /// Add daughter volume PHMD to mother CAVE           
                   place.TranslateZ(pmdg.zdist(pmdg.version));           
                   /// Translate z = pmdg.zdist(pmdg.version)           
                   _stacker -> Position( AgBlock::Find("PHMD"), place );           
             } // end placement of PHMD        
       }; // PhmdGeo     
 }; // namespace PhmdGeo  
 
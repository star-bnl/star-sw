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
                Float_t inr,outr,lengthz,centerz,z1,z2,k,phi1,phi2,sina,cosa,outrflat,boxr,resr,angr,m;        
                //        
                /// Float_t inr,outr,lengthz,centerz,z1,z2,k,phi1,phi2,sina,cosa,outrflat,boxr,resr,angr,m        
          ///@}     
          //  -----------------------------------------------------     
          /// @defgroup idsg_doc     
          /// \class Idsg_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t version;     
          ///Float_t lenm;     
          ///Float_t rm;     
          ///Float_t lenw;     
          ///Float_t rw;     
          ///Float_t thick;     
          ///Float_t zstart;     
          ///Float_t angdel;     
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
                            attr.par("colo")=6;              
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
                      _create = AgCreate("OSCA");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create OSCA              
                            Create("OSCA");               
                      }           
                      { AgPlacement place = AgPlacement("OSCA","IDSM");              
                            /// Add daughter volume OSCA to mother IDSM              
                            place.TranslateZ(0);              
                            /// Translate z = 0              
                            _stacker -> Position( AgBlock::Find("OSCA"), place );              
                      } // end placement of OSCA           
                      /// Loop on k from 0 to 1 step=1           
                      for ( k=0; (1>0)? (k<=1):(k>=1); k+=1 )           
                      {              
                            z1=lengthz/2-idsg.lenw/2.;              
                            z2=idsg.lenm/2.-idsg.thick;              
                            if ( k==1 )              
                            {                 
                                  z1=-lengthz/2+idsg.lenw/2.;                 
                                  z2=-idsg.lenm/2.+idsg.thick/2.;                 
                            }              
                            _create = AgCreate("WSCC");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create WSCC                 
                                  Create("WSCC");                  
                            }              
                            { AgPlacement place = AgPlacement("WSCC","IDSM");                 
                                  /// Add daughter volume WSCC to mother IDSM                 
                                  place.TranslateZ(z1);                 
                                  /// Translate z = z1                 
                                  _stacker -> Position( AgBlock::Find("WSCC"), place );                 
                            } // end placement of WSCC              
                            _create = AgCreate("WSCD");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create WSCD                 
                                  Create("WSCD");                  
                            }              
                            { AgPlacement place = AgPlacement("WSCD","IDSM");                 
                                  /// Add daughter volume WSCD to mother IDSM                 
                                  place.TranslateZ(z2);                 
                                  /// Translate z = z2                 
                                  _stacker -> Position( AgBlock::Find("WSCD"), place );                 
                            } // end placement of WSCD              
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
          void OSCA::Block( AgCreate create )     
          {         
                ///@addtogroup OSCA_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("OSCA");              
                            attr.par("seen")=1;              
                            attr.par("colo")=5;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material CFiber            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Cfiber");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=inr;              
                            shape.par("rmax")=inr+idsg.thick;              
                            shape.par("dz")=idsg.lenm/2.;              
                            /// Shape Tube rmin=inr rmax=inr+idsg.thick dz=idsg.lenm/2.               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_OSCA;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_OSCA:           
                      mCurrent = _save;           
                ///@}        
          } // End Block OSCA     
          // ---------------------------------------------------------------------------------------------------     
          void WSCC::Block( AgCreate create )     
          {         
                ///@addtogroup WSCC_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("WSCC");              
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
                            shape.par("rmin")=idsg.rw-idsg.thick/2.;              
                            shape.par("rmax")=idsg.rw+idsg.thick/2.;              
                            shape.par("dz")=idsg.lenw/2.;              
                            /// Shape Tube rmin=idsg.rw-idsg.thick/2. rmax=idsg.rw+idsg.thick/2. dz=idsg.lenw/2.               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_WSCC;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("WSCO");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create WSCO              
                            Create("WSCO");               
                      }           
                      { AgPlacement place = AgPlacement("WSCO","WSCC");              
                            /// Add daughter volume WSCO to mother WSCC              
                            place.TranslateZ(0);              
                            /// Translate z = 0              
                            _stacker -> Position( AgBlock::Find("WSCO"), place );              
                      } // end placement of WSCO           
                      END_OF_WSCC:           
                      mCurrent = _save;           
                ///@}        
          } // End Block WSCC     
          // ---------------------------------------------------------------------------------------------------     
          void WSCO::Block( AgCreate create )     
          {         
                ///@addtogroup WSCO_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("WSCO");              
                            attr.par("seen")=1;              
                            attr.par("colo")=4;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material CFiber            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Cfiber");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=idsg.rw-idsg.thick/2.;              
                            shape.par("rmax")=idsg.rw+idsg.thick/2.;              
                            shape.par("dz")=idsg.lenw/2.;              
                            /// Shape Tube rmin=idsg.rw-idsg.thick/2. rmax=idsg.rw+idsg.thick/2. dz=idsg.lenw/2.               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_WSCO;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_WSCO:           
                      mCurrent = _save;           
                ///@}        
          } // End Block WSCO     
          // ---------------------------------------------------------------------------------------------------     
          void WSCD::Block( AgCreate create )     
          {         
                ///@addtogroup WSCD_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("WSCD");              
                            attr.par("seen")=0;              
                            attr.par("colo")=2;              
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
                            shape.par("rmin")=idsg.rm-idsg.thick/2.;              
                            shape.par("rmax")=idsg.rw+idsg.thick/2.;              
                            shape.par("dz")=idsg.thick/2.;              
                            /// Shape Tube rmin=idsg.rm-idsg.thick/2. rmax=idsg.rw+idsg.thick/2. dz=idsg.thick/2.               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_WSCD;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("WSCP");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create WSCP              
                            Create("WSCP");               
                      }           
                      { AgPlacement place = AgPlacement("WSCP","WSCD");              
                            /// Add daughter volume WSCP to mother WSCD              
                            place.TranslateZ(0);              
                            /// Translate z = 0              
                            _stacker -> Position( AgBlock::Find("WSCP"), place );              
                      } // end placement of WSCP           
                      END_OF_WSCD:           
                      mCurrent = _save;           
                ///@}        
          } // End Block WSCD     
          // ---------------------------------------------------------------------------------------------------     
          void WSCP::Block( AgCreate create )     
          {         
                ///@addtogroup WSCP_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("WSCP");              
                            attr.par("seen")=1;              
                            attr.par("colo")=6;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material CFiber            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Cfiber");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=idsg.rm-idsg.thick/2.;              
                            shape.par("rmax")=idsg.rw+idsg.thick/2.;              
                            shape.par("dz")=idsg.thick/2.;              
                            /// Shape Tube rmin=idsg.rm-idsg.thick/2. rmax=idsg.rw+idsg.thick/2. dz=idsg.thick/2.               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_WSCP;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_WSCP:           
                      mCurrent = _save;           
                ///@}        
          } // End Block WSCP     
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
    // ----------------------------------------------------------------------- geoctr
       void IdsmGeo1::ConstructGeometry()     
       {        
             ///@addtogroup IdsmGeo1_revision        
             ///@{           
                   /// Created:   4/10/2011            
             ///@}        
             ///@addtogroup IdsmGeo1_revision        
             ///@{           
                   /// Author: Jan Balewski MIT            
             ///@}        
             AddBlock("IDSM");        
             AddBlock("OSCA");        
             AddBlock("WSCC");        
             AddBlock("WSCD");        
             AddBlock("WSCO");        
             AddBlock("WSCP");        
             AddBlock("TPRR");        
             AddBlock("TPRT");        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup idsg_doc        
             ///@{           
                   ++idsg._index;           
                   idsg . version = 1.0; // Versioning of the IDS geometry           
                   /// idsg . version = 1.0; // Versioning of the IDS geometry           
                   idsg . zstart = 0.; //  Z position of middle cone           
                   /// idsg . zstart = 0.; //  Z position of middle cone           
                   idsg . thick = 0.3; //  thickens of any element           
                   /// idsg . thick = 0.3; //  thickens of any element           
                   idsg . lenm = 100.; // Z length of  middle cone           
                   /// idsg . lenm = 100.; // Z length of  middle cone           
                   idsg . rm = 6.; //  radii of middle cone           
                   /// idsg . rm = 6.; //  radii of middle cone           
                   idsg . lenw = 120.; // Z lenght of west cone           
                   /// idsg . lenw = 120.; // Z lenght of west cone           
                   idsg . rw = 41.; //  radii of west cone           
                   /// idsg . rw = 41.; //  radii of west cone           
                   idsg . angflat = 106.; //  angle (deg) for center of flat           
                   /// idsg . angflat = 106.; //  angle (deg) for center of flat           
                   idsg . angdel = 36.; //  opening angle (deg) of flat           
                   /// idsg . angdel = 36.; //  opening angle (deg) of flat           
                   idsg . rrres = 44.; //  radial distance of  for TPC resistor tubes           
                   /// idsg . rrres = 44.; //  radial distance of  for TPC resistor tubes           
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
             /// Component C	a=12	z=6	w=1        
             /// Mixture CFiber dens=1.713        
             {  AgMaterial &mix = AgMaterial::Get("Cfiber");           
                   mix.Component("C",12,6,1);           
                   mix.par("dens")=1.713;           
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
             inr     = idsg.rm - idsg.thick/2.;        
             outr    = idsg.rrres + idsg.r2res;        
             lengthz = idsg.lenm + 2.*idsg.lenw;        
             centerz = 0.;        
             phi1=idsg.angflat-idsg.angdel/2.;        
             phi2=idsg.angflat+idsg.angdel/2.;        
             sina = sin( idsg.angflat * degrad );        
             cosa = cos( idsg.angflat * degrad );        
             outrflat = idsg.rw * cos(idsg.angdel * degrad );        
             boxr     = (outrflat + inr + idsg.thick)/2.;        
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
 
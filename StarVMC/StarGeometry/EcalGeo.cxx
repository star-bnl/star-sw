#include "EcalGeo.h"  
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
 namespace ECALGEO // $NMSPC  
 {     
       //     
       // ---------------------------------------------------------------------------------------------------     
          //  -----------------------------------------------------     
          /// @defgroup emcg_doc     
          /// \class Emcg_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t version;     
          ///Int_t onoff;     
          ///Int_t fillmode;     
          ///Int_t _index;     
          //     
          Emcg_t emcg;     
          //     
          //  -----------------------------------------------------     
          /// @defgroup emcs_doc     
          /// \class Emcs_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t type;     
          ///Float_t zorig;     
          ///Float_t zend;     
          ///Float_t etamin;     
          ///Float_t etamax;     
          ///Float_t phimin;     
          ///Float_t phimax;     
          ///Float_t offset;     
          ///Float_t nsupsec;     
          ///Float_t nsector;     
          ///Float_t nsection;     
          ///Float_t nslices;     
          ///Float_t front;     
          ///Float_t alincell;     
          ///Float_t frplast;     
          ///Float_t bkplast;     
          ///Float_t pbplate;     
          ///Float_t lamplate;     
          ///Float_t bckplate;     
          ///Float_t hub;     
          ///Float_t rmshift;     
          ///Float_t smshift;     
          ///Float_t gapplt;     
          ///Float_t gapcel;     
          ///Float_t gapsmd;     
          ///Float_t smdcentr;     
          ///Array_t<Float_t> tierod;     
          ///Float_t bckfrnt;     
          ///Float_t gaphalf;     
          ///Float_t cover;     
          ///Int_t _index;     
          //     
          Emcs_t emcs;     
          //     
          //  -----------------------------------------------------     
          /// @defgroup eetr_doc     
          /// \class Eetr_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t type;     
          ///Float_t etagr;     
          ///Float_t phigr;     
          ///Float_t neta;     
          ///Array_t<Float_t> etabin;     
          ///Int_t _index;     
          //     
          Eetr_t eetr;     
          //     
          //  -----------------------------------------------------     
          /// @defgroup esec_doc     
          /// \class Esec_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t isect;     
          ///Float_t fplmat;     
          ///Float_t cell;     
          ///Float_t scint;     
          ///Float_t nlayer;     
          ///Int_t _index;     
          //     
          Esec_t esec;     
          //     
          //  -----------------------------------------------------     
          /// @defgroup emxg_doc     
          /// \class Emxg_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t version;     
          ///Float_t sapex;     
          ///Float_t sbase;     
          ///Float_t rin;     
          ///Float_t rout;     
          ///Float_t f4;     
          ///Int_t _index;     
          //     
          Emxg_t emxg;     
          //     
          //  -----------------------------------------------------     
          /// @defgroup exse_doc     
          /// \class Exse_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t jsect;     
          ///Float_t zshift;     
          ///Array_t<Float_t> sectype;     
          ///Int_t _index;     
          //     
          Exse_t exse;     
          //     
          ///@addtogroup EcalGeo_vars     
          ///@{        
                Int_t i_section,j_section,ie,is,isec,i_str,nstr,type,ii,jj,cut,fsect,lsect,ihalf,filled,mykase;        
                //        
                /// Int_t i_section,j_section,ie,is,isec,i_str,nstr,type,ii,jj,cut,fsect,lsect,ihalf,filled,mykase        
          ///@}     
          ///@addtogroup EcalGeo_vars     
          ///@{        
                Float_t center,plate,cell,g10,diff,halfi,tan_low,tan_upp,rbot,rtop,deta,etax,sq2,sq3,dup,dd,d2,d3,rshift,dphi,radiator,orgkeep,endkeep;        
                //        
                /// Float_t center,plate,cell,g10,diff,halfi,tan_low,tan_upp,rbot,rtop,deta,etax,sq2,sq3,dup,dd,d2,d3,rshift,dphi,radiator,orgkeep,endkeep        
          ///@}     
          ///@addtogroup EcalGeo_vars     
          ///@{        
                Float_t maxcnt,msecwd,mxgten,curr,secwid,section,curcl,etatop,etabot,slcwid,zslice,gap,mgt,xleft,xright,yleft,yright,current,rth,len,p,xc,yc,xx,yy,rbotrad,rdel,dxy,ddn,ddup;        
                //        
                /// Float_t maxcnt,msecwd,mxgten,curr,secwid,section,curcl,etatop,etabot,slcwid,zslice,gap,mgt,xleft,xright,yleft,yright,current,rth,len,p,xc,yc,xx,yy,rbotrad,rdel,dxy,ddn,ddup        
          ///@}     
          ///@addtogroup EcalGeo_vars     
          ///@{        
                Int_t n;        
                //        
                /// Int_t n        
          ///@}     
       EcalGeo::EcalGeo()     
         : AgModule("EcalGeo"," is the EM EndCap Calorimeter GEOmetry ")     
       {        
       }     
          Float_t tanf(Float_t etax) { return tan(2*atan(exp(-etax))); }     
          // ---------------------------------------------------------------------------------------------------     
          void ECAL::Block( AgCreate create )     
          {         
                ///@addtogroup ECAL_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      /// Medium standard           
                      {  AgMedium med = AgMedium::CopyMedium("Standard");              
                            _medium = med;              
                      }           
                      { AgAttribute attr = AgAttribute("ECAL");              
                            attr.par("seen")=1;              
                            attr.par("colo")=7;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Cone");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dz")=(emcs.zend-emcs.zorig)/2;              
                            shape.par("rmn1")=orgkeep*tan_low-d2;              
                            shape.par("rmx1")=orgkeep*tan_upp+dup;              
                            shape.par("rmn2")=endkeep*tan_low-d2;              
                            shape.par("rmx2")=endkeep*tan_upp+dup;              
                            /// Shape Cone dz=(emcs.zend-emcs.zorig)/2 rmn1=orgkeep*tan_low-d2 rmx1=orgkeep*tan_upp+dup rmn2=endkeep*tan_low-d2 rmx2=endkeep*tan_upp+dup               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_ECAL;              
                            _stacker -> Build(this);              
                      }           
                      /// Loop on ihalf from 1 to 2 step=1           
                      for ( ihalf=1; (1>0)? (ihalf<=2):(ihalf>=2); ihalf+=1 )           
                      {              
                            filled=1;              
                            halfi = -105 + (ihalf-1)*180;              
                            if ( (ihalf==2&&emcg.fillmode<3) )              
                            {                 
                                  filled = 0;                 
                            }              
                            _create = AgCreate("EAGA");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create EAGA                 
                                  Create("EAGA");                  
                            }              
                            { AgPlacement place = AgPlacement("EAGA","ECAL");                 
                                  /// Add daughter volume EAGA to mother ECAL                 
                                  place.AlphaZ(halfi);                 
                                  /// Rotate: AlphaZ = halfi                 
                                  /// G3 Reference: thetax = 90                 
                                  /// G3 Reference: phix = 0                 
                                  /// G3 Reference: thetay = 90                 
                                  /// G3 Reference: phiy = 90                 
                                  /// G3 Reference: thetaz = 0                 
                                  /// G3 Reference: phiz = 0                 
                                  _stacker -> Position( AgBlock::Find("EAGA"), place );                 
                            } // end placement of EAGA              
                      }           
                      END_OF_ECAL:           
                      mCurrent = _save;           
                ///@}        
          } // End Block ECAL     
          // ---------------------------------------------------------------------------------------------------     
          void EAGA::Block( AgCreate create )     
          {         
                ///@addtogroup EAGA_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("EAGA");              
                            attr.par("seen")=1;              
                            attr.par("colo")=1;              
                            attr.par("serial")=filled;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Cons");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dz")=(emcs.zend-emcs.zorig)/2;              
                            shape.par("rmn1")=orgkeep*tan_low-d2;              
                            shape.par("rmx1")=orgkeep*tan_upp+dup;              
                            shape.par("rmn2")=endkeep*tan_low-d2;              
                            shape.par("rmx2")=endkeep*tan_upp+dup;              
                            shape.par("phi1")=emcs.phimin;              
                            shape.par("phi2")=emcs.phimax;              
                            /// Shape Cons dz=(emcs.zend-emcs.zorig)/2 rmn1=orgkeep*tan_low-d2 rmx1=orgkeep*tan_upp+dup rmn2=endkeep*tan_low-d2 rmx2=endkeep*tan_upp+dup phi1=emcs.phimin phi2=emcs.phimax               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_EAGA;              
                            _stacker -> Build(this);              
                      }           
                      if ( filled==1 )           
                      {              
                            _create = AgCreate("EMSS");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create EMSS                 
                                  Create("EMSS");                  
                            }              
                            { AgPlacement place = AgPlacement("EMSS","EAGA");                 
                                  /// Add daughter volume EMSS to mother EAGA                 
                                  place.par("only")=AgPlacement::kMany;                 
                                  /// Overlap: agplacement::kmany                 
                                  _stacker -> Position( AgBlock::Find("EMSS"), place );                 
                            } // end placement of EMSS              
                            curr = orgkeep ; curcl = endkeep;              
                            _create = AgCreate("ECGH");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create ECGH                 
                                  Create("ECGH");                  
                            }              
                            { AgPlacement place = AgPlacement("ECGH","EAGA");                 
                                  /// Add daughter volume ECGH to mother EAGA                 
                                  place.par("only")=AgPlacement::kOnly;                 
                                  /// Overlap: agplacement::konly                 
                                  place.AlphaZ(90);                 
                                  /// Rotate: AlphaZ = 90                 
                                  /// G3 Reference: thetax = 90                 
                                  /// G3 Reference: phix = 0                 
                                  /// G3 Reference: thetay = 90                 
                                  /// G3 Reference: phiy = 90                 
                                  /// G3 Reference: thetaz = 0                 
                                  /// G3 Reference: phiz = 0                 
                                  _stacker -> Position( AgBlock::Find("ECGH"), place );                 
                            } // end placement of ECGH              
                      }           
                      END_OF_EAGA:           
                      mCurrent = _save;           
                ///@}        
          } // End Block EAGA     
          // ---------------------------------------------------------------------------------------------------     
          void EMSS::Block( AgCreate create )     
          {         
                ///@addtogroup EMSS_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("EMSS");              
                            attr.par("seen")=1;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material Iron            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Iron");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Cons");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dz")=(emcs.zend-emcs.zorig)/2;              
                            shape.par("rmn1")=orgkeep*tan_low-d2;              
                            shape.par("rmx1")=orgkeep*tan_upp+dup;              
                            shape.par("rmn2")=endkeep*tan_low-d2;              
                            shape.par("rmx2")=endkeep*tan_upp+dup;              
                            shape.par("phi1")=emcs.phimin;              
                            shape.par("phi2")=emcs.phimax;              
                            /// Shape Cons dz=(emcs.zend-emcs.zorig)/2 rmn1=orgkeep*tan_low-d2 rmx1=orgkeep*tan_upp+dup rmn2=endkeep*tan_low-d2 rmx2=endkeep*tan_upp+dup phi1=emcs.phimin phi2=emcs.phimax               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_EMSS;              
                            _stacker -> Build(this);              
                      }           
                      zslice = emcs.zorig;           
                      // Print<level=%i> fmt=%s fortran format statements not supported           
                      slcwid  = emcs.front;           
                      _create = AgCreate("EFLP");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create EFLP              
                            Create("EFLP");               
                      }           
                      { AgPlacement place = AgPlacement("EFLP","EMSS");              
                            /// Add daughter volume EFLP to mother EMSS              
                            place.TranslateZ(zslice-center+slcwid/2);              
                            /// Translate z = zslice-center+slcwid/2              
                            _stacker -> Position( AgBlock::Find("EFLP"), place );              
                      } // end placement of EFLP           
                      zslice = zslice + slcwid;           
                      // Print<level=%i> fmt=%s fortran format statements not supported           
                      fsect = 1; lsect = 3;           
                      slcwid = emcs.smdcentr - emcs.gapsmd/2 - zslice;           
                      _create = AgCreate("ECVO");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create ECVO              
                            Create("ECVO");               
                      }           
                      { AgPlacement place = AgPlacement("ECVO","EMSS");              
                            /// Add daughter volume ECVO to mother EMSS              
                            place.TranslateZ(zslice-center+slcwid/2);              
                            /// Translate z = zslice-center+slcwid/2              
                            _stacker -> Position( AgBlock::Find("ECVO"), place );              
                      } // end placement of ECVO           
                      slcwid  = emcs.gapsmd;           
                      zslice = emcs.smdcentr - emcs.gapsmd/2;           
                      // Print<level=%i> fmt=%s fortran format statements not supported           
                      _create = AgCreate("ESHM");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create ESHM              
                            Create("ESHM");               
                      }           
                      { AgPlacement place = AgPlacement("ESHM","EMSS");              
                            /// Add daughter volume ESHM to mother EMSS              
                            place.TranslateZ(zslice-center+slcwid/2);              
                            /// Translate z = zslice-center+slcwid/2              
                            _stacker -> Position( AgBlock::Find("ESHM"), place );              
                      } // end placement of ESHM           
                      zslice = zslice + slcwid;           
                      // Print<level=%i> fmt=%s fortran format statements not supported           
                      slcwid = 0;           
                      fsect = 4; lsect = 5;           
                      /// Loop on i_section from fsect to lsect step=1           
                      for ( i_section=fsect; (1>0)? (i_section<=lsect):(i_section>=lsect); i_section+=1 )           
                      {              
                            /// USE esec isect=i_section   ;              
                            esec.Use("isect",(Float_t)i_section   );              
                            slcwid  = slcwid + esec.cell*esec.nlayer;              
                      }           
                      slcwid = emcs.bckfrnt - zslice;           
                      _create = AgCreate("ECVO");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create ECVO              
                            Create("ECVO");               
                      }           
                      { AgPlacement place = AgPlacement("ECVO","EMSS");              
                            /// Add daughter volume ECVO to mother EMSS              
                            place.TranslateZ(zslice-center+slcwid/2);              
                            /// Translate z = zslice-center+slcwid/2              
                            _stacker -> Position( AgBlock::Find("ECVO"), place );              
                      } // end placement of ECVO           
                      zslice = emcs.bckfrnt;           
                      // Print<level=%i> fmt=%s fortran format statements not supported           
                      slcwid  = emcs.bckplate;           
                      _create = AgCreate("ESSP");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create ESSP              
                            Create("ESSP");               
                      }           
                      { AgPlacement place = AgPlacement("ESSP","EMSS");              
                            /// Add daughter volume ESSP to mother EMSS              
                            place.TranslateZ(zslice-center+slcwid/2);              
                            /// Translate z = zslice-center+slcwid/2              
                            _stacker -> Position( AgBlock::Find("ESSP"), place );              
                      } // end placement of ESSP           
                      zslice = zslice + slcwid;           
                      // Print<level=%i> fmt=%s fortran format statements not supported           
                      slcwid = emcs.zend-emcs.zorig;           
                      _create = AgCreate("ERCM");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create ERCM              
                            Create("ERCM");               
                      }           
                      /// Loop on i_str from 1 to 2 step=1           
                      for ( i_str=1; (1>0)? (i_str<=2):(i_str>=2); i_str+=1 )           
                      {              
                            /// Loop on is from 1 to 5 step=1              
                            for ( is=1; (1>0)? (is<=5):(is>=5); is+=1 )              
                            {                 
                                  xx = emcs.phimin + is*30;                 
                                  yy = xx*degrad;                 
                                  xc = cos(yy)*emcs.tierod(i_str);                 
                                  yc = sin(yy)*emcs.tierod(i_str);                 
                                  { AgPlacement place = AgPlacement("ERCM","EMSS");                    
                                        /// Add daughter volume ERCM to mother EMSS                    
                                        place.TranslateX(xc);                    
                                        /// Translate x = xc                    
                                        place.TranslateY(yc);                    
                                        /// Translate y = yc                    
                                        place.TranslateZ(0);                    
                                        /// Translate z = 0                    
                                        _stacker -> Position( AgBlock::Find("ERCM"), place );                    
                                  } // end placement of ERCM                 
                            }              
                      }           
                      rth = orgkeep*tan_upp+dup + 2.5/2;           
                      xc = (endkeep - orgkeep)*tan_upp;           
                      len = .5*(endkeep + orgkeep)*tan_upp + dup + 2.5/2;           
                      yc = emcs.zend-emcs.zorig;           
                      p = atan(xc/yc)/degrad;           
                      _create = AgCreate("EPSB");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create EPSB              
                            Create("EPSB");               
                      }           
                      /// Loop on is from 1 to 6 step=1           
                      for ( is=1; (1>0)? (is<=6):(is>=6); is+=1 )           
                      {              
                            xx = -75 + (is-1)*30;              
                            yy = xx*degrad;              
                            xc = cos(yy)*len;              
                            yc = sin(yy)*len;              
                            { AgPlacement place = AgPlacement("EPSB","EMSS");                 
                                  /// Add daughter volume EPSB to mother EMSS                 
                                  place.TranslateX(xc);                 
                                  /// Translate x = xc                 
                                  place.TranslateY(yc);                 
                                  /// Translate y = yc                 
                                  place.AlphaZ(xx);                 
                                  /// Rotate: AlphaZ = xx                 
                                  /// G3 Reference: thetax = 90                 
                                  /// G3 Reference: phix = 0                 
                                  /// G3 Reference: thetay = 90                 
                                  /// G3 Reference: phiy = 90                 
                                  /// G3 Reference: thetaz = 0                 
                                  /// G3 Reference: phiz = 0                 
                                  _stacker -> Position( AgBlock::Find("EPSB"), place );                 
                            } // end placement of EPSB              
                      }           
                      END_OF_EMSS:           
                      mCurrent = _save;           
                ///@}        
          } // End Block EMSS     
          // ---------------------------------------------------------------------------------------------------     
          void ECVO::Block( AgCreate create )     
          {         
                ///@addtogroup ECVO_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("ECVO");              
                            attr.par("seen")=1;              
                            attr.par("colo")=3;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Cons");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dz")=slcwid/2;              
                            shape.par("rmn1")=zslice*tan_low-dd;              
                            shape.par("rmx1")=zslice*tan_upp+dup;              
                            shape.par("rmn2")=(zslice+slcwid)*tan_low-dd;              
                            shape.par("rmx2")=(zslice+slcwid)*tan_upp+dup;              
                            /// Shape Cons dz=slcwid/2 rmn1=zslice*tan_low-dd rmx1=zslice*tan_upp+dup rmn2=(zslice+slcwid)*tan_low-dd rmx2=(zslice+slcwid)*tan_upp+dup               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_ECVO;              
                            _stacker -> Build(this);              
                      }           
                      /// Loop on j_section from 1 to 6 step=1           
                      for ( j_section=1; (1>0)? (j_section<=6):(j_section>=6); j_section+=1 )           
                      {              
                            if ( emcg.fillmode>1 )              
                            {                 
                                  filled=1;                 
                            }              
                            else if ( j_section>1&&j_section<6 )              
                            {                 
                                  filled=1;                 
                            }              
                            else              
                            {                 
                                  filled=0;                 
                            }              
                            d3 = 75 - (j_section-1)*30;              
                            _create = AgCreate("EMOD");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create EMOD                 
                                  Create("EMOD");                  
                            }              
                            { AgPlacement place = AgPlacement("EMOD","ECVO");                 
                                  /// Add daughter volume EMOD to mother ECVO                 
                                  place.par("ncopy")=j_section;                 
                                  /// Ncopy: j_section                 
                                  place.AlphaZ(d3);                 
                                  /// Rotate: AlphaZ = d3                 
                                  /// G3 Reference: thetax = 90                 
                                  /// G3 Reference: phix = 0                 
                                  /// G3 Reference: thetay = 90                 
                                  /// G3 Reference: phiy = 90                 
                                  /// G3 Reference: thetaz = 0                 
                                  /// G3 Reference: phiz = 0                 
                                  _stacker -> Position( AgBlock::Find("EMOD"), place );                 
                            } // end placement of EMOD              
                      }           
                      END_OF_ECVO:           
                      mCurrent = _save;           
                ///@}        
          } // End Block ECVO     
          // ---------------------------------------------------------------------------------------------------     
          void ESHM::Block( AgCreate create )     
          {         
                ///@addtogroup ESHM_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("ESHM");              
                            attr.par("seen")=1;              
                            attr.par("colo")=4;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Cons");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dz")=slcwid/2;              
                            shape.par("rmn1")=zslice*tan_low-dd;              
                            shape.par("rmx1")=(zslice)*tan_upp+dup;              
                            shape.par("rmn2")=(zslice+slcwid)*tan_low-dd;              
                            shape.par("rmx2")=(zslice+slcwid)*tan_upp+dup;              
                            shape.par("phi1")=emcs.phimin;              
                            shape.par("phi2")=emcs.phimax;              
                            /// Shape Cons dz=slcwid/2 rmn1=zslice*tan_low-dd rmx1=(zslice)*tan_upp+dup rmn2=(zslice+slcwid)*tan_low-dd rmx2=(zslice+slcwid)*tan_upp+dup phi1=emcs.phimin phi2=emcs.phimax               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_ESHM;              
                            _stacker -> Build(this);              
                      }           
                      /// USE emxg version=1 ;           
                      emxg.Use("version",(Float_t)1 );           
                      maxcnt = emcs.smdcentr;           
                      // Print<level=%i> fmt=%s fortran format statements not supported           
                      /// Loop on j_section from 1 to 3 step=1           
                      for ( j_section=1; (1>0)? (j_section<=3):(j_section>=3); j_section+=1 )           
                      {              
                            /// USE exse jsect=j_section ;              
                            exse.Use("jsect",(Float_t)j_section );              
                            current = exse.zshift;              
                            secwid  = emxg.sapex + 2.*emxg.f4;              
                            section = maxcnt + exse.zshift;              
                            // Print<level=%i> fmt=%s fortran format statements not supported              
                            rbot=section*tan_low;              
                            rtop=section*tan_upp;              
                            // Print<level=%i> fmt=%s fortran format statements not supported              
                            _create = AgCreate("ESPL");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create ESPL                 
                                  Create("ESPL");                  
                            }              
                            { AgPlacement place = AgPlacement("ESPL","ESHM");                 
                                  /// Add daughter volume ESPL to mother ESHM                 
                                  place.TranslateZ(current);                 
                                  /// Translate z = current                 
                                  _stacker -> Position( AgBlock::Find("ESPL"), place );                 
                            } // end placement of ESPL              
                      }           
                      _create = AgCreate("ERSM");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create ERSM              
                            Create("ERSM");               
                      }           
                      /// Loop on i_str from 1 to 2 step=1           
                      for ( i_str=1; (1>0)? (i_str<=2):(i_str>=2); i_str+=1 )           
                      {              
                            /// Loop on is from 1 to 5 step=1              
                            for ( is=1; (1>0)? (is<=5):(is>=5); is+=1 )              
                            {                 
                                  xx = emcs.phimin + (is)*30;                 
                                  yy = xx*degrad;                 
                                  xc = cos(yy)*emcs.tierod(i_str);                 
                                  yc = sin(yy)*emcs.tierod(i_str);                 
                                  { AgPlacement place = AgPlacement("ERSM","ESHM");                    
                                        /// Add daughter volume ERSM to mother ESHM                    
                                        place.TranslateX(xc);                    
                                        /// Translate x = xc                    
                                        place.TranslateY(yc);                    
                                        /// Translate y = yc                    
                                        place.TranslateZ(0);                    
                                        /// Translate z = 0                    
                                        _stacker -> Position( AgBlock::Find("ERSM"), place );                    
                                  } // end placement of ERSM                 
                            }              
                      }           
                      END_OF_ESHM:           
                      mCurrent = _save;           
                ///@}        
          } // End Block ESHM     
          // ---------------------------------------------------------------------------------------------------     
          void ECGH::Block( AgCreate create )     
          {         
                ///@addtogroup ECGH_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      /// Medium standard           
                      {  AgMedium med = AgMedium::CopyMedium("Standard");              
                            _medium = med;              
                      }           
                      { AgAttribute attr = AgAttribute("ECGH");              
                            attr.par("seen")=0;              
                            attr.par("colo")=7;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Trd1");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx1")=orgkeep*tan_upp+dup;              
                            shape.par("dx2")=endkeep*tan_upp+dup;              
                            shape.par("dy")=(emcs.gaphalf+emcs.cover)/2;              
                            shape.par("dz")=(emcs.zend-emcs.zorig)/2;              
                            /// Shape Trd1 dx1=orgkeep*tan_upp+dup dx2=endkeep*tan_upp+dup dy=(emcs.gaphalf+emcs.cover)/2 dz=(emcs.zend-emcs.zorig)/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_ECGH;              
                            _stacker -> Build(this);              
                      }           
                      rth = emcs.gaphalf + emcs.cover;           
                      xx=curr*tan_low-d2;           
                      xleft = sqrt(xx*xx - rth*rth);           
                      yy=curr*tan_upp+dup;           
                      xright = sqrt(yy*yy - rth*rth);           
                      secwid = yy - xx;           
                      xx=curcl*tan_low-d2;           
                      yleft = sqrt(xx*xx - rth*rth);           
                      yy=curcl*tan_upp+dup;           
                      yright = sqrt(yy*yy - rth*rth);           
                      slcwid = yy - xx;           
                      xx=(xleft+xright)/2;           
                      yy=(yleft + yright)/2;           
                      xc = yy - xx;           
                      len = (xx+yy)/2;           
                      yc = curcl - curr;           
                      p = atan(xc/yc)/degrad;           
                      rth = -(emcs.gaphalf + emcs.cover)/2;           
                      _create = AgCreate("ECHC");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create ECHC              
                            Create("ECHC");               
                      }           
                      { AgPlacement place = AgPlacement("ECHC","ECGH");              
                            /// Add daughter volume ECHC to mother ECGH              
                            place.TranslateX(len);              
                            /// Translate x = len              
                            place.TranslateY(rth);              
                            /// Translate y = rth              
                            _stacker -> Position( AgBlock::Find("ECHC"), place );              
                      } // end placement of ECHC           
                      { AgPlacement place = AgPlacement("ECHC","ECGH");              
                            /// Add daughter volume ECHC to mother ECGH              
                            place.TranslateX(-len);              
                            /// Translate x = -len              
                            place.TranslateY(rth);              
                            /// Translate y = rth              
                            place.AlphaZ(180);              
                            /// Rotate: AlphaZ = 180              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            _stacker -> Position( AgBlock::Find("ECHC"), place );              
                      } // end placement of ECHC           
                      END_OF_ECGH:           
                      mCurrent = _save;           
                ///@}        
          } // End Block ECGH     
          // ---------------------------------------------------------------------------------------------------     
          void ECHC::Block( AgCreate create )     
          {         
                ///@addtogroup ECHC_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("ECHC");              
                            attr.par("seen")=1;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material Iron            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Iron");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Trap");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dz")=(curcl-curr)/2;              
                            shape.par("thet")=p;              
                            shape.par("phi")=0;              
                            shape.par("h1")=emcs.cover/2;              
                            shape.par("bl1")=secwid/2;              
                            shape.par("tl1")=secwid/2;              
                            shape.par("alp1")=0;              
                            shape.par("h2")=emcs.cover/2;              
                            shape.par("bl2")=slcwid/2;              
                            shape.par("tl2")=slcwid/2;              
                            shape.par("alp2")=0;              
                            /// Shape Trap dz=(curcl-curr)/2 thet=p phi=0 h1=emcs.cover/2 bl1=secwid/2 tl1=secwid/2 alp1=0 h2=emcs.cover/2 bl2=slcwid/2 tl2=slcwid/2 alp2=0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_ECHC;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_ECHC:           
                      mCurrent = _save;           
                ///@}        
          } // End Block ECHC     
          // ---------------------------------------------------------------------------------------------------     
          void ESSP::Block( AgCreate create )     
          {         
                ///@addtogroup ESSP_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Iron            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Iron");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("ESSP");              
                            attr.par("seen")=1;              
                            attr.par("colo")=6;              
                            attr.par("fill")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Cons");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dz")=emcs.bckplate/2;              
                            shape.par("rmn1")=zslice*tan_low-dd;              
                            shape.par("rmx1")=zslice*tan_upp+dup;              
                            shape.par("rmn2")=(zslice+slcwid)*tan_low-dd;              
                            shape.par("rmx2")=(zslice+slcwid)*tan_upp+dup;              
                            shape.par("phi1")=emcs.phimin;              
                            shape.par("phi2")=emcs.phimax;              
                            /// Shape Cons dz=emcs.bckplate/2 rmn1=zslice*tan_low-dd rmx1=zslice*tan_upp+dup rmn2=(zslice+slcwid)*tan_low-dd rmx2=(zslice+slcwid)*tan_upp+dup phi1=emcs.phimin phi2=emcs.phimax               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_ESSP;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_ESSP:           
                      mCurrent = _save;           
                ///@}        
          } // End Block ESSP     
          // ---------------------------------------------------------------------------------------------------     
          void EPSB::Block( AgCreate create )     
          {         
                ///@addtogroup EPSB_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Iron            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Iron");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("EPSB");              
                            attr.par("seen")=1;              
                            attr.par("colo")=6;              
                            attr.par("fill")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Trap");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dz")=(emcs.zend-emcs.zorig)/2;              
                            shape.par("thet")=p;              
                            shape.par("phi")=0;              
                            shape.par("h1")=2.0/2;              
                            shape.par("bl1")=2.5/2;              
                            shape.par("tl1")=2.5/2;              
                            shape.par("alp1")=0;              
                            shape.par("h2")=2.0/2;              
                            shape.par("bl2")=2.5/2;              
                            shape.par("tl2")=2.5/2;              
                            shape.par("alp2")=0;              
                            /// Shape Trap dz=(emcs.zend-emcs.zorig)/2 thet=p phi=0 h1=2.0/2 bl1=2.5/2 tl1=2.5/2 alp1=0 h2=2.0/2 bl2=2.5/2 tl2=2.5/2 alp2=0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_EPSB;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_EPSB:           
                      mCurrent = _save;           
                ///@}        
          } // End Block EPSB     
          // ---------------------------------------------------------------------------------------------------     
          void ERCM::Block( AgCreate create )     
          {         
                ///@addtogroup ERCM_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Iron            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Iron");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("ERSM");              
                            attr.par("seen")=1;              
                            attr.par("colo")=6;              
                            attr.par("fill")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=0;              
                            shape.par("rmax")=1.0425;              
                            shape.par("dz")=slcwid/2;              
                            /// Shape Tube rmin=0 rmax=1.0425 dz=slcwid/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_ERCM;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_ERCM:           
                      mCurrent = _save;           
                ///@}        
          } // End Block ERCM     
          // ---------------------------------------------------------------------------------------------------     
          void ERSM::Block( AgCreate create )     
          {         
                ///@addtogroup ERSM_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Iron            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Iron");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("ERSM");              
                            attr.par("seen")=1;              
                            attr.par("colo")=6;              
                            attr.par("fill")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=0;              
                            shape.par("rmax")=1.0425;              
                            shape.par("dz")=slcwid/2;              
                            /// Shape Tube rmin=0 rmax=1.0425 dz=slcwid/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_ERSM;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_ERSM:           
                      mCurrent = _save;           
                ///@}        
          } // End Block ERSM     
          // ---------------------------------------------------------------------------------------------------     
          void EMOD::Block( AgCreate create )     
          {         
                ///@addtogroup EMOD_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("EMOD");              
                            attr.par("seen")=1;              
                            attr.par("colo")=3;              
                            attr.par("serial")=filled;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Cons");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dz")=slcwid/2;              
                            shape.par("rmn1")=zslice*tan_low-dd;              
                            shape.par("rmx1")=zslice*tan_upp+dup;              
                            shape.par("rmn2")=(zslice+slcwid)*tan_low-dd;              
                            shape.par("rmx2")=(zslice+slcwid)*tan_upp+dup;              
                            shape.par("phi1")=emcs.phimin/emcs.nsupsec;              
                            shape.par("phi2")=emcs.phimax/emcs.nsupsec;              
                            /// Shape Cons dz=slcwid/2 rmn1=zslice*tan_low-dd rmx1=zslice*tan_upp+dup rmn2=(zslice+slcwid)*tan_low-dd rmx2=(zslice+slcwid)*tan_upp+dup phi1=emcs.phimin/emcs.nsupsec phi2=emcs.phimax/emcs.nsupsec               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_EMOD;              
                            _stacker -> Build(this);              
                      }           
                      section = zslice;           
                      curr = zslice + slcwid/2;           
                      /// Loop on i_section from fsect to lsect step=1           
                      for ( i_section=fsect; (1>0)? (i_section<=lsect):(i_section>=lsect); i_section+=1 )           
                      {              
                            /// USE esec isect=i_section   ;              
                            esec.Use("isect",(Float_t)i_section   );              
                            secwid  = esec.cell*esec.nlayer;              
                            if ( i_section==3||i_section==5 )              
                            {                 
                                  secwid  = secwid - radiator;                 
                            }              
                            else if ( i_section==4 )              
                            {                 
                                  secwid  = secwid - esec.cell + radiator;                 
                            }              
                            _create = AgCreate("ESEC");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create ESEC                 
                                  Create("ESEC");                  
                            }              
                            { AgPlacement place = AgPlacement("ESEC","EMOD");                 
                                  /// Add daughter volume ESEC to mother EMOD                 
                                  place.TranslateZ(section-curr+secwid/2);                 
                                  /// Translate z = section-curr+secwid/2                 
                                  _stacker -> Position( AgBlock::Find("ESEC"), place );                 
                            } // end placement of ESEC              
                            section = section + secwid;              
                      }           
                      END_OF_EMOD:           
                      mCurrent = _save;           
                ///@}        
          } // End Block EMOD     
          // ---------------------------------------------------------------------------------------------------     
          void ESEC::Block( AgCreate create )     
          {         
                ///@addtogroup ESEC_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("ESEC");              
                            attr.par("seen")=1;              
                            attr.par("colo")=1;              
                            attr.par("serial")=filled;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      /// Medium standard           
                      {  AgMedium med = AgMedium::CopyMedium("Standard");              
                            _medium = med;              
                      }           
                      {  AgShape shape = AgShape("Cons");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dz")=secwid/2;              
                            shape.par("rmn1")=(section-diff)*tan_low-dd;              
                            shape.par("rmx1")=(section-diff)*tan_upp+dup;              
                            shape.par("rmn2")=(section+secwid-diff)*tan_low-dd;              
                            shape.par("rmx2")=(section+secwid-diff)*tan_upp+dup;              
                            /// Shape Cons dz=secwid/2 rmn1=(section-diff)*tan_low-dd rmx1=(section-diff)*tan_upp+dup rmn2=(section+secwid-diff)*tan_low-dd rmx2=(section+secwid-diff)*tan_upp+dup               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_ESEC;              
                            _stacker -> Build(this);              
                      }           
                      len = -secwid/2;           
                      current = section;           
                      mgt = esec.scint + emcs.alincell                               + emcs.frplast + emcs.bkplast;           
                      gap = esec.cell - radiator - mgt;           
                      // Print<level=%i> fmt=%s fortran format statements not supported           
                      /// Loop on is from 1 to esec.nlayer step=1           
                      for ( is=1; (1>0)? (is<=esec.nlayer):(is>=esec.nlayer); is+=1 )           
                      {              
                            cell = esec.cell;              
                            plate = radiator;              
                            if ( is==nint(esec.nlayer)&&(i_section==3||i_section==5) )              
                            {                 
                                  cell = mgt + gap;                 
                                  plate=0;                 
                            }              
                            else if ( i_section==4&&is==1 )              
                            {                 
                                  cell = radiator;                 
                            }              
                            // Print<level=%i> fmt=%s fortran format statements not supported              
                            if ( i_section==4&&is==1 )              
                            {                 
                                  cell = radiator + .14;                 
                                  _create = AgCreate("ERAD");                 
                                  {                    
                                        AgShape myshape; // undefined shape                    
                                        ///Create ERAD                    
                                        Create("ERAD");                     
                                  }                 
                                  { AgPlacement place = AgPlacement("ERAD","ESEC");                    
                                        /// Add daughter volume ERAD to mother ESEC                    
                                        place.TranslateZ(len+(cell)/2);                    
                                        /// Translate z = len+(cell)/2                    
                                        _stacker -> Position( AgBlock::Find("ERAD"), place );                    
                                  } // end placement of ERAD                 
                                  len = len + cell;                 
                                  current = current + cell;                 
                            }              
                            else              
                            {                 
                                  cell = mgt;                 
                                  if ( filled==1 )                 
                                  {                    
                                        _create = AgCreate("EMGT");                    
                                        {                       
                                              AgShape myshape; // undefined shape                       
                                              ///Create EMGT                       
                                              Create("EMGT");                        
                                        }                    
                                        { AgPlacement place = AgPlacement("EMGT","ESEC");                       
                                              /// Add daughter volume EMGT to mother ESEC                       
                                              place.TranslateZ(len+(gap+cell)/2);                       
                                              /// Translate z = len+(gap+cell)/2                       
                                              _stacker -> Position( AgBlock::Find("EMGT"), place );                       
                                        } // end placement of EMGT                    
                                        xx = current + (gap+cell)/2;                    
                                        // Print<level=%i> fmt=%s fortran format statements not supported                    
                                  }                 
                                  len = len + cell + gap;                 
                                  current = current + cell + gap;                 
                                  if ( plate>0 )                 
                                  {                    
                                        cell = radiator;                    
                                        _create = AgCreate("ERAD");                    
                                        {                       
                                              AgShape myshape; // undefined shape                       
                                              ///Create ERAD                       
                                              Create("ERAD");                        
                                        }                    
                                        { AgPlacement place = AgPlacement("ERAD","ESEC");                       
                                              /// Add daughter volume ERAD to mother ESEC                       
                                              place.TranslateZ(len+cell/2);                       
                                              /// Translate z = len+cell/2                       
                                              _stacker -> Position( AgBlock::Find("ERAD"), place );                       
                                        } // end placement of ERAD                    
                                        len = len + cell;                    
                                        current = current + cell;                    
                                  }                 
                            }              
                      }           
                      END_OF_ESEC:           
                      mCurrent = _save;           
                ///@}        
          } // End Block ESEC     
          // ---------------------------------------------------------------------------------------------------     
          void EMGT::Block( AgCreate create )     
          {         
                ///@addtogroup EMGT_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("EMGT");              
                            attr.par("seen")=1;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      mykase=2;           
                      if ( (i_section==1||i_section==2||i_section==5) )           
                      {              
                            mykase=1;              
                      }           
                      if ( mykase==1 )           
                      {              
                            /// Material Air_EMGT1 isvol=0               
                            { AgMaterial &mat = AgMaterial::Get("Air_emgt1");                 
                                  mat.par("isvol")=0;                 
                                  _material = mat;                 
                            }              
                      }           
                      else           
                      {              
                            /// Material Air_EMGT2 isvol=0               
                            { AgMaterial &mat = AgMaterial::Get("Air_emgt2");                 
                                  mat.par("isvol")=0;                 
                                  _material = mat;                 
                            }              
                      }           
                      {  AgShape shape = AgShape("Cons");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dz")=mgt/2;              
                            shape.par("rmn1")=(current-diff)*tan_low-dd;              
                            shape.par("rmx1")=(current-diff)*tan_upp+dup;              
                            shape.par("rmn2")=(current+mgt-diff)*tan_low-dd;              
                            shape.par("rmx2")=(current+mgt-diff)*tan_upp+dup;              
                            /// Shape Cons dz=mgt/2 rmn1=(current-diff)*tan_low-dd rmx1=(current-diff)*tan_upp+dup rmn2=(current+mgt-diff)*tan_low-dd rmx2=(current+mgt-diff)*tan_upp+dup               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_EMGT;              
                            _stacker -> Build(this);              
                      }           
                      if ( mykase==1 )           
                      {              
                            // _medium.par("CUTGAM") = 0.00001;              
                            // _medium.par("CUTELE") = 0.00001;              
                      }           
                      else           
                      {              
                            // _medium.par("CUTGAM") = 0.00008;              
                            // _medium.par("CUTELE") = 0.001;              
                            // _medium.par("BCUTE") = 0.0001;              
                      }           
                      /// Loop on isec from 1 to nint(emcs.nslices) step=1           
                      for ( isec=1; (1>0)? (isec<=nint(emcs.nslices)):(isec>=nint(emcs.nslices)); isec+=1 )           
                      {              
                            _create = AgCreate("EPER");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create EPER                 
                                  Create("EPER");                  
                            }              
                            { AgPlacement place = AgPlacement("EPER","EMGT");                 
                                  /// Add daughter volume EPER to mother EMGT                 
                                  place.AlphaZ((emcs.nslices/2-isec+0.5)*dphi);                 
                                  /// Rotate: AlphaZ = (emcs.nslices/2-isec+0.5)*dphi                 
                                  /// G3 Reference: thetax = 90                 
                                  /// G3 Reference: phix = 0                 
                                  /// G3 Reference: thetay = 90                 
                                  /// G3 Reference: phiy = 90                 
                                  /// G3 Reference: thetaz = 0                 
                                  /// G3 Reference: phiz = 0                 
                                  _stacker -> Position( AgBlock::Find("EPER"), place );                 
                            } // end placement of EPER              
                      }           
                      END_OF_EMGT:           
                      mCurrent = _save;           
                ///@}        
          } // End Block EMGT     
          // ---------------------------------------------------------------------------------------------------     
          void EPER::Block( AgCreate create )     
          {         
                ///@addtogroup EPER_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material POLYSTYREN            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Polystyren");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("EPER");              
                            attr.par("seen")=1;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Cons");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dz")=mgt/2;              
                            shape.par("rmn1")=(current-diff)*tan_low-dd;              
                            shape.par("rmx1")=(current-diff)*tan_upp+dup;              
                            shape.par("rmn2")=(current+mgt-diff)*tan_low-dd;              
                            shape.par("rmx2")=(current+mgt-diff)*tan_upp+dup;              
                            shape.par("phi1")=emcs.phimin/emcs.nsector;              
                            shape.par("phi2")=+emcs.phimax/emcs.nsector;              
                            /// Shape Cons dz=mgt/2 rmn1=(current-diff)*tan_low-dd rmx1=(current-diff)*tan_upp+dup rmn2=(current+mgt-diff)*tan_low-dd rmx2=(current+mgt-diff)*tan_upp+dup phi1=emcs.phimin/emcs.nsector phi2=+emcs.phimax/emcs.nsector               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_EPER;              
                            _stacker -> Build(this);              
                      }           
                      curcl = current+mgt/2;           
                      /// Loop on ie from 1 to nint(eetr.neta) step=1           
                      for ( ie=1; (1>0)? (ie<=nint(eetr.neta)):(ie>=nint(eetr.neta)); ie+=1 )           
                      {              
                            etabot  = eetr.etabin(ie);              
                            etatop  = eetr.etabin(ie+1);              
                            rbot=(curcl-diff)*tanf(etabot);              
                            if ( plate>0 )              
                            {                 
                                  rtop=min((curcl-diff)*tanf(etatop),                     ((current-diff)*tan_upp+dup));                 
                            }              
                            else              
                            {                 
                                  rtop=min((curcl-diff)*tanf(etatop),                     ((current-diff)*tan_upp+dup));                 
                            }              
                            if ( not ( rbot<rtop )) { continue; }              
                            xx=tan(pi*emcs.phimax/180.0/emcs.nsector);              
                            yy=cos(pi*emcs.phimax/180.0/emcs.nsector);              
                            _create = AgCreate("ETAR");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create ETAR                 
                                  Create("ETAR");                  
                            }              
                            { AgPlacement place = AgPlacement("ETAR","EPER");                 
                                  /// Add daughter volume ETAR to mother EPER                 
                                  place.TranslateX((rbot+rtop)/2);                 
                                  /// Translate x = (rbot+rtop)/2                 
                                  /// G3 Reference: thetax = 90                 
                                  /// G3 Reference: phix = 0                 
                                  /// G3 Reference: thetay = 90                 
                                  /// G3 Reference: phiy = 90                 
                                  /// G3 Reference: thetaz = 0                 
                                  /// G3 Reference: phiz = 0                 
                                  place.Ortho( "YZX" ); // ORT=YZX                 
                                  /// Axis substitution: XYZ --> YZX                 
                                  _stacker -> Position( AgBlock::Find("ETAR"), place );                 
                            } // end placement of ETAR              
                            // Print<level=%i> fmt=%s fortran format statements not supported              
                      }           
                      END_OF_EPER:           
                      mCurrent = _save;           
                ///@}        
          } // End Block EPER     
          // ---------------------------------------------------------------------------------------------------     
          void ETAR::Block( AgCreate create )     
          {         
                ///@addtogroup ETAR_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("ETAR");              
                            attr.par("seen")=1;              
                            attr.par("colo")=4;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Trd1");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx1")=rbot*xx-emcs.gapcel/yy;              
                            shape.par("dx2")=rtop*xx-emcs.gapcel/yy;              
                            shape.par("dy")=mgt/2;              
                            shape.par("dz")=(rtop-rbot)/2;              
                            /// Shape Trd1 dx1=rbot*xx-emcs.gapcel/yy dx2=rtop*xx-emcs.gapcel/yy dy=mgt/2 dz=(rtop-rbot)/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_ETAR;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("EALP");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create EALP              
                            Create("EALP");               
                      }           
                      { AgPlacement place = AgPlacement("EALP","ETAR");              
                            /// Add daughter volume EALP to mother ETAR              
                            place.TranslateY((-mgt+emcs.alincell)/2);              
                            /// Translate y = (-mgt+emcs.alincell)/2              
                            _stacker -> Position( AgBlock::Find("EALP"), place );              
                      } // end placement of EALP           
                      g10 = esec.scint;           
                      _create = AgCreate("ESCI");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create ESCI              
                            Create("ESCI");               
                      }           
                      { AgPlacement place = AgPlacement("ESCI","ETAR");              
                            /// Add daughter volume ESCI to mother ETAR              
                            place.TranslateY((-mgt+g10)/2+emcs.alincell+emcs.frplast);              
                            /// Translate y = (-mgt+g10)/2+emcs.alincell+emcs.frplast              
                            _stacker -> Position( AgBlock::Find("ESCI"), place );              
                      } // end placement of ESCI           
                      END_OF_ETAR:           
                      mCurrent = _save;           
                ///@}        
          } // End Block ETAR     
          // ---------------------------------------------------------------------------------------------------     
          void ESCI::Block( AgCreate create )     
          {         
                ///@addtogroup ESCI_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material POLYSTYREN            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Polystyren");              
                            _material = mat;              
                      }           
                      /// Material Cpolystyren isvol=1            
                      { AgMaterial &mat = AgMaterial::Get("Cpolystyren");              
                            mat.par("isvol")=1;              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("ESCI");              
                            attr.par("seen")=1;              
                            attr.par("colo")=7;              
                            attr.par("fill")=0;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Trd1");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dy")=esec.scint/2;              
                            shape.par("dz")=(rtop-rbot)/2-emcs.gapcel;              
                            /// Shape Trd1 dy=esec.scint/2 dz=(rtop-rbot)/2-emcs.gapcel               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_ESCI;              
                            _stacker -> Build(this);              
                      }           
                      // _medium.par("CUTGAM") = 0.00008;           
                      // _medium.par("CUTELE") = 0.001;           
                      // _medium.par("BCUTE") = 0.0001;           
                      // _medium.par("CUTNEU") = 0.001;           
                      // _medium.par("CUTHAD") = 0.001;           
                      // _medium.par("CUTMUO") = 0.001;           
                      // _medium.par("BIRK1") = 1.;           
                      // _medium.par("BIRK2") = 0.013;           
                      // _medium.par("BIRK3") = 9.6E-6;           
                      END_OF_ESCI:           
                      mCurrent = _save;           
                ///@}        
          } // End Block ESCI     
          // ---------------------------------------------------------------------------------------------------     
          void ERAD::Block( AgCreate create )     
          {         
                ///@addtogroup ERAD_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Iron            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Iron");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("ERAD");              
                            attr.par("seen")=1;              
                            attr.par("colo")=6;              
                            attr.par("fill")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Cons");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dz")=radiator/2;              
                            shape.par("rmn1")=(current)*tan_low-dd;              
                            shape.par("rmx1")=(current)*tan_upp+dup;              
                            shape.par("rmn2")=(current+cell)*tan_low-dd;              
                            shape.par("rmx2")=(current+radiator)*tan_upp+dup;              
                            /// Shape Cons dz=radiator/2 rmn1=(current)*tan_low-dd rmx1=(current)*tan_upp+dup rmn2=(current+cell)*tan_low-dd rmx2=(current+radiator)*tan_upp+dup               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_ERAD;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("ELED");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create ELED              
                            Create("ELED");               
                      }           
                      { AgPlacement place = AgPlacement("ELED","ERAD");              
                            /// Add daughter volume ELED to mother ERAD              
                            _stacker -> Position( AgBlock::Find("ELED"), place );              
                      } // end placement of ELED           
                      END_OF_ERAD:           
                      mCurrent = _save;           
                ///@}        
          } // End Block ERAD     
          // ---------------------------------------------------------------------------------------------------     
          void ELED::Block( AgCreate create )     
          {         
                ///@addtogroup ELED_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Lead            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Lead");              
                            _material = mat;              
                      }           
                      /// Material Lead_ELED isvol=0            
                      { AgMaterial &mat = AgMaterial::Get("Lead_eled");              
                            mat.par("isvol")=0;              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("ELED");              
                            attr.par("seen")=1;              
                            attr.par("colo")=4;              
                            attr.par("fill")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tubs");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=(current)*tan_low;              
                            shape.par("rmax")=(current+emcs.pbplate)*tan_upp;              
                            shape.par("dz")=emcs.pbplate/2;              
                            /// Shape Tubs rmin=(current)*tan_low rmax=(current+emcs.pbplate)*tan_upp dz=emcs.pbplate/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_ELED;              
                            _stacker -> Build(this);              
                      }           
                      // _medium.par("CUTGAM") = 0.00008;           
                      // _medium.par("CUTELE") = 0.001;           
                      // _medium.par("BCUTE") = 0.0001;           
                      // _medium.par("CUTNEU") = 0.001;           
                      // _medium.par("CUTHAD") = 0.001;           
                      // _medium.par("CUTMUO") = 0.001;           
                      END_OF_ELED:           
                      mCurrent = _save;           
                ///@}        
          } // End Block ELED     
          // ---------------------------------------------------------------------------------------------------     
          void EFLP::Block( AgCreate create )     
          {         
                ///@addtogroup EFLP_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Aluminium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Aluminium");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("EFLP");              
                            attr.par("seen")=1;              
                            attr.par("colo")=3;              
                            attr.par("fill")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Cons");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dz")=emcs.front/2;              
                            shape.par("rmn1")=68.813;              
                            shape.par("rmx1")=(zslice-diff)*tan_upp+dup;              
                            shape.par("rmn2")=68.813;              
                            shape.par("rmx2")=(zslice+slcwid-diff)*tan_upp+dup;              
                            shape.par("phi1")=emcs.phimin;              
                            shape.par("phi2")=emcs.phimax;              
                            /// Shape Cons dz=emcs.front/2 rmn1=68.813 rmx1=(zslice-diff)*tan_upp+dup rmn2=68.813 rmx2=(zslice+slcwid-diff)*tan_upp+dup phi1=emcs.phimin phi2=emcs.phimax               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_EFLP;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_EFLP:           
                      mCurrent = _save;           
                ///@}        
          } // End Block EFLP     
          // ---------------------------------------------------------------------------------------------------     
          void EALP::Block( AgCreate create )     
          {         
                ///@addtogroup EALP_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Aluminium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Aluminium");              
                            _material = mat;              
                      }           
                      /// Material StrAluminium isvol=0            
                      { AgMaterial &mat = AgMaterial::Get("Straluminium");              
                            mat.par("isvol")=0;              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("EALP");              
                            attr.par("seen")=1;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Trd1");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dy")=emcs.alincell/2;              
                            shape.par("dz")=(rtop-rbot)/2;              
                            /// Shape Trd1 dy=emcs.alincell/2 dz=(rtop-rbot)/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_EALP;              
                            _stacker -> Build(this);              
                      }           
                      // _medium.par("CUTGAM") = 0.00001;           
                      // _medium.par("CUTELE") = 0.00001;           
                      // _medium.par("LOSS") = 1.;           
                      // _medium.par("STRA") = 1.;           
                      END_OF_EALP:           
                      mCurrent = _save;           
                ///@}        
          } // End Block EALP     
          // ---------------------------------------------------------------------------------------------------     
          void ESPL::Block( AgCreate create )     
          {         
                ///@addtogroup ESPL_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("ESPL");              
                            attr.par("seen")=1;              
                            attr.par("colo")=3;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tubs");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=section*tan_low-1.526;              
                            shape.par("rmax")=(section-secwid/2)*tan_upp+dup;              
                            shape.par("phi1")=emcs.phimin;              
                            shape.par("phi2")=emcs.phimax;              
                            shape.par("dz")=secwid/2;              
                            /// Shape Tubs rmin=section*tan_low-1.526 rmax=(section-secwid/2)*tan_upp+dup phi1=emcs.phimin phi2=emcs.phimax dz=secwid/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_ESPL;              
                            _stacker -> Build(this);              
                      }           
                      /// USE emxg version=1 ;           
                      emxg.Use("version",(Float_t)1 );           
                      msecwd = (emxg.sapex+emxg.f4)/2;           
                      /// Loop on isec from 1 to 6 step=1           
                      for ( isec=1; (1>0)? (isec<=6):(isec>=6); isec+=1 )           
                      {              
                            cut=1;              
                            d3 = 75 - (isec-1)*30;              
                            if ( exse.sectype(isec)==0||(emcg.fillmode==1&&(isec==6||isec==1)) )              
                            {                 
                                  cut = 0;                 
                                  _create = AgCreate("EXSG");                 
                                  {                    
                                        AgShape myshape; // undefined shape                    
                                        ///Create EXSG                    
                                        Create("EXSG");                     
                                  }                 
                                  { AgPlacement place = AgPlacement("EXSG","ESPL");                    
                                        /// Add daughter volume EXSG to mother ESPL                    
                                        place.par("ncopy")=isec;                    
                                        /// Ncopy: isec                    
                                        place.AlphaZ(d3);                    
                                        /// Rotate: AlphaZ = d3                    
                                        /// G3 Reference: thetax = 90                    
                                        /// G3 Reference: phix = 0                    
                                        /// G3 Reference: thetay = 90                    
                                        /// G3 Reference: phiy = 90                    
                                        /// G3 Reference: thetaz = 0                    
                                        /// G3 Reference: phiz = 0                    
                                        _stacker -> Position( AgBlock::Find("EXSG"), place );                    
                                  } // end placement of EXSG                 
                            }              
                            else if ( exse.sectype(isec)==1 )              
                            {                 
                                  _create = AgCreate("EXSG");                 
                                  {                    
                                        AgShape myshape; // undefined shape                    
                                        ///Create EXSG                    
                                        Create("EXSG");                     
                                  }                 
                                  { AgPlacement place = AgPlacement("EXSG","ESPL");                    
                                        /// Add daughter volume EXSG to mother ESPL                    
                                        place.par("ncopy")=isec;                    
                                        /// Ncopy: isec                    
                                        place.AlphaZ(d3);                    
                                        /// Rotate: AlphaZ = d3                    
                                        /// G3 Reference: thetax = 90                    
                                        /// G3 Reference: phix = 0                    
                                        /// G3 Reference: thetay = 90                    
                                        /// G3 Reference: phiy = 90                    
                                        /// G3 Reference: thetaz = 0                    
                                        /// G3 Reference: phiz = 0                    
                                        _stacker -> Position( AgBlock::Find("EXSG"), place );                    
                                  } // end placement of EXSG                 
                                  _create = AgCreate("EXGT");                 
                                  {                    
                                        AgShape myshape; // undefined shape                    
                                        ///Create EXGT                    
                                        Create("EXGT");                     
                                  }                 
                                  { AgPlacement place = AgPlacement("EXGT","ESPL");                    
                                        /// Add daughter volume EXGT to mother ESPL                    
                                        place.TranslateZ(msecwd);                    
                                        /// Translate z = msecwd                    
                                        place.AlphaZ(d3);                    
                                        /// Rotate: AlphaZ = d3                    
                                        /// G3 Reference: thetax = 90                    
                                        /// G3 Reference: phix = 0                    
                                        /// G3 Reference: thetay = 90                    
                                        /// G3 Reference: phiy = 90                    
                                        /// G3 Reference: thetaz = 0                    
                                        /// G3 Reference: phiz = 0                    
                                        _stacker -> Position( AgBlock::Find("EXGT"), place );                    
                                  } // end placement of EXGT                 
                            }              
                            else if ( exse.sectype(isec)==2 )              
                            {                 
                                  _create = AgCreate("EXSG");                 
                                  {                    
                                        AgShape myshape; // undefined shape                    
                                        ///Create EXSG                    
                                        Create("EXSG");                     
                                  }                 
                                  { AgPlacement place = AgPlacement("EXSG","ESPL");                    
                                        /// Add daughter volume EXSG to mother ESPL                    
                                        place.par("ncopy")=isec;                    
                                        /// Ncopy: isec                    
                                        place.AlphaZ(d3);                    
                                        /// Rotate: AlphaZ = d3                    
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
                                        place.Ortho( "X-Y-Z" ); // ORT=X-Y-Z                    
                                        /// Axis substitution: XYZ --> X-Y-Z                    
                                        _stacker -> Position( AgBlock::Find("EXSG"), place );                    
                                  } // end placement of EXSG                 
                                  _create = AgCreate("EXGT");                 
                                  {                    
                                        AgShape myshape; // undefined shape                    
                                        ///Create EXGT                    
                                        Create("EXGT");                     
                                  }                 
                                  { AgPlacement place = AgPlacement("EXGT","ESPL");                    
                                        /// Add daughter volume EXGT to mother ESPL                    
                                        place.TranslateZ(-msecwd);                    
                                        /// Translate z = -msecwd                    
                                        place.AlphaZ(d3);                    
                                        /// Rotate: AlphaZ = d3                    
                                        /// G3 Reference: thetax = 90                    
                                        /// G3 Reference: phix = 0                    
                                        /// G3 Reference: thetay = 90                    
                                        /// G3 Reference: phiy = 90                    
                                        /// G3 Reference: thetaz = 0                    
                                        /// G3 Reference: phiz = 0                    
                                        _stacker -> Position( AgBlock::Find("EXGT"), place );                    
                                  } // end placement of EXGT                 
                            }              
                            else if ( exse.sectype(isec)==3 )              
                            {                 
                                  cut=2;                 
                                  _create = AgCreate("EXSG");                 
                                  {                    
                                        AgShape myshape; // undefined shape                    
                                        ///Create EXSG                    
                                        Create("EXSG");                     
                                  }                 
                                  { AgPlacement place = AgPlacement("EXSG","ESPL");                    
                                        /// Add daughter volume EXSG to mother ESPL                    
                                        place.par("ncopy")=isec;                    
                                        /// Ncopy: isec                    
                                        place.AlphaZ(d3);                    
                                        /// Rotate: AlphaZ = d3                    
                                        /// G3 Reference: thetax = 90                    
                                        /// G3 Reference: phix = 0                    
                                        /// G3 Reference: thetay = 90                    
                                        /// G3 Reference: phiy = 90                    
                                        /// G3 Reference: thetaz = 0                    
                                        /// G3 Reference: phiz = 0                    
                                        _stacker -> Position( AgBlock::Find("EXSG"), place );                    
                                  } // end placement of EXSG                 
                                  _create = AgCreate("EXGT");                 
                                  {                    
                                        AgShape myshape; // undefined shape                    
                                        ///Create EXGT                    
                                        Create("EXGT");                     
                                  }                 
                                  { AgPlacement place = AgPlacement("EXGT","ESPL");                    
                                        /// Add daughter volume EXGT to mother ESPL                    
                                        place.TranslateZ(msecwd);                    
                                        /// Translate z = msecwd                    
                                        place.AlphaZ(d3);                    
                                        /// Rotate: AlphaZ = d3                    
                                        /// G3 Reference: thetax = 90                    
                                        /// G3 Reference: phix = 0                    
                                        /// G3 Reference: thetay = 90                    
                                        /// G3 Reference: phiy = 90                    
                                        /// G3 Reference: thetaz = 0                    
                                        /// G3 Reference: phiz = 0                    
                                        _stacker -> Position( AgBlock::Find("EXGT"), place );                    
                                  } // end placement of EXGT                 
                            }              
                            else if ( exse.sectype(isec)==4 )              
                            {                 
                                  cut=2;                 
                                  _create = AgCreate("EXSG");                 
                                  {                    
                                        AgShape myshape; // undefined shape                    
                                        ///Create EXSG                    
                                        Create("EXSG");                     
                                  }                 
                                  { AgPlacement place = AgPlacement("EXSG","ESPL");                    
                                        /// Add daughter volume EXSG to mother ESPL                    
                                        place.par("ncopy")=isec;                    
                                        /// Ncopy: isec                    
                                        place.AlphaZ(d3);                    
                                        /// Rotate: AlphaZ = d3                    
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
                                        place.Ortho( "X-Y-Z" ); // ORT=X-Y-Z                    
                                        /// Axis substitution: XYZ --> X-Y-Z                    
                                        _stacker -> Position( AgBlock::Find("EXSG"), place );                    
                                  } // end placement of EXSG                 
                                  _create = AgCreate("EXGT");                 
                                  {                    
                                        AgShape myshape; // undefined shape                    
                                        ///Create EXGT                    
                                        Create("EXGT");                     
                                  }                 
                                  { AgPlacement place = AgPlacement("EXGT","ESPL");                    
                                        /// Add daughter volume EXGT to mother ESPL                    
                                        place.TranslateZ(-msecwd);                    
                                        /// Translate z = -msecwd                    
                                        place.AlphaZ(d3);                    
                                        /// Rotate: AlphaZ = d3                    
                                        /// G3 Reference: thetax = 90                    
                                        /// G3 Reference: phix = 0                    
                                        /// G3 Reference: thetay = 90                    
                                        /// G3 Reference: phiy = 90                    
                                        /// G3 Reference: thetaz = 0                    
                                        /// G3 Reference: phiz = 0                    
                                        _stacker -> Position( AgBlock::Find("EXGT"), place );                    
                                  } // end placement of EXGT                 
                            }              
                      }           
                      END_OF_ESPL:           
                      mCurrent = _save;           
                ///@}        
          } // End Block ESPL     
          // ---------------------------------------------------------------------------------------------------     
          void EXSG::Block( AgCreate create )     
          {         
                ///@addtogroup EXSG_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("EXSG");              
                            attr.par("seen")=1;              
                            attr.par("colo")=7;              
                            attr.par("serial")=cut;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Tubs");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=section*tan_low-1.526;              
                            shape.par("rmax")=(section-secwid/2)*tan_upp+dup;              
                            shape.par("phi1")=emcs.phimin/emcs.nsupsec;              
                            shape.par("phi2")=emcs.phimax/emcs.nsupsec;              
                            shape.par("dz")=secwid/2;              
                            /// Shape Tubs rmin=section*tan_low-1.526 rmax=(section-secwid/2)*tan_upp+dup phi1=emcs.phimin/emcs.nsupsec phi2=emcs.phimax/emcs.nsupsec dz=secwid/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_EXSG;              
                            _stacker -> Build(this);              
                      }           
                      rbot = emxg.rin;           
                      rtop = emxg.rout;           
                      if ( cut>0 )           
                      {              
                            if ( cut==1 )              
                            {                 
                                  rdel = 3.938;                 
                                  nstr = 288;                 
                            }              
                            else              
                            {                 
                                  rdel = -.475;                 
                                  nstr = 285;                 
                            }              
                            rth = .53*rdel        ;// .53 --- tentatavily;              
                            ddn = sq3*1.713 + rdel;              
                            ddup = .5*1.846 + 1.713;              
                            // Print<level=%i> fmt=%s fortran format statements not supported              
                            mgt = emxg.sbase + .01;              
                            /// Loop on i_str from 1 to nstr step=1              
                            for ( i_str=1; (1>0)? (i_str<=nstr):(i_str>=nstr); i_str+=1 )              
                            {                 
                                  p = .5*(i_str-1)*mgt + 41.3655;                 
                                  if ( p<=(.5*rbot*sq3+rth) )                 
                                  {                    
                                        dxy = 1.9375*sq2;                    
                                        xleft = .5*sq2*p*(sq3 + 1.) - dxy;                    
                                        yleft = .5*sq2*p*(sq3 - 1.) - dxy;                    
                                        yright = .5*sq2*(sqrt( rbot*rbot - p*p) - p);                    
                                        xright = sq2*p + yright;                    
                                  }                 
                                  else if ( (.5*rbot*sq3+rth)<p&&p<=(.5*rtop+1.5) )                 
                                  {                    
                                        // Print<level=%i> fmt=%s fortran format statements not supported                    
                                        dxy = 1.9375*sq2;                    
                                        xleft = .5*sq2*p*(sq3 + 1.) - dxy;                    
                                        yleft = .5*sq2*p*(sq3 - 1.) - dxy;                    
                                        dxy = rdel*sq2/sq3;                    
                                        yright = .5*sq2*p*(1.- 1./sq3);                    
                                        xright = sq2*p - yright - dxy;                    
                                        yright = -yright - dxy;                    
                                  }                 
                                  else if ( p>(.5*rtop+1.5) )                 
                                  {                    
                                        // Print<level=%i> fmt=%s fortran format statements not supported                    
                                        yleft = (sqrt(rtop*rtop - p*p) - p)/sq2;                    
                                        xleft = sq2*p + yleft;                    
                                        dxy = rdel*sq2/sq3;                    
                                        yright = .5*sq2*p*(1.- 1./sq3);                    
                                        xright = sq2*p - yright - dxy;                    
                                        yright = -yright - dxy;                    
                                        dxy = 0.;                    
                                        if ( (.5*sq3*160.-ddn)<p&&p<=(.5*sq3*160.+ddup) )                    
                                        {                       
                                              // Print<level=%i> fmt=%s fortran format statements not supported                       
                                              xc = .5*(sq3*160.+1.846);                       
                                              yc = xc - .5*sq3*1.713;                       
                                              if ( p>yc )                       
                                              {                          
                                                    dxy = .5*sq2*(2/sq3*rdel + .5*sq3*1.846 +                                                                  sqrt(1.713*1.713 - (p-xc)*(p-xc)));                          
                                              }                       
                                              else                       
                                              {                          
                                                    dxy = sq2/sq3*(p - .5*sq3* 160. + ddn);                          
                                              }                       
                                        }                    
                                        else if ( (.5*sq3*195.-ddn)<p&&p<=(.5*sq3*195.+ddup) )                    
                                        {                       
                                              // Print<level=%i> fmt=%s fortran format statements not supported                       
                                              xc = .5*(sq3*195.+1.846);                       
                                              yc = xc - .5*sq3*1.713;                       
                                              if ( p>yc )                       
                                              {                          
                                                    dxy = .5*sq2*(2/sq3*rdel + .5*sq3*1.846 +                                                                  sqrt(1.713*1.713 - (p-xc)*(p-xc)));                          
                                              }                       
                                              else                       
                                              {                          
                                                    dxy = sq2/sq3*(p - .5*sq3*195. + ddn);                          
                                              }                       
                                        }                    
                                        xright = xright + dxy;                    
                                        yright = yright + dxy;                    
                                  }                 
                                  dxy = section*tan_upp - rtop;                 
                                  xc = .5*(xright+xleft) + dxy;                 
                                  yc = .5*(yright+yleft);                 
                                  xx = .5*sq2*(xleft+yleft);                 
                                  yy = .5*sq2*(xright+yright);                 
                                  len = xx-yy;                 
                                  // Print<level=%i> fmt=%s fortran format statements not supported                 
                                  _create = AgCreate("EHMS");                 
                                  {                    
                                        AgShape myshape; // undefined shape                    
                                        ///Create EHMS                    
                                        Create("EHMS");                     
                                  }                 
                                  if ( mod(i_str,2)!=0 )                 
                                  {                    
                                        { AgPlacement place = AgPlacement("EHMS","EXSG");                       
                                              /// Add daughter volume EHMS to mother EXSG                       
                                              place.TranslateX(xc);                       
                                              /// Translate x = xc                       
                                              place.TranslateY(yc);                       
                                              /// Translate y = yc                       
                                              place.AlphaZ(-45);                       
                                              /// Rotate: AlphaZ = -45                       
                                              /// G3 Reference: thetax = 90                       
                                              /// G3 Reference: phix = 0                       
                                              /// G3 Reference: thetay = 90                       
                                              /// G3 Reference: phiy = 90                       
                                              /// G3 Reference: thetaz = 0                       
                                              /// G3 Reference: phiz = 0                       
                                              _stacker -> Position( AgBlock::Find("EHMS"), place );                       
                                        } // end placement of EHMS                    
                                  }                 
                                  else                 
                                  {                    
                                        { AgPlacement place = AgPlacement("EHMS","EXSG");                       
                                              /// Add daughter volume EHMS to mother EXSG                       
                                              place.TranslateX(xc);                       
                                              /// Translate x = xc                       
                                              place.TranslateY(yc);                       
                                              /// Translate y = yc                       
                                              place.AlphaZ(-45);                       
                                              /// Rotate: AlphaZ = -45                       
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
                                              place.Ortho( "X-Y-Z" ); // ORT=X-Y-Z                       
                                              /// Axis substitution: XYZ --> X-Y-Z                       
                                              _stacker -> Position( AgBlock::Find("EHMS"), place );                       
                                        } // end placement of EHMS                    
                                  }                 
                            }              
                      }           
                      END_OF_EXSG:           
                      mCurrent = _save;           
                ///@}        
          } // End Block EXSG     
          // ---------------------------------------------------------------------------------------------------     
          void EHMS::Block( AgCreate create )     
          {         
                ///@addtogroup EHMS_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material POLYSTYREN            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Polystyren");              
                            _material = mat;              
                      }           
                      /// Material Cpolystyren isvol=1            
                      { AgMaterial &mat = AgMaterial::Get("Cpolystyren");              
                            mat.par("isvol")=1;              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("EHMS");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.par("serial")=cut;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Trd1");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx1")=0;              
                            shape.par("dx2")=emxg.sbase/2;              
                            shape.par("dy")=len/2;              
                            shape.par("dz")=emxg.sapex/2;              
                            /// Shape Trd1 dx1=0 dx2=emxg.sbase/2 dy=len/2 dz=emxg.sapex/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_EHMS;              
                            _stacker -> Build(this);              
                      }           
                      // _medium.par("CUTGAM") = 0.00008;           
                      // _medium.par("CUTELE") = 0.001;           
                      // _medium.par("BCUTE") = 0.0001;           
                      // _medium.par("BIRK1") = 1.;           
                      // _medium.par("BIRK2") = 0.0130;           
                      // _medium.par("BIRK3") = 9.6E-6;           
                      END_OF_EHMS:           
                      mCurrent = _save;           
                ///@}        
          } // End Block EHMS     
          // ---------------------------------------------------------------------------------------------------     
          void EXGT::Block( AgCreate create )     
          {         
                ///@addtogroup EXGT_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Component Si	a=28.08	z=14	w=0.6*1*28./60.           
                      /// Component O	a=16	z=8	w=0.6*2*16./60.           
                      /// Component C	a=12	z=6	w=0.4*8*12./174.           
                      /// Component H	a=1	z=1	w=0.4*14*1./174.           
                      /// Component O	a=16	z=8	w=0.4*4*16./174.           
                      /// Mixture g10 dens=1.7           
                      {  AgMaterial &mix = AgMaterial::Get("G10");              
                            mix.Component("Si",28.08,14,0.6*1*28./60.);              
                            mix.Component("O",16,8,0.6*2*16./60.);              
                            mix.Component("C",12,6,0.4*8*12./174.);              
                            mix.Component("H",1,1,0.4*14*1./174.);              
                            mix.Component("O",16,8,0.4*4*16./174.);              
                            mix.par("dens")=1.7;              
                            mix.lock();              
                            _material = mix;              
                            _material.lock();              
                      }           
                      { AgAttribute attr = AgAttribute("EXGT");              
                            attr.par("seen")=1;              
                            attr.par("colo")=7;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tubs");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=(section-diff)*tan_low-1.526;              
                            shape.par("rmax")=(section+msecwd-diff)*tan_upp;              
                            shape.par("phi1")=emcs.phimin/emcs.nsupsec;              
                            shape.par("phi2")=emcs.phimax/emcs.nsupsec;              
                            shape.par("dz")=emxg.f4/2;              
                            /// Shape Tubs rmin=(section-diff)*tan_low-1.526 rmax=(section+msecwd-diff)*tan_upp phi1=emcs.phimin/emcs.nsupsec phi2=emcs.phimax/emcs.nsupsec dz=emxg.f4/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_EXGT;              
                            _stacker -> Build(this);              
                      }           
                      // _medium.par("CUTGAM") = 0.00001;           
                      // _medium.par("CUTELE") = 0.00001;           
                      END_OF_EXGT:           
                      mCurrent = _save;           
                ///@}        
          } // End Block EXGT     
    // ----------------------------------------------------------------------- geoctr
       void EcalGeo::ConstructGeometry( const Char_t *dummy )     
       {        
             ///@addtogroup EcalGeo_revision        
             ///@{           
                   /// Created:    26 jan 1996            
             ///@}        
             ///@addtogroup EcalGeo_revision        
             ///@{           
                   /// Author: Rashid Mehdiyev           
             ///@}        
             AddBlock("EAGA");        
             AddBlock("EALP");        
             AddBlock("ECAL");        
             AddBlock("ECHC");        
             AddBlock("ECVO");        
             AddBlock("ECGH");        
             AddBlock("EFLP");        
             AddBlock("EHMS");        
             AddBlock("ELED");        
             AddBlock("EMGT");        
             AddBlock("EMOD");        
             AddBlock("EPER");        
             AddBlock("EPSB");        
             AddBlock("ERAD");        
             AddBlock("ERCM");        
             AddBlock("ERSM");        
             AddBlock("ESHM");        
             AddBlock("ESEC");        
             AddBlock("ESCI");        
             AddBlock("ESPL");        
             AddBlock("ESSP");        
             AddBlock("EMSS");        
             AddBlock("ETAR");        
             AddBlock("EXGT");        
             AddBlock("EXSG");        
             n=12;        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup emcg_doc        
             ///@{           
                   ++emcg._index;           
                   emcg . version = 5.0; //  Geometry version             
                   /// emcg . version = 5.0; //  Geometry version             
                   emcg . onoff = 3; //  Configurations 0-no, 1-west 2-east 3-both            
                   /// emcg . onoff = 3; //  Configurations 0-no, 1-west 2-east 3-both            
                   emcg . fillmode = 3; //  sectors fill mode             
                   /// emcg . fillmode = 3; //  sectors fill mode             
                   //           
                   emcg.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup emcs_doc        
             ///@{           
                   ++emcs._index;           
                   emcs . type = 1; //  =1 endcap, =2 fpd edcap prototype            
                   /// emcs . type = 1; //  =1 endcap, =2 fpd edcap prototype            
                   emcs . zorig = 268.763; //  calorimeter origin in z            
                   /// emcs . zorig = 268.763; //  calorimeter origin in z            
                   emcs . zend = 310.007; //  Calorimeter end in z            
                   /// emcs . zend = 310.007; //  Calorimeter end in z            
                   emcs . etamin = 1.086; //  upper feducial eta cut             
                   /// emcs . etamin = 1.086; //  upper feducial eta cut             
                   emcs . etamax = 2.000; //  lower fiducial eta cut             
                   /// emcs . etamax = 2.000; //  lower fiducial eta cut             
                   emcs . phimin = -90; //  Min phi             
                   /// emcs . phimin = -90; //  Min phi             
                   emcs . phimax = 90; //  Max phi            
                   /// emcs . phimax = 90; //  Max phi            
                   emcs . offset = 0.0; //  offset in x            
                   /// emcs . offset = 0.0; //  offset in x            
                   emcs . nsupsec = 6; //  Number of azimuthal supersectors                    
                   /// emcs . nsupsec = 6; //  Number of azimuthal supersectors                    
                   emcs . nsector = 30; //  Number of azimutal sectors (Phi granularity)            
                   /// emcs . nsector = 30; //  Number of azimutal sectors (Phi granularity)            
                   emcs . nslices = 5; //  number of phi slices in supersector            
                   /// emcs . nslices = 5; //  number of phi slices in supersector            
                   emcs . nsection = 4; //  Number of readout sections            
                   /// emcs . nsection = 4; //  Number of readout sections            
                   emcs . front = 0.953; //  thickness of the front AL plates            
                   /// emcs . front = 0.953; //  thickness of the front AL plates            
                   emcs . alincell = 0.02; //  Aluminim plate in cell            
                   /// emcs . alincell = 0.02; //  Aluminim plate in cell            
                   emcs . frplast = 0.015; //  Front plastic in megatile            
                   /// emcs . frplast = 0.015; //  Front plastic in megatile            
                   emcs . bkplast = 0.155; //  Fiber routing guides and back plastic            
                   /// emcs . bkplast = 0.155; //  Fiber routing guides and back plastic            
                   emcs . pbplate = 0.457; //  Lead radiator thickness            
                   /// emcs . pbplate = 0.457; //  Lead radiator thickness            
                   emcs . lamplate = 0.05; //  Laminated SS plate thickness            
                   /// emcs . lamplate = 0.05; //  Laminated SS plate thickness            
                   emcs . bckplate = 3.175; //  Back SS plate thickness            
                   /// emcs . bckplate = 3.175; //  Back SS plate thickness            
                   emcs . hub = 3.81; //  thickness of EndCap hub            
                   /// emcs . hub = 3.81; //  thickness of EndCap hub            
                   emcs . rmshift = 2.121; //  radial shift of module            
                   /// emcs . rmshift = 2.121; //  radial shift of module            
                   emcs . smshift = 0.12; //  radial shift of steel support walls            
                   /// emcs . smshift = 0.12; //  radial shift of steel support walls            
                   emcs . gapplt = 0.3/2; //  HALF of the inter-plate gap in phi            
                   /// emcs . gapplt = 0.3/2; //  HALF of the inter-plate gap in phi            
                   emcs . gapcel = 0.03/2; //  HALF of the radial inter-cell gap            
                   /// emcs . gapcel = 0.03/2; //  HALF of the radial inter-cell gap            
                   emcs . gapsmd = 3.400; //  space for SMD detector            
                   /// emcs . gapsmd = 3.400; //  space for SMD detector            
                   emcs . smdcentr = 279.542; //  SMD position            
                   /// emcs . smdcentr = 279.542; //  SMD position            
                   emcs . tierod.at(0) = 160.; //  Radial position of tie rods            
                   ///emcs . tierod.at(0) = 160.; //  Radial position of tie rods            
                   emcs . tierod.at(1) = 195; //  Radial position of tie rods            
                   ///emcs . tierod.at(1) = 195; //  Radial position of tie rods            
                   emcs . bckfrnt = 306.832; //  Backplate front Z            
                   /// emcs . bckfrnt = 306.832; //  Backplate front Z            
                   emcs . gaphalf = 0.4; //  1/2 Gap between halves of endcap wheel            
                   /// emcs . gaphalf = 0.4; //  1/2 Gap between halves of endcap wheel            
                   emcs . cover = 0.075; //  Cover of wheel half            
                   /// emcs . cover = 0.075; //  Cover of wheel half            
                   //           
                   emcs.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup eetr_doc        
             ///@{           
                   ++eetr._index;           
                   eetr . type = 1; //  =1 endcap, =2 fpd            
                   /// eetr . type = 1; //  =1 endcap, =2 fpd            
                   eetr . etagr = 1.0536; //  eta_top/eta_bot tower granularity            
                   /// eetr . etagr = 1.0536; //  eta_top/eta_bot tower granularity            
                   eetr . phigr = 0.0981747; //  Phi granularity (radians)            
                   /// eetr . phigr = 0.0981747; //  Phi granularity (radians)            
                   eetr . neta = 12; //  Eta granularity            
                   /// eetr . neta = 12; //  Eta granularity            
                   eetr . etabin.at(0) = 2.0; //  Eta rapidities            
                   ///eetr . etabin.at(0) = 2.0; //  Eta rapidities            
                   eetr . etabin.at(1) = 1.9008; //  Eta rapidities            
                   ///eetr . etabin.at(1) = 1.9008; //  Eta rapidities            
                   eetr . etabin.at(2) = 1.8065; //  Eta rapidities            
                   ///eetr . etabin.at(2) = 1.8065; //  Eta rapidities            
                   eetr . etabin.at(3) = 1.7168; //  Eta rapidities            
                   ///eetr . etabin.at(3) = 1.7168; //  Eta rapidities            
                   eetr . etabin.at(4) = 1.6317; //  Eta rapidities            
                   ///eetr . etabin.at(4) = 1.6317; //  Eta rapidities            
                   eetr . etabin.at(5) = 1.5507; //  Eta rapidities            
                   ///eetr . etabin.at(5) = 1.5507; //  Eta rapidities            
                   eetr . etabin.at(6) = 1.4738; //  Eta rapidities            
                   ///eetr . etabin.at(6) = 1.4738; //  Eta rapidities            
                   eetr . etabin.at(7) = 1.4007; //  Eta rapidities            
                   ///eetr . etabin.at(7) = 1.4007; //  Eta rapidities            
                   eetr . etabin.at(8) = 1.3312; //  Eta rapidities            
                   ///eetr . etabin.at(8) = 1.3312; //  Eta rapidities            
                   eetr . etabin.at(9) = 1.2651; //  Eta rapidities            
                   ///eetr . etabin.at(9) = 1.2651; //  Eta rapidities            
                   eetr . etabin.at(10) = 1.2023; //  Eta rapidities            
                   ///eetr . etabin.at(10) = 1.2023; //  Eta rapidities            
                   eetr . etabin.at(11) = 1.1427; //  Eta rapidities            
                   ///eetr . etabin.at(11) = 1.1427; //  Eta rapidities            
                   eetr . etabin.at(12) = 1.086; //  Eta rapidities            
                   ///eetr . etabin.at(12) = 1.086; //  Eta rapidities            
                   //           
                   eetr.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup esec_doc        
             ///@{           
                   ++esec._index;           
                   esec . isect = 1; //  Section number               
                   /// esec . isect = 1; //  Section number               
                   esec . nlayer = 1; //  Number of Sci layers along z            
                   /// esec . nlayer = 1; //  Number of Sci layers along z            
                   esec . cell = 1.505; //  Cell full width in z            
                   /// esec . cell = 1.505; //  Cell full width in z            
                   esec . scint = 0.5; //  Sci layer thickness            
                   /// esec . scint = 0.5; //  Sci layer thickness            
                   //           
                   esec.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup esec_doc        
             ///@{           
                   ++esec._index;           
                   esec . isect = 2; //  Section number               
                   /// esec . isect = 2; //  Section number               
                   esec . nlayer = 1; //  Number of Sci layers along z            
                   /// esec . nlayer = 1; //  Number of Sci layers along z            
                   esec . cell = 1.505; //  Cell full width in z            
                   /// esec . cell = 1.505; //  Cell full width in z            
                   esec . scint = 0.5; //  Sci layer thickness            
                   /// esec . scint = 0.5; //  Sci layer thickness            
                   //           
                   esec.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup esec_doc        
             ///@{           
                   ++esec._index;           
                   esec . isect = 3; //  Section number            
                   /// esec . isect = 3; //  Section number            
                   esec . nlayer = 4; //  Number of Sci layers along z            
                   /// esec . nlayer = 4; //  Number of Sci layers along z            
                   esec . cell = 1.405; //  Cell full width in z            
                   /// esec . cell = 1.405; //  Cell full width in z            
                   esec . scint = 0.4; //  Sci layer thickness            
                   /// esec . scint = 0.4; //  Sci layer thickness            
                   //           
                   esec.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup esec_doc        
             ///@{           
                   ++esec._index;           
                   esec . isect = 4; //  Section            
                   /// esec . isect = 4; //  Section            
                   esec . nlayer = 18; //  Number of layers along z            
                   /// esec . nlayer = 18; //  Number of layers along z            
                   esec . cell = 1.405; //  Cell full width in z            
                   /// esec . cell = 1.405; //  Cell full width in z            
                   esec . scint = 0.4; //  Sci layer thickness            
                   /// esec . scint = 0.4; //  Sci layer thickness            
                   //           
                   esec.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup esec_doc        
             ///@{           
                   ++esec._index;           
                   esec . isect = 5; //  Section            
                   /// esec . isect = 5; //  Section            
                   esec . nlayer = 1; //  Number of  layers along z            
                   /// esec . nlayer = 1; //  Number of  layers along z            
                   esec . cell = 1.505; //  Cell full width in z            
                   /// esec . cell = 1.505; //  Cell full width in z            
                   esec . scint = 0.5; //  Sci layer thickness            
                   /// esec . scint = 0.5; //  Sci layer thickness            
                   //           
                   esec.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup emxg_doc        
             ///@{           
                   ++emxg._index;           
                   emxg . version = 1; //  Geometry version            
                   /// emxg . version = 1; //  Geometry version            
                   emxg . sapex = 0.7; //  Scintillator strip apex            
                   /// emxg . sapex = 0.7; //  Scintillator strip apex            
                   emxg . sbase = 1.0; //  Scintillator strip base            
                   /// emxg . sbase = 1.0; //  Scintillator strip base            
                   emxg . rin = 77.41; //  inner radius of SMD plane              
                   /// emxg . rin = 77.41; //  inner radius of SMD plane              
                   emxg . rout = 213.922; //  outer radius of SMD plane            
                   /// emxg . rout = 213.922; //  outer radius of SMD plane            
                   emxg . f4 = .15; //  F4 thickness            
                   /// emxg . f4 = .15; //  F4 thickness            
                   //           
                   emxg.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup exse_doc        
             ///@{           
                   ++exse._index;           
                   exse . jsect = 1; //  Section number            
                   /// exse . jsect = 1; //  Section number            
                   exse . zshift = -1.215; //  Section width            
                   /// exse . zshift = -1.215; //  Section width            
                   exse . sectype.at(0) = 4; //  1-V,2-U,3-cutV,4-cutU                
                   ///exse . sectype.at(0) = 4; //  1-V,2-U,3-cutV,4-cutU                
                   exse . sectype.at(1) = 1; //  1-V,2-U,3-cutV,4-cutU                
                   ///exse . sectype.at(1) = 1; //  1-V,2-U,3-cutV,4-cutU                
                   exse . sectype.at(2) = 0; //  1-V,2-U,3-cutV,4-cutU                
                   ///exse . sectype.at(2) = 0; //  1-V,2-U,3-cutV,4-cutU                
                   exse . sectype.at(3) = 2; //  1-V,2-U,3-cutV,4-cutU                
                   ///exse . sectype.at(3) = 2; //  1-V,2-U,3-cutV,4-cutU                
                   exse . sectype.at(4) = 1; //  1-V,2-U,3-cutV,4-cutU                
                   ///exse . sectype.at(4) = 1; //  1-V,2-U,3-cutV,4-cutU                
                   exse . sectype.at(5) = 0; //  1-V,2-U,3-cutV,4-cutU                
                   ///exse . sectype.at(5) = 0; //  1-V,2-U,3-cutV,4-cutU                
                   //           
                   exse.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup exse_doc        
             ///@{           
                   ++exse._index;           
                   exse . jsect = 2; //  Section number               
                   /// exse . jsect = 2; //  Section number               
                   exse . zshift = 0.; //  Section width            
                   /// exse . zshift = 0.; //  Section width            
                   exse . sectype.at(0) = 0; //  1-V,2-U,3-cutV,4-cutU                
                   ///exse . sectype.at(0) = 0; //  1-V,2-U,3-cutV,4-cutU                
                   exse . sectype.at(1) = 2; //  1-V,2-U,3-cutV,4-cutU                
                   ///exse . sectype.at(1) = 2; //  1-V,2-U,3-cutV,4-cutU                
                   exse . sectype.at(2) = 1; //  1-V,2-U,3-cutV,4-cutU                
                   ///exse . sectype.at(2) = 1; //  1-V,2-U,3-cutV,4-cutU                
                   exse . sectype.at(3) = 0; //  1-V,2-U,3-cutV,4-cutU                
                   ///exse . sectype.at(3) = 0; //  1-V,2-U,3-cutV,4-cutU                
                   exse . sectype.at(4) = 2; //  1-V,2-U,3-cutV,4-cutU                
                   ///exse . sectype.at(4) = 2; //  1-V,2-U,3-cutV,4-cutU                
                   exse . sectype.at(5) = 3; //  1-V,2-U,3-cutV,4-cutU                
                   ///exse . sectype.at(5) = 3; //  1-V,2-U,3-cutV,4-cutU                
                   //           
                   exse.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup exse_doc        
             ///@{           
                   ++exse._index;           
                   exse . jsect = 3; //  Section number               
                   /// exse . jsect = 3; //  Section number               
                   exse . zshift = 1.215; //  Section width            
                   /// exse . zshift = 1.215; //  Section width            
                   exse . sectype.at(0) = 1; //  1-V,2-U,3-cutV,4-cutU                
                   ///exse . sectype.at(0) = 1; //  1-V,2-U,3-cutV,4-cutU                
                   exse . sectype.at(1) = 0; //  1-V,2-U,3-cutV,4-cutU                
                   ///exse . sectype.at(1) = 0; //  1-V,2-U,3-cutV,4-cutU                
                   exse . sectype.at(2) = 2; //  1-V,2-U,3-cutV,4-cutU                
                   ///exse . sectype.at(2) = 2; //  1-V,2-U,3-cutV,4-cutU                
                   exse . sectype.at(3) = 1; //  1-V,2-U,3-cutV,4-cutU                
                   ///exse . sectype.at(3) = 1; //  1-V,2-U,3-cutV,4-cutU                
                   exse . sectype.at(4) = 0; //  1-V,2-U,3-cutV,4-cutU                
                   ///exse . sectype.at(4) = 0; //  1-V,2-U,3-cutV,4-cutU                
                   exse . sectype.at(5) = 2; //  1-V,2-U,3-cutV,4-cutU                
                   ///exse . sectype.at(5) = 2; //  1-V,2-U,3-cutV,4-cutU                
                   //           
                   exse.fill();           
             ///@}        
             //        
             /// USE emcg _index=1;        
             emcg.Use();        
             sq3 = sqrt(3.);        
             sq2 = sqrt(2.);        
             // Print<level=%i> fmt=%s fortran format statements not supported        
             /// USE emcs type=1 ;        
             emcs.Use("type",(Float_t)1 );        
             /// USE eetr type=1 ;        
             eetr.Use("type",(Float_t)1 );        
             orgkeep =  emcs.zorig;        
             endkeep =  emcs.zend;        
             if ( emcg.onoff>0 )        
             {           
                   diff = 0.0;           
                   center  = (emcs.zorig+emcs.zend)/2;           
                   tan_upp = tanf(emcs.etamin);           
                   tan_low = tanf(emcs.etamax);           
                   rth  = sqrt(1. + tan_low*tan_low);           
                   rshift  = emcs.hub * rth;           
                   dup=emcs.rmshift*tan_upp;           
                   dd=emcs.rmshift*rth;           
                   d2=rshift + dd;           
                   radiator  = emcs.pbplate + 2*emcs.lamplate;           
                   dphi = (emcs.phimax-emcs.phimin)/emcs.nsector;           
                   _create = AgCreate("ECAL");           
                   {              
                         AgShape myshape; // undefined shape              
                         ///Create ECAL              
                         Create("ECAL");               
                   }           
                   if ( emcg.onoff==1||emcg.onoff==3 )           
                   {              
                         { AgPlacement place = AgPlacement("ECAL","CAVE");                 
                               /// Add daughter volume ECAL to mother CAVE                 
                               place.TranslateZ(+center);                 
                               /// Translate z = +center                 
                               _stacker -> Position( AgBlock::Find("ECAL"), place );                 
                         } // end placement of ECAL              
                   }           
                   if ( emcg.onoff==2||emcg.onoff==3 )           
                   {              
                         { AgPlacement place = AgPlacement("ECAL","CAVE");                 
                               /// Add daughter volume ECAL to mother CAVE                 
                               place.TranslateZ(-center);                 
                               /// Translate z = -center                 
                               /// G3 Reference: thetax = 90                 
                               /// G3 Reference: phix = 0                 
                               /// G3 Reference: thetay = 90                 
                               /// G3 Reference: phiy = 90                 
                               /// G3 Reference: thetaz = 180                 
                               /// G3 Reference: phiz = 0                 
                               Double_t _thetax=90,_phix=0,_thetay=90,_phiy=90,_thetaz=180,_phiz=0;                 
                               place.Reference( _thetax, _phix, _thetay, _phiy, _thetaz, _phiz );                 
                               _stacker -> Position( AgBlock::Find("ECAL"), place );                 
                         } // end placement of ECAL              
                   }           
                   if ( section>emcs.zend )           
                   {              
                         // Print<level=%i> fmt=%s fortran format statements not supported              
                   }           
                   // Print<level=%i> fmt=%s fortran format statements not supported           
             }        
             // Print<level=%i> fmt=%s fortran format statements not supported        
       }; // EcalGeo     
 }; // namespace EcalGeo  
 
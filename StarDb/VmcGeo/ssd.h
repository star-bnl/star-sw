// $Id: ssd.h,v 1.7 2009/04/09 21:49:45 fisyak Exp $
// $Log: ssd.h,v $
// Revision 1.7  2009/04/09 21:49:45  fisyak
// swap  (x,y,z) = > ( x,-y, z) for sensitive detectors
//
// Revision 1.6  2009/01/21 23:57:43  fisyak
// Add dead area accounting strip stereo
//
// Revision 1.5  2009/01/21 16:22:37  fisyak
// Split SSD ladders into sectors
//
// Revision 1.4  2009/01/20 21:31:26  fisyak
// Decodessd geometry from mortran to Cint
//
/* as base geometry it was used  sisdgeo5.g,v 1.1 2007/02/02 17:19:54 potekhin Exp $
   Revision 1.1  2007/02/02 17:19:54  potekhin
   Silicon wafers have an active area which is smaller than
   the total size of the wafer. This was not reflected in the
   previous design, and is corrected in this piece of code.
   SisdConfig = 55; "fifth version, corrected radii, gaps, dead material"
   SisdConfig = 65  upgrade geometry

ssdConfig  =  3 => y2004*
ssdConfig  =  8 => y2005*, y2006*, y2007*

Splitted version .DEV2:   SFMO  Computed weight: 17.466824 [kg] => 15.39 +/- 0.14 [kg]
dev version           :                          17.466824 [kg] => 15.39 +/- 0.14 [kg]
ssd.New               :                          17.466824 [kg] => 15.24 +/- 0.14 [kg]
ssd.New Fix overlap   :                          17.466824 [kg] => 16.08 +/- 0.15 [kg]

*/
#ifndef ssdConfig
#define ssdConfig 8
#endif
#ifdef ssdConfig
#if !defined(__CINT__)
#include "Riostream.h"
#include "TMath.h"
#include "TGeoManager.h"
#include "Rotations.h"
#include "Material.h"
#include "Media.h"
#include "CreateGeometry.h"
#include "TGeoMatrix.h"
//#include "TGeoPcon.h"
#include "TGeoPgon.h"
#include "TGeoPara.h"
#include "TGeoCompositeShape.h"
#endif
//#define DEBUG
TGeoVolume *ssd() {
  // SSD Parameters:
#if 0
  struct SSDP_t {   // Silicon Strips					
    Int_t Version;  // Version  = Version				
    Int_t Config;   // Config   = There are a few configuraions possible	
    Int_t Placement;// Placement= 0=cave, 1=svtt                         
  };
  SSDP_t SSDP  = { // Silicon Strips					
    1,             // Version  = Version				
    1,             // Config   = There are a few configuraions possible	
    0              // Placement= 0=cave, 1=svtt                         
  };
#endif
  struct SFJP_t { 
    Int_t    Version;          // Version    = new version information 					  
    Double_t AlphaZ;	       // AlphaZ     = Alpha of the SFLA and other parts				  
    Double_t AlphaZH;	       // AlphaZH    = Alpha of other parts					  
    Double_t SSST_Rmin;	       // SSST_Rmin  = Minimum Radius of the Small aluminium sector (outside structur
    Double_t SSST_Rmax;	       // SSST_Rmax  = Maximum Radios of the Small aluminium sector (outside structur
    Double_t SSST_Width;       // SSST_Width = Width of the small aluminium sector (outside structure)	  
    Double_t SSST_Pz;	       // SSST_Pz    = Additional Z-Position of the aluminium sectors		  
    Double_t SSSS_Rmin;	       // SSSS_Rmin  = Width of the small inner aluminium sector (outside structure) 
    Double_t SSSS_Width;       // SSSS_Width = Width of the small inner aluminium sector (outside structure) 
    Double_t SSRS_Rmin;	       // SSRS_Rmin  = Width of the side of the small rib (outside structure)	  
    Double_t SSLB_Dx;	       // SSLB_Dx    = Mother volume of the linking box (sector to the cone)	  
    Double_t SSLB_Px;	       // SSLB_Px    = X-Position = Y-Position of the mother volume of the linking bo
    Double_t SSLB_Dy;	       // SSLB_Dy    = Mother volume of the linking box (sector to the cone)	  
    Double_t SSLB_Dz;	       // SSLB_Dz    = Mother volume of the linking box (sector to the cone)	  
    Double_t SSBQ_Dx;	       // SSBQ_Dx    = Linking box (sector to the cone)				  
    Double_t SSBQ_Dy;	       // SSBQ_Dy    = Linking box (sector to the cone)				  
    Double_t SSBQ_Dz;	       // SSBQ_Dz    = Linking box (sector to the cone)				  
    Double_t SSCR_ThA;	       // SSCR_ThA   = Linking cross total thickness (aluminum part)		  
    Double_t SSCR_ThD;	       // SSCR_ThD   = Linking cross total thickness (DELRIN part)			  
    Double_t SSCR_Wd;	       // SSCR_Wd    = Linking cross total width					  
    Double_t SSCR_Hl;	       // SSCR_Hl    = Linking cross horizontal arm full length			  
    Double_t SSCR_Vl;	       // SSCR_Vl    = Linking cross vertical arm full length			  
    Double_t SSLT_Px;	       // SSLT_Px    = X-Position = Y-Position of the linking tube			  
    Double_t SSLT_Rmax;	       // SSLT_Rmax  = Radius of the linking tube (sector to the cone)		  
    Double_t SSLT_Dz;	       // SSLT_Dz    = Width of the linking tube (sector of the cone)		  
    Double_t SCMP_Dx;	       // SCMP_Dx    = Width of the mounting plate inserted in the cones		  
    Double_t SCMP_Dy;	       // SCMP_Dy    = Width of the mounting plate inserted in the cones		  
    Double_t SCMP_Dz;	       // SCMP_Dz    = Width of the mounting plate inserted in the cones		  
    Double_t SCMP_Px;	       // SCMP_Px    = X-Position = Y-Position of the mounting plate		  
    Double_t SCVM_Dz;	       // SCVM_Dz    = Mother volume of the V-shape piece length			  
    Double_t SCVM_Dx;	       // SCVM_Dx    = Mother volume of the V-shape piece dx			  
    Double_t SCVM_Px;	       // SCVM_Px    = X-Position = Y-Position of the V-Shape piece		  
    Double_t SCVM_Dy;	       // SCVM_Dy    = Mother volume of the V-Shape piece dy			  
    Double_t SCVM_Pz;	       // SCVM_Pz    = Z-Position of the mother volume				  
    Double_t SCVB_Dy;	       // SCVB_Dy    = Height of the base of the V-Shape piece			  
    Double_t SCVS_Dx;	       // SCVS_Dx    = Dx of the V-plates of the V-shape piece 45 degrees		  
    Double_t SCVS_Dy;	       // SCVS_Dy    = Medium Height of the V-plates of the V-shape piece 45 degrees 
    Double_t SCVS_Px;	       // SCVS_Px    = x-Position of the V-plates of the V-shape piece 45 degrees	  
    Double_t SCVS_Py;	       // SCVS_Py    = x-Position of the V-plates of the V-shape piece 45 degrees	  
    Double_t SFCO_Dx;	       // SFCO_Dx    = Dx of the connection board (end of the ladder)		  
    Double_t SFCO_Dy;	       // SFCO_Dy    = Width of the connection board (end of the ladder)		  
    Double_t SFCO_Dz;	       // SFCO_Dz    = Length of the connection board (end of the ladder)		  
    Double_t SFCO_Px;	       // SFCO_Px    = X-Position of the connection board (end of the ladder)	  
    Double_t SFCO_Py;	       // SFCO_Py    = Y-Position of the connection board (end of the ladder)	  
    Double_t SFCM_Dx;	       // SFCM_Dx    = Dx of the second connection board (smaller than the above one,
    Double_t SFCS_Dz;	       // SFCS_Dz    = Dx of the small part of the second connection board		  
    Double_t SFKF_Dy;	       // SFKF_Dy    = Width of the kapton flex circuit between the connection boards
    Double_t SFKF_Dz;	       // SFKF_Dz    = Length of the kapton flex circuit between the connection board
    Double_t SFKF_Dx;	       // SFKF_Dx    = Dx of the first part of the kapton flex circuit		  
    Double_t SFKF_Px;	       // SFKF_Px    = x-Position of the first part of the kapton flex circuit	  
    Double_t SFKF_Py;	       // SFKF_Py    = y-Position of the first part of the kapton flex circuit	  
    Double_t SFKS_Dx;	       // SFKS_Dx    = Dx of the second part of the kapton flex circuit		  
    Double_t SFKS_Px;	       // SFKS_Px    = x-Position of the second part ot the kapton flex		  
    Double_t SFKS_Py;	       // SFKS_Py    = y-Position of the second part ot the kapton flex		  
    Double_t SFPR_Py;	       // SFPR_Py    = Y-Position of the mechanical parts				  
    Double_t SFPB_Py;	       // SFPB_Py    = Y-Position of the mechanical parts 2			  
    Double_t SFPB_Py2;	       // SFPB_Py2   = Y-Position of the mechanical parts 2			  
    Double_t SFCB_Dx;	       // SFCB_Dx    = Dx of the big part of the second connection board		  
    Double_t SSBS_Dy;	       // SSBS_Dy    = Width of the aluminium plates linking the ladder to the sector
    Double_t SSBS_Dx;	       // SSBS_Dx    = Dx = Dz = small part of the aluminium plate linking the ladder
    Double_t SSBB_Dx;	       // SSBB_Dx    = Dx of the Big part of the aluminium plate linking the ladder  
    Double_t SSBB_Dz;	       // SSBB_Dz    = Dz of the Big part of the aluminium plate linking the ladder  
    Double_t FLEX_Di;	       // FLEX_Di    = Distanze between overlaying flexes				  
    Double_t SFPB_Dx;	       // SFPB_Dx    = Dx of the outside mechanical part (G10 rectangle - box base)  
    Double_t SFPB_Dy;	       // SFPB_Dy    = Width of the outside mechanical part (G10 rectangle - box base
    Double_t SFPBDz;	       // SFPBDz     = Length of the outside mechanical part (G10 rectangle - box bas
    Double_t SAPP_Dxe;	       // SAPP_Dxe   = Dx of the epoxy core of the adc board appendice mother volume 
    Double_t SAPP_Dxz;	       // SAPP_Dxz   = Dx of the carbon side shell of the adc board appendice	  
    Double_t SAPP_Dy;	       // SAPP_Dy    = Width of the epoxy core of the adc board appendice mother volu
    Double_t SAPP_Dz;	       // SAPP_Dz    = Length of the adc board appendice mother volume		  
    Double_t SAPP_Py1;	       // SAPP_Py1   = 1. y-Position of the mother colume of the adc board		  
    Double_t SAPP_Py2;	       // SAPP_Py2   = 2. y-Position of the mother colume of the adc board		  
    Double_t SAPP_Py3;	       // SAPP_Py3   = 3. y-Position of the mother colume of the adc board		  
    Double_t SFAM_Dxe;	       // SFAM_Dxe   = Dx of the mother volume of the adc board			  
    Double_t SFAM_Dxz;	       // SFAM_Dxz   = Dx of the small volume of the adc board			  
    Double_t SFAM_Dy;	       // SFAM_Dy    = Width of the mother volume of the adc board			  
    Double_t SFAM_Dz;	       // SFAM_Dz    = Length of the mother volume of the adc board		  
    Double_t SFAM_DZs;	       // SFAM_DZs   = Length of the small volume of the adc board			  
    Double_t SFLA_Dx;	       // SFLA_Dx    = Dx of the long part of the bus cable linking the modules	  
    Double_t SFLA_Dy;	       // SFLA_Dy    = Width of the long part of the bus cable linking the modules	  
    Double_t SFLA_Dz;	       // SFLA_Dz    = Length of the long part of the bus cable linking the modules  
    Double_t SFLB_Dz;	       // SFLB_Dz    = Length of the long bus cable				  
    Double_t SFLC_Dz;	       // SFLC_Dz    = Length of the long bus cable on the cb up to the connector	  
    Double_t SFEB_Dx;	       // SFEB_Dx    = Dx of the big bus elbow					  
    Double_t SFEB_Dz;	       // SFEB_Dz    = Length of the bus elbow					  
    Double_t SFES_Dx;	       // SFES_Dx    = Dx of the big bus elbow					  
    Double_t SFFK_Dxe;	       // SFFK_Dxe   = Dx of the horizontal part of the ladder skeleton carbon base  
    Double_t SFFK_Dy;	       // SFFK_Dy    = Width of the horizontal part of the ladder skeleton carbon bas
    Double_t SFFK_Dz;	       // SFFK_Dz    = Lengthof of the horizontal part of the ladder skeleton carbon      
    Double_t SFFL_Dx;	       // SFFL_Dx    = Dx of the tilted part of the ladder			     
    Double_t SFFK_Dxz;	       // SFFK_Dxz   = Dx of the horizontal part of the kapton film		          
    Double_t SFFK_Px;	       // SFFK_Px    =    x-Position of the carbon base under the ladder	          
    Double_t SFFK_Py1;	       // SFFK_Py1   = 1. y-Position of the carbon base under the ladder	          
    Double_t SFFK_Py2;	       // SFFK_Py2   = 2. y-Position of the carbon base under the ladder	     
    Double_t SFKL_Px;	       // SFKL_Px    =    x-Position of the tilted kapton film under the ladder	     
    Double_t SFKK_Dy;	       // SFKK_Dy    = Width of the kapton film					     
    Double_t SFLU_Dz;	       // SFLU_Dz    = Length of the triangular ladder skeleton			     
    Double_t SFLU_h1;	       // SFLU_h1    = Height of the triangular ladder skeleton			     
    Double_t SFLU_bl1;	       // SFLU_bl1   = Base length  of the triangular ladder skeleton		          
    Double_t SFRA_Dx;	       // SFRA_Dx    =         Dx of the hybrid stiffner			          
    Double_t SFRA_Dy;	       // SFRA_Dy    =      Width of the hybrid stiffner			          
    Double_t SFRA_Py;	       // SFRA_Py    = Y-Position of the hybrid stiffner			          
    Double_t SFRA_Dz;	       // SFRA_Dz    =     Length of the hybrid stiffner			          
    Double_t SFRA_Pz;	       // SFRA_Pz    = Z-Position of the hybrid stiffner			          
    Double_t SFSW_Dy;	       // SFSW_Dy    = Width of a single wafer container			     
    Double_t SFSW_Dz;	       // SFSW_Dz    = Lengthof of a single wafer container			     
    Double_t SFRS_Dx;	       // SFRS_Dx    = Dx of the two supports of the hybrid stiffner		     
    Double_t SFRS_Dy;	       // SFRS_Dy    = Dy of the two supports of the hybrid stiffner		     
    Double_t SFRS_Px;	       // SFRS_Px    = x-Position of the two supports of the hybrid stiffner	     
    Double_t SFRS_Py;	       // SFRS_Py    = y-Position of the two supports of the hybrid stiffner	     
    Double_t SFSM_Ll;	       // SFSM_Ll    = Length of the triangular base				     
    Double_t SFFX_Dx;	       // SFFX_Dx    = Dx of the flex						          
    Double_t SFFX_Dyz;	       // SFFX_Dyz   = Width of the flex					          
    Double_t SFPI_Rmin;	       // SFPI_Rmin  = Rmin of the pions					          
    Double_t SFPI_Rmax;	       // SFPI_Rmax  = Rmax of the pions					     
    Double_t SFPI_Px;	       // SFPI_Px    = X-Position of the four pions 				     
    Double_t SFPI_Py1;	       // SFPI_Py1   = 1. Y-Position of the four pions 				     
    Double_t SFPI_Py2;	       // SFPI_Py2   = 2. Y-Position of the four pions 				     
    Double_t SFPI_Pz;	       // SFPI_Pz    = Z-Position of the four pions 				     
    Double_t SFPJ_Dx1;	       // SFPJ_Dx1   = 1. Dx of the base of the pions				     
    Double_t SFPJ_Dx2;	       // SFPJ_Dx2   = 2. Dx of the base of the pions				          
    Double_t SFPJ_Dy;	       // SFPJ_Dy    = Dy of the base of the pions				          
    Double_t SFPJ_Dz;	       // SFPJ_Dz    = Dz of the base of the pions				     
    Double_t SFAA_Dx;	       // SFAA_Dx    = Dx of the A128C chip					     
    Double_t SFAA_Dy;	       // SFAA_Dy    = Dy of the A128C chip					          
    Double_t SFAA_Dz;	       // SFAA_Dz    = Dz of the A128C chip       				          
    Double_t SFAA_Px1;	       // SFAA_Px1   = 1. x-=Position of the A128C chips			          
    Double_t SFAA_Px2;	       // SFAA_Px2   = 2. x-=Position of the A128C chips			          
    Double_t SFAA_Px3;	       // SFAA_Px3   = 3. x-=Position of the A128C chips			          
    Double_t SFAA_Pz1;	       // SFAA_Pz1   = 1. z-=Position of the A128C chips			          
    Double_t SFAA_Pz2;	       // SFAA_Pz2   = 2. z-=Position of the A128C chips			          
    Double_t SFAA_Pz3;	       // SFAA_Pz3   = 3. z-=Position of the A128C chips			     
    Double_t SFSL_Dx;	       // SFSL_Dx    = Dx of the strip detector					     
    Double_t SFSL_Dy;	       // SFSL_Dy    = Dy of the strip detector					     
    Double_t SFSL_Dz;	       // SFSL_Dz    = Dz of the strip detector					     
    Double_t SFSD_Dx;	       // SFSD_Dx    = Dx of the sensitive area					     
    Double_t SFSD_Dy;	       // SFSD_Dy    = Dy of the sensitive area					     
    Double_t SFSD_Dz;	       // SFSD_Dz    = Dz of the sensitive area					     
    Double_t SFSD_stereo;      // SFSD_stereo= stero angle between P and N strips
    Double_t SFLA_Px;	       // SFLA_Px    = x-Position of the top flex along the z-axis    		          
    Double_t SFLA_Py;	       // SFLA_Py    = y-Position of the top flex along the z-axis		     
    Double_t SFLC_Px;	       // SFLC_Px    = x-Position of the bottom flex along the z-axis		     
    Double_t SFLC_Py;	       // SFLC_Py    = y-Position of the bottom flex along the z-axis		          
    Double_t SFES_Px;	       // SFES_Px    = x-Position of the small elbow bus			          
    Double_t SFES_Py;	       // SFES_Py    = y-Position of the small elbow bus			          
    Double_t SFES_Pz;	       // SFES_Pz    = z-Position of the small elbow bus			     
    Double_t SFEB_Px;	       // SFEB_Px    = x-Position of the big elbow bus				     
    Double_t SFEB_Py;	       // SFEB_Py    = y-Position of the big elbow bus                               
  };
  SFJP_t SFJP = {// Different Dimension parameters
       2          , // Version    = new version information 
       54         , // AlphaZ     = Alpha of the SFLA and other parts
       45         , // AlphaZH    = Alpha of other parts
       31.285     , // SSST_Rmin  = Minimum Radius of the Small aluminium sector (outside structure)
       31.8       , // SSST_Rmax  = Maximum Radios of the Small aluminium sector (outside structure)
       2.5        , // SSST_Width = Width of the small aluminium sector (outside structure)
       0.2        , // SSST_Pz    = Additional Z-Position of the aluminium sectors
       23.3       , // SSSS_Rmin  = Width of the small inner aluminium sector (outside structure)
       0.5        , // SSSS_Width = Width of the small inner aluminium sector (outside structure)
       23.3       , // SSRS_Rmin  = Width of the side of the small rib (outside structure)
       8.5        , // SSLB_Dx    = Mother volume of the linking box (sector to the cone)
       19.1       , // SSLB_Px    = X-Position = Y-Position of the mother volume of the linking box
       7.3        , // SSLB_Dy    = Mother volume of the linking box (sector to the cone)
       2.5        , // SSLB_Dz    = Mother volume of the linking box (sector to the cone)
       7.8        , // SSBQ_Dx    = Linking box (sector to the cone)
       7.3        , // SSBQ_Dy    = Linking box (sector to the cone)
       2.5        , // SSBQ_Dz    = Linking box (sector to the cone)
       1.5        , // SSCR_ThA   = Linking cross total thickness (aluminum part)
       1.0        , // SSCR_ThD   = Linking cross total thickness (DELRIN part)
       1.2        , // SSCR_Wd    = Linking cross total width
       8.5        , // SSCR_Hl    = Linking cross horizontal arm full length
       1.6        , // SSCR_Vl    = Linking cross vertical arm full length
       18.1       , // SSLT_Px    = X-Position = Y-Position of the linking tube
       0.8        , // SSLT_Rmax  = Radius of the linking tube (sector to the cone)
       5.05       , // SSLT_Dz    = Width of the linking tube (sector of the cone)
       6.3        , // SCMP_Dx    = Width of the mounting plate inserted in the cones
       0.4        , // SCMP_Dy    = Width of the mounting plate inserted in the cones
       3.6        , // SCMP_Dz    = Width of the mounting plate inserted in the cones
       15.945     , // SCMP_Px    = X-Position = Y-Position of the mounting plate
       3.6        , // SCVM_Dz    = Mother volume of the V-shape piece length
       6.4        , // SCVM_Dx    = Mother volume of the V-shape piece dx
       17.00      , // SCVM_Px    = X-Position = Y-Position of the V-Shape piece
       2.6        , // SCVM_Dy    = Mother volume of the V-Shape piece dy
       57.5       , // SCVM_Pz    = Z-Position of the mother volume
       0.5        , // SCVB_Dy    = Height of the base of the V-Shape piece
       2.3        , // SCVS_Dx    = Dx of the V-plates of the V-shape piece 45 degrees
       0.6        , // SCVS_Dy    = Medium Height of the V-plates of the V-shape piece 45 degrees
       1.0        , // SCVS_Px    = x-Position of the V-plates of the V-shape piece 45 degrees
       0.22       , // SCVS_Py    = x-Position of the V-plates of the V-shape piece 45 degrees
       4.4        , // SFCO_Dx    = Dx of the connection board (end of the ladder)
       0.37       , // SFCO_Dy    = Width of the connection board (end of the ladder)
       15.8       , // SFCO_Dz    = Length of the connection board (end of the ladder)
       1.7        , // SFCO_Px    = X-Position of the connection board (end of the ladder)
       0.2        , // SFCO_Py    = Y-Position of the connection board (end of the ladder)
       4.0        , // SFCM_Dx    = Dx of the second connection board (smaller than the above one, //, //)
       7.5        , // SFCS_Dz    = Dx of the small part of the second connection board
       0.065      , // SFKF_Dy    = Width of the kapton flex circuit between the connection boards
       8.1        , // SFKF_Dz    = Length of the kapton flex circuit between the connection boards
       0.44       , // SFKF_Dx    = Dx of the first part of the kapton flex circuit
       0.16       , // SFKF_Px    = x-Position of the first part of the kapton flex circuit
       2.07       , // SFKF_Py    = y-Position of the first part of the kapton flex circuit
       1.65       , // SFKS_Dx    = Dx of the second part of the kapton flex circuit
       0.505      , // SFKS_Px    = x-Position of the second part ot the kapton flex
       1.585      , // SFKS_Py    = y-Position of the second part ot the kapton flex
       1.79       , // SFPR_Py    = Y-Position of the mechanical parts
       3.48       , // SFPB_Py    = Y-Position of the mechanical parts 2
       0.08       , // SFPB_Py2   = Y-Position of the mechanical parts 2
       3.0        , // SFCB_Dx    = Dx of the big part of the second connection board
       0.5        , // SSBS_Dy    = Width of the aluminium plates linking the ladder to the sector
       2.5        , // SSBS_Dx    = Dx = Dz = small part of the aluminium plate linking the ladder
       4.3        , // SSBB_Dx    = Dx of the Big part of the aluminium plate linking the ladder 
       1.9        , // SSBB_Dz    = Dz of the Big part of the aluminium plate linking the ladder 
       0.02       , // FLEX_Di    = Distanze between overlaying flexes
       4.3        , // SFPB_Dx    = Dx of the outside mechanical part (G10 rectangle - box base)
       0.8        , // SFPB_Dy    = Width of the outside mechanical part (G10 rectangle - box base)
       0.95       , // SFPBDz     = Length of the outside mechanical part (G10 rectangle - box base)
       7.6        , // SAPP_Dxe   = Dx of the epoxy core of the adc board appendice mother volume
       0.04       , // SAPP_Dxz   = Dx of the carbon side shell of the adc board appendice
       0.5        , // SAPP_Dy    = Width of the epoxy core of the adc board appendice mother volume
       1.275      , // SAPP_Dz    = Length of the adc board appendice mother volume
       0.04       , // SAPP_Py1   = 1. y-Position of the mother colume of the adc board
       0.5        , // SAPP_Py2   = 2. y-Position of the mother colume of the adc board
       0.08       , // SAPP_Py3   = 3. y-Position of the mother colume of the adc board
       7.2        , // SFAM_Dxe   = Dx of the mother volume of the adc board
       2.1        , // SFAM_Dxz   = Dx of the small volume of the adc board
       0.3        , // SFAM_Dy    = Width of the mother volume of the adc board
       12.4       , // SFAM_Dz    = Length of the mother volume of the adc board
       2.15       , // SFAM_DZs   = Length of the small volume of the adc board
       1.5        , // SFLA_Dx    = Dx of the long part of the bus cable linking the modules
       0.0130     , // SFLA_Dy    = Width of the long part of the bus cable linking the modules
       4.3        , // SFLA_Dz    = Length of the long part of the bus cable linking the modules
       7.5        , // SFLB_Dz    = Length of the long bus cable
       0.93       , // SFLC_Dz    = Length of the long bus cable on the cb up to the connector
       3.89       , // SFEB_Dx    = Dx of the big bus elbow
       1.5        , // SFEB_Dz    = Length of the bus elbow
       1.875      , // SFES_Dx    = Dx of the big bus elbow
       0.4        , // SFFK_Dxe   = Dx of the horizontal part of the ladder skeleton carbon base
       0.02       , // SFFK_Dy    = Width of the horizontal part of the ladder skeleton carbon base
       69.75      , // SFFK_Dz    = Lengthof of the horizontal part of the ladder skeleton carbon base
       0.6        , // SFFL_Dx    = Dx of the tilted part of the ladder
       5.1        , // SFFK_Dxz   = Dx of the horizontal part of the kapton film
       3.5        , // SFFK_Px    =    x-Position of the carbon base under the ladder
       0.02       , // SFFK_Py1   = 1. y-Position of the carbon base under the ladder
       0.04       , // SFFK_Py2   = 2. y-Position of the carbon base under the ladder
       2.55       , // SFKL_Px    =    x-Position of the tilted kapton film under the ladder
       0.0025     , // SFKK_Dy    = Width of the kapton film
       49.8       , // SFLU_Dz    = Length of the triangular ladder skeleton
       0.02       , // SFLU_h1    = Height of the triangular ladder skeleton
       0.2        , // SFLU_bl1   = Base length  of the triangular ladder skeleton
       3.76       , // SFRA_Dx    =         Dx of the hybrid stiffner
       0.019      , // SFRA_Dy    =      Width of the hybrid stiffner
       0.099      , // SFRA_Py    = Y-Position of the hybrid stiffner
       1.0        , // SFRA_Dz    =     Length of the hybrid stiffner
       1.1        , // SFRA_Pz    = Z-Position of the hybrid stiffner
       1.0        , // SFSW_Dy    = Width of a single wafer container
       2.25       , // SFSW_Dz    = Lengthof of a single wafer container
       0.04       , // SFRS_Dx    = Dx of the two supports of the hybrid stiffner
       0.0915     , // SFRS_Dy    = Dy of the two supports of the hybrid stiffner
       3.8        , // SFRS_Px    = x-Position of the two supports of the hybrid stiffner
       0.0265     , // SFRS_Py    = y-Position of the two supports of the hybrid stiffner
       2.6        , // SFSM_Ll    = Length of the triangular base
       3.84       , // SFFX_Dx    = Dx of the flex
       0.0007     , // SFFX_Dyz   = Width of the flex
       0.15       , // SFPI_Rmin  = Rmin of the pions
       0.2        , // SFPI_Rmax  = Rmax of the pions
       3.2        , // SFPI_Px    = X-Position of the four pions 
       0.35       , // SFPI_Py1   = 1. Y-Position of the four pions 
       0.025      , // SFPI_Py2   = 2. Y-Position of the four pions 
       0.7        , // SFPI_Pz    = Z-Position of the four pions 
       0.075      , // SFPJ_Dx1   = 1. Dx of the base of the pions
       0.025      , // SFPJ_Dx2   = 2. Dx of the base of the pions
       0.3        , // SFPJ_Dy    = Dy of the base of the pions
       0.0125     , // SFPJ_Dz    = Dz of the base of the pions
       0.3        , // SFAA_Dx    = Dx of the A128C chip
       0.015      , // SFAA_Dy    = Dy of the A128C chip
       0.4        , // SFAA_Dz    = Dz of the A128C chip       
       0.325      , // SFAA_Px1   = 1. x-=Position of the A128C chips
       0.65       , // SFAA_Px2   = 2. x-=Position of the A128C chips
       0.6        , // SFAA_Px3   = 3. x-=Position of the A128C chips
       1.1        , // SFAA_Pz1   = 1. z-=Position of the A128C chips
       1.0        , // SFAA_Pz2   = 2. z-=Position of the A128C chips
       0.02       , // SFAA_Pz3   = 3. z-=Position of the A128C chips
       3.75       , // SFSL_Dx    = Dx of the strip detector
       0.015      , // SFSL_Dy    = Dy of the strip detector
       2.1        , // SFSL_Dz    = Dz of the strip detector
       3.65       , // SFSD_Dx    = Dx of the sensitive area
       0.015      , // SFSD_Dy    = Dy of the sensitive area
       2.0        , // SFSD_Dz    = Dz of the sensitive area
       35e-3      , // SFSD_stereo= stero angle between P and N strips
       1.1        , // SFLA_Px    = x-Position of the top flex along the z-axis    
       1.35       , // SFLA_Py    = y-Position of the top flex along the z-axis
       2.3        , // SFLC_Px    = x-Position of the bottom flex along the z-axis
       0.3        , // SFLC_Py    = y-Position of the bottom flex along the z-axis
       3.32       , // SFES_Px    = x-Position of the small elbow bus
       1.6        , // SFES_Py    = y-Position of the small elbow bus
       0.1        , // SFES_Pz    = z-Position of the small elbow bus
       2.71       , // SFEB_Px    = x-Position of the big elbow bus
       0.75         // SFEB_Py    = y-Position of the big elbow bus                               
  };

  struct SFPB_t {    // Some SSD Shell dimensions
    Double_t Hhight; // Hhight  = haut 1			     
    Double_t Khight; // Khight  = haut 2			     
    Double_t Hbase;  // Hbase   = bas 1			     
    Double_t Kbase;  // Kbase   = bas 2			     
    Double_t Fsize;  // Fsize   = lenght of the side kapton film  
    Double_t Zcoor;  // Zcoor   = z coord                         
  };
  Double_t pi = TMath::Pi();
  Double_t z1 = (4.4-2.6/TMath::Tan(56.89*pi/180));
  Double_t z2 = (2.6/TMath::Cos(54*pi/180));
  Double_t Zcoor = TMath::Sqrt(z1*z1+z2*z2);
  SFPB_t SFPB = {// Some SSD Shell dimensions
    (0.02/TMath::Tan(54*pi/180)+0.02/(2*TMath::Tan(pi/5))+0.2)*TMath::Sin(pi/5)       , // Hhight  = haut 1			     
     0.02/TMath::Sin(54*pi/180)                                                       , // Khight  = haut 2			     
    (0.02/TMath::Tan(63*pi/180)+0.01/TMath::Tan(27*pi/180)+0.2)*TMath::Sin(27*pi/180) , // Hbase   = bas 1			     
     0.02/TMath::Sin(63*pi/180)                                                       , // Kbase   = bas 2			     
     0.6/TMath::Cos(15*pi/180)+0.02*TMath::Tan(15*pi/180)                             , // Fsize   = lenght of the side kapton film  
     Zcoor // Zcoor   = z coord                         
  };

  struct SFPA_t {             // Silicon Strip detector parameters			      
    Int_t    Version;         // version  = geometry version				      
    Double_t rmin;	      // rmin     = mother rmin				      
    Double_t rmax;	      // rmax     = mother rmax				      
    Double_t Len;	      // Len      = mother Len along the z direction		      
    Double_t rad;	      // rad      = distance from beam axis to detector center	      
    Double_t nssd;	      // nssd     = number of silicon strip detectors 		      
    Double_t dmWid;	      // dmWid    = detector mother width 			      
    Double_t dmThk;	      // dmThk    = detector mother thickness			      
    Double_t dmLen;	      // dmLen    = detector mother length (detectors + adc board)   
    Double_t smWid;	      // smWid    = structure mother width			      
    Double_t smThk;	      // smThk    = structure mother thickness			      
    Double_t smLen;	      // smLen    = structure mother length 			      
    Double_t ssLen;	      // ssLen    = length of a subvolume of the structure	      
    Double_t wpLen;	      // wpLen    = length of wafer pack			      
    Double_t sdlen;	      // sdlen    = length of one strip detector (along beam axis)   
    Double_t tilt;	      // tilt     = tiling angle (degrees)			      
    Double_t cprad;	      // cprad    = cooling pipe outer radius			      
    Double_t cpral;	      // cpral    = cooling pipe inner radius			      
    Double_t cfrad;	      // cfrad    = carbon fiber tube radius (support structure)     
    Double_t gpThk;	      // gpThk    = gap between structure mother and detector	      
    Int_t    *ladderMap;      //            presence of ladders			      
    Double_t *ladderAngle;    //            individual angles				      
    Double_t *ladderTilt;     //            individual tilts				      
    Double_t *ladderRadius;   //            individual radii                                 
  };
  /*
   * In the following, all the big sector ladders (3 to 9 and 13 to 19) have their radius
   * reduced by 2 mm. EXCEPTED the ladder 16 which stays at 22.5 cm.
   * LM 21-Feb-06
   */
#if ssdConfig == 3
  //        version  = 2          ! geometry version
  Int_t    ladderMap[20]   = {     1,     1,     1,     0,     0,
				   0,     0,     0,     1,     1,
				   1,     1,     1,     0,     0,
				   0,     0,     0,     1,     1}; // presence of ladders
  Double_t ladderAngle[20] = {  90.0, 108.3, 130.0,  -1.0,  -1.0,
				-1.0,  -1.0,  -1.0, 230.0, 251.7,
				270.0, 288.3, 310.0,  -1.0,  -1.0,
				-1.0,  -1.0,  -1.0,  50.0,  71.7}; // individual angles
  Double_t ladderTilt[20]  = {   0.0,  -6.0,   0.0,  -1.0,  -1.0,
				-1.0,  -1.0,   0.0,   0.0,   6.0,
				 0.0,  -6.0,   0.0,  -1.0,  -1.0,
				-1.0,  -1.0,  -1.0,   0.0,   6.0}; // individual tilts
  Double_t ladderRadius[20]= {23.174,22.800,24.600,  -1.0,  -1.0,
			      -1.0,  -1.0,  -1.0,    24.600,22.800,
			      23.174,22.800,24.600,  -1.0,  -1.0,
			      -1.0,  -1.0,  -1.0,    24.600,22.800}; // individual radii
#else
  //    version  = 5          ! geometry version
  Int_t    ladderMap[20] = {     1,     1,     1,     1,     1,
				 1,     1,     1,     1,     1,
				 1,     1,     1,     1,     1,
				 1,     1,     1,     1,     1}; // presence of ladders
  Double_t ladderAngle[20] = {  90.0, 108.3, 126.6, 144.4, 162.2,
                		180.0, 197.8, 215.6, 233.4, 251.7,
                		270.0, 288.3, 306.6, 324.4, 342.2,
                		0.0,  17.8,  35.6,  53.4,  71.7}; // individual angles
  Double_t ladderTilt[20]  = {   0.0,  -6.0,  -7.0,  -7.0,  -7.0,
                		 0.0,   7.0,   7.0,   7.0,   6.0,
                		 0.0,  -6.0,  -7.0,  -7.0,  -7.0,
                		 0.0,   7.0,   7.0,   7.0,   6.0}; // individual tilts
  Double_t ladderRadius[20]= {23.177,22.800,22.600,22.600,22.600,
			      22.300,22.600,22.600,22.600,22.800,
			      23.177,22.800,22.600,22.600,22.600,
			      22.500,22.600,22.600,22.600,22.800}; // individual radii
#endif
  SFPA_t SFPA = {    // Silicon Strip detector parameters			      
    5      ,         // version  = geometry version				      
    21.8   ,         // rmin     = mother rmin				      
    32.    ,         // rmax     = mother rmax				      
    107.   ,         // Len      = mother Len along the z direction		      
    23.    ,         // rad      = distance from beam axis to detector center	      
    16     ,         // nssd     = number of silicon strip detectors 		      
    7.8    ,         // dmWid    = detector mother width 			      
    2.0    ,         // dmThk    = detector mother thickness			      
    90.    ,         // dmLen    = detector mother length (detectors + adc board)   
    7.5    ,         // smWid    = structure mother width			      
    5.0    ,         // smThk    = structure mother thickness			      
    101.9  ,         // smLen    = structure mother length 			      
    95./20.,         // ssLen    = length of a subvolume of the structure	      
    69.6   ,         // wpLen    = length of wafer pack			      
    4.2    ,         // sdlen    = length of one strip detector (along beam axis)   
    5.0    ,         // tilt     = tiling angle (degrees)			      
    0.1    ,         // cprad    = cooling pipe outer radius			      
    0.09   ,         // cpral    = cooling pipe inner radius			      
    0.1    ,         // cfrad    = carbon fiber tube radius (support structure)     
    -1.0   ,         // gpThk    = gap between structure mother and detector	      
    &ladderMap[0],   //            presence of ladders			      
    &ladderAngle[0], //            individual angles				      
    &ladderTilt[0],  //            individual tilts				      
    &ladderRadius[0] //            individual radii                                 
  };
  Int_t    ilad,iwaf,jwaf,nc;
  Double_t wafpckLen,dthk,radtilt,ang;
  Double_t essai,hight,yoffset;
  TGeoRotation *rot = 0;
  Double_t dx, dy, dz, dx1, dx2;
  Double_t x, y, z;
  Double_t rmin, rmax;
  Double_t AlphaZ;
  rmin = SFPA.rmin;
  rmax = SFPA.rmax;
  //  dz   = SFPA.Len/2; // 107
  dz = SFJP.SCVM_Pz + SFJP.SCVM_Dz/2;
  dthk = SFPA.smThk + SFPA.gpThk;//  5 + -1
  TGeoVolume *SFMO = gGeoManager->MakeTube("SFMO",GetMed("SISD_AIR"),rmin,rmax,dz); 
  SFMO->SetTitle("the mother of all Silicon Strip Detector volumes");
  SFMO->SetLineColor(kPink);

  dx = SFPA.dmWid/2;
  dy = (SFPA.dmThk+SFPA.gpThk+SFPA.smThk)/2;
  dz = SFPA.smLen/2;
  TGeoVolume *SFLM = gGeoManager->MakeBox("SFLM",GetMed("SISD_AIR"),dx,dy,dz); 
  SFLM->SetTitle("the mother of the ladder");
  SFLM->SetLineColor(kPink);
  Int_t sector = 0;
  TGeoVolumeAssembly *Sectors[4] = {0,0,0,0};
  for (ilad = 1; ilad <= 20; ilad++) {
    if(SFPA.ladderMap[ilad-1] > 0) { // only position ladders marked as present
      nc = 1;
      if(ilad != 1) nc=20-ilad+2;
      if (nc <=  2 || nc == 20) sector = 1;
      if (nc >=  3 && nc <=  9) sector = 2;
      if (nc >= 10 && nc <= 12) sector = 3;
      if (nc >= 13 && nc <= 19) sector = 4;
      if (! Sectors[sector-1]) {
	Sectors[sector-1] = new TGeoVolumeAssembly("SsdSector");
	if (sector == 1) Sectors[sector-1]->SetTitle("SSD Top Sector");
	if (sector == 2) Sectors[sector-1]->SetTitle("SSD Sourth Sector");
	if (sector == 3) Sectors[sector-1]->SetTitle("SSD Bottom Sector");
	if (sector == 4) Sectors[sector-1]->SetTitle("SSD North Sector");
	SFMO->AddNode(Sectors[sector-1],sector);
      }
      ang     = (SFPA.ladderAngle[ilad-1]*pi)/180.0;
      radtilt = (SFPA.ladderTilt[ilad-1] *pi)/180.0;
      x = (SFPA.ladderRadius[ilad-1]*TMath::Cos(ang)+(dthk*TMath::Cos(ang+radtilt))/2.0);
      y = (SFPA.ladderRadius[ilad-1]*TMath::Sin(ang)+(dthk*TMath::Sin(ang+radtilt))/2.0);
      AlphaZ = SFPA.ladderAngle[ilad-1]-90.0+SFPA.ladderTilt[ilad-1];
      rot = new TGeoRotation("next",90,AlphaZ,90,90+AlphaZ,0,0);
      Sectors[sector-1]->AddNode(SFLM,nc,new TGeoCombiTrans(x,y,0,rot));
    }
  }

  
  dx = SFPA.dmWid/2;
  dy = SFPA.dmThk/2;
  dz = SFPA.wpLen/2;
  TGeoVolume *SFDM = gGeoManager->MakeBox("SFDM",GetMed("SISD_AIR"),dx,dy,dz); 
  SFDM->SetTitle("the mother of the detectors");
  SFDM->SetVisibility(0);
  SFDM->SetLineColor(kPink);
  y = -(SFPA.smThk+SFPA.gpThk)/2;
  SFLM->AddNode(SFDM,1,new TGeoTranslation(0,y,0));
       
  dx = SFPA.dmWid/2;
  dy = SFJP.SFSW_Dy;
  dz = SFJP.SFSW_Dz;
  TGeoVolume *SFSW = gGeoManager->MakeBox("SFSW",GetMed("SISD_AIR"),dx,dy,dz); 
  SFSW->SetTitle("a single wafer container");
  SFSW->SetVisibility(0);
  SFSW->SetLineColor(kPink);
  wafpckLen=SFPA.wpLen/SFPA.nssd;
  for (Int_t iwaf = 1; iwaf <= SFPA.nssd; iwaf++) {
    z = -(SFPA.wpLen+wafpckLen)/2+iwaf*wafpckLen;
    SFDM->AddNode(SFSW,iwaf,new TGeoTranslation(0,0,z));
  }
  
  dx = SFJP.SFSL_Dx;
  dy = SFJP.SFSL_Dy;
  dz = SFJP.SFSL_Dz;
  TGeoVolume *SFSL = gGeoManager->MakeBox("SFSL",GetMed("SISD_SILICON"),dx,dy,dz); 
  SFSL->SetTitle("the Silicon of the strip detector");
  SFSL->SetVisibility(2);
  SFSW->AddNode(SFSL,1,gGeoIdentity);

  dx = SFJP.SFSD_Dx;
  dy = SFJP.SFSD_Dy;
  dz = SFJP.SFSD_Dz;
  TGeoVolume *SFSD = gGeoManager->MakeBox("sfsd",GetMed("SISD_SENSITIVE"),dx,dy,dz); 
  SFSD->SetTitle("the strip detector");
  SFSD->SetVisibility(1);
  SFSD->SetLineColor(kPink);
  SFSL->AddNode(SFSD,1,GetRot("XZXD")); // (x,y,z) = > ( x,-y, z)
    
  dx1 = SFJP.SFSD_Dz;
  dx2 = 0;
  dy  = SFJP.SFSD_Dy;
  dz  = dx1*TMath::Tan(SFJP.SFSD_stereo)/2;
  TGeoVolume *SFTR = gGeoManager->MakeTrd1("SFTR",GetMed("SISD_SILICON"),dx1,dx2,dy,dz); 
  SFTR->SetVisibility(1);
  SFTR->SetLineColor(kYellow);
  x   = - SFJP.SFSD_Dx + dz;
  SFSD->AddNode(SFTR, 1,new TGeoCombiTrans( x,0,0,GetRot("90XY"))); // (x,y,z) = > ( z,-y, x): (-x,-y, z) => ( z, y,-x)
  SFSD->AddNode(SFTR, 2,new TGeoCombiTrans(-x,0,0,GetRot("XY90"))); // (x,y,z) = > (-z, y, x): (-x,-y, z) => (-z,-y,-x)
  //  SFSD->AddNode(SFTR, 1,new TGeoCombiTrans( x,0,0,GetRot("Z2XY"))); 
  //  SFSD->AddNode(SFTR, 2,new TGeoCombiTrans(-x,0,0,GetRot("Z3XY"))); 

  dx = SFJP.SFRA_Dx;
  dy = SFJP.SFRA_Dy;
  dz = SFJP.SFRA_Dz;
  TGeoVolume *SFRA = gGeoManager->MakeBox("SFRA",GetMed("SISD_CARBON"),dx,dy,dz); 
  SFRA->SetTitle("the hybrid stiffneer");
  SFRA->SetVisibility(2);
  x =  0.0;           y = SFJP.SFRA_Py; z=  SFJP.SFRA_Pz;
  SFSW->AddNode(SFRA,1,new TGeoTranslation(x,y, z));
  SFSW->AddNode(SFRA,2,new TGeoTranslation(x,y,-z));
 
  dx = SFJP.SFRS_Dx;
  dy = SFJP.SFRS_Dy;
  dz = SFJP.SFRA_Dz;
  TGeoVolume *SFRS = gGeoManager->MakeBox("SFRS",GetMed("SISD_CARBON"),dx,dy,dz); 
  SFRS->SetTitle("two supports of the hybrid stiffneer (piece of it)");
  SFRS->SetVisibility(2);
  x =  SFJP.SFRS_Px;  y = SFJP.SFRS_Py; z=  SFJP.SFRA_Pz;
  SFSW->AddNode(SFRS,1,new TGeoTranslation( x,y, z));
  SFSW->AddNode(SFRS,2,new TGeoTranslation( x,y,-z));
  SFSW->AddNode(SFRS,3,new TGeoTranslation(-x,y, z));
  SFSW->AddNode(SFRS,4,new TGeoTranslation(-x,y,-z));

  dx = SFJP.SFFX_Dx;
  dy = SFJP.SFFX_Dyz;
  dz = SFJP.SFRA_Dz;
  TGeoVolume *SFFX = gGeoManager->MakeBox("SFFX",GetMed("SISD_COPPER"),dx,dy,dz); 
  SFFX->SetTitle("the flex");
  SFFX->SetLineColor(42);
  x =  0.0;  y = SFJP.SFRA_Py+SFJP.SFRA_Dy+SFJP.SFFX_Dyz; z =  SFJP.SFRA_Pz;
  SFSW->AddNode(SFFX,1,new TGeoTranslation(x,y, z));
  SFSW->AddNode(SFFX,2,new TGeoTranslation(x,y,-z));

  rmin = SFJP.SFPI_Rmin/2.;
  rmax = SFJP.SFPI_Rmax/2.;
  dz   = SFJP.SFPI_Pz/2.0;
  TGeoVolume *SFPI = gGeoManager->MakeTube("SFPI",GetMed("SISD_ALUMINIUM"),rmin,rmax,dz); 
  SFPI->SetTitle("the pions");
  SFPI->SetVisibility(2);
  SFPI->SetLineColor(33);
  x =  SFJP.SFPI_Px; 
  y =  SFJP.SFRA_Py+SFJP.SFRA_Dy+SFJP.SFPI_Py1-SFJP.SFPI_Py2; 
  z =  SFJP.SFPI_Pz;
  SFSW->AddNode(SFPI,1,new TGeoCombiTrans( x,y, z,GetRot("D180")));
  SFSW->AddNode(SFPI,2,new TGeoCombiTrans(-x,y, z,GetRot("D180")));
  SFSW->AddNode(SFPI,3,new TGeoCombiTrans( x,y,-z,GetRot("D180")));
  SFSW->AddNode(SFPI,4,new TGeoCombiTrans(-x,y,-z,GetRot("D180")));
  
  rmin = SFJP.SFPJ_Dx1+SFJP.SFPJ_Dx2;
  rmax = SFJP.SFPJ_Dy;
  dz   = SFJP.SFPJ_Dz;
  TGeoVolume *SFPJ = gGeoManager->MakeTube("SFPJ",GetMed("SISD_ALUMINIUM"),rmin,rmax,dz); 
  SFPJ->SetTitle("the base of the pions");
  SFPJ->SetVisibility(2);
  SFPJ->SetLineColor(33);
  x =  0.; y =  0.; z = -SFJP.SFPI_Pz/2.0+SFJP.SFPJ_Dz;
  SFPI->AddNode(SFPJ,1,new TGeoTranslation(x,y,z));

  dx = SFJP.SFAA_Dx;
  dy = SFJP.SFAA_Dy;
  dz = SFJP.SFAA_Dz;
  TGeoVolume *SFAA = gGeoManager->MakeBox("SFAA",GetMed("SISD_SILICON"),dx,dy,dz); 
  SFAA->SetTitle("the A128C chip");
  SFAA->SetLineColor(41);
  Double_t xyzFAA[12][3] = {
    {-SFJP.SFAA_Px1-SFJP.SFAA_Dx,         
     SFJP.SFRA_Py+SFJP.SFRA_Dy+SFJP.SFFX_Dyz*2+SFJP.SFAA_Dy, 
     SFJP.SFAA_Pz1+SFJP.SFAA_Pz2-SFJP.SFAA_Pz3-SFJP.SFAA_Dz}, 
    {-SFJP.SFAA_Px1-SFJP.SFAA_Dx-SFJP.SFAA_Px2-SFJP.SFAA_Px3, 
     SFJP.SFRA_Py+SFJP.SFRA_Dy+SFJP.SFFX_Dyz*2+SFJP.SFAA_Dy, 
     SFJP.SFAA_Pz1+SFJP.SFAA_Pz2-SFJP.SFAA_Pz3-SFJP.SFAA_Dz}, 
    {-SFJP.SFAA_Px1-SFJP.SFAA_Dx-SFJP.SFAA_Px2-SFJP.SFAA_Px3-SFJP.SFAA_Px2-SFJP.SFAA_Px3, 
     SFJP.SFRA_Py+SFJP.SFRA_Dy+SFJP.SFFX_Dyz*2+SFJP.SFAA_Dy, 
     SFJP.SFAA_Pz1+SFJP.SFAA_Pz2-SFJP.SFAA_Pz3-SFJP.SFAA_Dz}, 
    {+SFJP.SFAA_Px1+SFJP.SFAA_Dx, 
     SFJP.SFRA_Py+SFJP.SFRA_Dy+SFJP.SFFX_Dyz*2+SFJP.SFAA_Dy, 
     SFJP.SFAA_Pz1+SFJP.SFAA_Pz2-SFJP.SFAA_Pz3-SFJP.SFAA_Dz},  
    {+SFJP.SFAA_Px1+SFJP.SFAA_Dx+SFJP.SFAA_Px2+SFJP.SFAA_Px3, 
     SFJP.SFRA_Py+SFJP.SFRA_Dy+SFJP.SFFX_Dyz*2+SFJP.SFAA_Dy, 
     SFJP.SFAA_Pz1+SFJP.SFAA_Pz2-SFJP.SFAA_Pz3-SFJP.SFAA_Dz}, 
    {+SFJP.SFAA_Px1+SFJP.SFAA_Dx+SFJP.SFAA_Px2+SFJP.SFAA_Px3+SFJP.SFAA_Px2+SFJP.SFAA_Px3, 
     SFJP.SFRA_Py+SFJP.SFRA_Dy+SFJP.SFFX_Dyz*2+SFJP.SFAA_Dy, 
     SFJP.SFAA_Pz1+SFJP.SFAA_Pz2-SFJP.SFAA_Pz3-SFJP.SFAA_Dz},  
    {-SFJP.SFAA_Px1-SFJP.SFAA_Dx, 
     SFJP.SFRA_Py+SFJP.SFRA_Dy+SFJP.SFFX_Dyz*2+SFJP.SFAA_Dy, 
     -SFJP.SFAA_Pz1-SFJP.SFAA_Pz2+SFJP.SFAA_Pz3+SFJP.SFAA_Dz}, 
    {-SFJP.SFAA_Px1-SFJP.SFAA_Dx-SFJP.SFAA_Px2-SFJP.SFAA_Px3, 
     SFJP.SFRA_Py+SFJP.SFRA_Dy+SFJP.SFFX_Dyz*2+SFJP.SFAA_Dy, 
     -SFJP.SFAA_Pz1-SFJP.SFAA_Pz2+SFJP.SFAA_Pz3+SFJP.SFAA_Dz}, 
    {-SFJP.SFAA_Px1-SFJP.SFAA_Dx-SFJP.SFAA_Px2-SFJP.SFAA_Px3-SFJP.SFAA_Px2-SFJP.SFAA_Px3, 
     SFJP.SFRA_Py+SFJP.SFRA_Dy+SFJP.SFFX_Dyz*2+SFJP.SFAA_Dy, 
     -SFJP.SFAA_Pz1-SFJP.SFAA_Pz2+SFJP.SFAA_Pz3+SFJP.SFAA_Dz}, 
    {+SFJP.SFAA_Px1+SFJP.SFAA_Dx, 
     SFJP.SFRA_Py+SFJP.SFRA_Dy+SFJP.SFFX_Dyz*2+SFJP.SFAA_Dy, 
     -SFJP.SFAA_Pz1-SFJP.SFAA_Pz2+SFJP.SFAA_Pz3+SFJP.SFAA_Dz},  
    {+SFJP.SFAA_Px1+SFJP.SFAA_Dx+SFJP.SFAA_Px2+SFJP.SFAA_Px3, 
     SFJP.SFRA_Py+SFJP.SFRA_Dy+SFJP.SFFX_Dyz*2+SFJP.SFAA_Dy, 
     -SFJP.SFAA_Pz1-SFJP.SFAA_Pz2+SFJP.SFAA_Pz3+SFJP.SFAA_Dz}, 
    {+SFJP.SFAA_Px1+SFJP.SFAA_Dx+SFJP.SFAA_Px2+SFJP.SFAA_Px3+SFJP.SFAA_Px2+SFJP.SFAA_Px3, 
     SFJP.SFRA_Py+SFJP.SFRA_Dy+SFJP.SFFX_Dyz*2+SFJP.SFAA_Dy, 
     -SFJP.SFAA_Pz1-SFJP.SFAA_Pz2+SFJP.SFAA_Pz3+SFJP.SFAA_Dz}
  };
  for (Int_t i = 0; i < 12; i++) {
    SFSW->AddNode(SFAA,i+1,new TGeoTranslation(xyzFAA[i][0],xyzFAA[i][1],xyzFAA[i][2]));
  }

  dx =  SFPA.dmWid/2; 
  dy =  SFPA.smThk/2; 
  dz =  SFPA.smLen/2.;
  TGeoVolume *SFSM = gGeoManager->MakeBox("SFSM",GetMed("SISD_AIR"),dx,dy,dz); 
  SFSM->SetTitle("the mother of the ladder struct.");
  SFSM->SetVisibility(0);
  SFSM->SetLineColor(kPink);
  y = (SFPA.dmThk+SFPA.gpThk)/2;
  SFLM->AddNode(SFSM,1,new TGeoTranslation(0,y,0));

  dz   = SFJP.SFLU_Dz; 
  Double_t theta = 0; 
  Double_t phi  = 0; 
  Double_t h1   = SFJP.SFLU_h1; 
  Double_t bl1  = SFJP.SFLU_bl1; 
  Double_t tl1  = SFJP.SFLU_bl1-SFJP.SFLU_h1/TMath::Tan(pi/5);
  Double_t alpha1 = (pi/2-TMath::ATan(2*TMath::Tan(pi/5)))*360/(2*pi); 
  Double_t h2   = SFJP.SFLU_h1; 
  Double_t bl2  = SFJP.SFLU_bl1; 
  Double_t tl2  = SFJP.SFLU_bl1-SFJP.SFLU_h1/TMath::Tan(pi/5);
  Double_t alpha2 = (pi/2-TMath::ATan(2*TMath::Tan(pi/5)))*360/(2*pi);
  TGeoVolume *SFLT = gGeoManager->MakeTrap("SFLT",GetMed("SISD_CARBON"), 
					   dz, theta, phi, h1, bl1, tl1, alpha1, h2, bl2, tl2, alpha2);
  SFLT->SetTitle("(half) the top corner of the triangular ladder skeleton");
  hight = SFJP.SFSM_Ll*TMath::Tan(54*pi/180)-SFPB.Hhight/TMath::Tan(pi/5)-0.02;
  yoffset = -1.7;
  x = -(SFPB.Hhight-SFPB.Khight);
  y =  hight+yoffset; 
  z =  0.0; 
  AlphaZ = -90-36;
  rot = new TGeoRotation("next",90,AlphaZ,90,AlphaZ+90,0,0);
  SFSM->AddNode(SFLT,1,new TGeoCombiTrans(x,y,z,rot));
  rot = new TGeoRotation("next",90,-AlphaZ,90,-AlphaZ+90,0,0);
  SFSM->AddNode(SFLT,2,new TGeoCombiTrans(-x,y,z,rot));
  
  dz   = SFJP.SFLU_Dz; 
  theta = 0; 
  phi  = 0; 
  h1   = SFJP.SFLU_h1; 
  bl1  = SFJP.SFLU_bl1; 
  tl1  = SFJP.SFLU_bl1-SFJP.SFLU_h1/TMath::Tan(27*2*pi/360);
  alpha1 = (pi/2-TMath::ATan(2*TMath::Tan(27*2*pi/360)))*360/(2*pi); 
  h2   = SFJP.SFLU_h1; 
  bl2  = SFJP.SFLU_bl1;
  tl2  = SFJP.SFLU_bl1-SFJP.SFLU_h1/TMath::Tan(27*2*pi/360); 
  alpha2 = (pi/2-TMath::ATan(2*TMath::Tan(27*2*pi/360)))*360/(2*pi);   
  TGeoVolume *SFLU = gGeoManager->MakeTrap("SFLU",GetMed("SISD_CARBON"),
					   dz, theta, phi, h1, bl1, tl1, alpha1, h2, bl2, tl2, alpha2);
  SFLU->SetTitle("(half) the side corner of the triangular ladder skeleton");
  x =   0.-(SFJP.SFSM_Ll-(SFJP.SFLU_bl1+SFJP.SFLU_h1/2*1/TMath::Tan(pi/5)));
  y =   0.+yoffset;
  z =   0;
  SFSM->AddNode(SFLU,1,new TGeoTranslation(x,y,z));
  essai = 180 + 27 + 27;
  rot = new TGeoRotation("next",270,essai,90,essai+90,180,0);
  x = -TMath::Cos(63*pi/180)*2*(SFPB.Hbase-SFPB.Kbase)-(SFJP.SFSM_Ll-(0.2+0.01/TMath::Tan(pi/5)));
  y =  TMath::Cos(27*pi/180)*2*(SFPB.Hbase-SFPB.Kbase)+yoffset;
  z =    0;
  SFSM->AddNode(SFLU,2,new TGeoCombiTrans(x,y,z,rot));
  x =   0.+(SFJP.SFSM_Ll-(0.2+0.01/TMath::Tan(pi/5)));
  y =   0.+yoffset;
  z =   0.;
  SFSM->AddNode(SFLU,3,new TGeoCombiTrans(x,y,z,GetRot("P270")));
  rot = new TGeoRotation("next",90,-essai,90,-essai+90,0,0);
  x =  TMath::Cos(63*pi/180)*2*(SFPB.Hbase-SFPB.Kbase)+(SFJP.SFSM_Ll-(0.2+0.01/TMath::Tan(pi/5)));
  y =  TMath::Cos(27*pi/180)*2*(SFPB.Hbase-SFPB.Kbase)+yoffset;
  z =  0;
  SFSM->AddNode(SFLU,4,new TGeoCombiTrans(x,y,z,rot));

  dx = SFJP.SFFK_Dxe; 
  dy = SFJP.SFFK_Dy; 
  dz = SFJP.SFFK_Dz/2;
  TGeoVolume *SFFK = gGeoManager->MakeBox("SFFK",GetMed("SISD_CARBON"),dx,dy,dz); 
  SFFK->SetTitle("horizontal part of the ladder skeleton carbon base");
  x =  SFJP.SFFK_Px/2+SFJP.SFFK_Dxe;
  y = -SFJP.SFFK_Py1-SFJP.SFFK_Py2-SFJP.SFFK_Dy+yoffset;
  z = 0;
  SFSM->AddNode(SFFK,1,new TGeoTranslation( x,y,z));
  SFSM->AddNode(SFFK,2,new TGeoTranslation(-x,y,z));

  dx = SFJP.SFFL_Dx/TMath::Cos(15*pi/180);
  dy = SFJP.SFFK_Dy;
  dz = SFJP.SFFK_Dz/2; 
  TGeoVolume *SFFL = gGeoManager->MakeBox("SFFL",GetMed("SISD_CARBON"),dx,dy,dz); 
  SFFL->SetTitle("titled part of the ladder skeleton carbon base");
  x = -SFJP.SFKL_Px-SFPB.Fsize*TMath::Cos(15*pi/180);
  y = -SFJP.SFFK_Py1-SFJP.SFFK_Py2-SFJP.SFFK_Dy-SFPB.Fsize*TMath::Cos(75*pi/180)+yoffset;
  z =  0;
  SFSM->AddNode(SFFL,1,new TGeoCombiTrans( x, y,z,GetRot("R015")));
  SFSM->AddNode(SFFL,2,new TGeoCombiTrans(-x, y,z,GetRot("R345")));

  dx = SFJP.SFFK_Dxz/2.;
  dy = SFJP.SFKK_Dy/2.; 
  dz = SFJP.SFFK_Dz/2;
  TGeoVolume *SFKK = gGeoManager->MakeBox("SFKK",GetMed("SISD_MYLAR"),dx,dy,dz);
  SFKK->SetTitle("horizontal part of the kapton film under the ladder base");
  y = -SFJP.SFFK_Py1-SFJP.SFFK_Py2-SFJP.SFFK_Py2+yoffset-SFJP.SFKK_Dy/2.;
  SFSM->AddNode(SFKK,1,new TGeoTranslation(0,y,0));

  dx = SFJP.SFFL_Dx/TMath::Cos(15*pi/180); 
  dy = SFJP.SFKK_Dy/2.;
  dz = SFJP.SFFK_Dz/2;
  TGeoVolume *SFKL = gGeoManager->MakeBox("SFKL",GetMed("SISD_MYLAR"),dx,dy,dz);
  SFKL->SetTitle("titled part of the kpaton film under the ladder base");
  x = -SFJP.SFKL_Px-SFPB.Fsize*TMath::Cos(15*pi/180)+0.005;
  y = -SFJP.SFFK_Py1-SFJP.SFFK_Py2-SFJP.SFFK_Dy-SFPB.Fsize*TMath::Cos(75*pi/180)+yoffset-0.021;
  SFSM->AddNode(SFKL,1,new TGeoCombiTrans( x,y,0,GetRot("R015")));
  SFSM->AddNode(SFKL,2,new TGeoCombiTrans(-x,y,0,GetRot("R345")));

  dx =  (SFJP.SAPP_Dxe+2*SFJP.SAPP_Dxz)/2.; 
  dy =  (SFJP.SAPP_Dy+2*SFJP.SAPP_Dxz)/2.; 
  dz =  SFJP.SAPP_Dz/2.;
  TGeoVolume *SAPP = gGeoManager->MakeBox("SAPP",GetMed("SISD_AIR"),dx,dy,dz);
  SAPP->SetTitle("the mother volume of the adc board appendice");
  SAPP->SetVisibility(0);
  SAPP->SetLineColor(kPink);
  y = -SFJP.SAPP_Py1-(SFJP.SAPP_Py2+SFJP.SAPP_Py3)/2.+yoffset;
  z = SFJP.SFFK_Dz/2+SFJP.SAPP_Dz/2.;
  SFSM->AddNode(SAPP,1,new TGeoCombiTrans(0,y,z,GetRot("P270")));
  SFSM->AddNode(SAPP,2,new TGeoTranslation(0,y,-z));

  dx = SFJP.SAPP_Dxe/2.;
  dy = SFJP.SAPP_Dy/2.; 
  dz = SFJP.SAPP_Dy/2.;
  TGeoVolume *SAPC = gGeoManager->MakeBox("SAPC",GetMed("SISD_CARBON"),dx,dy,dz);
  SAPC->SetTitle("the core (Epoxy) of the adc board appendice");
  z =  (-SFJP.SAPP_Dz+SFJP.SAPP_Dy)/2;
  SAPP->AddNode(SAPC,1,new TGeoTranslation(0,0,z));

  dx = SFJP.SAPP_Dxz/2.;
  dy = (SFJP.SAPP_Dy+2*SFJP.SAPP_Dxz)/2.;
  dz = SFJP.SAPP_Dz/2.;
  TGeoVolume *SAPS = gGeoManager->MakeBox("SAPS",GetMed("SISD_CARBON"),dx,dy,dz);
  SAPS->SetTitle("the side shell (Carbon) of the adc board appendice");
  x =  SFJP.SAPP_Dxe/2.+SFJP.SAPP_Dxz/2;
  SAPP->AddNode(SAPS,1,new TGeoTranslation( x,0,0));
  SAPP->AddNode(SAPS,2,new TGeoTranslation(-x,0,0));

  dx = SFJP.SAPP_Dxe/2.;
  dy = SFJP.SAPP_Dxz/2.;
  dz = SFJP.SAPP_Dz/2.;
  TGeoVolume *SAPT = gGeoManager->MakeBox("SAPT",GetMed("SISD_CARBON"),dx,dy,dz);
  y =  (SFJP.SAPP_Dy+SFJP.SAPP_Dxz)/2.;
  SAPP->AddNode(SAPT,1,new TGeoTranslation(0, y,0));
  SAPP->AddNode(SAPT,2,new TGeoTranslation(0,-y,0));

  dx = SFJP.SFAM_Dxe/2.;
  dy = SFJP.SFAM_Dy/2.; 
  dz = SFJP.SFAM_Dz/2;
  TGeoVolume *SFAM = gGeoManager->MakeBox("SFAM",GetMed("SISD_AIR"),dx,dy,dz);
  SFAM->SetTitle("the mother volume of the adc board");
  SFAM->SetVisibility(0);
  SFAM->SetLineColor(kPink);
  y = -SFJP.SAPP_Py1-(SFJP.SAPP_Py2+SFJP.SAPP_Py3)/2.+yoffset;
  z = -(SFJP.SFFK_Dz/2+SFJP.SAPP_Dz)-SFJP.SFAM_Dz/2.;
  SFSM->AddNode(SFAM,1,new TGeoTranslation(0,y,z));
  SFSM->AddNode(SFAM,2,new TGeoCombiTrans(0,y,-z,GetRot("P270")));

  dx = SFJP.SFAM_Dxe/2.;
  dy = SFJP.SFAM_Dy/2.;
  dz = (SFJP.SFAM_Dz-SFJP.SFAM_DZs)/2;
  TGeoVolume *SFAB = gGeoManager->MakeBox("SFAB",GetMed("SISD_G10"),dx,dy,dz);
  SFAB->SetTitle("the big volume of the adc board");
  SFAB->SetLineColor(30);
  z = SFJP.SFAM_DZs/2;
  SFAM->AddNode(SFAB,1,new TGeoTranslation(0,0,z));

  dx = (SFJP.SFAM_Dxz)/2.;
  dy = (SFJP.SFAM_Dy)/2.;
  dz = (SFJP.SFAM_DZs)/2;
  TGeoVolume *SFAS = gGeoManager->MakeBox("SFAS",GetMed("SISD_G10"),dx,dy,dz);
  SFAS->SetTitle("the small volume of the adc board");
  SFAS->SetLineColor(30);
  x = (SFJP.SFAM_Dxe-SFJP.SFAM_Dxz)/2.;
  z = (-SFJP.SFAM_Dz+SFJP.SFAM_DZs)/2;
  SFAM->AddNode(SFAS,1,new TGeoTranslation( x,0,z));
  SFAM->AddNode(SFAS,2,new TGeoTranslation(-x,0,z));

  dx = SFJP.SFCO_Dx/2.; 
  dy = SFJP.SFCO_Dy/2.;
  dz = SFJP.SFCO_Dz/2;
  TGeoVolume *SFCO = gGeoManager->MakeBox("SFCO",GetMed("SISD_G10"),dx,dy,dz);
  SFCO->SetTitle("the connection board (rectangular with Hirose connectors)");
  SFCO->SetLineColor(30);
  x = -SFJP.SFCO_Px;
  y =  SFJP.SFCO_Py;
  z = -(SFJP.SFFK_Dz/2-(SFJP.SFKF_Dz-SFJP.SFCS_Dz))-SFJP.SFCO_Dz/2.;
  SFSM->AddNode(SFCO,1,new TGeoCombiTrans( x,y, z,GetRot("054D")));
  SFSM->AddNode(SFCO,2,new TGeoCombiTrans(-x,y,-z,GetRot("054N")));

  dx = SFJP.SFCM_Dx/2.; 
  dy = SFJP.SFCO_Dy/2.; 
  dz = SFJP.SFCO_Dz/2;
  TGeoVolume *SFCM = gGeoManager->MakeBox("SFCM",GetMed("SISD_AIR"),dx,dy,dz);
  SFCM->SetTitle("the mother volume of the second connection board");
  SFCM->SetVisibility(0);
  SFCM->SetLineColor(kPink);
  x = +SFJP.SFCO_Px;
  y =  SFJP.SFCO_Py; 
  z = -(SFJP.SFFK_Dz/2-(SFJP.SFKF_Dz-SFJP.SFCS_Dz))-SFJP.SFCO_Dz/2.; 
  SFSM->AddNode(SFCM,1,new TGeoCombiTrans( x,y, z,GetRot("054N")));
  rot = new TGeoRotation("next",270,54,90,144,180,0);
  SFSM->AddNode(SFCM,2,new TGeoCombiTrans(-x,y,-z,rot));

  dx = SFJP.SFCB_Dx/2.; 
  dy = SFJP.SFCO_Dy/2.; 
  dz = SFJP.SFCO_Dz/2;
  TGeoVolume *SFCB = gGeoManager->MakeBox("SFCB",GetMed("SISD_G10"),dx,dy,dz);
  SFCB->SetTitle("the big part of the second connection board");
  SFCB->SetLineColor(30);
  SFCM->AddNode(SFCB,1,new TGeoTranslation(0.5,0,0));
  
  dx = (SFJP.SFCM_Dx-SFJP.SFCB_Dx)/2.; 
  dy = SFJP.SFCO_Dy/2.; 
  dz = SFJP.SFCS_Dz/2;
  TGeoVolume *SFCS = gGeoManager->MakeBox("SFCS",GetMed("SISD_G10"),dx,dy,dz);
  SFCS->SetTitle("the big part of the second connection board");
  SFCS->SetLineColor(30);
  x = -1.5;
  z = -SFJP.SFCO_Dz/2 + SFJP.SFCS_Dz/2;
  SFCM->AddNode(SFCS,1,new TGeoTranslation(x,0,z));
  

  dx = SFJP.SFKF_Dx/2.; 
  dy = SFJP.SFKF_Dy/2.; 
  dz = SFJP.SFKF_Dz/2;
  TGeoVolume *SFKF = gGeoManager->MakeBox("SFKF",GetMed("SISD_G10"),dx,dy,dz);
  SFKF->SetTitle("the first part of the kapton flex circuit");
  SFKF->SetLineColor(30);
  x = +SFJP.SFKF_Px;
  y =  SFJP.SFKF_Py; 
  z = +(SFJP.SFFK_Dz/2-(SFJP.SFKF_Dz-SFJP.SFCS_Dz))+SFJP.SFKF_Dz/2+0.2;
  SFSM->AddNode(SFKF,1,new TGeoCombiTrans(-x,y,-z,GetRot("054D")));
  SFSM->AddNode(SFKF,2,new TGeoCombiTrans( x,y, z,GetRot("054N")));

  dx = SFJP.SFKS_Dx/2.; 
  dy = SFJP.SFKF_Dy/2.; 
  dz = SFJP.SFKF_Dz/2;
  TGeoVolume *SFKS = gGeoManager->MakeBox("SFKS",GetMed("SISD_G10"),dx,dy,dz);
  SFKS->SetTitle("the second part of the kapton flex circuit");
  SFKS->SetLineColor(30);
  x = +SFJP.SFKS_Px;
  y =  SFJP.SFKS_Py;
  z = -(SFJP.SFFK_Dz/2-(SFJP.SFKF_Dz-SFJP.SFCS_Dz))-SFJP.SFKF_Dz/2-0.2;
  SFSM->AddNode(SFKS,1,new TGeoCombiTrans( x,y, z,GetRot("054N")));
  SFSM->AddNode(SFKS,2,new TGeoCombiTrans(-x,y,-z,GetRot("054D")));

  TGeoVolume *SFPR = gGeoManager->MakePgon("SFPR",GetMed("SISD_G5"),-126,72,1,2); 
  SFPR->SetTitle("the ladder end inside mechanical part (prism with g10 with half density)");
  ((TGeoPcon*)SFPR->GetShape())->DefineSection(0,0,0,3.48);
  ((TGeoPcon*)SFPR->GetShape())->DefineSection(1,3.7,0,3.48);
  SFPR->SetLineColor(4);
  y = +SFJP.SFPR_Py;
  z = -SFJP.SFLU_Dz;
  SFSM->AddNode(SFPR,1,new TGeoTranslation(0,y,z));
  z = +SFJP.SFLU_Dz-3.7;
  SFSM->AddNode(SFPR,2,new TGeoTranslation(0,y,z));

  dx = SFJP.SFPB_Dx/2.; 
  dy = SFJP.SFPB_Dy/2.; 
  dz = SFJP.SFPBDz/2.;
  TGeoVolume *sfpb = gGeoManager->MakeBox("SFPB",GetMed("SISD_G10"),dx,dy,dz);
  sfpb->SetTitle("the ladder end outside mechanical part (rectangle with g10)");
  sfpb->SetLineColor(4);
  y = +SFJP.SFPR_Py-SFJP.SFPB_Py+SFJP.SFPB_Py2;
  z = -SFJP.SFLU_Dz-SFJP.SFPBDz/2.;
  SFSM->AddNode(sfpb,1,new TGeoTranslation(0,y, z));
  SFSM->AddNode(sfpb,2,new TGeoTranslation(0,y,-z));
  
  dx = SFJP.SSBS_Dx/2.; 
  dy = SFJP.SSBS_Dy/2.; 
  dz = SFJP.SSBS_Dx/2.;
  TGeoVolume *SSBS = gGeoManager->MakeBox("SSBS",GetMed("SISD_ALUMINIUM"),dx,dy,dz);
  SSBS->SetTitle("the small part of the aluminium plate linking the ladder to the sector");
  y = +SFJP.SFPR_Py-SFJP.SFPB_Py+SFJP.SFPB_Py2-0.8/2.-SFJP.SSBS_Dy/2.; 
  z = -SFJP.SFLU_Dz-SFJP.SFPBDz-0.2+SFJP.SSBB_Dz+SFJP.SSBS_Dx/2.;
  SFSM->AddNode(SSBS,1,new TGeoTranslation(0,y, z));
  SFSM->AddNode(SSBS,2,new TGeoTranslation(0,y,-z));

  dx = SFJP.SSBB_Dx/2.; 
  dy = SFJP.SSBS_Dy/2.; 
  dz = SFJP.SSBB_Dz/2.;
  TGeoVolume *SSBB = gGeoManager->MakeBox("SSBB",GetMed("SISD_ALUMINIUM"),dx,dy,dz);
  SSBB->SetTitle("the Big part of the aluminium plate linking the ladder to the sector");
  y = +SFJP.SFPR_Py-SFJP.SFPB_Py+SFJP.SFPB_Py2-0.8/2.-SFJP.SSBS_Dy/2.;  
  z = -SFJP.SFLU_Dz-SFJP.SFPBDz-0.2+SFJP.SSBB_Dz/2.;
  SFSM->AddNode(SSBB,1,new TGeoTranslation(0,y, z));
  SFSM->AddNode(SSBB,2,new TGeoTranslation(0,y,-z));

  dx = SFJP.SFLA_Dx/2.; 
  dy = SFJP.SFLA_Dy/2.; 
  dz = SFJP.SFLA_Dz/2.; 
  TGeoVolume *SFLA = gGeoManager->MakeBox("SFLA",GetMed("SISD_MYLAR"),dx,dy,dz);
  SFLA->SetTitle("the long part of the bus cable linking the syss to the connection board");
  
  dx = SFJP.SFLA_Dx/2.; 
  dy = SFJP.SFLA_Dy/2.; 
  dz = SFJP.SFLB_Dz/2. ;
  TGeoVolume *SFLB = gGeoManager->MakeBox("SFLB",GetMed("SISD_MYLAR"),dx,dy,dz);
  SFLB->SetTitle("the part of the long bus cable on the connection board");

  dx = SFJP.SFLA_Dx/2.; 
  dy = SFJP.SFLA_Dy/2.; 
  dz = SFJP.SFLC_Dz/2;
  TGeoVolume *SFLC = gGeoManager->MakeBox("SFLC",GetMed("SISD_MYLAR"),dx,dy,dz);
  SFLC->SetTitle("the part of the long bus cable on the connection board up to the connector");

  dx = SFJP.SFEB_Dx/2.; 
  dy = SFJP.SFLA_Dy/2.; 
  dz = SFJP.SFEB_Dz/2.;
  TGeoVolume *SFEB = gGeoManager->MakeBox("SFEB",GetMed("SISD_MYLAR"),dx,dy,dz);
  SFEB->SetTitle("the big bus elbow");

  dx = SFJP.SFES_Dx/2.; 
  dy = SFJP.SFLA_Dy/2.; 
  dz = SFJP.SFEB_Dz/2.;
  TGeoVolume *SFES = gGeoManager->MakeBox("SFES",GetMed("SISD_MYLAR"),dx,dy,dz);
  SFES->SetTitle("the small bus elbow");
  wafpckLen=SFPA.wpLen/SFPA.nssd;
  Int_t nSFLA = 0;
  Int_t nSFLC = 0;
  AlphaZ = 57.66;
  TGeoRotation *rotSFESp = new TGeoRotation("R057.66",90, AlphaZ,90,90+AlphaZ,0,0);
  TGeoRotation *rotSFESn = new TGeoRotation("R-57.66",90,-AlphaZ,90,90-AlphaZ,0,0);
  AlphaZ = 55.35;
  TGeoRotation *rotSFEBp = new TGeoRotation("R055.35",90, AlphaZ,90,90+AlphaZ,0,0);
  TGeoRotation *rotSFEBn = new TGeoRotation("R-55.35",90,-AlphaZ,90,90-AlphaZ,0,0);
  for (iwaf=1; iwaf <= 8; iwaf++) {
    for (jwaf=1; jwaf <= iwaf; jwaf++) {
      x = +SFJP.SFLA_Px+SFJP.FLEX_Di*jwaf;
      y =  SFJP.SFLA_Py+SFJP.FLEX_Di*jwaf; 
      z = (SFPA.wpLen+wafpckLen)/2-iwaf*wafpckLen; 
      // Top row the start
      SFSM->AddNode(SFLA,++nSFLA,new TGeoCombiTrans( x, y,-z,GetRot("054N")));
      SFSM->AddNode(SFLA,++nSFLA,new TGeoCombiTrans(-x, y, z,GetRot("054D")));
      // Bottom row the start
      SFSM->AddNode(SFLA,++nSFLA,new TGeoCombiTrans( x,-y,-z,GetRot("054N")));
      SFSM->AddNode(SFLA,++nSFLA,new TGeoCombiTrans(-x,-y, z,GetRot("054D")));
      
      x = +SFJP.SFLA_Px+SFJP.FLEX_Di*jwaf;
      y =  SFJP.SFLA_Py+SFJP.FLEX_Di*jwaf;
      z = -(SFPA.wpLen+wafpckLen)/2.+16*wafpckLen+wafpckLen/2.+SFJP.SFLB_Dz+8.5*SFJP.SFLC_Dz-iwaf*SFJP.SFLC_Dz;
      SFSM->AddNode(SFLC,++nSFLC,new TGeoCombiTrans( x, y, z,GetRot("054N")));
      SFSM->AddNode(SFLC,++nSFLC,new TGeoCombiTrans(-x, y,-z,GetRot("054D")));
      SFSM->AddNode(SFLC,++nSFLC,new TGeoCombiTrans( x,-y, z,GetRot("054N")));
      SFSM->AddNode(SFLC,++nSFLC,new TGeoCombiTrans(-x,-y,-z,GetRot("054D")));
    }
    for (jwaf = 1; jwaf <= 8; jwaf++) {      // Top row : Common part 8 Times on the active area*
      x = +SFJP.SFLA_Px+SFJP.FLEX_Di*jwaf;
      y =  SFJP.SFLA_Py+SFJP.FLEX_Di*jwaf; 
      z = -(SFPA.wpLen+wafpckLen)/2+(iwaf+8)*wafpckLen; 
      SFSM->AddNode(SFLA,++nSFLA,new TGeoCombiTrans( x,y, z,GetRot("054N")));
      SFSM->AddNode(SFLA,++nSFLA,new TGeoCombiTrans(-x,y,-z,GetRot("054D")));
    }
    // Top row : Common part of the long bus on the connection baord
    x = +SFJP.SFLA_Px+SFJP.FLEX_Di*iwaf;
    y =  SFJP.SFLA_Py+SFJP.FLEX_Di*iwaf;
    z = -(SFPA.wpLen+wafpckLen)/2+16*wafpckLen+wafpckLen/2.+SFJP.SFLB_Dz/2.; 
    SFSM->AddNode(SFLB,2*iwaf-1,new TGeoCombiTrans( x,y, z,GetRot("054N")));
    SFSM->AddNode(SFLB,2*iwaf ,new TGeoCombiTrans(-x,y,-z,GetRot("054D")));
    // Small elbow bus
    x = -SFJP.SFES_Px;
    y = -SFJP.SFES_Py;
    z = -(SFPA.wpLen+wafpckLen)/2+(iwaf)*wafpckLen+wafpckLen/2.-SFJP.SFEB_Dz/2.-SFJP.SFES_Pz/2.; 
    SFSM->AddNode(SFES,2*iwaf-1,new TGeoCombiTrans( x, y, z,rotSFESp));
    SFSM->AddNode(SFES,2*iwaf  ,new TGeoCombiTrans(-x, y,-z,rotSFESn));
    // Big elbow bus
    x = -SFJP.SFEB_Px;
    y = -SFJP.SFEB_Py;
    z = -(SFPA.wpLen+wafpckLen)/2+(iwaf+8)*wafpckLen+wafpckLen/2.-SFJP.SFEB_Dz/2.-SFJP.SFES_Pz/2.; 
    SFSM->AddNode(SFEB,2*iwaf-1,new TGeoCombiTrans( x,y, z,rotSFEBp));
    SFSM->AddNode(SFEB,2*iwaf  ,new TGeoCombiTrans(-x,y,-z,rotSFEBn));
  }
  rmin = SFJP.SSST_Rmin; 
  rmax = SFJP.SSST_Rmax; 
  dz   = SFJP.SSST_Width/2.; 
  Double_t phi1 =  SFJP.AlphaZH; 
  Double_t phi2 = (180.-SFJP.AlphaZH);
  TGeoVolume *SSST = gGeoManager->MakeTubs("SSST",GetMed("SISD_ALUMINIUM"),rmin,rmax,dz,phi1,phi2);
  SSST->SetTitle("the top of the small sector");
  z = +SFJP.SFLU_Dz+SFJP.SFPBDz+SFJP.SSST_Width/2.+SFJP.SSST_Pz;
  SFMO->AddNode(SSST,1,new TGeoTranslation(0,0,-z));
  SFMO->AddNode(SSST,2,new TGeoTranslation(0,0, z));
  SFMO->AddNode(SSST,3,new TGeoCombiTrans( 0,0,-z,GetRot("R180")));
  SFMO->AddNode(SSST,4,new TGeoCombiTrans( 0,0, z,GetRot("R180")));

  rmin = SFJP.SSSS_Rmin; 
  rmax = SFJP.SSST_Rmin; 
  dz   = SFJP.SSSS_Width/2.; 
  phi1 =  SFJP.AlphaZH; 
  phi2 = (180.-SFJP.AlphaZH);
  TGeoVolume *SSSS = gGeoManager->MakeTubs("SSSS",GetMed("SISD_ALUMINIUM"),rmin,rmax,dz,phi1,phi2);
  SSSS->SetTitle("the side of the small sector");
  z = +SFJP.SFLU_Dz+SFJP.SFPBDz+SFJP.SSSS_Width/2.+SFJP.SSST_Pz;
  SFMO->AddNode(SSSS,1,new TGeoTranslation(0,0,-z));
  SFMO->AddNode(SSSS,2,new TGeoTranslation(0,0, z));
  SFMO->AddNode(SSSS,3,new TGeoCombiTrans( 0,0,-z,GetRot("R180")));
  SFMO->AddNode(SSSS,4,new TGeoCombiTrans( 0,0, z,GetRot("R180")));

  rmin = SFJP.SSST_Rmin; 
  rmax = SFJP.SSST_Rmax; 
  dz   = SFJP.SSST_Width/2.; 
  phi1 = -SFJP.AlphaZH; 
  phi2 = +SFJP.AlphaZH;
#if ssdConfig <= 8
  TGeoVolume *SSRT = gGeoManager->MakeTubs("SSRT",GetMed("SISD_ALUMINIUM"),rmin,rmax,dz,phi1,phi2);
#else
  TGeoVolume *SSRT = gGeoManager->MakeTubs("SSRT",GetMed("SISD_CARBON"),rmin,rmax,dz,phi1,phi2);
#endif
  SSRT->SetTitle("the top of the side rib");
  SFMO->AddNode(SSRT,1,new TGeoTranslation(0,0,-z));
  SFMO->AddNode(SSRT,2,new TGeoTranslation(0,0, z));
  SFMO->AddNode(SSRT,3,new TGeoCombiTrans( 0,0,-z,GetRot("R180")));
  SFMO->AddNode(SSRT,4,new TGeoCombiTrans( 0,0, z,GetRot("R180")));

  rmin =  SFJP.SSRS_Rmin; 
  rmax =  SFJP.SSST_Rmin; 
  dz   =  SFJP.SSSS_Width/2.; 
  phi1 = -SFJP.AlphaZH; 
  phi2 = +SFJP.AlphaZH;
  TGeoVolume *SSRS = gGeoManager->MakeTubs("SSRS",GetMed("SISD_ALUMINIUM"),rmin,rmax,dz,phi1,phi2);
  SSRS->SetTitle("the side of the small rib");
  z = +SFJP.SFLU_Dz+SFJP.SFPBDz+SFJP.SSSS_Width/2.+SFJP.SSST_Pz;
  SFMO->AddNode(SSRS,1,new TGeoTranslation(0,0,-z));
  SFMO->AddNode(SSRS,2,new TGeoTranslation(0,0, z));
  SFMO->AddNode(SSRS,3,new TGeoCombiTrans( 0,0,-z,GetRot("R180")));
  SFMO->AddNode(SSRS,4,new TGeoCombiTrans( 0,0,z,GetRot("R180")));

  dx = SFJP.SSLB_Dx/2.; 
  dy = SFJP.SSLB_Dy/2.; 
  dz = SFJP.SSLB_Dz/2.;
  TGeoVolume *SSLB = gGeoManager->MakeBox("SSLB",GetMed("SISD_AIR"),dx,dy,dz);
  SSLB->SetTitle("the linking (sector to the cone) box");
  SSLB->SetVisibility(0);
  SSLB->SetLineColor(kPink);
#if 0 
  if (SFJP.version == 1) {}
  //---- Run IV version. The piece is just a box rotated by 45dgr around z. With its  own positioning.
#endif
  // Run V version. The piece is made of two crosses. the largest arms are horizontal. 
  // The cross is centered to the linking tube axis (placed below)...
  Double_t X =  SFJP.SSLT_Px;
  Double_t Z = +SFJP.SFLU_Dz+SFJP.SFPBDz+SFJP.SSSS_Width+SFJP.SSST_Pz+SFJP.SSLB_Dz/2.;
  SFMO->AddNodeOverlap(SSLB,1,new TGeoTranslation( X, X,-Z));
  SFMO->AddNodeOverlap(SSLB,2,new TGeoTranslation(-X,-X,-Z));
  SFMO->AddNodeOverlap(SSLB,3,new TGeoTranslation( X,-X,-Z));
  SFMO->AddNodeOverlap(SSLB,4,new TGeoTranslation(-X, X,-Z));
  SFMO->AddNodeOverlap(SSLB,5,new TGeoCombiTrans(  X, X, Z,GetRot("P270")));
  SFMO->AddNodeOverlap(SSLB,6,new TGeoCombiTrans( -X,-X, Z,GetRot("P270")));
  SFMO->AddNodeOverlap(SSLB,7,new TGeoCombiTrans(  X,-X, Z,GetRot("P270")));
  SFMO->AddNodeOverlap(SSLB,8,new TGeoCombiTrans( -X, X, Z,GetRot("P270")));

  dx = SFJP.SSCR_Hl/2.; 
  dy = SFJP.SSCR_Wd/2.; 
  dz = SFJP.SSCR_ThD/2.;
  TGeoVolume *SBCH = gGeoManager->MakeBox("SBCH",GetMed("SISD_DELRIN"),dx,dy,dz);
  SBCH->SetTitle("the horizontal branch of the DELRIN cross");
  SBCH->SetLineColor(kRed);
  z=SFJP.SSLB_Dz/2.-SFJP.SSCR_ThD/2;
  SSLB->AddNode(SBCH,1,new TGeoTranslation(0,0,z));

  dx = SFJP.SSCR_Wd/2.; 
  dy = SFJP.SSCR_Vl/2.; 
  dz = SFJP.SSCR_ThD/2.;
  TGeoVolume *SBCV = gGeoManager->MakeBox("SBCV",GetMed("SISD_DELRIN"),dx,dy,dz);
  SBCV->SetTitle("the vertical branch of the DELRIN cross");
  SBCV->SetLineColor(kRed);
  y= (SFJP.SSCR_Wd+SFJP.SSCR_Vl)/2; z=SFJP.SSLB_Dz/2.-SFJP.SSCR_ThD/2;
  SSLB->AddNode(SBCV,1,new TGeoTranslation(0, y,z));
  SSLB->AddNode(SBCV,2,new TGeoTranslation(0,-y,z));

  dx = SFJP.SSCR_Hl/2.; 
  dy = SFJP.SSCR_Wd/2.; 
  dz = SFJP.SSCR_ThA/2;
  TGeoVolume *SBFH = gGeoManager->MakeBox("SBFH",GetMed("SISD_ALUMINIUM"),dx,dy,dz);
  SBFH->SetTitle("the horizontal branch of the Alumimium cross");
  z=SFJP.SSLB_Dz/2.-SFJP.SSCR_ThD-SFJP.SSCR_ThA/2.;
  SSLB->AddNode(SBFH,1,new TGeoTranslation(0,0,z));

  dx = SFJP.SSCR_Wd/2.; 
  dy = SFJP.SSCR_Vl/2.; 
  dz = SFJP.SSCR_ThA/2.;
  TGeoVolume *SBFV = gGeoManager->MakeBox("SBFV",GetMed("SISD_ALUMINIUM"),dx,dy,dz);
  SBFV->SetTitle("the vertical branch of the Alumimium cross");
  y= (SFJP.SSCR_Wd+SFJP.SSCR_Vl)/2; z=SFJP.SSLB_Dz/2.-SFJP.SSCR_ThD-SFJP.SSCR_ThA/2.;
  SSLB->AddNode(SBFV,1,new TGeoTranslation(0, y, z));
  SSLB->AddNode(SBFV,2,new TGeoTranslation(0,-y,-z));

  rmin = 0.; 
  rmax = SFJP.SSLT_Rmax; 
  dz   = SFJP.SSLT_Dz/2.;
  TGeoVolume *SSLT = gGeoManager->MakeTube("SSLT",GetMed("SISD_ALUMINIUM"),rmin,rmax,dz); 
  SSLT->SetTitle("the linking (sector to the cone) tube");
  X = SFJP.SSLT_Px;
  Z = SFJP.SFLU_Dz+SFJP.SFPBDz+SFJP.SSSS_Width+SFJP.SSST_Pz+SFJP.SSLB_Dz+SFJP.SSLT_Dz/2.;
  SFMO->AddNode(SSLT,1,new TGeoTranslation( X, X,-Z));
  SFMO->AddNode(SSLT,2,new TGeoTranslation(-X,-X,-Z));
  SFMO->AddNode(SSLT,3,new TGeoTranslation( X,-X,-Z));
  SFMO->AddNode(SSLT,4,new TGeoTranslation(-X, X,-Z));
  SFMO->AddNode(SSLT,5,new TGeoTranslation( X, X, Z));
  SFMO->AddNode(SSLT,6,new TGeoTranslation(-X,-X, Z));
  SFMO->AddNode(SSLT,7,new TGeoTranslation( X,-X, Z));
  SFMO->AddNode(SSLT,8,new TGeoTranslation(-X, X, Z));

  dx = SFJP.SCMP_Dx/2.; 
  dy = SFJP.SCMP_Dy/2.; 
  dz = SFJP.SCMP_Dz/2;
  TGeoVolume *SCMP = gGeoManager->MakeBox("SCMP",GetMed("SISD_ALUMINIUM"),dx,dy,dz);
  SCMP->SetTitle("the mounting plate inserted in the cones.");
  X =  SFJP.SCMP_Px; Z =  SFJP.SCVM_Pz;
  SFMO->AddNode(SCMP,1,new TGeoCombiTrans( X, X, Z,GetRot("045D")));
  SFMO->AddNode(SCMP,2,new TGeoCombiTrans(-X,-X, Z,GetRot("045D")));
  SFMO->AddNode(SCMP,3,new TGeoCombiTrans( X,-X, Z,GetRot("R045")));
  SFMO->AddNode(SCMP,4,new TGeoCombiTrans(-X, X, Z,GetRot("R045")));
  SFMO->AddNode(SCMP,5,new TGeoCombiTrans( X, X,-Z,GetRot("045D")));
  SFMO->AddNode(SCMP,6,new TGeoCombiTrans(-X,-X,-Z,GetRot("045D")));
  SFMO->AddNode(SCMP,7,new TGeoCombiTrans( X,-X,-Z,GetRot("R045")));
  SFMO->AddNode(SCMP,8,new TGeoCombiTrans(-X, X,-Z,GetRot("R045")));

  dx = SFJP.SCVM_Dx/2.; 
  dy = SFJP.SCVM_Dy/2.; 
  dz = SFJP.SCVM_Dz/2;
  TGeoVolume *SCVM = gGeoManager->MakeBox("SCVM",GetMed("SISD_AIR"),dx,dy,dz);
  SCVM->SetTitle("the mother volume of the V-shape piece");
  SCVM->SetVisibility(0);
  SCVM->SetLineColor(kPink);
  X =  SFJP.SCVM_Px; Z =  SFJP.SCVM_Pz;
  rot = new TGeoRotation("next",90,-135,90,-45,0,0);
  SFMO->AddNodeOverlap(SCVM,1,new TGeoCombiTrans( X, X,Z,GetRot("045D")));
  SFMO->AddNodeOverlap(SCVM,2,new TGeoCombiTrans(-X,-X,Z,GetRot("R135")));
  SFMO->AddNodeOverlap(SCVM,3,new TGeoCombiTrans( X,-X,Z,rot));
  SFMO->AddNodeOverlap(SCVM,4,new TGeoCombiTrans(-X, X,Z,GetRot("R045")));
  SFMO->AddNodeOverlap(SCVM,5,new TGeoCombiTrans( X, X,-Z,GetRot("045D")));
  SFMO->AddNodeOverlap(SCVM,6,new TGeoCombiTrans(-X,-X,-Z,GetRot("R135")));
  SFMO->AddNodeOverlap(SCVM,7,new TGeoCombiTrans( X,-X,-Z,rot));
  SFMO->AddNodeOverlap(SCVM,8,new TGeoCombiTrans(-X, X,-Z,GetRot("R045")));

  dx = SFJP.SCVM_Dx/2.; 
  dy = SFJP.SCVB_Dy/2.; 
  dz = SFJP.SCVM_Dz/2;
  TGeoVolume *SCVB = gGeoManager->MakeBox("SCVB",GetMed("SISD_ALUMINIUM"),dx,dy,dz);
  SCVB->SetTitle("the base plate of the V-shape piece");
  y = -SFJP.SCVM_Dy/2.+SFJP.SCVB_Dy/2.; 
  SCVM->AddNode(SCVB,1,new TGeoTranslation(0,y,0));

  dx = SFJP.SCVS_Dx/2.;
  dy = SFJP.SCVS_Dy/2.; 
  dz = SFJP.SCVM_Dz/2;
  TGeoVolume *SCVS = gGeoManager->MakeBox("SCVS",GetMed("SISD_ALUMINIUM"),dx,dy,dz);
  SCVS->SetTitle("the side plate of the V-shape piece");
  x =  SFJP.SCVS_Px; y = SFJP.SCVS_Py;
  SCVM->AddNode(SCVS,1,new TGeoCombiTrans( x,y,0,GetRot("R045")));
  SCVM->AddNode(SCVS,2,new TGeoCombiTrans(-x,y,0,GetRot("045D")));

  
  return SFMO;
}
#endif

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_srs_Maker class for Makers                                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "St_srs_Maker.h"
#include "StChain.h"
#include "St_DataSet.h"
#include "svt/St_svg_am_Module.h"
#include "svt/St_srs_am_Module.h"
ClassImp(St_srs_Maker)

//_____________________________________________________________________________
St_srs_Maker::St_srs_Maker(){
   drawinit=kFALSE;
}
//_____________________________________________________________________________
St_srs_Maker::St_srs_Maker(const char *name, const char *title):StMaker(name,title){
   drawinit=kFALSE;
}
//_____________________________________________________________________________
St_srs_Maker::~St_srs_Maker(){
 if (m_DataSet) delete m_DataSet;
 m_DataSet = 0;
}
//_____________________________________________________________________________
void St_srs_Maker::Clear(Option_t *option){
  if (m_DataSet) {delete m_DataSet; m_DataSet = 0;}
}

//_____________________________________________________________________________
void St_srs_Maker::Finish(){ 
 Clear();
}
//_____________________________________________________________________________
void St_srs_Maker::Init(){
// Create tables
   St_DataSetIter       local(gStChain->GetParams());
// geometry parameters
   St_DataSet *svt = local("svt");
   if (!svt) {// Make svt parameter data set
     local.Mkdir("svt");
     svt = local("svt");
     St_DataSetIter svt_Iter(svt);
// Shape configuration
     svg_shape_st shape = { 1, // long id; /* Type of SDD shape */
                            3, // long n_shape; /* number of shape parameters */
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // float active[10]; /* active area of sdd */
                            3., 3.0, 0.0150, 0, 0, 0, 0, 0, 0, 0}; // float shape[10]; /* custom shape parameters of SDD wafer */
     m_shape  = new St_svg_shape("shape",1);
     m_shape->AddAt((ULong_t *)&shape,0);   svt_Iter.Add(m_shape);
//
// general configuration
//
//message "initalizing config table..."

     svg_config_st config = {
       3,                                   // long cap_nshape; /* number of shape parameters */
       1,                                   // long cap_shape_id; /* GEANT shape id for end caps */
       1,                                   // long cap_trackmed; /* tracking medium for end caps */
       10,                                  // long frame_trackmed; /* tracking medium for frame */
       1, 1, 1, 1, 1, 1, 0, 0, 0, 0,        // long layer_drift[10]; /* drift direction indicator 3=Z=beam axis */
       4, 4, 6, 6, 8, 8, 0, 0, 0, 0,        // long n_ladder[10]; /* Number of ladders in layer(x)  x-->1-5 */
       6,                                   // long n_layer; /* SVT layer number */
       4, 4, 6, 6, 7, 7, 0, 0, 0, 0,        // long n_wafer[10]; /* Number of wafers on ladder for layer(x) */
       0.015, 0.015, 0.015, 0.015, 0.015, 0.015, 0, 0, 0, 0, //	float be_support_thick[10]; /* berylium support */
       0.3, 0.3, 0.3, 0.3, 0.3, 0.3,0, 0, 0, 0, //	float be_support_width[10]; /* berylium support */
       29.5,                                //  float cap_position; /* distance of svt end cap from origin */
	5., 16., 1., 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,// float cap_shape[20]; /* shape parameter values */
	29., 29., 29., 29., 29., 29., 0, 0, 0, 0, //	float frame_al_length[10]; /* frame */
	0.013, 0.013, 0.013, 0.013, 0.013, 0.013, 0, 0, 0, 0, //	float frame_al_thick[10]; /* frame */
	1., 1., 1., 1., 1., 1., 0, 0, 0, 0, //	float frame_al_width[10]; /* frame */
	29., 29., 29., 29., 29., 29., 0, 0, 0, 0, //	float frame_cs_length[10]; /* frame */
	0.075, 0.075, 0.075, 0.075, 0.075, 0.075, 0, 0, 0, 0, //	float frame_cs_thick[10]; /* fram */
	1., 1., 1., 1., 1., 1., 0, 0, 0, 0, //	float frame_cs_width[10]; /* fram */
	29., 29., 29., 29., 29., 29., 0, 0, 0, 0, //	float frame_length[10]; /* length of frame */
	29., 29., 29., 29., 29., 29., 0, 0, 0, 0, //	float frame_si_length[10]; /* frame */
	0.015, 0.015, 0.015, 0.015, 0.015, 0.015, 0, 0, 0, 0, //	float frame_si_thick[10]; /* frame */
	.2, .2, .2, .2, .2, .2, 0, 0, 0, 0, //	float frame_si_width[10]; /* frame */
	0.103, 0.103, 0.103, 0.103, 0.103, 0.103, 0, 0, 0, 0, //	float frame_thick[10]; /* frame thickness */
	1., 1., 1., 1., 1., 1., 0, 0, 0, 0, //	float frame_width[10]; /* frame width */
	0.0075, 0.0075, 0.0075, 0.0075, 0.0075, 0.0075, 0, 0, 0, 0, //	float layer_back[10]; /* layer back */
	0., 0., 0., 0., 0., 0., 0, 0, 0, 0, //	float layer_lateral[10]; /* lateral offset for ladder */
	0., 0., 0., 0., 0., 0., 0, 0, 0, 0, //	float layer_longi[10]; /* longitudinal offset for ladder */
	0., 45.0, 0.0, 30.0, 0.0, 22.5,  0, 0, 0, 0, //	float layer_phioff[10]; /* phi offset for 1st ladder of layer */
	0., 0., 0., 0., 0., 0., 0, 0, 0, 0, //	float layer_radial[10]; /* radial offset (wafer staggering) */
	6.125, 7.185, 10.185, 11.075, 13.995, 14.935,  0, 0, 0, 0, //	float layer_radius[10]; /* radius of layers */
	1, 1, 1, 1, 1, 1, 0, 0, 0, 0, //	float layer_shape[10]; /* shape id for layer */
	5.0, 16.0, 30.0,  //	float size[3]; /* TUBE inner,outer radius and half lenght */
	0.05, 0.05, 0.05, 0.05, 0.05, 0.05,  0, 0, 0, 0, //	float water_channel_thick[10]; /* water channel */
	.95, .95, .95, .95, .95, .95,  0, 0, 0, 0, //	float water_channel_width[10]; /* water channel */
	0., 0., 0.}; //	float x[3]; /* position of the SVT in STAR */
     m_config  = new St_svg_config("config",1);
     m_config->AddAt((ULong_t *)&config,0);   svt_Iter.Add(m_config);
     m_geom   = new St_svg_geom("geom",216);   svt_Iter.Add(m_geom);
     Int_t res = svg_am(m_config,m_shape,m_geom);
// Rectangle parameters: 
//       1=width of standoff at anodes
//       2=width of standoff next to guard region + mini-guard width
     srs_activea_st srs_activea0 = { // active area shape parameter
         1,                                       //	long id; 
	 0., .135, 0., 0., 0., 0., 0., 0., 0., 0};//	float param[10]; 
     m_srs_activea  = new St_srs_activea("srs_activea",3);
     m_srs_activea->AddAt((ULong_t *)&srs_activea0,0);   svt_Iter.Add(m_srs_activea);
// Butterfly parameters: 
//       1=width of standoff at anodes
//       2=width of standoff next to guard region
//       3=maximum transverse width of guard region
//       These numbers are from Gintas V circa Feb. 1994
       srs_activea_st srs_activea1 = { // active area shape parameter
         2, //	long id; 
	 .3475, .125, .34, 0, 0, 0, 0, 0, 0, 0};//	float param[10]; 
     m_srs_activea->AddAt((ULong_t *)&srs_activea1,1);
// Trapezoidal parameters: [like STAR 1] 
//       1=width of standoff at anodes
//       2=width of standoff next to guard region
//       3=maximum transverse width of guard region
//       4=width of guard region in drift direction
       srs_activea_st srs_activea2 = { // active area shape parameter
         3, //	long id; 
	 .3475, .125, .68, .68, 0, 0, 0, 0, 0, 0};//	float param[10]; 
     m_srs_activea->AddAt((ULong_t *)&srs_activea2,2);
//*************************************************************************
//**************************************************************************
// This section chooses the method (only 2 and 3 are supported now),
// initializes the random seed, chooses the shape of the active area,
// and fixes the resolution
//message 'initializing the srs parameter table...'
//           Method=1 (default) uses Ken Wilson's svt_resolution_sim,           
//                    the full blown resolution simulator                       
//           Method=2 uses Claude Pruneau's spt_direct routine to make          
//                    a direct copy from hits to space points (no smearing)     
//           Method=3 uses Claude Pruneau's spt_direct_gs routine which         
//                    smears the hits with a fixed sigma which is taken from    
//                    the table direct(1).sd (drift direction)                  
//                                           .st (transverse direction)         
  srs_srspar_st srs_srspar = {
        1, 1, 1, 1, 1, 1, 0, 0, 0, 0, //long id_active[10]; /* id for active area on each layer */
	111111,                    // long init_ran; /* if not 0 init seed with this each event */
	0,                         //long merge; /* merge=1 when merging enabled */
	3,                         // long method; /* how to copy hits to spt table */
	0,                         //long nsca; /* number of sca channels */
	0,                         //float bucket; /* space extent of time bucket */
	0,                         //float enc; /* equivalent noise charge per bucket */
	0,                         //float fsca; /* sca frequency */
	0,                         //float pitch; /* distance between anodes (cm) */
	0,                         //float shaper; /* gaussian responce of electronics (s) */
	0.,                        //float thickness; /* SVT wafer thickness */
	0};                        //float vd; /* drift velocity */
     m_srs_srspar  = new St_srs_srspar("srs_srspar",1);
     m_srs_srspar->AddAt((ULong_t *)&srs_srspar,0);   svt_Iter.Add(m_srs_srspar);
     // 
     srs_direct_st srs_direct = { // initializing the srs direct table...
       0.020, // float sd; /* sigma in drift direction */
       0.020}; // float st; /* sigma transverse direction */
     m_srs_direct  = new St_srs_direct("srs_direct",1);
     m_srs_direct->AddAt((ULong_t *)&srs_direct,0);   svt_Iter.Add(m_srs_direct);
   }
// Create Histograms    
   StMaker::Init();
}
//_____________________________________________________________________________
Int_t St_srs_Maker::Make(){
// Create output tables
   if (!m_DataSet->GetList())  {
     St_DataSetIter local(m_DataSet);
     local.Mkdir("hits"); local.Cd("hits");
     St_scs_spt    *scs_spt    = new St_scs_spt("scs_spt",20000); local.Add(scs_spt);
     St_srs_result *srs_result = new St_srs_result("srs_result",20000); local.Add(srs_result);
     St_DataSetIter geant(gStChain->GetGeant());
     St_DataSetIter g2t(geant("Event"));
     St_g2t_svt_hit *g2t_svt_hit = (St_g2t_svt_hit *) geant("Event/g2t_svt_hit");
     Int_t res =  srs_am (srs_result,g2t_svt_hit,scs_spt,
                          m_geom,m_config,m_shape,m_srs_srspar,m_srs_direct,m_srs_activea);
   }
     m_DataSet->ls("*");
  //  PrintInfo();
 return kSTAFCV_OK;
}
//_____________________________________________________________________________
void St_srs_Maker::PrintInfo(){
  printf("**************************************************************\n");
  printf("* $Id: St_srs_Maker.cxx,v 1.4 1998/07/21 00:36:45 fisyak Exp $    *\n");
  //  printf("* %s    *\n",m_VersionCVS);
  printf("**************************************************************\n");
  if (gStChain->Debug()) StMaker::PrintInfo();
}


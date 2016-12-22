// $Id: Material.h,v 1.11 2009/01/26 17:00:30 fisyak Exp $
// $Log: Material.h,v $
// Revision 1.11  2009/01/26 17:00:30  fisyak
// CAVE_CONCRETE & CAVE_STANDARD => Concrete
//
// Revision 1.10  2009/01/21 16:22:37  fisyak
// Split SSD ladders into sectors
//
// Revision 1.9  2009/01/20 21:31:25  fisyak
// Decodessd geometry from mortran to Cint
//
// Revision 1.8  2009/01/14 16:31:52  fisyak
// Freeze conversion from mortran to Cint for SVT
//
// Revision 1.7  2008/12/01 21:51:27  fisyak
// Add Cables to Tpc, add more indexes
//
// Revision 1.6  2008/11/17 14:16:04  fisyak
// Add Al + Water mixture
//
// Revision 1.5  2008/10/20 15:44:17  fisyak
// Add TPCE_Adhesive
//
// Revision 1.4  2008/09/03 20:44:47  fisyak
// replace tpc geometry with translate one from mortran, clean ups
//
// Revision 1.3  2008/08/27 21:48:11  fisyak
//
// the STAR materials
#ifndef __Material__
#define __Material__
#ifndef __CINT__
#include "TGeoMaterial.h"
#endif
void Material() {

TGeoMixture *mix = 0;
Int_t imat = 0;
(new TGeoMaterial("VACUUM"),    1.E-16,1.E-16,        1.E-16)->SetUniqueID(++imat);
(new TGeoMaterial("HYDROGEN",     1.01,     1,0.7080000E-01))->SetUniqueID(++imat);
(new TGeoMaterial("DEUTERIUM",    2.01, 1,0.162))->SetUniqueID(++imat);
(new TGeoMaterial("HELIUM",       4   , 2,0.125))->SetUniqueID(++imat);
(new TGeoMaterial("LITHIUM",      6.94, 3,0.534))->SetUniqueID(++imat);
(new TGeoMaterial("BERILLIUM",    9.01, 4,1.848))->SetUniqueID(++imat);
(new TGeoMaterial("NITROGEN_GAS",14.01, 7,0.1164676E-02))->SetUniqueID(++imat);
(new TGeoMaterial("OXIGEN_GAS",  16   , 8,0.1332389E-02))->SetUniqueID(++imat);
(new TGeoMaterial("SILICON",     28.09,14,2.33))->SetUniqueID(++imat);
(new TGeoMaterial("ARGON_GAS",   39.95,18,0.1658498E-02))->SetUniqueID(++imat);
(new TGeoMaterial("LIQUID_ARGON",39.95,18,1.4))->SetUniqueID(++imat);
(new TGeoMaterial("CARBON",    12.01, 6,2.265))->SetUniqueID(++imat);
(new TGeoMaterial("NITROGEN",  14.01, 7,0.808))->SetUniqueID(++imat);
(new TGeoMaterial("NEON",      20.18,10,1.207))->SetUniqueID(++imat);
(new TGeoMaterial("ALUMINIUM", 26.98,13,2.7))->SetUniqueID(++imat);
(new TGeoMaterial("IRON",      55.85,26,7.87))->SetUniqueID(++imat);
(new TGeoMaterial("COPPER",    63.54,29,8.96))->SetUniqueID(++imat);
(new TGeoMaterial("TUNGSTEN", 183.85,74,19.3))->SetUniqueID(++imat);
(new TGeoMaterial("LEAD",     207.19,82,11.35))->SetUniqueID(++imat);
(new TGeoMaterial("URANIUM",  238.03,92,18.95))->SetUniqueID(++imat);
//(new TGeoMaterial("AIR",14.61,7.3,0.1205000E-02))->SetUniqueID(++imat);
 mix = new TGeoMixture("AIR",4, 1.214e-3);mix->SetUniqueID(++imat); //  Huhtinen, Air 18 degr.C and 58% humidity
 mix->DefineElement(0,14.010,  7, 0.7494); // "Nitrogen"         74.94            #. Weight fraction
 mix->DefineElement(1,15.999,  8, 0.2369); // "Oxygen"           23.69            #. Weight fraction
 mix->DefineElement(2,39.948, 18, 0.0129); // "Argon"             1.29            #. Weight fraction
 mix->DefineElement(3,1.00794, 1, 0.0008); // "Hydrogen"          0.08            #. Weight fraction
mix = new TGeoMixture("POLYSTYREN",2,   1.03200    );mix->SetUniqueID(++imat);
    mix->DefineElement(0,12,6,0.9230769);
    mix->DefineElement(1,1,1,0.7692308E-01);
mix = new TGeoMixture("POLYETHYLENE",2,  0.930000    );mix->SetUniqueID(++imat);
    mix->DefineElement(0,12,6,0.8571429);
    mix->DefineElement(1,1,1,0.1428571);
mix = new TGeoMixture("MYLAR",3,   1.39000    );mix->SetUniqueID(++imat);
    mix->DefineElement(0,12,6,0.625);
    mix->DefineElement(1,1,1,0.4166667E-01);
    mix->DefineElement(2,16,8,0.3333333);
mix = new TGeoMixture("Concrete",2,   2.50000    );mix->SetUniqueID(++imat);
    mix->DefineElement(0,28.08,14,0.4673768);
    mix->DefineElement(1,16,8,0.5326232);
(new TGeoMaterial("PIPE",26.98,13,2.7))->SetUniqueID(++imat);
mix = new TGeoMixture("PIPE_MLI",4,   1.82667    );mix->SetUniqueID(++imat);
    mix->DefineElement(0,12,6,0.3170606);
    mix->DefineElement(1,1,1,0.2113737E-01);
    mix->DefineElement(2,16,8,0.169099);
    mix->DefineElement(3,27,13,0.4927031);
mix = new TGeoMixture("PIPE_ALKAP",4,   1.43200    );mix->SetUniqueID(++imat);
    mix->DefineElement(0,12,6,0.5869957);
    mix->DefineElement(1,1,1,0.3913305E-01);
    mix->DefineElement(2,16,8,0.3130644);
    mix->DefineElement(3,27,13,0.6080688E-01);
mix = new TGeoMixture("SVTT_G10",5,   1.70000    );mix->SetUniqueID(++imat);
    mix->DefineElement(0,28.08,14,0.28);
    mix->DefineElement(1,16,8,0.32);
    mix->DefineElement(2,12,6,0.2206897);
    mix->DefineElement(3,1,1,0.3218391E-01);
    mix->DefineElement(4,16,8,0.1471264);
mix = new TGeoMixture("SVTT_ALKAP",4,   1.43200    );mix->SetUniqueID(++imat);
    mix->DefineElement(0,12,6,0.5869957);
    mix->DefineElement(1,1,1,0.3913305E-01);
    mix->DefineElement(2,16,8,0.3130644);
    mix->DefineElement(3,27,13,0.6080688E-01);
mix = new TGeoMixture("SVTT_SSDALMY",4,   1.40845    );mix->SetUniqueID(++imat);
    mix->DefineElement(0,12,6,0.6081356);
    mix->DefineElement(1,1,1,0.4054238E-01);
    mix->DefineElement(2,16,8,0.324339);
    mix->DefineElement(3,27,13,0.2698298E-01);
mix = new TGeoMixture("SVTT_WATER",2,   1.00000    );mix->SetUniqueID(++imat);
    mix->DefineElement(0,1,1,0.1111111);
    mix->DefineElement(1,16,8,0.8888889);
mix = new TGeoMixture("SVTT_CH2",2,  0.935000    );mix->SetUniqueID(++imat);
    mix->DefineElement(0,12,6,0.8571429);
    mix->DefineElement(1,1,1,0.1428571);
(new TGeoMaterial("SVTT_PYREX",20.719,10.307,2.23))->SetUniqueID(++imat);
(new TGeoMaterial("SVTT_SENSITIVE",28.09,14,2.33))->SetUniqueID(++imat);
//(new TGeoMaterial("SVTT_SENSSI"   ,28.09,14,2.33))->SetUniqueID(++imat);
(new TGeoMaterial("SVTT_NONSENSSILI",28.09,14,2.33))->SetUniqueID(++imat);
(new TGeoMaterial("SVTT_NONSENSSI",28.09,14,2.33))->SetUniqueID(++imat);
(new TGeoMaterial("SVTT_NONSENSSIL",28.09,14,2.33))->SetUniqueID(++imat);
mix = new TGeoMixture("SVTT_ROHA",2,  0.304000E-01);mix->SetUniqueID(++imat);
    mix->DefineElement(0,12,6,0.8571429);
    mix->DefineElement(1,1,1,0.1428571);
mix = new TGeoMixture("SVTT_BEO",2,   2.85000    );mix->SetUniqueID(++imat);
    mix->DefineElement(0,9,4,0.36);
    mix->DefineElement(1,16,8,0.64);
mix = new TGeoMixture("SVTT_GLASS",2,   2.20000    );mix->SetUniqueID(++imat);
    mix->DefineElement(0,28,14,0.4666667);
    mix->DefineElement(1,16,8,0.5333334);
mix = new TGeoMixture("SVTT_AGPD",2,   11.2500    );mix->SetUniqueID(++imat);
    mix->DefineElement(0,108,47,0.5046729);
    mix->DefineElement(1,106,46,0.4953271);
mix = new TGeoMixture("SISD_WATER",2,   1.00000    );mix->SetUniqueID(++imat);
    mix->DefineElement(0,1,1,0.1111111);
    mix->DefineElement(1,16,8,0.8888889);
mix = new TGeoMixture("SISD_G10",5,   1.70000    );mix->SetUniqueID(++imat);
    mix->DefineElement(0,28.08,14,0.28);
    mix->DefineElement(1,16,8,0.32);
    mix->DefineElement(2,12,6,0.2206897);
    mix->DefineElement(3,1,1,0.3218391E-01);
    mix->DefineElement(4,16,8,0.1471264);
mix = new TGeoMixture("SISD_G5",5,  0.850000    );mix->SetUniqueID(++imat);
    mix->DefineElement(0,28.08,14,0.28);
    mix->DefineElement(1,16,8,0.32);
    mix->DefineElement(2,12,6,0.2206897);
    mix->DefineElement(3,1,1,0.3218391E-01);
    mix->DefineElement(4,16,8,0.1471264);
mix = new TGeoMixture("SISD_DELRIN",3,   1.20000    );mix->SetUniqueID(++imat);
    mix->DefineElement(0,16,8,0.32);
    mix->DefineElement(1,12,6,0.6);
    mix->DefineElement(2,1,1,0.8000000E-01);
mix = new TGeoMixture("TPCE_P10",3,  0.154053E-02);mix->SetUniqueID(++imat);
    mix->DefineElement(0,40,18,0.9574468);
    mix->DefineElement(1,12,6,0.3191489E-01);
    mix->DefineElement(2,1,1,0.1063830E-01);
(new TGeoMaterial("TPCE_SENSITIVE_GAS",38.69149,17.43617,0.1540534E-02))->SetUniqueID(++imat);
 mix = new TGeoMixture("TPCE_Adhesive",3,   1.2);mix->SetUniqueID(++imat); // Mylar with density = 1.3 g/cm**3
    mix->DefineElement(0,12,6,0.625);
    mix->DefineElement(1,1,1,0.4166667E-01);
    mix->DefineElement(2,16,8,0.3333333);
mix = new TGeoMixture("TPCE_Water_Pipe",3,  2.32155);mix->SetUniqueID(++imat);
    mix->DefineElement(0,    1, 1,1.06546299984001044e-02);
    mix->DefineElement(1,   16, 8,8.52370399872008355e-02);
    mix->DefineElement(2,26.98,13,9.04108330014399053e-01);
mix = new TGeoMixture("TPCE_NOMEX",3,  0.064);mix->SetUniqueID(++imat);
    mix->DefineElement(0,12,6,0.6);
    mix->DefineElement(1,1,1,0.8000000E-01);
    mix->DefineElement(2,16,8,0.32);
mix = new TGeoMixture("TPCE_AL_HONEYCOMB",3,  0.282000    );mix->SetUniqueID(++imat);
    mix->DefineElement(0,27,13,0.1050000E-01);
    mix->DefineElement(1,14,7,0.7395);
    mix->DefineElement(2,9,4.5,0.25);
mix = new TGeoMixture("TPCE_G10",5,   1.70000    );mix->SetUniqueID(++imat);
    mix->DefineElement(0,28.08,14,0.28);
    mix->DefineElement(1,16,8,0.32);
    mix->DefineElement(2,12,6,0.2206897);
    mix->DefineElement(3,1,1,0.3218391E-01);
    mix->DefineElement(4,16,8,0.1471264);
mix = new TGeoMixture("FTPC_AR_CO2",3,  0.187900E-02);mix->SetUniqueID(++imat);
    mix->DefineElement(0,40,18,0.5);
    mix->DefineElement(1,12,6,0.1363636);
    mix->DefineElement(2,16,8,0.3636364);
(new TGeoMaterial("FTPC_PYREX",20.719,10.307,2.23))->SetUniqueID(++imat);
mix = new TGeoMixture("FTPC_G10",5,   1.70000    );mix->SetUniqueID(++imat);
    mix->DefineElement(0,28.08,14,0.28);
    mix->DefineElement(1,16,8,0.32);
    mix->DefineElement(2,12,6,0.2206897);
    mix->DefineElement(3,1,1,0.3218391E-01);
    mix->DefineElement(4,16,8,0.1471264);
mix = new TGeoMixture("SUPO_G10",5,   1.70000    );mix->SetUniqueID(++imat);
    mix->DefineElement(0,28.08,14,0.28);
    mix->DefineElement(1,16,8,0.32);
    mix->DefineElement(2,12,6,0.2206897);
    mix->DefineElement(3,1,1,0.3218391E-01);
    mix->DefineElement(4,16,8,0.1471264);
mix = new TGeoMixture("FTRO_G10",5,   1.70000    );mix->SetUniqueID(++imat);
    mix->DefineElement(0,28.08,14,0.28);
    mix->DefineElement(1,16,8,0.32);
    mix->DefineElement(2,12,6,0.2206897);
    mix->DefineElement(3,1,1,0.3218391E-01);
    mix->DefineElement(4,16,8,0.1471264);
mix = new TGeoMixture("BTOF_G10",5,   1.70000    );mix->SetUniqueID(++imat);
    mix->DefineElement(0,28.08,14,0.28);
    mix->DefineElement(1,16,8,0.32);
    mix->DefineElement(2,12,6,0.2206897);
    mix->DefineElement(3,1,1,0.3218391E-01);
    mix->DefineElement(4,16,8,0.1471264);
mix = new TGeoMixture("BTOF_RPCGAS",4,  0.455000E-02);mix->SetUniqueID(++imat);
    mix->DefineElement(0,1,1,0.2626775E-01);
    mix->DefineElement(1,12,6,0.253144);
    mix->DefineElement(2,19,9,0.7096294);
    mix->DefineElement(3,32,16,0.1095890E-01);
#if ! defined(y2008)
mix = new TGeoMixture("BTOF_HONEYCOMB",3,  0.282000    );mix->SetUniqueID(++imat);
#else /* y2008 */
mix = new TGeoMixture("BTOF_HONEYCOMB",3,  0.730000    );mix->SetUniqueID(++imat);
#endif /* !y2008 */
    mix->DefineElement(0,27,13,0.1050000E-01);
    mix->DefineElement(1,14,7,0.7395);
    mix->DefineElement(2,9,4.5,0.25);
mix = new TGeoMixture("BTOF_WATER",2,   1.00000    );mix->SetUniqueID(++imat);
    mix->DefineElement(0,1,1,0.1111111);
    mix->DefineElement(1,16,8,0.8888889);
#if ! defined(y2008)
mix = new TGeoMixture("BTOF_GLASS",2,   2.20000    );mix->SetUniqueID(++imat);
#else /* y2008 */
mix = new TGeoMixture("BTOF_GLASS",2,   2.50000    );mix->SetUniqueID(++imat);
#endif /* y2008 */
    mix->DefineElement(0,28,14,0.4666667);
    mix->DefineElement(1,16,8,0.5333334);
mix = new TGeoMixture("BTOF_LUCITE",3,   1.18000    );mix->SetUniqueID(++imat);
    mix->DefineElement(0,16,8,0.32);
    mix->DefineElement(1,12,6,0.6);
    mix->DefineElement(2,1,1,0.8000000E-01);
mix = new TGeoMixture("BTOF_LASTAFOAM",2,  0.480000E-01);mix->SetUniqueID(++imat);
    mix->DefineElement(0,12,6,0.8571429);
    mix->DefineElement(1,1,1,0.1428571);
mix = new TGeoMixture("VPDD_G10",5,   1.70000    );mix->SetUniqueID(++imat);
    mix->DefineElement(0,28.08,14,0.28);
    mix->DefineElement(1,16,8,0.32);
    mix->DefineElement(2,12,6,0.2206897);
    mix->DefineElement(3,1,1,0.3218391E-01);
    mix->DefineElement(4,16,8,0.1471264);
(new TGeoMaterial("CALB_EALUMINIUM",26.98,13,2.7))->SetUniqueID(++imat);
mix = new TGeoMixture("CALB_CELLULOSE",3,  0.350000    );mix->SetUniqueID(++imat);
    mix->DefineElement(0,12.01,6,0.2857143);
    mix->DefineElement(1,1,1,0.4761905);
    mix->DefineElement(2,16,8,0.2380952);
(new TGeoMaterial("CALB_CPOLYSTYREN",11.15385,5.615385,1.032))->SetUniqueID(++imat);
(new TGeoMaterial("CALB_CLEAD",207.19,82,11.35))->SetUniqueID(++imat);
mix = new TGeoMixture("CALB_G10",5,   1.70000    );mix->SetUniqueID(++imat);
    mix->DefineElement(0,28.08,14,0.28);
    mix->DefineElement(1,16,8,0.32);
    mix->DefineElement(2,12,6,0.2206897);
    mix->DefineElement(3,1,1,0.3218391E-01);
    mix->DefineElement(4,16,8,0.1471264);
(new TGeoMaterial("CALB_CALUMINIUM",26.98,13,2.7))->SetUniqueID(++imat);
mix = new TGeoMixture("CALB_SENS_GAS",3,  0.180150E-02);mix->SetUniqueID(++imat);
    mix->DefineElement(0,39.95,18,0.9);
    mix->DefineElement(1,12.01,6,0.2728926E-01);
    mix->DefineElement(2,16,8,0.7271075E-01);
mix = new TGeoMixture("RICH_QUARZ",2,   2.64000    );mix->SetUniqueID(++imat);
    mix->DefineElement(0,16,8,0.5325345);
    mix->DefineElement(1,28.09,14,0.4674655);
mix = new TGeoMixture("RICH_OPACO",2,   2.64000    );mix->SetUniqueID(++imat);
    mix->DefineElement(0,16,8,0.5325345);
    mix->DefineElement(1,28.09,14,0.4674655);
mix = new TGeoMixture("RICH_METHANE",2,  0.717000E-03);mix->SetUniqueID(++imat);
    mix->DefineElement(0,12.01,6,0.7501562);
    mix->DefineElement(1,1,1,0.2498439);
(new TGeoMaterial("RICH_CARBONIO",12.01,6,2.265))->SetUniqueID(++imat);
(new TGeoMaterial("RICH_ANODE",63.54,29,8.96))->SetUniqueID(++imat);
mix = new TGeoMixture("RICH_FREON",2,   1.70000    );mix->SetUniqueID(++imat);
    mix->DefineElement(0,12,6,0.2130177);
    mix->DefineElement(1,19,9,0.7869822);
(new TGeoMaterial("ECAL_CAIR",14.61,7.3,0.1205000E-02))->SetUniqueID(++imat);
(new TGeoMaterial("ECAL_CLEAD",207.19,82,11.35))->SetUniqueID(++imat);
(new TGeoMaterial("ECAL_CALUMINIUM",26.98,13,2.7))->SetUniqueID(++imat);
(new TGeoMaterial("ECAL_CPOLYSTYREN",11.15385,5.615385,1.032))->SetUniqueID(++imat);
(new TGeoMaterial("ECAL_AIR",55.85,26,7.87))->SetUniqueID(++imat);
mix = new TGeoMixture("ECAL_G10",5,   1.70000    );mix->SetUniqueID(++imat);
    mix->DefineElement(0,28.08,14,0.28);
    mix->DefineElement(1,16,8,0.32);
    mix->DefineElement(2,12,6,0.2206897);
    mix->DefineElement(3,1,1,0.3218391E-01);
    mix->DefineElement(4,16,8,0.1471264);
mix = new TGeoMixture("BBCM_ALKAP",4,   1.43200    );mix->SetUniqueID(++imat);
    mix->DefineElement(0,12,6,0.5869957);
    mix->DefineElement(1,1,1,0.3913305E-01);
    mix->DefineElement(2,16,8,0.3130644);
    mix->DefineElement(3,27,13,0.6080688E-01);
(new TGeoMaterial("BBCM_CPOLYSTYREN",11.15385,5.615385,1.032))->SetUniqueID(++imat);
(new TGeoMaterial("FPDM_CAIR",14.61,7.3,0.1205000E-02))->SetUniqueID(++imat);
(new TGeoMaterial("FPDM_CPOLYSTYREN",11.15385,5.615385,1.032))->SetUniqueID(++imat);
mix = new TGeoMixture("FPDM_G10",5,   1.70000    );mix->SetUniqueID(++imat);
    mix->DefineElement(0,28.08,14,0.28);
    mix->DefineElement(1,16,8,0.32);
    mix->DefineElement(2,12,6,0.2206897);
    mix->DefineElement(3,1,1,0.3218391E-01);
    mix->DefineElement(4,16,8,0.1471264);
(new TGeoMaterial("FPDM_CLEAD",207.19,82,11.35))->SetUniqueID(++imat);
(new TGeoMaterial("FPDM_CALUMINIUM",26.98,13,2.7))->SetUniqueID(++imat);
#if ! defined(y2006a) && ! defined(y2006b) && ! defined(y2006c) && ! defined(y2006g)  
mix = new TGeoMixture("FPDM_PBG",5,   3.86000    );mix->SetUniqueID(++imat);
#else
mix = new TGeoMixture("FPDM_PBG",5,   3.61000    );mix->SetUniqueID(++imat);
#endif
    mix->DefineElement(0,207.19,82,0.60712);
    mix->DefineElement(1,39.102,19,0.2324000E-01);
    mix->DefineElement(2,28.088,14,0.14771);
    mix->DefineElement(3,15.999,8,0.22041);
    mix->DefineElement(4,74.922,33,0.1520000E-02);
(new TGeoMaterial("ZCAL_DIRTY_LEAD",207.19,82,11.35))->SetUniqueID(++imat);
(new TGeoMaterial("ZCAL_SCINTILLATOR",11.15385,5.615385,1.032))->SetUniqueID(++imat);
(new TGeoMaterial("PIXL_SENSITIVE",28.09,14,2.33))->SetUniqueID(++imat);
mix = new TGeoMixture("ISTB_WATER",2,   1.00000    );mix->SetUniqueID(++imat);
    mix->DefineElement(0,1,1,0.1111111);
    mix->DefineElement(1,16,8,0.8888889);
mix = new TGeoMixture("ISTB_KAPTON",4,   1.42000    );mix->SetUniqueID(++imat);
    mix->DefineElement(0,12,6,0.7213115);
    mix->DefineElement(1,1,1,0.2732240E-01);
    mix->DefineElement(2,14,7,0.7650273E-01);
    mix->DefineElement(3,16,8,0.1748634);
#if !defined(upgr05) && !defined(upgr07) && !defined(upgr08) && !defined(upgr10) && !defined(upgr11) && !defined(upgr12) && !defined(upgr13) 
mix = new TGeoMixture("ISTB_CFOAM",2,  0.100000    );mix->SetUniqueID(++imat);
#else
mix = new TGeoMixture("ISTB_CFOAM",2,  0.240000    );mix->SetUniqueID(++imat);
#endif
    mix->DefineElement(0,12,6,0.5000000E-01);
    mix->DefineElement(1,14,7,0.95);
mix = new TGeoMixture("ISTB_EPOXY",3,   1.30000    );mix->SetUniqueID(++imat);
    mix->DefineElement(0,12,6,0.5517241);
    mix->DefineElement(1,1,1,0.8045977E-01);
    mix->DefineElement(2,16,8,0.3678161);
(new TGeoMaterial("ISTB_SENSITIVE",28.09,14,2.33))->SetUniqueID(++imat);
mix = new TGeoMixture("ISTB_ALN",2,   3.30000    );mix->SetUniqueID(++imat);
    mix->DefineElement(0,27,13,0.6585366);
    mix->DefineElement(1,14,7,0.3414634);
(new TGeoMaterial("FSTD_SENSITIVE",28.09,14,2.33))->SetUniqueID(++imat);
mix = new TGeoMixture("FSTD_ALN",2,   3.30000    );mix->SetUniqueID(++imat);
    mix->DefineElement(0,27,13,0.6585366);
    mix->DefineElement(1,14,7,0.3414634);
mix = new TGeoMixture("FSTD_WATER",2,   1.00000    );mix->SetUniqueID(++imat);
    mix->DefineElement(0,1,1,0.1111111);
    mix->DefineElement(1,16,8,0.8888889);
mix = new TGeoMixture("IGTD_ALKAP",4,   1.43200    );mix->SetUniqueID(++imat);
    mix->DefineElement(0,12,6,0.5869957);
    mix->DefineElement(1,1,1,0.3913305E-01);
    mix->DefineElement(2,16,8,0.3130644);
    mix->DefineElement(3,27,13,0.6080688E-01);
mix = new TGeoMixture("FGTD_ALKAP",4,   1.43200    );mix->SetUniqueID(++imat);
    mix->DefineElement(0,12,6,0.5869957);
    mix->DefineElement(1,1,1,0.3913305E-01);
    mix->DefineElement(2,16,8,0.3130644);
    mix->DefineElement(3,27,13,0.6080688E-01);
mix = new TGeoMixture("FGTD_G10",5,   1.70000    );mix->SetUniqueID(++imat);
    mix->DefineElement(0,28.08,14,0.28);
    mix->DefineElement(1,16,8,0.32);
    mix->DefineElement(2,12,6,0.2206897);
    mix->DefineElement(3,1,1,0.3218391E-01);
    mix->DefineElement(4,16,8,0.1471264);
mix = new TGeoMixture("FGTD_AR_MIX",3,  0.180150E-02);mix->SetUniqueID(++imat);
    mix->DefineElement(0,39.95,18,0.9);
    mix->DefineElement(1,12.01,6,0.2728926E-01);
    mix->DefineElement(2,16,8,0.7271075E-01);
(new TGeoMaterial("FGTD_SENSITIVE",37.44611,8,0.1801500E-02))->SetUniqueID(++imat);
mix = new TGeoMixture("IGTD_G10",5,   1.70000    );mix->SetUniqueID(++imat);
    mix->DefineElement(0,28.08,14,0.28);
    mix->DefineElement(1,16,8,0.32);
    mix->DefineElement(2,12,6,0.2206897);
    mix->DefineElement(3,1,1,0.3218391E-01);
    mix->DefineElement(4,16,8,0.1471264);
mix = new TGeoMixture("FPDM_F2",5,   3.61000    );mix->SetUniqueID(++imat);
    mix->DefineElement(0,207.19,82,0.41774);
    mix->DefineElement(1,39.102,19,0.4151000E-01);
    mix->DefineElement(2,28.088,14,0.21047);
    mix->DefineElement(3,15.999,8,0.2933);
    mix->DefineElement(4,22.99,11,0.3710000E-01);
mix = new TGeoMixture("MUTD_RPCGAS",4,  0.455000E-02);mix->SetUniqueID(++imat);
    mix->DefineElement(0,1,1,0.2626775E-01);
    mix->DefineElement(1,12,6,0.253144);
    mix->DefineElement(2,19,9,0.7096294);
    mix->DefineElement(3,32,16,0.1095890E-01);
mix = new TGeoMixture("IGTD_AR_MIX",3,  0.180150E-02);mix->SetUniqueID(++imat);
    mix->DefineElement(0,39.95,18,0.9);
    mix->DefineElement(1,12.01,6,0.2728926E-01);
    mix->DefineElement(2,16,8,0.7271075E-01);
(new TGeoMaterial("IGTD_SENSITIVE",37.44611,8,0.1801500E-02))->SetUniqueID(++imat);
(new TGeoMaterial("HPDT_SENSITIVE",28.09,14,2.33))->SetUniqueID(++imat);
mix = new TGeoMixture("ITSP_G10",5,   1.70000    );mix->SetUniqueID(++imat);
    mix->DefineElement(0,28.08,14,0.28);
    mix->DefineElement(1,16,8,0.32);
    mix->DefineElement(2,12,6,0.2206897);
    mix->DefineElement(3,1,1,0.3218391E-01);
    mix->DefineElement(4,16,8,0.1471264);
    mix = new TGeoMixture("ITSP_ALKAP",4,   1.43200    );mix->SetUniqueID(++imat);
    mix->DefineElement(0,12,6,0.5869957);
    mix->DefineElement(1,1,1,0.3913305E-01);
    mix->DefineElement(2,16,8,0.3130644);
    mix->DefineElement(3,27,13,0.6080688E-01);
mix = new TGeoMixture("ITSP_CH2",2,  0.935000    );mix->SetUniqueID(++imat);
    mix->DefineElement(0,12,6,0.8571429);
    mix->DefineElement(1,1,1,0.1428571);
mix = new TGeoMixture("PHMD_G10",5,   1.70000    );mix->SetUniqueID(++imat);
    mix->DefineElement(0,28.08,14,0.28);
    mix->DefineElement(1,16,8,0.32);
    mix->DefineElement(2,12,6,0.2206897);
    mix->DefineElement(3,1,1,0.3218391E-01);
    mix->DefineElement(4,16,8,0.1471264);
mix = new TGeoMixture("PHMD_AR_CO2",3,  0.184050E-02);mix->SetUniqueID(++imat);
    mix->DefineElement(0,40,18,0.7);
    mix->DefineElement(1,12,6,0.8181819E-01);
    mix->DefineElement(2,16,8,0.2181818);
(new TGeoMaterial("HPDT_SENSITIVE",28.09,14,2.33))->SetUniqueID(++imat);
 mix = new TGeoMixture("Cables",4,  2.68);mix->SetUniqueID(++imat);
 mix->DefineElement(0, 63.54,29, 0.586); // Copper
 mix->DefineElement(1, 12.01, 6, 0.259); // Carbon
 mix->DefineElement(2,15.999, 8, 0.138); // Oxigen
 mix->DefineElement(3,1.00794,1, 0.017); // Hydrogen
}
#endif /*  ! __Material__ */

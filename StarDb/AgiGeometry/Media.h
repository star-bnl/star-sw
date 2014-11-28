// $Id: Media.h,v 1.13 2009/02/25 20:23:19 fisyak Exp $
// $Log: Media.h,v $
// Revision 1.13  2009/02/25 20:23:19  fisyak
// Add the first version for misalignment
//
// Revision 1.12  2009/01/26 16:59:58  fisyak
// CAVE_CONCRETE & CAVE_STANDARD => Concrete
//
// Revision 1.11  2009/01/21 16:22:37  fisyak
// Split SSD ladders into sectors
//
// Revision 1.10  2009/01/20 21:31:25  fisyak
// Decodessd geometry from mortran to Cint
//
// Revision 1.9  2009/01/14 16:31:52  fisyak
// Freeze conversion from mortran to Cint for SVT
//
// Revision 1.8  2008/12/08 22:22:24  fisyak
// Cables is not sensitive
//
// Revision 1.7  2008/12/01 21:51:41  fisyak
// Add Cables to Tpc, add more indexes
//
// Revision 1.6  2008/11/17 14:16:04  fisyak
// Add Al + Water mixture
//
// Revision 1.5  2008/10/20 15:43:55  fisyak
// Add TPCE_Adhesive
//
// Revision 1.4  2008/09/03 20:44:47  fisyak
// replace tpc geometry with translate one from mortran, clean ups
//
// Revision 1.3  2008/08/27 21:48:11  fisyak
//
// STAR traking media
#ifndef __Media__
#define __Media__
#include "CreateGeometry.h"
#ifndef __CINT__
#include "TGeoMedium.h"
#endif
void Media() {
  Int_t imed = 0;
  //             name, numed,                        imat, isvol, ifield,fieldm, tmaxfd, stemax,   deemax,epsil, stmin
(new TGeoMedium("Air",0,GetMat("AIR")->GetUniqueID(),     0,      0,    20,     20,     10,0.2488534,1e-2,1.150551))->SetUniqueID(++imed);
(new TGeoMedium("STANDARD",0,GetMat("AIR")->GetUniqueID(),     0,      0,    20,     20,     10,0.2488534,1e-2,1.150551))->SetUniqueID(++imed);
(new TGeoMedium("ATLAS",0,GetMat("AIR")->GetUniqueID(),0,0,20,20,10,0.2488534,1e-2,1.150551))->SetUniqueID(++imed);
(new TGeoMedium("Concrete",0,GetMat("Concrete")->GetUniqueID(),0,1,20,20,10,0.1891153,1e-2,0.3120083E-01))->SetUniqueID(++imed);
(new TGeoMedium("CAVE_SOMETHING",0,GetMat("AIR")->GetUniqueID(),0,1,20,20,100,0.2488534,1e-2,1.150551))->SetUniqueID(++imed);
(new TGeoMedium("PIPE_STANDARD",0,GetMat("AIR")->GetUniqueID(),0,1,20,20,10,0.2488534,1e-2,1.150551))->SetUniqueID(++imed);
(new TGeoMedium("PIPE_BERILLIUM",0,GetMat("BERILLIUM")->GetUniqueID(),0,1,20,20,10,0.2163378,1e-2,0.2434062E-01))->SetUniqueID(++imed);
(new TGeoMedium("PIPE_PVACUUM",0,GetMat("VACUUM")->GetUniqueID(),0,1,20,20,10,0.2499964,1e-2,72.52007))->SetUniqueID(++imed);
(new TGeoMedium("PIPE",0,GetMat("PIPE")->GetUniqueID(),0,1,20,20,10,0.1829598,1e-2,0.3344225E-01))->SetUniqueID(++imed);
(new TGeoMedium("PIPE_IRON",0,GetMat("IRON")->GetUniqueID(),0,1,20,20,10,0.25,1e-2,0.2813468E-01))->SetUniqueID(++imed);
(new TGeoMedium("PIPE_AIR",0,GetMat("AIR")->GetUniqueID(),0,1,20,20,10,0.2488534,1e-2,1.150551))->SetUniqueID(++imed);
(new TGeoMedium("PIPE_MLI",0,GetMat("PIPE_MLI")->GetUniqueID(),0,1,20,20,10,0.2006452,1e-2,0.3385480E-01))->SetUniqueID(++imed);
(new TGeoMedium("PIPE_ALKAP",0,GetMat("PIPE_ALKAP")->GetUniqueID(),0,1,20,20,10,0.211247,1e-2,0.3151248E-01))->SetUniqueID(++imed);
(new TGeoMedium("UPST_STANDARD",0,GetMat("AIR")->GetUniqueID(),0,1,20,20,10,0.2488534,1e-2,1.150551))->SetUniqueID(++imed);
(new TGeoMedium("UPST_IRON",0,GetMat("IRON")->GetUniqueID(),0,1,20,20,10,0.25,1e-2,0.2813468E-01))->SetUniqueID(++imed);
(new TGeoMedium("UPST_VACUUM",0,GetMat("VACUUM")->GetUniqueID(),0,1,20,20,10,0,1e-2,0))->SetUniqueID(++imed);
(new TGeoMedium("SVTT_AIR",0,GetMat("AIR")->GetUniqueID(),0,1,20,20,10,0.2488534,1e-2,1.150551))->SetUniqueID(++imed);
(new TGeoMedium("SVTT_BERILLIUM",0,GetMat("BERILLIUM")->GetUniqueID(),0,1,20,20,10,0.2163378,1e-2,0.2434062E-01))->SetUniqueID(++imed);
(new TGeoMedium("SVTT_CARBON",0,GetMat("CARBON")->GetUniqueID(),0,1,20,20,10,0.2038734,1e-2,0.2484640E-01))->SetUniqueID(++imed);
(new TGeoMedium("SVTT_ALUMINIUM",0,GetMat("ALUMINIUM")->GetUniqueID(),0,1,20,20,10,0.1829598,1e-2,0.3344225E-01))->SetUniqueID(++imed);
(new TGeoMedium("SVTT_WATER",0,GetMat("SVTT_WATER")->GetUniqueID(),0,1,20,20,10,0.2165401,1e-2,0.3589959E-01))->SetUniqueID(++imed);
(new TGeoMedium("SVTT_G10",0,GetMat("SVTT_G10")->GetUniqueID(),0,1,20,20,10,0.2031303,1e-2,0.3338060E-01))->SetUniqueID(++imed);
(new TGeoMedium("SVTT_CH2",0,GetMat("SVTT_CH2")->GetUniqueID(),0,1,20,20,10,0.2208773,1e-2,0.3196166E-01))->SetUniqueID(++imed);
(new TGeoMedium("SVTT_CARBON",0,GetMat("CARBON")->GetUniqueID(),0,1,20,20,10,0.2038734,1e-2,0.2484640E-01))->SetUniqueID(++imed);
(new TGeoMedium("SVTT_COPPER",0,GetMat("COPPER")->GetUniqueID(),0,1,20,20,10,0.25,1e-2,0.2813861E-01))->SetUniqueID(++imed);
(new TGeoMedium("SVTT_WATER",0,GetMat("SVTT_WATER")->GetUniqueID(),0,1,20,20,10,0.2489962,1e-2,1.178267))->SetUniqueID(++imed);
(new TGeoMedium("SVTT_MYLAR",0,GetMat("MYLAR")->GetUniqueID(),0,1,20,20,10,0.2125565,1e-2,0.3113845E-01))->SetUniqueID(++imed);
(new TGeoMedium("SVTT_ALUMINIUM",0,GetMat("ALUMINIUM")->GetUniqueID(),0,1,20,20,10,0.1829598,1e-2,0.3344225E-01))->SetUniqueID(++imed);
(new TGeoMedium("SVTT_G10",0,GetMat("SVTT_G10")->GetUniqueID(),0,1,20,20,10,0.2031303,1e-2,0.3338060E-01))->SetUniqueID(++imed);
(new TGeoMedium("SVTT_PYREX",0,GetMat("SVTT_PYREX")->GetUniqueID(),0,1,20,20,10,0.1936564,1e-2,0.3243283E-01))->SetUniqueID(++imed);
(new TGeoMedium("SVTT_IRON",0,GetMat("IRON")->GetUniqueID(),0,1,20,20,10,0.25,1e-2,0.2813468E-01))->SetUniqueID(++imed);
(new TGeoMedium("SVTT_SENSITIVE",0,GetMat("SVTT_SENSITIVE")->GetUniqueID(),1,1,20,20,10,0.6537204E-01,1e-2,0.2992920E-01))->SetUniqueID(++imed);
//(new TGeoMedium("SVTT_SENSSI",0,GetMat("SVTT_SENSSI")->GetUniqueID(),1,1,20,20,10,0.6537204E-01,1e-2,0.2992920E-01))->SetUniqueID(++imed);
(new TGeoMedium("SVTT_NONSENSSILI",0,GetMat("SVTT_NONSENSSILI")->GetUniqueID(),0,1,20,20,10,0.184628,1e-2,0.3662629E-01))->SetUniqueID(++imed);
(new TGeoMedium("SVTT_NONSENSSI",0,GetMat("SVTT_NONSENSSI")->GetUniqueID(),0,1,20,20,10,0.184628,1e-2,0.3662629E-01))->SetUniqueID(++imed);
(new TGeoMedium("SVTT_NONSENSSIL",0,GetMat("SVTT_NONSENSSIL")->GetUniqueID(),0,1,20,20,10,0.184628,1e-2,0.3662629E-01))->SetUniqueID(++imed);
(new TGeoMedium("SVTT_ROHA",0,GetMat("SVTT_ROHA")->GetUniqueID(),0,1,20,20,10,0.2447488,1e-2,0.1727457))->SetUniqueID(++imed);
(new TGeoMedium("SVTT_BEO",0,GetMat("SVTT_BEO")->GetUniqueID(),0,1,20,20,10,0.1972404,1e-2,0.2366078E-01))->SetUniqueID(++imed);
(new TGeoMedium("SVTT_GLASS",0,GetMat("SVTT_GLASS")->GetUniqueID(),0,1,20,20,10,0.192847,1e-2,0.3317548E-01))->SetUniqueID(++imed);
(new TGeoMedium("SVTT_BEO",0,GetMat("SVTT_BEO")->GetUniqueID(),0,1,20,20,10,0.1972404,1e-2,0.2366078E-01))->SetUniqueID(++imed);
(new TGeoMedium("SVTT_AGPD",0,GetMat("SVTT_AGPD")->GetUniqueID(),0,1,20,20,10,0.25,1e-2,0.3180166E-01))->SetUniqueID(++imed);
(new TGeoMedium("SVTT_SILICON",0,GetMat("SILICON")->GetUniqueID(),0,1,20,20,10,0.184628,1e-2,0.3662629E-01))->SetUniqueID(++imed);
(new TGeoMedium("SVTT_ALKAP",0,GetMat("SVTT_ALKAP")->GetUniqueID(),0,1,20,20,10,0.211247,1e-2,0.3151248E-01))->SetUniqueID(++imed);
(new TGeoMedium("SVTT_SSDAMY",0,GetMat("TPCE_P10")->GetUniqueID(),0,1,20,20,10,0.2481738,1e-2,1.542206))->SetUniqueID(++imed);
(new TGeoMedium("SVTT_SSDALMY",0,GetMat("SVTT_SSDALMY")->GetUniqueID(),0,1,20,20,10,0.2119779,1e-2,0.3130699E-01))->SetUniqueID(++imed);
(new TGeoMedium("SISD_AIR",0,GetMat("AIR")->GetUniqueID(),0,1,20,20,10,0.2488534,1e-2,1.150551))->SetUniqueID(++imed);
(new TGeoMedium("SISD_SILICON",0,GetMat("SILICON")->GetUniqueID(),0,1,20,20,10,0.184628,1e-2,0.3662629E-01))->SetUniqueID(++imed);
(new TGeoMedium("SISD_SENSITIVE",0,GetMat("SILICON")->GetUniqueID(),1,1,20,20,10,0.6537204E-01,1e-2,0.2992920E-01))->SetUniqueID(++imed);
(new TGeoMedium("SISD_CARBON",0,GetMat("CARBON")->GetUniqueID(),0,1,20,20,10,0.2038734,1e-2,0.2484640E-01))->SetUniqueID(++imed);
(new TGeoMedium("SISD_WATER",0,GetMat("SISD_WATER")->GetUniqueID(),0,1,20,20,10,0.2165401,1e-2,0.3589959E-01))->SetUniqueID(++imed);
(new TGeoMedium("SISD_COPPER",0,GetMat("COPPER")->GetUniqueID(),0,1,20,20,10,0.25,1e-2,0.2813861E-01))->SetUniqueID(++imed);
(new TGeoMedium("SISD_ALUMINIUM",0,GetMat("ALUMINIUM")->GetUniqueID(),0,1,20,20,10,0.1829598,1e-2,0.3344225E-01))->SetUniqueID(++imed);
(new TGeoMedium("SISD_MYLAR",0,GetMat("MYLAR")->GetUniqueID(),0,1,20,20,10,0.2125565,1e-2,0.3113845E-01))->SetUniqueID(++imed);
(new TGeoMedium("SISD_G10",0,GetMat("SISD_G10")->GetUniqueID(),0,1,20,20,10,0.2031303,1e-2,0.3338060E-01))->SetUniqueID(++imed);
(new TGeoMedium("SISD_G5",0,GetMat("SISD_G5")->GetUniqueID(),0,1,20,20,10,0.2168581,1e-2,0.4673590E-01))->SetUniqueID(++imed);
(new TGeoMedium("SISD_DELRIN",0,GetMat("SISD_DELRIN")->GetUniqueID(),0,1,20,20,10,0.2154252,1e-2,0.3183565E-01))->SetUniqueID(++imed);

(new TGeoMedium("TPCE_STANDARD",0,GetMat("AIR")->GetUniqueID(),0,1,20,20,10,0.2488534,1e-2,1.150551))->SetUniqueID(++imed);
(new TGeoMedium("TPCE_P10",0,GetMat("TPCE_P10")->GetUniqueID(),0,1,20,20,10,0.2481738,1e-2,1.542206))->SetUniqueID(++imed);
(new TGeoMedium("TPCE_SENSITIVE_GAS",0,GetMat("TPCE_SENSITIVE_GAS")->GetUniqueID(),1,1,20,20,2.875,0.1826148E-02,1e-2,0.3598796E-01))->SetUniqueID(++imed);
(new TGeoMedium("TPCE_SENSITIVE_GAS",0,GetMat("TPCE_SENSITIVE_GAS")->GetUniqueID(),1,1,20,20,4.875,0.1826148E-02,1e-2,0.3598796E-01))->SetUniqueID(++imed);
(new TGeoMedium("TPCE_P10",0,GetMat("TPCE_P10")->GetUniqueID(),0,1,20,20,10,0.2482463,1e-2,1.605941))->SetUniqueID(++imed);
(new TGeoMedium("TPCE_SENSITIVE_GAS",0,GetMat("TPCE_SENSITIVE_GAS")->GetUniqueID(),1,1,20,20,2.875,0.1753672E-02,1e-2,0.3598795E-01))->SetUniqueID(++imed);
(new TGeoMedium("TPCE_SENSITIVE_GAS",0,GetMat("TPCE_SENSITIVE_GAS")->GetUniqueID(),1,1,20,20,4.875,0.1753672E-02,1e-2,0.3598795E-01))->SetUniqueID(++imed);
(new TGeoMedium("TPCE_MYLAR",0,GetMat("MYLAR")->GetUniqueID(),0,1,20,20,10,0.2125565,1e-2,0.3113845E-01))->SetUniqueID(++imed);
(new TGeoMedium("TPCE_Adhesive",0,GetMat("TPCE_Adhesive")->GetUniqueID(),0,1,20,20,10,0.2125565,1e-2,0.3113845E-01))->SetUniqueID(++imed);
(new TGeoMedium("TPCE_Water_Pipe",0,GetMat("TPCE_Water_Pipe")->GetUniqueID(),0,1,20,20,10,0.2125565,1e-2,0.3113845E-01))->SetUniqueID(++imed);
(new TGeoMedium("TPCE_NOMEX",0,GetMat("TPCE_NOMEX")->GetUniqueID(),0,1,20,20,10,0.243085,1e-2,0.1559551))->SetUniqueID(++imed);
(new TGeoMedium("TPCE_ALUMINIUM",0,GetMat("ALUMINIUM")->GetUniqueID(),0,1,20,20,10,0.1829598,1e-2,0.3344225E-01))->SetUniqueID(++imed);
(new TGeoMedium("TPCE_NITROGEN_GAS",0,GetMat("NITROGEN_GAS")->GetUniqueID(),0,1,20,20,10,0.2488926,1e-2,1.145545))->SetUniqueID(++imed);
(new TGeoMedium("TPCE_COPPER",0,GetMat("COPPER")->GetUniqueID(),0,1,20,20,10,0.25,1e-2,0.2813861E-01))->SetUniqueID(++imed);
(new TGeoMedium("TPCE_AL_HONEYCOMB",0,GetMat("TPCE_AL_HONEYCOMB")->GetUniqueID(),0,1,20,20,10,0.2332799,1e-2,0.7076892E-01))->SetUniqueID(++imed);
(new TGeoMedium("TPCE_SENSITIVE_GAS",0,GetMat("TPCE_SENSITIVE_GAS")->GetUniqueID(),1,1,20,20,5,0.1826148E-02,1e-2,0.3598796E-01))->SetUniqueID(++imed);
(new TGeoMedium("TPCE_SENSITIVE_GAS",0,GetMat("TPCE_SENSITIVE_GAS")->GetUniqueID(),1,1,20,20,5,0.1753672E-02,1e-2,0.3598795E-01))->SetUniqueID(++imed);
(new TGeoMedium("TPCE_AIR",0,GetMat("AIR")->GetUniqueID(),0,1,20,20,10,0.2488534,1e-2,1.150551))->SetUniqueID(++imed);
(new TGeoMedium("TPCE_G10",0,GetMat("TPCE_G10")->GetUniqueID(),0,1,20,20,10,0.2031303,1e-2,0.3338060E-01))->SetUniqueID(++imed);

(new TGeoMedium("FTPC_STANDARD",0,GetMat("AIR")->GetUniqueID(),0,1,20,20,10,0.2488534,1e-2,1.150551))->SetUniqueID(++imed);
(new TGeoMedium("FTPC_ALUMINIUM",0,GetMat("ALUMINIUM")->GetUniqueID(),0,1,20,20,10,0.1829598,1e-2,0.3344225E-01))->SetUniqueID(++imed);
(new TGeoMedium("FTPC_POLYETHYLENE",0,GetMat("POLYETHYLENE")->GetUniqueID(),0,1,20,20,10,0.2209553,1e-2,0.3204402E-01))->SetUniqueID(++imed);
(new TGeoMedium("FTPC_MYLAR",0,GetMat("MYLAR")->GetUniqueID(),0,1,20,20,10,0.2125565,1e-2,0.3113845E-01))->SetUniqueID(++imed);
(new TGeoMedium("FTPC_ARGON_GAS",0,GetMat("ARGON_GAS")->GetUniqueID(),0,1,20,20,10,0.2481602,1e-2,1.604161))->SetUniqueID(++imed);
(new TGeoMedium("FTPC_SENSITIVE",0,GetMat("ARGON_GAS")->GetUniqueID(),1,1,20,20,10,0.1839753E-02,1e-2,0.3689074E-01))->SetUniqueID(++imed);
(new TGeoMedium("FTPC_AR_CO2",0,GetMat("FTPC_AR_CO2")->GetUniqueID(),0,1,20,20,10,0.2482782,1e-2,1.207905))->SetUniqueID(++imed);
(new TGeoMedium("FTPC_SENSITIVE",0,GetMat("FTPC_AR_CO2")->GetUniqueID(),1,1,20,20,10,0.1721783E-02,1e-2,0.2599689E-01))->SetUniqueID(++imed);
(new TGeoMedium("FTPC_PYREX",0,GetMat("FTPC_PYREX")->GetUniqueID(),0,1,20,20,10,0.1936564,1e-2,0.3243283E-01))->SetUniqueID(++imed);
(new TGeoMedium("FTPC_AIR",0,GetMat("AIR")->GetUniqueID(),0,1,20,20,10,0.2488534,1e-2,1.150551))->SetUniqueID(++imed);
(new TGeoMedium("FTPC_G10",0,GetMat("FTPC_G10")->GetUniqueID(),0,1,20,20,10,0.2031303,1e-2,0.3338060E-01))->SetUniqueID(++imed);
(new TGeoMedium("FTPC_COPPER",0,GetMat("COPPER")->GetUniqueID(),0,1,20,20,10,0.25,1e-2,0.2813861E-01))->SetUniqueID(++imed);

(new TGeoMedium("SUPO_STANDARD",0,GetMat("AIR")->GetUniqueID(),0,1,20,20,10,0.2488534,1e-2,1.150551))->SetUniqueID(++imed);
(new TGeoMedium("SUPO_ALUMINIUM",0,GetMat("ALUMINIUM")->GetUniqueID(),0,1,20,20,10,0.1829598,1e-2,0.3344225E-01))->SetUniqueID(++imed);
(new TGeoMedium("SUPO_G10",0,GetMat("SUPO_G10")->GetUniqueID(),0,1,20,20,10,0.2031303,1e-2,0.3338060E-01))->SetUniqueID(++imed);

(new TGeoMedium("FTRO_AIR",0,GetMat("AIR")->GetUniqueID(),0,1,20,20,10,0.2488534,1e-2,1.150551))->SetUniqueID(++imed);
(new TGeoMedium("FTRO_ALUMINIUM",0,GetMat("ALUMINIUM")->GetUniqueID(),0,1,20,20,10,0.1829598,1e-2,0.3344225E-01))->SetUniqueID(++imed);
(new TGeoMedium("FTRO_G10",0,GetMat("FTRO_G10")->GetUniqueID(),0,1,20,20,10,0.2031303,1e-2,0.3338060E-01))->SetUniqueID(++imed);

(new TGeoMedium("BTOF_STANDARD",0,GetMat("AIR")->GetUniqueID(),0,1,20,20,10,0.2488534,1e-2,1.150551))->SetUniqueID(++imed);
(new TGeoMedium("BTOF_ALUMINIUM",0,GetMat("ALUMINIUM")->GetUniqueID(),0,1,20,20,10,0.1829598,1e-2,0.3344225E-01))->SetUniqueID(++imed);
(new TGeoMedium("BTOF_AIR",0,GetMat("AIR")->GetUniqueID(),0,1,20,20,10,0.2488534,1e-2,1.150551))->SetUniqueID(++imed);
(new TGeoMedium("BTOF_SENSITIVE",0,GetMat("POLYSTYREN")->GetUniqueID(),1,1,20,20,10,0.3086727E-01,1e-2,0.1270225E-01))->SetUniqueID(++imed);
(new TGeoMedium("BTOF_POLYSTYREN",0,GetMat("POLYSTYREN")->GetUniqueID(),0,1,20,20,10,0.2191327,1e-2,0.3292095E-01))->SetUniqueID(++imed);
(new TGeoMedium("BTOF_SILICON",0,GetMat("SILICON")->GetUniqueID(),0,1,20,20,10,0.184628,1e-2,0.3662629E-01))->SetUniqueID(++imed);
(new TGeoMedium("BTOF_WATER",0,GetMat("BTOF_WATER")->GetUniqueID(),0,1,20,20,10,0.2165401,1e-2,0.3589959E-01))->SetUniqueID(++imed);
(new TGeoMedium("BTOF_LASTAFOAM",0,GetMat("BTOF_LASTAFOAM")->GetUniqueID(),0,1,20,20,10,0.2434015,1e-2,0.1374749))->SetUniqueID(++imed);
(new TGeoMedium("BTOF_HONEYCOMB",0,GetMat("BTOF_HONEYCOMB")->GetUniqueID(),0,1,20,20,10,0.2332799,1e-2,0.7076892E-01))->SetUniqueID(++imed);
(new TGeoMedium("BTOF_HONEYCOMB",0,GetMat("BTOF_HONEYCOMB")->GetUniqueID(),0,1,20,20,10,0.2230986,1e-2,0.4409510E-01))->SetUniqueID(++imed);
(new TGeoMedium("BTOF_RPCGAS",0,GetMat("BTOF_RPCGAS")->GetUniqueID(),0,1,20,20,10,0.2477186,1e-2,0.6120169))->SetUniqueID(++imed);
(new TGeoMedium("BTOF_G10",0,GetMat("BTOF_G10")->GetUniqueID(),0,1,20,20,10,0.2031303,1e-2,0.3338060E-01))->SetUniqueID(++imed);
(new TGeoMedium("BTOF_MYLAR",0,GetMat("MYLAR")->GetUniqueID(),0,1,20,20,10,0.2125565,1e-2,0.3113845E-01))->SetUniqueID(++imed);
(new TGeoMedium("BTOF_CARBON",0,GetMat("CARBON")->GetUniqueID(),0,1,20,20,10,0.2038734,1e-2,0.2484640E-01))->SetUniqueID(++imed);
(new TGeoMedium("BTOF_GLASS",0,GetMat("BTOF_GLASS")->GetUniqueID(),0,1,20,20,10,0.192847,1e-2,0.3317548E-01))->SetUniqueID(++imed);
(new TGeoMedium("BTOF_GLASS",0,GetMat("BTOF_GLASS")->GetUniqueID(),0,1,20,20,10,0.1890747,1e-2,0.3118069E-01))->SetUniqueID(++imed);
(new TGeoMedium("BTOF_SENSITIVE",0,GetMat("BTOF_RPCGAS")->GetUniqueID(),1,1,20,20,10,0.2281429E-02,1e-2,0.1745342E-01))->SetUniqueID(++imed);
(new TGeoMedium("BTOF_LUCITE",0,GetMat("BTOF_LUCITE")->GetUniqueID(),0,1,20,20,10,0.2157145,1e-2,0.3209332E-01))->SetUniqueID(++imed);
(new TGeoMedium("BTOF_LASTAFOAM",0,GetMat("BTOF_LASTAFOAM")->GetUniqueID(),0,1,20,20,10,0.2434015,1e-2,0.1374749))->SetUniqueID(++imed);
(new TGeoMedium("BTOF_WATER",0,GetMat("BTOF_WATER")->GetUniqueID(),0,1,20,20,10,0.2165401,1e-2,0.3589959E-01))->SetUniqueID(++imed);
(new TGeoMedium("VPDD_STANDARD",0,GetMat("AIR")->GetUniqueID(),0,1,20,20,10,0.2488534,1e-2,1.150551))->SetUniqueID(++imed);
(new TGeoMedium("VPDD_ALUMINIUM",0,GetMat("ALUMINIUM")->GetUniqueID(),0,1,20,20,10,0.1829598,1e-2,0.3344225E-01))->SetUniqueID(++imed);
(new TGeoMedium("VPDD_POLYSTYREN",0,GetMat("POLYSTYREN")->GetUniqueID(),0,1,20,20,10,0.2191327,1e-2,0.3292095E-01))->SetUniqueID(++imed);
(new TGeoMedium("VPDD_AIR",0,GetMat("AIR")->GetUniqueID(),0,1,20,20,10,0.2488534,1e-2,1.150551))->SetUniqueID(++imed);
(new TGeoMedium("VPDD_G10",0,GetMat("VPDD_G10")->GetUniqueID(),0,1,20,20,10,0.2031303,1e-2,0.3338060E-01))->SetUniqueID(++imed);
(new TGeoMedium("VPDD_IRON",0,GetMat("IRON")->GetUniqueID(),0,1,20,20,10,0.25,1e-2,0.2813468E-01))->SetUniqueID(++imed);
(new TGeoMedium("VPDD_LEAD",0,GetMat("LEAD")->GetUniqueID(),0,1,20,20,10,0.25,1e-2,0.4122224E-01))->SetUniqueID(++imed);
(new TGeoMedium("VPDD_SENSITIVE",0,GetMat("POLYSTYREN")->GetUniqueID(),1,1,20,20,10,0.3086727E-01,1e-2,0.1270225E-01))->SetUniqueID(++imed);
(new TGeoMedium("VPDD_VACUUM",0,GetMat("VACUUM")->GetUniqueID(),0,1,20,20,10,0,1e-2,0))->SetUniqueID(++imed);
(new TGeoMedium("VPDD_IRON",0,GetMat("IRON")->GetUniqueID(),0,1,20,20,10,0.25,1e-2,0.2813468E-01))->SetUniqueID(++imed);
(new TGeoMedium("CALB_STANDARD",0,GetMat("AIR")->GetUniqueID(),0,1,20,20,10,0.2488534,1e-2,1.150551))->SetUniqueID(++imed);
(new TGeoMedium("CALB_AL_EMC",0,GetMat("CALB_EALUMINIUM")->GetUniqueID(),0,1,20,20,10,0.6704016E-01,1e-2,0.2836696E-01))->SetUniqueID(++imed);
(new TGeoMedium("CALB_CELLULOSE",0,GetMat("CALB_CELLULOSE")->GetUniqueID(),1,1,20,20,10,0.1755021E-01,1e-2,0.3558577E-02))->SetUniqueID(++imed);
(new TGeoMedium("CALB_SENS_SCI",0,GetMat("CALB_CPOLYSTYREN")->GetUniqueID(),1,1,20,20,10,0.3086727E-01,1e-2,0.1378078E-01))->SetUniqueID(++imed);
(new TGeoMedium("CALB_SCI",0,GetMat("CALB_CPOLYSTYREN")->GetUniqueID(),0,1,20,20,10,0.3086727E-01,1e-2,0.1378078E-01))->SetUniqueID(++imed);
(new TGeoMedium("CALB_LEAD_EMC",0,GetMat("CALB_CLEAD")->GetUniqueID(),0,1,20,20,10,0.25,1e-2,0.4580215E-01))->SetUniqueID(++imed);
(new TGeoMedium("CALB_G10",0,GetMat("CALB_G10")->GetUniqueID(),0,1,20,20,10,0.2031303,1e-2,0.1494809E-01))->SetUniqueID(++imed);
(new TGeoMedium("CALB_AL_SMD",0,GetMat("CALB_CALUMINIUM")->GetUniqueID(),0,1,20,20,10,0.1829598,1e-2,0.1542631E-01))->SetUniqueID(++imed);
(new TGeoMedium("CALB_SENS_GAS",0,GetMat("CALB_SENS_GAS")->GetUniqueID(),1,1,20,20,10,0.1876452E-02,1e-2,0.1655801E-01))->SetUniqueID(++imed);
(new TGeoMedium("RICH_STANDARD",0,GetMat("AIR")->GetUniqueID(),0,1,20,20,10,0.2488534,1e-2,1.150551))->SetUniqueID(++imed);
(new TGeoMedium("RICH_ALUMINIUM",0,GetMat("ALUMINIUM")->GetUniqueID(),0,1,20,20,10,0.1829598,1e-2,0.3344225E-01))->SetUniqueID(++imed);
(new TGeoMedium("RICH_METHANE",0,GetMat("RICH_METHANE")->GetUniqueID(),0,1,20,20,10,0.2492055,1e-2,0.9910752))->SetUniqueID(++imed);
(new TGeoMedium("RICH_CARBONIO",0,GetMat("RICH_CARBONIO")->GetUniqueID(),0,1,20,20,10,0.25,1e-2,0.1756906))->SetUniqueID(++imed);
(new TGeoMedium("RICH_OPACO_QUARTZ",0,GetMat("RICH_OPACO")->GetUniqueID(),1,1,20,20,10,0.6256106E-01,1e-2,0.2376492E-01))->SetUniqueID(++imed);
(new TGeoMedium("RICH_OPACO",0,GetMat("RICH_OPACO")->GetUniqueID(),0,1,20,20,10,0.1874389,1e-2,0.3038941E-01))->SetUniqueID(++imed);
(new TGeoMedium("RICH_FREON",0,GetMat("RICH_FREON")->GetUniqueID(),1,1,20,20,10,0.4439774E-01,1e-2,0.1895803E-01))->SetUniqueID(++imed);
(new TGeoMedium("RICH_QUARZ",0,GetMat("RICH_QUARZ")->GetUniqueID(),0,1,20,20,10,0.1874389,1e-2,0.3038941E-01))->SetUniqueID(++imed);
(new TGeoMedium("RICH_QUARTZ",0,GetMat("RICH_QUARZ")->GetUniqueID(),1,1,20,20,10,0.6256106E-01,1e-2,0.2376492E-01))->SetUniqueID(++imed);
(new TGeoMedium("RICH_METHANE_GAP",0,GetMat("RICH_METHANE")->GetUniqueID(),1,1,20,20,10,0.7944608E-03,1e-2,0.9842129E-02))->SetUniqueID(++imed);
(new TGeoMedium("RICH_AIR",0,GetMat("AIR")->GetUniqueID(),0,1,20,20,10,0.2488534,1e-2,1.150551))->SetUniqueID(++imed);
(new TGeoMedium("RICH_CSI",0,GetMat("RICH_CARBONIO")->GetUniqueID(),1,1,20,20,10,0.326164,1e-2,0.7162994))->SetUniqueID(++imed);
(new TGeoMedium("ECAL_STANDARD",0,GetMat("AIR")->GetUniqueID(),0,1,20,20,10,0.2488534,1e-2,1.150551))->SetUniqueID(++imed);
(new TGeoMedium("ECAL_AIR",0,GetMat("AIR")->GetUniqueID(),0,1,20,20,10,0.2488534,1e-2,1.150551))->SetUniqueID(++imed);
(new TGeoMedium("ECAL_IRON",0,GetMat("IRON")->GetUniqueID(),0,1,20,20,10,0.25,1e-2,0.2813468E-01))->SetUniqueID(++imed);
(new TGeoMedium("ECAL_ALUMINIUM",0,GetMat("ALUMINIUM")->GetUniqueID(),0,1,20,20,10,0.1829598,1e-2,0.3344225E-01))->SetUniqueID(++imed);
(new TGeoMedium("ECAL_STANDARD",0,GetMat("ECAL_CAIR")->GetUniqueID(),0,1,20,20,10,0.2488534,1e-2,0.5292183))->SetUniqueID(++imed);
(new TGeoMedium("ECAL_CLEAD",0,GetMat("ECAL_CLEAD")->GetUniqueID(),0,1,20,20,10,0.25,1e-2,0.4580215E-01))->SetUniqueID(++imed);
(new TGeoMedium("ECAL_POLYSTYREN",0,GetMat("POLYSTYREN")->GetUniqueID(),0,1,20,20,10,0.2191327,1e-2,0.3292095E-01))->SetUniqueID(++imed);
(new TGeoMedium("ECAL_CALUMINIUM",0,GetMat("ECAL_CALUMINIUM")->GetUniqueID(),0,1,20,20,10,0.1829598,1e-2,0.1542631E-01))->SetUniqueID(++imed);
(new TGeoMedium("ECAL_CPOLYSTYREN",0,GetMat("ECAL_CPOLYSTYREN")->GetUniqueID(),1,1,20,20,10,0.3086727E-01,1e-2,0.1378078E-01))->SetUniqueID(++imed);
(new TGeoMedium("ECAL_CLEAD",0,GetMat("ECAL_CLEAD")->GetUniqueID(),0,1,20,20,10,0.25,1e-2,0.4580215E-01))->SetUniqueID(++imed);
(new TGeoMedium("ECAL_AIR",0,GetMat("ECAL_AIR")->GetUniqueID(),0,1,20,20,10,0.25,1e-2,0.2813468E-01))->SetUniqueID(++imed);
(new TGeoMedium("ECAL_G10",0,GetMat("ECAL_G10")->GetUniqueID(),0,1,20,20,10,0.2031303,1e-2,0.1494809E-01))->SetUniqueID(++imed);
(new TGeoMedium("BBCM_STANDARD",0,GetMat("AIR")->GetUniqueID(),0,1,20,20,10,0.2488534,1e-2,1.150551))->SetUniqueID(++imed);
(new TGeoMedium("BBCM_ALKAP",0,GetMat("BBCM_ALKAP")->GetUniqueID(),0,1,20,20,10,0.211247,1e-2,0.3151248E-01))->SetUniqueID(++imed);
(new TGeoMedium("BBCM_CPOLYSTYREN",0,GetMat("BBCM_CPOLYSTYREN")->GetUniqueID(),1,1,20,20,10,0.3086727E-01,1e-2,0.1378078E-01))->SetUniqueID(++imed);

(new TGeoMedium("FPDM_STANDARD",0,GetMat("AIR")->GetUniqueID(),0,1,20,20,10,0.2488534,1e-2,1.150551))->SetUniqueID(++imed);
(new TGeoMedium("FPDM_IRON",0,GetMat("IRON")->GetUniqueID(),0,1,20,20,10,0.25,1e-2,0.2813468E-01))->SetUniqueID(++imed);
(new TGeoMedium("FPDM_AIR",0,GetMat("AIR")->GetUniqueID(),0,1,20,20,10,0.2488534,1e-2,1.150551))->SetUniqueID(++imed);
(new TGeoMedium("FPDM_ALUMINIUM",0,GetMat("ALUMINIUM")->GetUniqueID(),0,1,20,20,10,0.1829598,1e-2,0.3344225E-01))->SetUniqueID(++imed);
(new TGeoMedium("FPDM_STANDARD",0,GetMat("FPDM_CAIR")->GetUniqueID(),0,1,20,20,10,0.2488534,1e-2,1.158491))->SetUniqueID(++imed);
(new TGeoMedium("FPDM_CPOLYSTYREN",0,GetMat("FPDM_CPOLYSTYREN")->GetUniqueID(),1,1,20,20,10,0.3086727E-01,1e-2,0.1378078E-01))->SetUniqueID(++imed);
(new TGeoMedium("FPDM_G10",0,GetMat("FPDM_G10")->GetUniqueID(),0,1,20,20,10,0.2031303,1e-2,0.1494809E-01))->SetUniqueID(++imed);
(new TGeoMedium("FPDM_CLEAD",0,GetMat("FPDM_CLEAD")->GetUniqueID(),0,1,20,20,10,0.25,1e-2,0.4580215E-01))->SetUniqueID(++imed);
(new TGeoMedium("FPDM_CALUMINIUM",0,GetMat("FPDM_CALUMINIUM")->GetUniqueID(),0,1,20,20,10,0.1829598,1e-2,0.1542631E-01))->SetUniqueID(++imed);
(new TGeoMedium("FPDM_LEADGLASS",0,GetMat("FPDM_PBG")->GetUniqueID(),1,1,20,20,10,0.1302874,1e-2,0.8376447E-01))->SetUniqueID(++imed);
(new TGeoMedium("FPDM_SILICON",0,GetMat("SILICON")->GetUniqueID(),0,1,20,20,10,0.184628,1e-2,0.3662629E-01))->SetUniqueID(++imed);
(new TGeoMedium("FPDM_IRON",0,GetMat("IRON")->GetUniqueID(),0,1,20,20,10,0.25,1e-2,0.2813468E-01))->SetUniqueID(++imed);
(new TGeoMedium("FPDM_PHOTCATH",0,GetMat("AIR")->GetUniqueID(),1,1,20,20,10,0.1146641E-02,1e-2,0.1649085E-01))->SetUniqueID(++imed);
(new TGeoMedium("FPDM_LEAD",0,GetMat("LEAD")->GetUniqueID(),0,1,20,20,10,0.25,1e-2,0.4122224E-01))->SetUniqueID(++imed);
(new TGeoMedium("FPDM_G10",0,GetMat("FPDM_G10")->GetUniqueID(),0,1,20,20,10,0.2031303,1e-2,0.1494809E-01))->SetUniqueID(++imed);
(new TGeoMedium("FPDM_CPOLYSTYREN",0,GetMat("FPDM_CPOLYSTYREN")->GetUniqueID(),1,1,20,20,10,0.3086727E-01,1e-2,0.1378078E-01))->SetUniqueID(++imed);
(new TGeoMedium("FPDM_LEADGLASS",0,GetMat("FPDM_PBG")->GetUniqueID(),1,1,20,20,10,0.1259976,1e-2,0.7404882E-01))->SetUniqueID(++imed);
(new TGeoMedium("FPDM_LEADGLASS",0,GetMat("FPDM_F2")->GetUniqueID(),1,1,20,20,10,0.1124198,1e-2,0.6328612E-01))->SetUniqueID(++imed);
(new TGeoMedium("ZCAL_STANDARD",0,GetMat("AIR")->GetUniqueID(),0,1,20,20,10,0.2488534,1e-2,1.150551))->SetUniqueID(++imed);
(new TGeoMedium("ZCAL_IRON",0,GetMat("IRON")->GetUniqueID(),0,1,20,20,10,0.25,1e-2,0.2813468E-01))->SetUniqueID(++imed);
(new TGeoMedium("ZCAL_VACUUM",0,GetMat("VACUUM")->GetUniqueID(),0,1,20,20,10,0,1e-2,0))->SetUniqueID(++imed);
(new TGeoMedium("ZCAL_DIRTY_LEAD",0,GetMat("ZCAL_DIRTY_LEAD")->GetUniqueID(),0,1,20,20,10,0.25,1e-2,0.3500415E-01))->SetUniqueID(++imed);
(new TGeoMedium("ZCAL_SCINTILLATOR",0,GetMat("ZCAL_SCINTILLATOR")->GetUniqueID(),1,1,20,20,10,0.3086727E-01,1e-2,0.9938389E-02))->SetUniqueID(++imed);
(new TGeoMedium("MAGP_STANDARD",0,GetMat("AIR")->GetUniqueID(),0,1,20,20,10,0.2488534,1e-2,1.150551))->SetUniqueID(++imed);
(new TGeoMedium("MAGP_ALUMINIUM",0,GetMat("ALUMINIUM")->GetUniqueID(),0,1,20,20,10,0.1829598,1e-2,0.3344225E-01))->SetUniqueID(++imed);
(new TGeoMedium("MAGP_IRON",0,GetMat("IRON")->GetUniqueID(),0,1,20,20,10,0.25,1e-2,0.2813468E-01))->SetUniqueID(++imed);
(new TGeoMedium("MUTD_STANDARD",0,GetMat("AIR")->GetUniqueID(),0,1,20,20,10,0.2488534,1e-2,1.150551))->SetUniqueID(++imed);
(new TGeoMedium("MUTD_ALUMINIUM",0,GetMat("ALUMINIUM")->GetUniqueID(),0,1,20,20,10,0.1829598,1e-2,0.3344225E-01))->SetUniqueID(++imed);
(new TGeoMedium("MUTD_AIR",0,GetMat("AIR")->GetUniqueID(),0,1,20,20,10,0.2488534,1e-2,1.150551))->SetUniqueID(++imed);
(new TGeoMedium("MUTD_SENSITIVE",0,GetMat("POLYSTYREN")->GetUniqueID(),1,1,20,20,10,0.3086727E-01,1e-2,0.1270225E-01))->SetUniqueID(++imed);
(new TGeoMedium("MUTD_SENSITIVE",0,GetMat("MUTD_RPCGAS")->GetUniqueID(),1,1,20,20,10,0.2281429E-02,1e-2,0.1745342E-01))->SetUniqueID(++imed);
(new TGeoMedium("PIXL_AIR",0,GetMat("AIR")->GetUniqueID(),0,1,20,20,10,0.2488534,1e-2,1.150551))->SetUniqueID(++imed);
(new TGeoMedium("PIXL_SENSITIVE",0,GetMat("PIXL_SENSITIVE")->GetUniqueID(),1,1,20,20,10,0.6537204E-01,1e-2,0.2992920E-01))->SetUniqueID(++imed);
(new TGeoMedium("PIXL_SILICON",0,GetMat("SILICON")->GetUniqueID(),0,1,20,20,10,0.184628,1e-2,0.3662629E-01))->SetUniqueID(++imed);
(new TGeoMedium("PIXL_BERILLIUM",0,GetMat("BERILLIUM")->GetUniqueID(),0,1,20,20,10,0.2163378,1e-2,0.2434062E-01))->SetUniqueID(++imed);
(new TGeoMedium("ISTB_AIR",0,GetMat("AIR")->GetUniqueID(),0,1,20,20,10,0.2488534,1e-2,1.150551))->SetUniqueID(++imed);
(new TGeoMedium("ISTB_CARBON",0,GetMat("CARBON")->GetUniqueID(),0,1,20,20,10,0.2038734,1e-2,0.2484640E-01))->SetUniqueID(++imed);
(new TGeoMedium("ISTB_WATER",0,GetMat("ISTB_WATER")->GetUniqueID(),0,1,20,20,10,0.2165401,1e-2,0.3589959E-01))->SetUniqueID(++imed);
(new TGeoMedium("ISTB_KAPTON",0,GetMat("ISTB_KAPTON")->GetUniqueID(),0,1,20,20,10,0.2126101,1e-2,0.3089750E-01))->SetUniqueID(++imed);
(new TGeoMedium("ISTB_COPPER",0,GetMat("COPPER")->GetUniqueID(),0,1,20,20,10,0.25,1e-2,0.2813861E-01))->SetUniqueID(++imed);
(new TGeoMedium("ISTB_CFOAM",0,GetMat("ISTB_CFOAM")->GetUniqueID(),0,1,20,20,10,0.2341107,1e-2,0.7965542E-01))->SetUniqueID(++imed);
(new TGeoMedium("ISTB_EPOXY",0,GetMat("ISTB_EPOXY")->GetUniqueID(),0,1,20,20,10,0.2138171,1e-2,0.3080761E-01))->SetUniqueID(++imed);
(new TGeoMedium("ISTB_SENSITIVE",0,GetMat("ISTB_SENSITIVE")->GetUniqueID(),1,1,20,20,10,0.6537204E-01,1e-2,0.2992920E-01))->SetUniqueID(++imed);
(new TGeoMedium("ISTB_ALN",0,GetMat("ISTB_ALN")->GetUniqueID(),0,1,20,20,10,0.1805969,1e-2,0.2769805E-01))->SetUniqueID(++imed);
(new TGeoMedium("ISTB_SILICON",0,GetMat("SILICON")->GetUniqueID(),0,1,20,20,10,0.184628,1e-2,0.3662629E-01))->SetUniqueID(++imed);
(new TGeoMedium("ISTB_CFOAM",0,GetMat("ISTB_CFOAM")->GetUniqueID(),0,1,20,20,10,0.2397435,1e-2,0.1234016))->SetUniqueID(++imed);
(new TGeoMedium("FSTD_AIR",0,GetMat("AIR")->GetUniqueID(),0,1,20,20,10,0.2488534,1e-2,1.150551))->SetUniqueID(++imed);
(new TGeoMedium("FSTD_SENSITIVE",0,GetMat("FSTD_SENSITIVE")->GetUniqueID(),1,1,20,20,10,0.6537204E-01,1e-2,0.2992920E-01))->SetUniqueID(++imed);
(new TGeoMedium("FSTD_ALN",0,GetMat("FSTD_ALN")->GetUniqueID(),0,1,20,20,10,0.1805969,1e-2,0.2769805E-01))->SetUniqueID(++imed);
(new TGeoMedium("FSTD_SILICON",0,GetMat("SILICON")->GetUniqueID(),0,1,20,20,10,0.184628,1e-2,0.3662629E-01))->SetUniqueID(++imed);
(new TGeoMedium("FSTD_CARBON",0,GetMat("CARBON")->GetUniqueID(),0,1,20,20,10,0.2038734,1e-2,0.2484640E-01))->SetUniqueID(++imed);
(new TGeoMedium("FSTD_WATER",0,GetMat("FSTD_WATER")->GetUniqueID(),0,1,20,20,10,0.2165401,1e-2,0.3589959E-01))->SetUniqueID(++imed);
(new TGeoMedium("IGTD_AIR",0,GetMat("AIR")->GetUniqueID(),0,1,20,20,10,0.2488534,1e-2,1.150551))->SetUniqueID(++imed);
(new TGeoMedium("IGTD_AR_MIX",0,GetMat("IGTD_AR_MIX")->GetUniqueID(),0,1,20,20,10,0.2481235,1e-2,1.477491))->SetUniqueID(++imed);
(new TGeoMedium("IGTD_ALKAP",0,GetMat("IGTD_ALKAP")->GetUniqueID(),0,1,20,20,10,0.211247,1e-2,0.3151248E-01))->SetUniqueID(++imed);
(new TGeoMedium("IGTD_G10",0,GetMat("IGTD_G10")->GetUniqueID(),0,1,20,20,10,0.2031303,1e-2,0.3338060E-01))->SetUniqueID(++imed);
(new TGeoMedium("IGTD_SENSITIVE",0,GetMat("IGTD_SENSITIVE")->GetUniqueID(),1,1,20,20,10,0.1876452E-02,1e-2,0.6963173E-01))->SetUniqueID(++imed);
(new TGeoMedium("HPDT_AIR",0,GetMat("AIR")->GetUniqueID(),0,1,20,20,10,0.2488534,1e-2,1.150551))->SetUniqueID(++imed);
(new TGeoMedium("HPDT_MYLAR",0,GetMat("MYLAR")->GetUniqueID(),0,1,20,20,10,0.2125565,1e-2,0.3113845E-01))->SetUniqueID(++imed);
(new TGeoMedium("HPDT_CARBON",0,GetMat("CARBON")->GetUniqueID(),0,1,20,20,10,0.2038734,1e-2,0.2484640E-01))->SetUniqueID(++imed);
(new TGeoMedium("HPDT_SILICON",0,GetMat("SILICON")->GetUniqueID(),0,1,20,20,10,0.184628,1e-2,0.3662629E-01))->SetUniqueID(++imed);
(new TGeoMedium("HPDT_SENSITIVE",0,GetMat("HPDT_SENSITIVE")->GetUniqueID(),1,1,20,20,10,0.6537204E-01,1e-2,0.2992920E-01))->SetUniqueID(++imed);
(new TGeoMedium("ITSP_AIR",0,GetMat("AIR")->GetUniqueID(),0,1,20,20,10,0.2488534,1e-2,1.150551))->SetUniqueID(++imed);
(new TGeoMedium("ITSP_CARBON",0,GetMat("CARBON")->GetUniqueID(),0,1,20,20,10,0.2038734,1e-2,0.2484640E-01))->SetUniqueID(++imed);
(new TGeoMedium("ITSP_CH2",0,GetMat("ITSP_CH2")->GetUniqueID(),0,1,20,20,10,0.2208773,1e-2,0.3196166E-01))->SetUniqueID(++imed);
(new TGeoMedium("ITSP_MYLAR",0,GetMat("MYLAR")->GetUniqueID(),0,1,20,20,10,0.2125565,1e-2,0.3113845E-01))->SetUniqueID(++imed);
(new TGeoMedium("ITSP_ALUMINIUM",0,GetMat("ALUMINIUM")->GetUniqueID(),0,1,20,20,10,0.1829598,1e-2,0.3344225E-01))->SetUniqueID(++imed);
(new TGeoMedium("ISTB_AIR",0,GetMat("AIR")->GetUniqueID(),0,1,20,20,10,0.2488534,1e-2,1.150551))->SetUniqueID(++imed);
(new TGeoMedium("ISTB_CARBON",0,GetMat("CARBON")->GetUniqueID(),0,1,20,20,10,0.2038734,1e-2,0.2484640E-01))->SetUniqueID(++imed);
(new TGeoMedium("ISTB_WATER",0,GetMat("ISTB_WATER")->GetUniqueID(),0,1,20,20,10,0.2165401,1e-2,0.3589959E-01))->SetUniqueID(++imed);
(new TGeoMedium("ISTB_SENSITIVE",0,GetMat("ISTB_SENSITIVE")->GetUniqueID(),1,1,20,20,10,0.6537204E-01,1e-2,0.2992920E-01))->SetUniqueID(++imed);
(new TGeoMedium("ISTB_ALN",0,GetMat("ISTB_ALN")->GetUniqueID(),0,1,20,20,10,0.1805969,1e-2,0.2769805E-01))->SetUniqueID(++imed);
(new TGeoMedium("ISTB_SILICON",0,GetMat("SILICON")->GetUniqueID(),0,1,20,20,10,0.184628,1e-2,0.3662629E-01))->SetUniqueID(++imed);
(new TGeoMedium("FGTD_AIR",0,GetMat("AIR")->GetUniqueID(),0,1,20,20,10,0.2488534,1e-2,1.150551))->SetUniqueID(++imed);
(new TGeoMedium("FGTD_AR_MIX",0,GetMat("FGTD_AR_MIX")->GetUniqueID(),0,1,20,20,10,0.2481235,1e-2,1.477491))->SetUniqueID(++imed);
(new TGeoMedium("FGTD_ALKAP",0,GetMat("FGTD_ALKAP")->GetUniqueID(),0,1,20,20,10,0.211247,1e-2,0.3151248E-01))->SetUniqueID(++imed);
(new TGeoMedium("FGTD_G10",0,GetMat("FGTD_G10")->GetUniqueID(),0,1,20,20,10,0.2031303,1e-2,0.3338060E-01))->SetUniqueID(++imed);
(new TGeoMedium("FGTD_SENSITIVE",0,GetMat("FGTD_SENSITIVE")->GetUniqueID(),1,1,20,20,10,0.1876452E-02,1e-2,0.6963173E-01))->SetUniqueID(++imed);
(new TGeoMedium("PHMD_STANDARD",0,GetMat("AIR")->GetUniqueID(),0,1,20,20,10,0.2488534,1e-2,1.150551))->SetUniqueID(++imed);
(new TGeoMedium("PHMD_AIR",0,GetMat("AIR")->GetUniqueID(),0,1,20,20,10,0.2488534,1e-2,1.150551))->SetUniqueID(++imed);
(new TGeoMedium("PHMD_ALUMINIUM",0,GetMat("ALUMINIUM")->GetUniqueID(),0,1,20,20,10,0.1829598,1e-2,0.3344225E-01))->SetUniqueID(++imed);
(new TGeoMedium("PHMD_G10",0,GetMat("PHMD_G10")->GetUniqueID(),0,1,20,20,10,0.2031303,1e-2,0.2158788E-01))->SetUniqueID(++imed);
(new TGeoMedium("PHMD_G10",0,GetMat("PHMD_G10")->GetUniqueID(),0,1,20,20,10,0.2031303,1e-2,0.3338060E-01))->SetUniqueID(++imed);
(new TGeoMedium("PHMD_COPPER",0,GetMat("COPPER")->GetUniqueID(),0,1,20,20,10,0.25,1e-2,0.2813861E-01))->SetUniqueID(++imed);
(new TGeoMedium("PHMD_SENSITIVE",0,GetMat("COPPER")->GetUniqueID(),1,1,20,20,10,0.1672484,1e-2,0.5882673E-01))->SetUniqueID(++imed);
(new TGeoMedium("PHMD_AR_CO2",0,GetMat("PHMD_AR_CO2")->GetUniqueID(),1,1,20,20,10,0.1802346E-02,1e-2,0.1427377E-01))->SetUniqueID(++imed);
(new TGeoMedium("PHMD_LEAD",0,GetMat("LEAD")->GetUniqueID(),0,1,20,20,10,0.25,1e-2,0.3206524E-01))->SetUniqueID(++imed);
(new TGeoMedium("PHMD_IRON",0,GetMat("IRON")->GetUniqueID(),0,1,20,20,10,0.25,1e-2,0.1929271E-01))->SetUniqueID(++imed);
(new TGeoMedium("PHMD_AR_CO2",0,GetMat("PHMD_AR_CO2")->GetUniqueID(),1,1,20,20,10,0.1802346E-02,1e-2,0.3016429E-01))->SetUniqueID(++imed);
(new TGeoMedium("PHMD_LEAD",0,GetMat("LEAD")->GetUniqueID(),0,1,20,20,10,0.25,1e-2,0.4122224E-01))->SetUniqueID(++imed);
(new TGeoMedium("PHMD_IRON",0,GetMat("IRON")->GetUniqueID(),0,1,20,20,10,0.25,1e-2,0.2813468E-01))->SetUniqueID(++imed);
(new TGeoMedium("HPDT_AIR",0,GetMat("AIR")->GetUniqueID(),0,1,20,20,10,0.2488534,1e-2,1.150551))->SetUniqueID(++imed);
(new TGeoMedium("HPDT_MYLAR",0,GetMat("MYLAR")->GetUniqueID(),0,1,20,20,10,0.2125565,1e-2,0.3113845E-01))->SetUniqueID(++imed);
(new TGeoMedium("HPDT_CARBON",0,GetMat("CARBON")->GetUniqueID(),0,1,20,20,10,0.2038734,1e-2,0.2484640E-01))->SetUniqueID(++imed);
(new TGeoMedium("HPDT_SILICON",0,GetMat("SILICON")->GetUniqueID(),0,1,20,20,10,0.184628,1e-2,0.3662629E-01))->SetUniqueID(++imed);
(new TGeoMedium("HPDT_SENSITIVE",0,GetMat("HPDT_SENSITIVE")->GetUniqueID(),1,1,20,20,10,0.6537204E-01,1e-2,0.2992920E-01))->SetUniqueID(++imed);
(new TGeoMedium("Cables",0,GetMat("Cables")->GetUniqueID(),0,0,20,20,10,0.2488534,1e-2,1.150551))->SetUniqueID(++imed);
}
#endif

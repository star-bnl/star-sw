#include "CreateGeometry.h"
TDataSet *CreateTable() {
  gEnv->SetValue("tpcg_version",2);
  gEnv->SetValue("tpcg_tpadconfig",46.107);
  gEnv->SetValue("btog_version",5);
  gEnv->SetValue("btog_choice",5);
  gEnv->SetValue("btog_posit1(1)",32);
  gEnv->SetValue("btog_posit1(2)",33);
  gEnv->SetValue("btog_posit2",23);
  gEnv->SetValue("btog_version",5);
  gEnv->SetValue("calg_version",3);
  gEnv->SetValue("calg_nmodule(1)",60);
  gEnv->SetValue("calg_nmodule(2)",60);
  gEnv->SetValue("calg_netaT",20);
  gEnv->SetValue("calg_maxmodule",60);
  gEnv->SetValue("calg_nsub",2);
  gEnv->SetValue("calg_netasmdp",10);
  gEnv->SetValue("calg_nphistr",15);
  gEnv->SetValue("calg_netfirst",75);
  gEnv->SetValue("calg_netsecon",75);
  gEnv->SetValue("emcg_version",6.1);
  gEnv->SetValue("emcg_onoff",1);
  gEnv->SetValue("emcg_fillmode",3);
  gEnv->SetValue("ismg_layer",1);
  gEnv->SetValue("ismg_rin",11.1);
  gEnv->SetValue("ismg_rout",18);
  gEnv->SetValue("ismg_totallength",54.5);
  gEnv->SetValue("ismg_code",160915);
  gEnv->SetValue("fpdg_version",5);
  return CreateGeometry("upgr13");
}

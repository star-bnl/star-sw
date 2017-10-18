#include "CreateGeometry.h"
TDataSet *CreateTable() {
  TEnv *configGeom = new TEnv("configGeom");
 configGeom ->SetValue("isvt",-1);
 configGeom ->SetValue("itpc",4);
 configGeom ->SetValue("ibtf",8);
 configGeom ->SetValue("ical",12);
 configGeom ->SetValue("ivpd",-1);
 configGeom ->SetValue("istb",-1);
 configGeom ->SetValue("ifpd",-1);
 configGeom ->SetValue("ifms",-1);
 configGeom ->SetValue("ifsc",-1);
 configGeom ->SetValue("tpcg_version",5);
 configGeom ->SetValue("btog_version",6);
 configGeom ->SetValue("btog_choice",13);
 configGeom ->SetValue("btog_posit1(1)",32);
 configGeom ->SetValue("btog_posit1(2)",33);
 configGeom ->SetValue("btog_posit2",23);
 configGeom ->SetValue("btog_posit3",33);
 configGeom ->SetValue("calg_version",3);
 configGeom ->SetValue("calg_nmodule(1)",60);
 configGeom ->SetValue("calg_nmodule(2)",60);
 configGeom ->SetValue("calg_netaT",20);
 configGeom ->SetValue("calg_maxmodule",60);
 configGeom ->SetValue("calg_nsub",2);
 configGeom ->SetValue("calg_netasmdp",10);
 configGeom ->SetValue("calg_nphistr",15);
 configGeom ->SetValue("calg_netfirst",75);
 configGeom ->SetValue("calg_netsecon",75);
 configGeom ->SetValue("emcg_version",6.1);
 configGeom ->SetValue("emcg_onoff",1);
 configGeom ->SetValue("emcg_fillmode",3);
 configGeom ->SetValue("mtdg_version",1);
  return CreateGeometry("y2014c",configGeom);
}

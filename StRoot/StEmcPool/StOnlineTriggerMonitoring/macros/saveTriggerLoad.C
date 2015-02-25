/* 
 * saveTriggerLoad.C
 * Update: 03/27/2014 Kevin Adkins
 * Change previous access method of
 * saveTrigger(...). Previously it was accessed
 * via StOnlineTriggerMonitoring::saveTrigger(...)
 * now declare StOnlineTriggerMonitoring member onltrg
 * and use -> class access operator to call
 * saveTrigger(...)
 * 
 */

void saveTriggerLoad(
char *timestamp = "20150214.000001",
bool status = true,
bool pedestal = true,
bool lut = true,
bool saveDB = false,
bool saveTables = true,
char *tables_dir = "tables.emconline_trg",
char *saved_dir = "last_config.emconline_trg",
char *bemcStatusCopy = "bemcStatus.txt",
char *bceTable = "bce_table.txt",
char *bcwTable = "bcw_table.txt"
) {
    gROOT->Macro("loadMuDst.C");
    gSystem->Load("StDbBroker");
    gSystem->Load("St_db_Maker");
    gSystem->Load("StDaqLib");
    gSystem->Load("StOnlineTriggerMonitoring");

    StOnlineTriggerMonitoring *onltrg = new StOnlineTriggerMonitoring();
    onltrg->saveTrigger(timestamp, status, pedestal, lut, saveDB, saveTables, tables_dir, saved_dir, bemcStatusCopy, bceTable, bcwTable);
}

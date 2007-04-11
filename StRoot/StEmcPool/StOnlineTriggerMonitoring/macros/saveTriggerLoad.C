void saveTriggerLoad(
char *timestamp = "",
bool status = true,
bool pedestal = true,
bool lut = true,
bool saveDB = true,
bool saveTables = true,
char *tables_dir = "tables.emconline_trg",
char *saved_dir = "last_config.emconline_trg",
char *bemcStatusCopy = "bemcStatus.txt",
char *bceTable = "bce_table.txt",
char *bcwTable = "bcw_table.txt"
) {
    gSystem->Load("St_base");
    gSystem->Load("St_Tables");
    gSystem->Load("StChain");
    gSystem->Load("StDbLib");
    gSystem->Load("StDbBroker");
    gSystem->Load("St_db_Maker");
    gSystem->Load("StDaqLib");
    gSystem->Load("StOnlineTriggerMonitoring");
    StOnlineTriggerMonitoring::saveTrigger(timestamp, status, pedestal, lut, saveDB, saveTables, tables_dir, saved_dir, bemcStatusCopy, bceTable, bcwTable);
}

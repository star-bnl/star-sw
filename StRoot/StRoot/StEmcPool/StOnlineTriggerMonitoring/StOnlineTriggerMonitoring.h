#ifndef StOnlineTriggerMonitoring_H
#define StOnlineTriggerMonitoring_H

#include <TObject.h>

class StOnlineTriggerMonitoring {
 public:
  void saveTrigger(const Char_t *TS = "", Bool_t status = true, Bool_t pedestal = true, Bool_t lut = true, Bool_t saveDB = true, Bool_t saveTables = true, const Char_t *tables_dir = "tables.emconline_trg", const Char_t *saved_dir = "last_conf.emconline_trg", const Char_t *bemcStatusCopy = "bemcStatus.txt", const Char_t *bceTable = "bce_table.txt", const Char_t *bcwTable = "bcw_table.txt");
  ClassDef(StOnlineTriggerMonitoring, 1);
};

#endif

#!/bin/bash

export EMCONLINE_PED_DIR=/home/emc/online/emc/pedestal

export EMCONLINE_PED_SCRIPT=${EMCONLINE_PED_DIR}/makeOnlinePed.C

export EMCONLINE_PED_START="${EMCONLINE_PED_DIR}/status.emconline_ped RUN"
export EMCONLINE_PED_STOP="${EMCONLINE_PED_DIR}/status.emconline_ped STOP"
export EMCONLINE_PED_UPDATE=${EMCONLINE_PED_DIR}/update.emconline_ped

export EMCONLINE_PED_LOG_FILE=${EMCONLINE_PED_DIR}/last_log.emconline_ped.txt
export EMCONLINE_PED_RUNMODE_FILE=${EMCONLINE_PED_DIR}/RUNMODE.emconline_ped
export EMCONLINE_PED_FILELIST_FILE=${EMCONLINE_PED_DIR}/runlist.emconline_ped.txt
export EMCONLINE_PED_LASTRUN_FILE=${EMCONLINE_PED_DIR}/LAST_TIME_RUN.emconline_ped

export EMCONLINE_PED_BACKUP_DIR=${EMCONLINE_PED_DIR}/backup.emconline_ped
export EMCONLINE_PED_LOGS_DIR=${EMCONLINE_PED_DIR}/logs.emconline_ped
export EMCONLINE_PED_TEMP_DIR=${EMCONLINE_PED_DIR}/tmp.emconline_ped

export EMCONLINE_PED_TABLES_DIR=${EMCONLINE_PED_DIR}/tables.emconline_ped
export EMCONLINE_PED_TABLES_SCRIPT=${EMCONLINE_PED_DIR}/transformBackupHistoToDBTable.C

export EMCONLINE_PED_STARVER=dev
export EMCONLINE_PED_NEVENTS=2000
export EMCONLINE_PED_SAVEDB=true

export EVP_READER_LIB=libevpSO.2.0.so
export EVP_DIR=/evp

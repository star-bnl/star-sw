
export EMCONLINE_TRG_SCRIPT=${EMCONLINE_TRG_DIR}/saveTriggerLoad.C

export EMCONLINE_TRG_START="${EMCONLINE_TRG_DIR}/status.emconline_trg RUN"
export EMCONLINE_TRG_STOP="${EMCONLINE_TRG_DIR}/status.emconline_trg STOP"
export EMCONLINE_TRG_UPDATE=${EMCONLINE_TRG_DIR}/update.emconline_trg

export EMCONLINE_TRG_LOG_FILE=${EMCONLINE_TRG_DIR}/last_log.emconline_trg.txt
export EMCONLINE_TRG_RUNMODE_FILE=${EMCONLINE_TRG_DIR}/RUNMODE.emconline_trg
export EMCONLINE_TRG_LASTRUN_FILE=${EMCONLINE_TRG_DIR}/LAST_TIME_RUN.emconline_trg

export EMCONLINE_TRG_BACKUP_DIR=${EMCONLINE_TRG_DIR}/backup.emconline_trg
export EMCONLINE_TRG_LOGS_DIR=${EMCONLINE_TRG_DIR}/logs.emconline_trg
export EMCONLINE_TRG_TABLES_DIR=${EMCONLINE_TRG_DIR}/tables.emconline_trg/StarDb/Calibrations/emc/trigger
export EMCONLINE_TRG_CURRCONF_DIR=${EMCONLINE_TRG_DIR}/current_config.emconline_trg
export EMCONLINE_TRG_LASTCONF_DIR=${EMCONLINE_TRG_DIR}/last_config.emconline_trg

export EMCONLINE_TRG_BCECONF_FILE=${EMCONLINE_TRG_DIR}/bce_table.txt
export EMCONLINE_TRG_BCWCONF_FILE=${EMCONLINE_TRG_DIR}/bcw_table.txt

export EMCONLINE_TRG_BEMCSTATUS_FILE=${EMCONLINE_TRG_DIR}/bemcStatus.txt
export EMCONLINE_TRG_BEMCSTATUS_CONSUMERS=""

export EMCONLINE_TRG_STARVER=dev
export EMCONLINE_TRG_SAVEDB=true
export EMCONLINE_TRG_SAVETABLES=true

export CRATE_CONFIG_FILES_MASK="config_crate0x??.dat"
export CRATE_CONFIG_FILES="\
config_crate0x01.dat \
config_crate0x02.dat \
config_crate0x03.dat \
config_crate0x04.dat \
config_crate0x05.dat \
config_crate0x06.dat \
config_crate0x07.dat \
config_crate0x08.dat \
config_crate0x09.dat \
config_crate0x0a.dat \
config_crate0x0b.dat \
config_crate0x0c.dat \
config_crate0x0d.dat \
config_crate0x0e.dat \
config_crate0x0f.dat \
config_crate0x10.dat \
config_crate0x11.dat \
config_crate0x12.dat \
config_crate0x13.dat \
config_crate0x14.dat \
config_crate0x15.dat \
config_crate0x16.dat \
config_crate0x17.dat \
config_crate0x18.dat \
config_crate0x19.dat \
config_crate0x1a.dat \
config_crate0x1b.dat \
config_crate0x1c.dat \
config_crate0x1d.dat \
config_crate0x1e.dat \
"

export CRATE_PEDESTAL_FILES_MASK="pedestal_crate0x??.dat"
export CRATE_PEDESTAL_FILES="\
pedestal_crate0x01.dat \
pedestal_crate0x02.dat \
pedestal_crate0x03.dat \
pedestal_crate0x04.dat \
pedestal_crate0x05.dat \
pedestal_crate0x06.dat \
pedestal_crate0x07.dat \
pedestal_crate0x08.dat \
pedestal_crate0x09.dat \
pedestal_crate0x0a.dat \
pedestal_crate0x0b.dat \
pedestal_crate0x0c.dat \
pedestal_crate0x0d.dat \
pedestal_crate0x0e.dat \
pedestal_crate0x0f.dat \
pedestal_crate0x10.dat \
pedestal_crate0x11.dat \
pedestal_crate0x12.dat \
pedestal_crate0x13.dat \
pedestal_crate0x14.dat \
pedestal_crate0x15.dat \
pedestal_crate0x16.dat \
pedestal_crate0x17.dat \
pedestal_crate0x18.dat \
pedestal_crate0x19.dat \
pedestal_crate0x1a.dat \
pedestal_crate0x1b.dat \
pedestal_crate0x1c.dat \
pedestal_crate0x1d.dat \
pedestal_crate0x1e.dat \
"

export CRATES_CONFIG_FILE_MASK="BemcConfig.dat"
export CRATES_CONFIG_FILE="\
BemcConfig.dat \
"

export DSM_MASK_FILES_MASK="bc?.lut.bin"
export DSM_MASK_FILES="\
bce.lut.bin \
bcw.lut.bin \
"

########################################################
# Most recent real configuration in the control room

# Directory that contains pedestal files
if [[ "${EMCONLINE_SLOWCTRL_PED_DIR}" == "" ]] ; then export EMCONLINE_SLOWCTRL_PED_DIR='sysuser@sc3.starp.bnl.gov:/export/home/users/sysuser/epics/R3.12.2-LBL.4/radstone/unix' ; fi

# Directory that contains crate configuration files
if [[ "${EMCONLINE_SLOWCTRL_CFG_DIR}" == "" ]] ; then export EMCONLINE_SLOWCTRL_CFG_DIR='sysuser@sc3.starp.bnl.gov:/export/home/users/sysuser/epics/R3.12.2-LBL.4/radstone/unix' ; fi

# Directory that contains DSM mask files
if [[ "${EMCONLINE_SLOWCTRL_DSMMASK_DIR}" == "" ]] ; then export EMCONLINE_SLOWCTRL_DSMMASK_DIR='staruser@startrg2.starp.bnl.gov:/home/startrg/trg/cfg/Tier1/DSM_LUT' ; fi

# Directory that contains EMC Pplots installation
if [[ "${EMCONLINE_PPLOTS_DIR}" == "" ]] ; then export EMCONLINE_PPLOTS_DIR='/home/emc/online/emc/pplots' ; fi

export EMCONLINE_TRG_BEMCSTATUS_CONSUMERS="\
staruser@startrg2.starp.bnl.gov:/home/startrg/trg/cfg/Tier1/DSM_LUT \
bemc@evp.starp.bnl.gov:/home_local/bemc \
bemc@evp.starp.bnl.gov:/evp/a \
${EMCONLINE_PPLOTS_DIR} \
"

if [[ "${SCP}" == "" ]] ; then export SCP='/home/emc/online/emc/scp' ; fi

########################################################

########################################################
# External setup required for this monitoring to run

# Directory that contains pedestal files
if [[ "${EMCONLINE_SLOWCTRL_PED_DIR}" == "" ]]
then
    export EMCONLINE_SLOWCTRL_PED_DIR='~/emconline_slowctrl/pedestals'
    echo "EMC Slow Control pedestals directory not specified, assuming ${EMCONLINE_SLOWCTRL_PED_DIR}"
fi

# Directory that contains crate configuration files
if [[ "${EMCONLINE_SLOWCTRL_CFG_DIR}" == "" ]]
then
    export EMCONLINE_SLOWCTRL_CFG_DIR='~/emconline_slowctrl/configuration'
    echo "EMC Slow Control crate configurations directory not specified, assuming ${EMCONLINE_SLOWCTRL_CFG_DIR}"
fi

# Directory that contains DSM mask files
if [[ "${EMCONLINE_SLOWCTRL_DSMMASK_DIR}" == "" ]]
then
    export EMCONLINE_SLOWCTRL_DSMMASK_DIR='~/emconline_slowctrl/dsmmask'
    echo "EMC Slow Control DSM masks directory not specified, assuming ${EMCONLINE_SLOWCTRL_DSMMASK_DIR}"
fi
########################################################

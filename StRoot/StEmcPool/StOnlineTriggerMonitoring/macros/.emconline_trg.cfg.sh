# main directory
export EMCONLINE_TRG_DIR=/ldaphome/onlmon/bemctrgdb2013

# web output directory
export EMCONLINE_TRG_WEBDIR=/onlineweb/www/test2012/stevens4/bemctrgdb2013

# ROOT script
export EMCONLINE_TRG_SCRIPT=${EMCONLINE_TRG_DIR}/saveTriggerLoad.C

# Command to enable monitoring
export EMCONLINE_TRG_START="${EMCONLINE_TRG_DIR}/status.emconline_trg RUN"
# Command to disable monitoring
export EMCONLINE_TRG_STOP="${EMCONLINE_TRG_DIR}/status.emconline_trg STOP"
# Command to run monitoring
export EMCONLINE_TRG_UPDATE=${EMCONLINE_TRG_DIR}/update.emconline_trg

# Log file from the last run
export EMCONLINE_TRG_LOG_FILE=${EMCONLINE_TRG_DIR}/last_log.emconline_trg.txt
# File that contains RUN or STOP, shows if the monitoring is enabled or not
export EMCONLINE_TRG_RUNMODE_FILE=${EMCONLINE_TRG_DIR}/RUNMODE.emconline_trg
# File that contains the current status of the job
export EMCONLINE_TRG_RUNSTATUS_FILE=${EMCONLINE_TRG_DIR}/RUNSTATUS.emconline_trg
# Short summary from the last run
export EMCONLINE_TRG_LASTRUN_FILE=${EMCONLINE_TRG_DIR}/LAST_TIME_RUN.emconline_trg

# Directory for the backup files
export EMCONLINE_TRG_BACKUP_DIR=${EMCONLINE_TRG_DIR}/backup.emconline_trg
# Directory for the logs backup
export EMCONLINE_TRG_LOGS_DIR=${EMCONLINE_TRG_DIR}/logs.emconline_trg
# Directory to store the DB tables
export EMCONLINE_TRG_TABLES_DIR=${EMCONLINE_TRG_DIR}/tables.emconline_trg/StarDb/Calibrations/emc/trigger
# Directory for the config files currently being processed
export EMCONLINE_TRG_CURRCONF_DIR=${EMCONLINE_TRG_DIR}/current_config.emconline_trg
# Directory for hte last seen configuration, to be compared with the latest downloaded files
export EMCONLINE_TRG_LASTCONF_DIR=${EMCONLINE_TRG_DIR}/last_config.emconline_trg

# Mapping between DSM addresses and trigger patches
export EMCONLINE_TRG_BCECONF_FILE=${EMCONLINE_TRG_DIR}/bce_table.txt
export EMCONLINE_TRG_BCWCONF_FILE=${EMCONLINE_TRG_DIR}/bcw_table.txt

# bemcStatus.txt file with the latest BEMC configuration
export EMCONLINE_TRG_BEMCSTATUS_FILE=${EMCONLINE_TRG_DIR}/bemcStatus.txt

# web page
export EMCONLINE_TRG_WEBTEMPLATE_BEGIN=${EMCONLINE_TRG_DIR}/index_template_begin.html
export EMCONLINE_TRG_WEBTEMPLATE_LINE=${EMCONLINE_TRG_DIR}/index_template_line.html
export EMCONLINE_TRG_WEBTEMPLATE_END=${EMCONLINE_TRG_DIR}/index_template_end.html
export EMCONLINE_TRG_WEBPAGE=${EMCONLINE_TRG_WEBDIR}/index.html
export EMCONLINE_TRG_WEBPAGE_BODY=${EMCONLINE_TRG_WEBDIR}/index_body.html

# EPICS monitoring
export EPICS_CAPUT_CMD=/ldaphome/onlmon/slowcontrolinterface/base-3.14.10/bin/linux-x86/caput
export EMCONLINE_TRG_EPICS_CHANNEL=monit_bemctrgdb
export EMCONLINE_TRG_EPICS_HEARTBEAT=${EMCONLINE_TRG_DIR}/heartbeat.emconline_trg
export EMCONLINE_TRG_EPICS_STOPPED=${EMCONLINE_TRG_DIR}/stopped.emconline_trg

# STAR library version
export EMCONLINE_TRG_STARVER=dev
# Save tables into the DB?
export EMCONLINE_TRG_SAVEDB=false
# Save tables on disk?
export EMCONLINE_TRG_SAVETABLES=true

# Update the desktop icon when status changes?
export EMCONLINE_TRG_UPDATEDESKTOPICON=false
# Desktop icon file
export EMCONLINE_TRG_DESKTOPICON_FILE=~/.gnome-desktop/emconline_trg.desktop

function update_desktop_icon_trg () {
    if [[ "${EMCONLINE_TRG_UPDATEDESKTOPICON}" == "true" ]]
    then
	export MODE=""
	if [[ -f ${EMCONLINE_TRG_RUNMODE_FILE} ]] ; then export MODE=`cat ${EMCONLINE_TRG_RUNMODE_FILE}` ; fi
	export MODE=`echo ${MODE}`
	if [[ "${MODE}" == "RUN" ]] ; then export MODESTR="enabled" ; else export MODESTR="disabled" ; fi

	export MODENEW=RUN
	if [[ "${MODE}" == "RUN" ]] ; then export MODENEW=STOP ; fi
	if [[ "${MODENEW}" == "RUN" ]] ; then export MODENEWSTR="enable" ; else export MODENEWSTR="disable" ; fi

	export STATUS=""
	if [[ -f ${EMCONLINE_TRG_RUNSTATUS_FILE} ]] ; then export STATUS=`cat ${EMCONLINE_TRG_RUNSTATUS_FILE}` ; fi
	export STATUS=`echo ${STATUS}`

	export NAMESTR="Trigger monitoring"
	if [[ "${MODE}" != "" ]] ; then export NAMESTR="${NAMESTR}: ${MODESTR} (click here to ${MODENEWSTR})" ; fi
	if [[ "${STATUS}" != "" ]] ; then export NAMESTR="${NAMESTR} - ${STATUS}" ; fi

	touch ${EMCONLINE_TRG_DESKTOPICON_FILE}
	chmod a+x ${EMCONLINE_TRG_DESKTOPICON_FILE}
	echo "[Desktop Entry]" >| ${EMCONLINE_TRG_DESKTOPICON_FILE}
	echo "Encoding=UTF-8" >> ${EMCONLINE_TRG_DESKTOPICON_FILE}
	echo "Version=1.0" >> ${EMCONLINE_TRG_DESKTOPICON_FILE}
	echo "Type=Application" >> ${EMCONLINE_TRG_DESKTOPICON_FILE}
	echo "Exec=\"${EMCONLINE_TRG_DIR}/status.emconline_trg ${MODENEW}\"" >> ${EMCONLINE_TRG_DESKTOPICON_FILE}
	echo "TryExec=" >> ${EMCONLINE_TRG_DESKTOPICON_FILE}
	echo "X-GNOME-DocPath=" >> ${EMCONLINE_TRG_DESKTOPICON_FILE}
	echo "Terminal=true" >> ${EMCONLINE_TRG_DESKTOPICON_FILE}
	echo "Name[en_US]=\"${NAMESTR}\"" >> ${EMCONLINE_TRG_DESKTOPICON_FILE}
	echo "GenericName[en_US]=" >> ${EMCONLINE_TRG_DESKTOPICON_FILE}
	echo "Comment[en_US]=\"EMC Online trigger monitoring\"" >> ${EMCONLINE_TRG_DESKTOPICON_FILE}
    else
	rm -f ${EMCONLINE_TRG_DESKTOPICON_FILE}
    fi
}

########################################################
# Most recent real configuration in the control room

# Directory that contains pedestal files
if [[ "${EMCONLINE_SLOWCTRL_PED_DIR}" == "" ]] ; then export EMCONLINE_SLOWCTRL_PED_DIR='sysuser@sc5.starp.bnl.gov:/home/sysuser/GUI/emc/unix' ; fi

# Directory that contains crate configuration files
if [[ "${EMCONLINE_SLOWCTRL_CFG_DIR}" == "" ]] ; then export EMCONLINE_SLOWCTRL_CFG_DIR='sysuser@sc5.starp.bnl.gov:/home/sysuser/GUI/emc/unix' ; fi

# Directory that contains DSM mask files
if [[ "${EMCONLINE_SLOWCTRL_DSMMASK_DIR}" == "" ]] ; then export EMCONLINE_SLOWCTRL_DSMMASK_DIR='staruser@startrg.starp.bnl.gov:/home/startrg/trg/cfg/Tier1/DSM_LUT' ; fi

# Directory that contains pedestal monitoring installation
#if [[ "${EMCONLINE_PED_DIR}" == "" ]] ; then export EMCONLINE_PED_DIR='/home/emc/online/emc/pedestal' ; fi

if [[ "${EMCONLINE_TRG_BEMCSTATUS_CONSUMERS}" == "" ]] ; then export EMCONLINE_TRG_BEMCSTATUS_CONSUMERS="\
staruser@startrg.starp.bnl.gov:/home/startrg/trg/cfg/Tier1/DSM_LUT \
evpops@evp.starp.bnl.gov:/RTScache/conf/jevp/bemc \
" ; fi
#evpops@evp.starp.bnl.gov:/a/pplot/files/bemc \ #old location for pplots

if [[ "${SCP}" == "" ]] ; then export SCP="${EMCONLINE_TRG_DIR}/scp" ; fi

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

# Crate configuration files
if [[ "${CRATE_CONFIG_FILES_MASK}" == "" ]] ; then export CRATE_CONFIG_FILES_MASK="config_crate0x??.dat" ; fi
if [[ "${CRATE_CONFIG_FILES}" == "" ]] ; then export CRATE_CONFIG_FILES="\
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
" ; fi

# Crate pedestal files
if [[ "${CRATE_PEDESTAL_FILES_MASK}" == "" ]] ; then export CRATE_PEDESTAL_FILES_MASK="pedestal_crate0x??.dat" ; fi
if [[ "${CRATE_PEDESTAL_FILES}" == "" ]] ; then export CRATE_PEDESTAL_FILES="\
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
" ; fi

# Main configuration file
if [[ "${CRATES_CONFIG_FILE_MASK}" == "" ]] ; then export CRATES_CONFIG_FILE_MASK="BemcConfig.dat" ; fi
if [[ "${CRATES_CONFIG_FILE}" == "" ]] ; then export CRATES_CONFIG_FILE="\
BemcConfig.dat \
" ; fi

# DSM mask files
if [[ "${DSM_MASK_FILES_MASK}" == "" ]] ; then export DSM_MASK_FILES_MASK="bc?.lut.bin" ; fi
if [[ "${DSM_MASK_FILES}" == "" ]] ; then export DSM_MASK_FILES="\
bce.lut.bin \
bcw.lut.bin \
" ; fi
########################################################

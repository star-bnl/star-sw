
# ROOT script to run the pedestal monitoring
export EMCONLINE_PED_SCRIPT=${EMCONLINE_PED_DIR}/makeOnlinePed.C

# Command to enable monitoring
export EMCONLINE_PED_START="${EMCONLINE_PED_DIR}/status.emconline_ped RUN"
# Command to disable monitoring
export EMCONLINE_PED_STOP="${EMCONLINE_PED_DIR}/status.emconline_ped STOP"
# Command to run monitoring
export EMCONLINE_PED_UPDATE=${EMCONLINE_PED_DIR}/update.emconline_ped

# Log file from the last run
export EMCONLINE_PED_LOG_FILE=${EMCONLINE_PED_DIR}/last_log.emconline_ped.txt
# File that contains current status, if monitoring is enabled or not, RUN or STOP
export EMCONLINE_PED_RUNMODE_FILE=${EMCONLINE_PED_DIR}/RUNMODE.emconline_ped
# File that contains current status, what is running now
export EMCONLINE_PED_RUNSTATUS_FILE=${EMCONLINE_PED_DIR}/RUNSTATUS.emconline_ped
# List of runs in the event pool from the last run
export EMCONLINE_PED_FILELIST_FILE=${EMCONLINE_PED_DIR}/runlist.emconline_ped.txt
# Veto file, runs from this file will be excluded from processing
export EMCONLINE_PED_FILELIST_VETO_FILE=${EMCONLINE_PED_DIR}/runlist_veto.emconline_ped.txt
# Summary from the last run
export EMCONLINE_PED_LASTRUN_FILE=${EMCONLINE_PED_DIR}/LAST_TIME_RUN.emconline_ped
# Local copy of bemcStatus.txt
export EMCONLINE_PED_BEMCSTATUS_FILE=${EMCONLINE_PED_DIR}/bemcStatus.txt

# Directory for the backup histograms
export EMCONLINE_PED_BACKUP_DIR=${EMCONLINE_PED_DIR}/backup.emconline_ped
# Directory for the logs backup
export EMCONLINE_PED_LOGS_DIR=${EMCONLINE_PED_DIR}/logs.emconline_ped
# Temporary directory
export EMCONLINE_PED_TEMP_DIR=${EMCONLINE_PED_DIR}/tmp.emconline_ped
# Directory where the DB tables are saved
export EMCONLINE_PED_TABLES_DIR=${EMCONLINE_PED_DIR}/tables.emconline_ped/StarDb/Calibrations/emc
# Directory where the last DB tables are saved
export EMCONLINE_PED_LASTTABLES_DIR=${EMCONLINE_PED_DIR}/tables.emconline_ped/last
# ROOT script to make a DB tables from the backup histograms
export EMCONLINE_PED_TABLES_SCRIPT=${EMCONLINE_PED_DIR}/transformBackupHistoToDBTable.C

# STAR library version
export EMCONLINE_PED_STARVER=dev
# Number of events needed to calculate pedestal
export EMCONLINE_PED_NEVENTS=1000
# Save tables into the DB?
export EMCONLINE_PED_SAVEDB=true
# Save tables on disk?
export EMCONLINE_PED_SAVETABLES=true
# Produce one tables per run? (otherwise, one table per day)
export EMCONLINE_PED_TABLEPERRUN=true
# Process the last day's runs only? (otherwise, all available runs in the event pool)
export EMCONLINE_PED_LASTDAYONLY=false
# Exclude runs from the veto file?
export EMCONLINE_PED_USEVETOFILE=true
# Compare peds to the last saved DB table before saving?
export EMCONLINE_PED_COMPARELASTTABLEDB=true
# Ignore towers masked out in bemcStatus.txt?
export EMCONLINE_PED_USEBEMCSTATUS=true
# Deliver new pedestal files to EMCONLINE_SLOWCTRL_PEDMON_DIR?
export EMCONLINE_PED_DELIVERPEDFILES=true

# BTOW
# Min ped difference from the last saved table, for table to be saved, in terms of RMS
export EMCONLINE_PED_MINPEDDIFFDB_BTOW=2.0
# Number of towers that must have ped difference above minimum to save the table
export EMCONLINE_PED_MINPEDDIFFNUM_BTOW=3
# Min time period after which the table must be saved
export EMCONLINE_PED_MINPEDDIFFMINTIME_BTOW=86400

# Same for BPRS
# No matter how pedestals change, save once per day
export EMCONLINE_PED_MINPEDDIFFDB_BPRS=100000
export EMCONLINE_PED_MINPEDDIFFNUM_BPRS=100000
export EMCONLINE_PED_MINPEDDIFFMINTIME_BPRS=86400

# Same for BSMD
# No matter how pedestals change, save once per day
export EMCONLINE_PED_MINPEDDIFFDB_BSMD=100000
export EMCONLINE_PED_MINPEDDIFFNUM_BSMD=100000
export EMCONLINE_PED_MINPEDDIFFMINTIME_BSMD=86400

# Update the desktop icon when the current status changes?
export EMCONLINE_PED_UPDATEDESKTOPICON=true
# The desktop icon file
export EMCONLINE_PED_DESKTOPICON_FILE=~/.gnome-desktop/emconline_ped.desktop

function update_desktop_icon_ped () {
    if [[ "${EMCONLINE_PED_UPDATEDESKTOPICON}" == "true" ]]
    then
        export MODE=""
	if [[ -f ${EMCONLINE_PED_RUNMODE_FILE} ]] ; then export MODE=`cat ${EMCONLINE_PED_RUNMODE_FILE}` ; fi
	export MODE=`echo ${MODE}`
	if [[ "${MODE}" == "RUN" ]] ; then export MODESTR="enabled" ; else export MODESTR="disabled" ; fi

        export MODENEW=RUN
        if [[ "${MODE}" == "RUN" ]] ; then export MODENEW=STOP ; fi
	if [[ "${MODENEW}" == "RUN" ]] ; then export MODENEWSTR="enable" ; else export MODENEWSTR="disable" ; fi

        export STATUS=""
        if [[ -f ${EMCONLINE_PED_RUNSTATUS_FILE} ]] ; then export STATUS=`cat ${EMCONLINE_PED_RUNSTATUS_FILE}` ; fi
        export STATUS=`echo ${STATUS}`

        export NAMESTR="Pedestal monitoring"
        if [[ "${MODE}" != "" ]] ; then export NAMESTR="${NAMESTR}: ${MODESTR} (click here to ${MODENEWSTR})" ; fi
        if [[ "${STATUS}" != "" ]] ; then export NAMESTR="${NAMESTR} - ${STATUS}" ; fi

        touch ${EMCONLINE_PED_DESKTOPICON_FILE}
        chmod a+x ${EMCONLINE_PED_DESKTOPICON_FILE}
        echo "[Desktop Entry]" >| ${EMCONLINE_PED_DESKTOPICON_FILE}
        echo "Encoding=UTF-8" >> ${EMCONLINE_PED_DESKTOPICON_FILE}
        echo "Version=1.0" >> ${EMCONLINE_PED_DESKTOPICON_FILE}
        echo "Type=Application" >> ${EMCONLINE_PED_DESKTOPICON_FILE}
        echo "Exec=\"${EMCONLINE_PED_DIR}/status.emconline_ped ${MODENEW}\"" >> ${EMCONLINE_PED_DESKTOPICON_FILE}
        echo "TryExec=" >> ${EMCONLINE_PED_DESKTOPICON_FILE}
        echo "X-GNOME-DocPath=" >> ${EMCONLINE_PED_DESKTOPICON_FILE}
        echo "Terminal=true" >> ${EMCONLINE_PED_DESKTOPICON_FILE}
        echo "Name[en_US]=\"${NAMESTR}\"" >> ${EMCONLINE_PED_DESKTOPICON_FILE}
        echo "GenericName[en_US]=" >> ${EMCONLINE_PED_DESKTOPICON_FILE}
        echo "Comment[en_US]=\"EMC Online pedestal monitoring\"" >> ${EMCONLINE_PED_DESKTOPICON_FILE}
    else
        rm -f ${EMCONLINE_PED_DESKTOPICON_FILE}
    fi
}

##################################################################
# Most recent real configuration in the control room

# EVP_READER_ library location
if [[ "${EVP_READER_LIB}" == "" ]] ; then export EVP_READER_LIB='/home/emc/online/emc/libevpSO.2.0.so' ; fi

# Event pool mounting point
if [[ "${EVP_DIR}" == "" ]] ; then export EVP_DIR='/evp' ; fi

# Directory to upload pedestal files into
if [[ "${EMCONLINE_SLOWCTRL_PEDMON_DIR}" == "" ]] ; then export EMCONLINE_SLOWCTRL_PEDMON_DIR='sysuser@sc3.starp.bnl.gov:/export/home/users/sysuser/epics/R3.12.2-LBL.4/radstone/unix/emconline_slowctrl/pedestal_crate.emconline_ped' ; fi

if [[ "${SCP}" == "" ]] ; then export SCP='/home/emc/online/emc/scp' ; fi
##################################################################

##################################################################
# External setup required for this monitoring to run

# EVP_READER library location
if [[ "${EVP_READER_LIB}" == "" ]]
then
    export EVP_READER_LIB='libevpSO.2.0.so'
    echo "EVP_READER library location is not specified, assuming ${EVP_READER_LIB}"
fi

# Event pool directory mount point
if [[ "${EVP_DIR}" == "" ]]
then
    export EVP_DIR='/evp'
    echo "Event pool directory is not specified, assuming ${EVP_DIR}"
fi

# Directory to upload crate pedestal files into
if [[ "${EMCONLINE_SLOWCTRL_PEDMON_DIR}" == "" ]]
then
    export EMCONLINE_SLOWCTRL_PEDMON_DIR='~/emconline_slowctrl/pedestal_crate.emconline_ped'
    echo "EMC Slow Control pedestals directory for uploading not specified, assuming ${EMCONLINE_SLOWCTRL_PEDMON_DIR}"
fi

if [[ "${CRATE_PEDESTAL_FILES_MASK}" == "" ]] ; then export CRATE_PEDESTAL_FILES_MASK="pedestal_crate0x??.dat" ; fi
if [[ "${CRATE_PEDESTAL_FILES_FORMAT}" == "" ]] ; then export CRATE_PEDESTAL_FILES_FORMAT="pedestal_crate0x%02x.dat" ; fi
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
##################################################################

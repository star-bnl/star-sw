#! /usr/local/bin/tcsh -f
#set list = "StFtpcClusterMaker StFtpcTrackMaker StFtpcV0Maker StLaserEventMaker StMagF StMinidaqMaker StTrsMaker St_ctf_Maker St_dst_Maker St_ebye_Maker St_ems_Maker St_fmg_Maker St_fpt_Maker St_fss_Maker St_l3Clufi_Maker St_l3t_Maker St_mwc_Maker St_rl0_Maker St_srs_Maker St_stk_Maker St_tcl_Maker St_tpt_Maker St_tptsts_Maker"
set list = "StEbyeScaTagsMaker StEclMaker StFtpcV0Maker StPreEclMaker StSvtDaqMaker St_io_Maker"
foreach name ($list)
    grep $name $CVSROOT/CVSROOT/avail | awk -F\| '{print $2}' | awk -F\, '{for (i=1;i<=NF;i++) print $i;}'

end
#e-o-f


<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<html>
  <head>
    <title>Track by track comparision for 2013 pp510W sample with different compilers</title>
  </head>
  <body>
    <h1>Track by track comparision for 2013 pp510W sample with different compilers</h1>
<hline>
    <table width="90%" border="1" cellspacing="2" cellpadding="0">
<hline>
	<?php
$dirs = array(
            'gcc482/ia32/debug',
            'gcc482/ia32/opt',
            'gcc482/x8664/debug',
            'gcc482/x8664/opt',
            'gcc492/ia32/debug',
            'gcc492/ia32/opt',
            'gcc492/x8664/debug',
            'gcc492/x8664/opt',
            'gcc521/x8664/debug',
            'gcc521/x8664/opt'
	    );
$figs = array(
	      'Fit_Points_Corr.png',
	      'Fit_Points_CorrFit_Points_Corr_Matched.png',
	      'Global_track_efficiencies_vs_pT.png',
	      'Global_track_efficiencies_vs_pT_for_All.png',
	      'Global_track_efficiencies_vs_pT_for_0_lt_Mult_lt_200.png',
	      'Global_track_efficiencies_vs_pT_for_200_lt_Mult_lt_400.png',
	      'Global_track_efficiencies_vs_pT_for_400_lt_Mult_lt_600.png',
	      'Global_track_efficiencies_vs_pT_for_Mult_gt_600.png',
	      'Global_track_efficiencies_vs_phi.png',
	      'Global_track_efficiencies_vs_phi_for_All.png',
	      'Global_track_efficiencies_vs_phi_for_0_lt_Mult_lt_200.png',
	      'Global_track_efficiencies_vs_phi_for_200_lt_Mult_lt_400.png',
	      'Global_track_efficiencies_vs_phi_for_400_lt_Mult_lt_600.png',
	      'Global_track_efficiencies_vs_phi_for_Mult_gt_600.png',
	      'Gl_Momentum_Dist_for_Global.png',
	      'ltp_T_Old_-_p_T_New_gt_versus_p_T_for_Global.png',
	      'pT_diff_perc_vs_pT_for_Global.png',
	      'Inv_pT_diff_perc_vs_pT_for_Global.png',
	      'Inv_pT_diff_perc_vs_pT_for_GlobalSigma.png',
	      'P_diff_vs_NoFitPnts_for_Global.png',
	      'P_diff_vs_NoFitPnts_for_Global_Shift.png',
	      'P_diff_vs_NoFitPnts_for_Global_Sigma.png',
	      'P_diff_vs_NoFitPnts_for_Global_with_pT_gt_0_5_.png',
	      'P_diff_vs_NoFitPnts_for_Global_with_pT_gt_0_5_Shift.png',
	      'P_diff_vs_NoFitPnts_for_Global_with_pT_gt_0_5_Sigma.png',
	      'Primary_track_efficiencies_vs_pT.png',
	      'Primary_track_efficiencies_vs_pT_for_All.png',
	      'Primary_track_efficiencies_vs_pT_for_0_lt_Mult_lt_200.png',
	      'Primary_track_efficiencies_vs_pT_for_200_lt_Mult_lt_400.png',
	      'Primary_track_efficiencies_vs_pT_for_400_lt_Mult_lt_600.png',
	      'Primary_track_efficiencies_vs_pT_for_Mult_gt_600.png',
	      'Primary_track_efficiencies_vs_phi.png',
	      'Primary_track_efficiencies_vs_phi_for_All.png',
	      'Primary_track_efficiencies_vs_phi_for_0_lt_Mult_lt_200.png',
	      'Primary_track_efficiencies_vs_phi_for_200_lt_Mult_lt_400.png',
	      'Primary_track_efficiencies_vs_phi_for_400_lt_Mult_lt_600.png',
	      'Primary_track_efficiencies_vs_phi_for_Mult_gt_600.png',
	      'Pr_Momentum_Dist_for_Primary.png',
	      'ltp_T_Old_-_p_T_New_gt_versus_p_T_for_Primary.png',
	      'Inv_pT_diff_perc_vs_pT_for_Primary.png',
	      'Inv_pT_diff_perc_vs_pT_for_PrimarySigma.png',
	      'pT_diff_perc_vs_pT_for_Primary.png',
	      'pT_diff_perc_vs_pT_for_PrimarySigma.png',
	      'P_diff_vs_NoFitPnts_for_Primary.png',
	      'P_diff_vs_NoFitPnts_for_Primary_Shift.png',
	      'P_diff_vs_NoFitPnts_for_Primary_Sigma.png',
	      'P_diff_vs_NoFitPnts_for_Primary_with_pT_gt_0_5_.png',
	      'P_diff_vs_NoFitPnts_for_Primary_with_pT_gt_0_5_Shift.png',
	      'P_diff_vs_NoFitPnts_for_Primary_with_pT_gt_0_5_Sigma.png',
	      'PrimaryVertexXdiff.png',
	      'PrimaryVertexYdiff.png',
	      'PrimaryVertexZdiff.png',
	      'NewVsOldChargeAllPr.png',
	      'NewVsOldChargePrNFPGE15.png'
	      );
echo "<tr>";
foreach ($dirs as $dir) {
  echo  "<th>$dir (new) vs gcc492/ia32/debug (old)</th>";
}
echo "</tr>";
$figNo = 0;
foreach ($figs as $fig) {
  $figNo++;
  echo "<tr>";
  $dirNo = 0;
  foreach ($dirs as $dir) {
    $dirNo++;
    if ($dirNo == 1) {
      echo "<td><a name=\"Fig.$figNo\">Fig.$figNo</a>";
    } else {
      echo "<td>";
    }
    echo "<img src=\"$dir/$fig\" alt=\"\" width=\"400\" border=\"0\"></td>";
  }
   echo "</tr>";
}
	?>
	</table>
    <hr>
    <address><a href='mailto:fisyak@rcas6006.rcf.bnl.gov'>Yuri Fisyak</a></address>
  </body>
</html>

<?php
$chisq = array(
	       'ChiSqXY_zx_pfx'
	      ,'ChiSqXY_zy_pfx'
	      ,'ChiSqXY_x'
	      ,'ChiSqXY_y'
	      ,'ChiSqXY_yx_pfx'
	       );
$paramsG = array(
		 'dDcaXY_zx_1'
		 ,'dDcaXY_zy_1'
		 ,'dDcaZ_zx_1'
		 ,'dDcaZ_zy_1'
		 ,'dPsi_zx_1'
		 ,'dPsi_zy_1'
		 ,'dPti_zx_1'
		 ,'dPti_zy_1'
		 ,'dPtiR_zx_1'
		 ,'dPtiR_zy_1'
		 ,'dTanL_zx_1'
		 ,'dTanL_zy_1'
		 ,'pDcaXY_zx_1'
		 ,'pDcaXY_zy_1'
		 ,'pDcaZ_zx_1'
		 ,'pDcaZ_zy_1'
		 ,'pPsi_zx_1'
		 ,'pPsi_zy_1'
		 ,'pPti_zx_1'
		 ,'pPti_zy_1'
		 ,'pPtiR_zx_1'
		 ,'pPtiR_zy_1'
		 ,'pTanL_zx_1'
		 ,'pTanL_zy_1'
		 );
$paramsP = array(
		 'dPsi_zx_1'
		 ,'dPsi_zy_1'
		 ,'dPti_zx_1'
		 ,'dPti_zy_1'
		 ,'dPtiR_zx_1'
		 ,'dPtiR_zy_1'
		 ,'dTanL_zx_1'
		 ,'dTanL_zy_1'
		 ,'pPsi_zx_1'
		 ,'pPsi_zy_1'
		 ,'pPti_zx_1'
		 ,'pPti_zy_1'
		 ,'pPtiR_zx_1'
		 ,'pPtiR_zy_1'
	      );
$APhi = array('APhi_x','APhi_y','APhi_z');
$GPhi = array('GPhi_x','GPhi_y','GPhi_z');	      
$DD = '';
$figNo = 0;
//function Table($title,$section,$figNo,$DD,$dirs,$tag,$figs) {
function Table($title,$section,$dirs,$name,$tag,$figs) {
  // start table
  $html = '<h4><a name "' . $title . '">' . $section . ' ' . $title . '.</a></h4>
<table width="90%" border="1" cellspacing="2" cellpadding="0">
';
  // header row
  $html .= '<tr>
';
  //  var_dump($dirs);
  //   var_dump($figs);
  foreach($dirs as $dir) {
    $html .= '<th>' . $dir . '</th>
';
  }
  $html .= '</tr>
';
  // data rows
  foreach( $figs as $fig) {
    $figNo++;
    $html .= '<tr>';
    $dirNo = 0;
    foreach($dirs as $dir) {
      $dirNo++;
      if ($dirNo == 1) {
	$html .= '<td><a name="' . $section . 'Fig.' . $figNo. '">Fig.' . $figNo . '</a>
';
      } else {
	$html .= '<td>
';
      }
      $html .= '<img src="' . $DD . $dir .'/' . $tag . $fig . '.png'.'" alt="" width="400" border="0"></td>
';
    }
    $html .= '</tr>
';
  }
  // finish table and return it
  $html .= '</table>
';
  return $html;
}
?>

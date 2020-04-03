<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<html> 
<head>
<title>Reconstruction versus Simulation</title>
</head>
<body>
<h1>Reconstruction versus Simulation, G3 versus VMC, old versus new CA</h1>
<h2>Legend</h2>
<p>The plots are designed to answer on the following questions:
<ul>
<li> What tracks (globals  and primaries) can be considered as "good" ones depending on total no. of fit points and no. of bad hits ? </li>
<li> What is the track parameters and their errors dependence on the track kinematics (&phi;, &eta;, 1/pT) ? </li>
<li> What is the track paramerer pulls dependence on above kinematics ? </li>
<li> What is the track reconstruction efficiencies for : </li>
<ul>
   <li> Geometrical acceptance (MC only), </li>
   <li> Reconstruction effiency for track with only match between MC and RC </li>
   <li> Clones, for multiple (>1) match between single MC track to RC one, </li>
   <li> Lost tracks, MC tracks which have no RC partner. </li>
   <li> Ghost tracks, RC tracks which have no MC partner. </li>
   </ul> 
   <li> Color scheme: <font color=black>&nbsp;&bull; Positive</font> and <font color=red>&nbsp;&bull; Negative</font> Tracks. </li>
   <li> Results of Gauss fit for slices are presented as &bull; for &mu; and as  box for &sigma;.  </li>
   <li>Navigations:
</ul>
</ul>
<hr>
<?php
$dirs = array(
	      '2016',
	      '2017',
	      '2020',
	      '2021'
	      );
//var_dump($dirs);
include('/afs/rhic.bnl.gov/star/users/fisyak/WWW/star/muMc/indexMc.php');
?>

<H1>1. Tracks</H1>
<H2>1.1. Quality of reconstructed tracks with respect to MC.</H2>
<h3>1.1.1. Global tracks. </h3>
<?php echo Table('Global  tracks.  All.NoHits','1.1.1.1.',$dirs,'Global  tracks.  All. No. of fit and quality',                      'Tracks_Global_Rec_All_NoHits_',$chisq); ?>      
<?php echo Table('Global  tracks.  All.Kinema','1.1.1.2.',$dirs,'Global  tracks.  All.Track parameters',            			'Tracks_Global_Rec_All_EtapT_',$paramsG); ?>     
<?php echo Table('Global  tracks.  Pions.NoHits','1.1.1.3.',$dirs,'Global  tracks.  Pions. No. of fit and quality',			'Tracks_Global_Rec_Pion_NoHits_',$chisq); ?>   
<?php echo Table('Global  tracks.  Pions.Kinema','1.1.1.4.',$dirs,'Global  tracks.  Pions.Track parameters',       			'Tracks_Global_Rec_Pion_EtapT_',$paramsG); ?>    
<h3>1.1.2. Primary tracks. </h3>
<?php echo Table('Primary  tracks.  All.NoHits','1.1.2.1.',$dirs,'Primary  tracks.  All. No. of fit and quality',   			'Tracks_Primary_Rec_All_NoHits_',$chisq); ?> 
<?php echo Table('Primary  tracks.  All.Kinema','1.1.2.2.',$dirs,'Primary  tracks.  All.Track parameters',          			'Tracks_Primary_Rec_All_EtapT_',$paramsP); ?>  
<?php echo Table('Primary  tracks.  Pions.NoHits','1.1.2.3.',$dirs,'Primary  tracks.  Pions. No. of fit and quality',                'Tracks_Primary_Rec_Pion_NoHits_',$chisq); ?>
<?php echo Table('Primary  tracks.  Pions.Kinema','1.1.2.4.',$dirs,'Primary  tracks.  Pions.Track parameters',       		'Tracks_Primary_Rec_Pion_EtapT_',$paramsP); ?> 
<h3>1.2.1. Global tracks. </h3>
<?php echo Table('Global tracks.  All. Geometrical acceptance','1.2.1.1.',$dirs,'Global tracks.  All. Geometrical acceptance',      'Tracks_Global_Tpc_All_EtapT_',$APhi); ?>
<?php echo Table('Global tracks.  All. Efficiency over all','1.2.1.2.',$dirs,'Global tracks.  All. Efficiency over all',      	'Tracks_Global_Rec_All_EtapT_Eff',$APhi); ?> 
<?php echo Table('Global tracks.  All. Efficiency wrt Geom','1.2.1.3.',$dirs,'Global tracks.  All. Efficiency wrt Geom',	  	'Tracks_Global_Rec_All_EtapT_Eff',$GPhi); ?> 
<?php echo Table('Global tracks.  All. Clone  over all','1.2.1.4.',$dirs,'Global tracks.  All. Clone  over all',                    'Tracks_Global_Clone_All_EtapT_Clone',$APhi); ?>  
<?php echo Table('Global tracks.  All. Clone wrt Geom','1.2.1.5.',$dirs,'Global tracks.  All. Clone wrt Geom',     			'Tracks_Global_Clone_All_EtapT_Clone',$GPhi); ?>  
<?php echo Table('Global tracks.  All. Lost over all','1.2.1.6.',$dirs,'Global tracks.  All. Lost over all',       			'Tracks_Global_Lost_All_EtapT_Lost',$APhi); ?>    
<?php echo Table('Global tracks.  All. Lost wrt Geom','1.2.1.7.',$dirs,'Global tracks.  All. Lost wrt Geom',        		'Tracks_Global_Lost_All_EtapT_Lost',$GPhi); ?>    
<?php echo Table('Global tracks.  Pions. Geometrical acceptance','1.2.1.1.',$dirs,'Global tracks.  Pions. Geometrical acceptance',  'Tracks_Global_Tpc_Pion_EtapT_',$APhi); ?>	   
<?php echo Table('Global tracks.  Pions. Efficiency over all','1.2.1.2.',$dirs,'Global tracks.  Pions. Efficiency over all',      	'Tracks_Global_Rec_Pion_EtapT_Eff',$APhi); ?>	   
<?php echo Table('Global tracks.  Pions. Efficiency wrt Geom','1.2.1.3.',$dirs,'Global tracks.  Pions. Efficiency wrt Geom',      	'Tracks_Global_Rec_Pion_EtapT_Eff',$GPhi); ?>	   
<?php echo Table('Global tracks.  Pions. Clone  over all','1.2.1.4.',$dirs,'Global tracks.  Pions. Clone  over all',              	'Tracks_Global_Clone_Pion_EtapT_Clone',$APhi); ?> 
<?php echo Table('Global tracks.  Pions. Clone wrt Geom','1.2.1.5.',$dirs,'Global tracks.  Pions. Clone wrt Geom',                	'Tracks_Global_Clone_Pion_EtapT_Clone',$GPhi); ?> 
<?php echo Table('Global tracks.  Pions. Lost over all','1.2.1.6.',$dirs,'Global tracks.  Pions. Lost over all',                  	'Tracks_Global_Lost_Pion_EtapT_Lost',$APhi); ?>   
<h3>1.12.2. Primary tracks. </h3>
<?php echo Table('Primary tracks.  All. Geometrical acceptance','1.2.2.1.',$dirs,'Primary tracks.  All. Geometrical acceptance',     'Tracks_Primary_Tpc_All_EtapT_',$APhi); ?>	     
<?php echo Table('Primary tracks.  All. Efficiency over all','1.2.2.2.',$dirs,'Primary tracks.  All. Efficiency over all',         	'Tracks_Primary_Rec_All_EtapT_Eff',$APhi); ?>	     
<?php echo Table('Primary tracks.  All. Efficiency wrt Geom','1.2.2.3.',$dirs,'Primary tracks.  All. Efficiency wrt Geom',         	'Tracks_Primary_Rec_All_EtapT_Eff',$GPhi); ?>	     
<?php echo Table('Primary tracks.  All. Clone  over all','1.2.2.4.',$dirs,'Primary tracks.  All. Clone  over all',                 	'Tracks_Primary_Clone_All_EtapT_Clone',$APhi); ?>   
<?php echo Table('Primary tracks.  All. Clone wrt Geom','1.2.2.5.',$dirs,'Primary tracks.  All. Clone wrt Geom',                   	'Tracks_Primary_Clone_All_EtapT_Clone',$GPhi); ?>   
<?php echo Table('Primary tracks.  All. Lost over all','1.2.2.6.',$dirs,'Primary tracks.  All. Lost over all',                     	'Tracks_Primary_Lost_All_EtapT_Lost',$APhi); ?>     
<?php echo Table('Primary tracks.  All. Lost wrt Geom','1.2.2.7.',$dirs,'Primary tracks.  All. Lost wrt Geom',                     	'Tracks_Primary_Lost_All_EtapT_Lost',$GPhi); ?>     
<?php echo Table('Primary tracks.  All. Ghost over all','1.2.2.8.',$dirs,'Primary tracks.  All. Ghost over all',                   	'Tracks_Primary_Ghost_All_EtapT_Ghost',$APhi); ?>   
<?php echo Table('Primary tracks.  All. Ghost wrt Geom','1.2.2.9.',$dirs,'Primary tracks.  All. Ghost wrt Geom',                   	'Tracks_Primary_Ghost_All_EtapT_Ghost',$GPhi); ?>   
<?php echo Table('Primary tracks.  Pions. Geometrical acceptance','1.2.2.1.',$dirs,'Primary tracks.  Pions. Geometrical acceptance' ,'Tracks_Primary_Tpc_Pion_EtapT_',$APhi); ?>    
<?php echo Table('Primary tracks.  Pions. Efficiency over all','1.2.2.2.',$dirs,'Primary tracks.  Pions. Efficiency over all',       'Tracks_Primary_Rec_Pion_EtapT_Eff',$APhi); ?>	    
<?php echo Table('Primary tracks.  Pions. Efficiency wrt Geom','1.2.2.3.',$dirs,'Primary tracks.  Pions. Efficiency wrt Geom',     	'Tracks_Primary_Rec_Pion_EtapT_Eff',$GPhi); ?>	    
<?php echo Table('Primary tracks.  Pions. Clone  over all','1.2.2.4.',$dirs,'Primary tracks.  Pions. Clone  over all',             	'Tracks_Primary_Clone_Pion_EtapT_Clone',$APhi); ?> 
<?php echo Table('Primary tracks.  Pions. Clone wrt Geom','1.2.2.5.',$dirs,'Primary tracks.  Pions. Clone wrt Geom',               	'Tracks_Primary_Clone_Pion_EtapT_Clone',$GPhi); ?> 
<?php echo Table('Primary tracks.  Pions. Lost over all','1.2.2.6.',$dirs,'Primary tracks.  Pions. Lost over all',                 	'Tracks_Primary_Lost_Pion_EtapT_Lost',$APhi); ?>   
<?php echo Table('Primary tracks.  Pions. Lost wrt Geom','1.2.2.7.',$dirs,'Primary tracks.  Pions. Lost wrt Geom',                   'Tracks_Primary_Lost_Pion_EtapT_Lost',$GPhi); ?>   

<hr>
</body>
</html>

Hi all,

The files in this directory,

StRoot/StEEmcPool/StEEmcHitMaker

are the files which compute the "hits" for Part II of the EEmcTrees,
which are made with the makers in the directory

StRoot/StEEmcPool/StEEmcTreeMaker

This code is contained in a seperate directory as to distinguish code
which focuses on reconstructing particles (hits, i.e. this directory)
vs. code mainly concerned with the IO of making and reading trees (the
StEEmcTreeMaker directory).  There are also historical reasons for
this seperation. See, e.g., many more algorithms for SMD clustering at

$CVSROOT/offline/users/sgliske/StRoot/StEEmcPool/StEEmcHitMaker

--Steve Gliske (Nov, 2012)

/*
 * $Id: README.txt,v 1.1 2012/11/26 19:04:11 sgliske Exp $
 * $Log: README.txt,v $
 * Revision 1.1  2012/11/26 19:04:11  sgliske
 * creation
 *
 *
 */

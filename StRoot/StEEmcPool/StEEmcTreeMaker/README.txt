Hi all,

The files in this directory,

StRoot/StEEmcPool/StEEmcTreeMaker

containe the basic makers for making the EEmcTrees.  Directories also deeply involved in making the trees are:

StRoot/StEEmcPool/StEEmcHitMaker      (code for reconstructing particles, i.e. hits)
StRoot/StEEmcPool/EEmcTreeContainers  (the containers stored in the trees)

The containers are stored in a seperate directory, so that one only
needs this container directory to read the trees.  The files in the
"hit maker" directory are there to distinguish code which focuses on
reconstructing particles (hits) versus code mainly concerned with the
IO of making and reading trees. See also README.txt files in those
other directories.


--Steve Gliske (Nov, 2012)

/*
 * $Id: README.txt,v 1.1 2012/11/26 19:04:12 sgliske Exp $
 * $Log: README.txt,v $
 * Revision 1.1  2012/11/26 19:04:12  sgliske
 * creation
 *
 *
 */

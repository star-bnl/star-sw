Hi all,

The files in this directory,

StRoot/StEEmcPool/EEmcTreeContainers

are the files which are storted in the EEmcTrees, which are made with
the makers in the directory

StRoot/StEEmcPool/StEEmcTreeMaker

The containers are stored in a seperate directory, so that one only
needs this container directory to read the trees.  Specifically, one
can then easily copy just this container directory and generated trees
to an offline (non-RCF) node, generate the dictionaries, and read the
trees, without having to have the whole STAR St-framework available.
See the example macros for making and loading dictionaries without the
usual STAR framework in the macro directory.

--Steve Gliske (Nov, 2012)

/*
 * $Id: README.txt,v 1.1 2012/11/26 19:04:11 sgliske Exp $
 * $Log: README.txt,v $
 * Revision 1.1  2012/11/26 19:04:11  sgliske
 * creation
 *
 *
 */

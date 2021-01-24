#!/usr/bin/env bash

set -e

RED='\033[0;31m'
BLUE='\033[0;34m'
GREEN='\033[0;32m'
NC='\033[0m'


# Config
OPT="-aPh"
SRC="/home/$USER/"
DES="/mnt/backup"
SNAP="$DES/$USER"
LAST="$SNAP/last"

LINK="--link-dest=$LAST"
EXCLUDE='--exclude=.* --exclude=Downloads --exclude=**/node_modules/* --exclude=**/target/*'
DATE=`date "+%Y-%b-%d_%T"`

if ! mount | grep $DES > /dev/null; then
    echo "Back destination is not mounted!"
    exit 1
fi

mkdir -p $SNAP

if [[ -r $SNAP && -w $SNAP ]]; then
    echo "Permission are ok"
else
    echo "Missing permissions"
    exit 1
fi

# Run rsync to create snapshot
rsync $OPT $EXCLUDE $LINK $SRC ${SNAP}/$DATE

# if [ ! -L $LAST ]; then
#     printf "${BLUE}No system link to a previous backup...${NC}\n"
# else
#     rm -f $LAST
# fi

if [ -L ${LAST} ] ; then
   if [ -e ${LAST} ] ; then
      echo "Link was broken"
   fi
   rm -f $LAST
elif [ -e ${LAST} ] ; then
   echo "Not a link. Remove anyway?"
   rm -rf $LAST
fi

# Create new symlink to latest snapshot for the next backup to hardlink
ln -s ${SNAP}/$DATE $LAST

printf "${GREEN}Backup complete!${NC}\n"

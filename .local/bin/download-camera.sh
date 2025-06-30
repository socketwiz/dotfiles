#!/bin/bash
# Script to download all files from the Sony ILCE-7M4
#
gphoto2 --auto-detect
gphoto2 --get-all-files
echo "All files have been downloaded."

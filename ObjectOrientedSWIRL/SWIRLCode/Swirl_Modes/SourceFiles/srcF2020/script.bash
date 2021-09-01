#!/bin/bash
sed -f '39,$s/\<r\>/rC/g \
        39,$s/d0/_rDef/g' \
SourceTermMMS.f90 
# $@ means All positional arguments (as separate strings) and need quotes


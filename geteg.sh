#!/bin/bash
nelect=`grep 'NELECT' OUTCAR |tail -1|awk '{printf "%12.3f\n", $3}'`
hae=`echo "$nelect / 2.0" |bc -l  |awk '{printf "%d\n", $1}'`
lae=`echo  " $hae + 1" |bc -l`

echo 'Band index for VBM:' $hae
echo "############K-point index:      k-point,               Band-index,  Eigvalue,  Occu"
tmp_vbm=`grep  " $hae   " OUTCAR |cat -n |sed -e '/IALGO/d' | sort -k3 -g |tail -1 `
tmp_cbm=`grep  " $lae   " OUTCAR |cat -n |sed -e '/IALGO/d' | sort -k3 -g -r  |tail -1`

#evbm=`grep  " $hae   " OUTCAR | sed -e '/IALGO/d' |  sort -k2 -g |tail -1 |awk '{printf "%10.3f\n", $2}'`
#ecbm=`grep  " $lae   " OUTCAR | sed -e '/IALGO/d' | sort -k2 -g -r |tail -1 |awk '{printf "%10.3f\n", $2}'`
evbm=`echo "$tmp_vbm" | awk '{printf "%12.5f\n", $3}' `
ecbm=`echo "$tmp_cbm" | awk '{printf "%12.5f\n", $3}' `
kivbm=`echo "$tmp_vbm" | awk '{printf "%d\n", $1}' `
kicbm=`echo "$tmp_cbm" | awk '{printf "%d\n", $1}' `
kpvbm=`grep "k-point"  OUTCAR |grep " $kivbm : " |tail -1`
kpcbm=`grep "k-point"  OUTCAR |grep " $kicbm : " |tail -1`
bivbm=`echo "$tmp_vbm" | awk '{printf "%d\n", $2}' `
bicbm=`echo "$tmp_cbm" | awk '{printf "%d\n", $2}' `
occvbm=`echo "$tmp_vbm" | awk '{printf "%d\n", $4}' `
occcbm=`echo "$tmp_cbm" | awk '{printf "%d\n", $4}' `
echo "VBM at $kpvbm ,  $bivbm , $evbm , $occvbm"
echo "CBM at $kpcbm ,  $bicbm , $ecbm , $occcbm"
eg=`echo  "scale=3; $ecbm - $evbm" |bc -l`
echo "NELECT = $nelect ; EVBM = $evbm ; ECBM = $ecbm ; Eg = $eg"

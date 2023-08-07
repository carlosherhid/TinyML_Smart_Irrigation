#! /bin/bash
#case $(hostname) in
#  dibulibu) wd="/home/aurora/RiegoInteligente/code/"
#  ;;
#  juno) wd="/home/gardens/RiegoInteligente/code/"
#  ;;
#esac
#wd=$(dirname "$0")/
wd=/home/gardens/RiegoInteligente/code/
nohup Rscript ${wd}run.R -a -r -v -i > ${wd}logOUT.txt 2> ${wd}logERR.txt < /dev/null &
